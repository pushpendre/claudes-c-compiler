# AArch64 Built-in Assembler -- Design Document

## Overview

The built-in AArch64 assembler is a self-contained subsystem that translates
GNU-style assembly text (`.s` files), as emitted by the compiler's AArch64
codegen, into ELF64 relocatable object files (`.o`).  Its purpose is to
eliminate the external dependency on `aarch64-linux-gnu-gcc` for assembling,
making the compiler fully self-hosting on AArch64 Linux targets.

The assembler is activated when the environment variable `MY_ASM=builtin` is
set.  It accepts the same textual assembly that GCC's gas would consume and
produces ABI-compatible `.o` files that any standard AArch64 ELF linker (or
the companion built-in linker) can link.

The implementation spans roughly 6,260 lines of Rust across four files and is
organized as a clean three-stage pipeline.

```
                    AArch64 Built-in Assembler
  ================================================================

  .s assembly text
       |
       v
  +------------------+
  |   parser.rs       |   Stage 1: Tokenize + Parse
  |   (1,234 lines)   |   Lines -> AsmStatement[]
  +------------------+
       |
       | Vec<AsmStatement>
       v
  +------------------+
  |   elf_writer.rs   |   Stage 2: Process + Encode + Emit
  |   (797 lines)     |   Walks statements, calls encoder,
  |                    |   builds sections, symbols, relocs
  +------------------+
       |         ^
       |         |  encode_instruction()
       v         |
  +------------------+
  |   encoder.rs      |   Instruction Encoding Library
  |   (4,012 lines)   |   Mnemonic + Operands -> u32 words
  +------------------+
       |
       v
  ELF64 .o file on disk
```

The single public entry point is:

```rust
// mod.rs (221 lines)
pub fn assemble(asm_text: &str, output_path: &str) -> Result<(), String>
```

It calls `parse_asm()`, creates an `ElfWriter`, feeds it the parsed
statements, and writes the final `.o` file.


---

## Stage 1: Parser (`parser.rs`)

### Purpose

Convert raw assembly text into a structured, typed intermediate
representation -- a `Vec<AsmStatement>`.  Every subsequent stage works on this
IR; no raw text parsing happens after this point.

### Key Data Structures

| Type | Role |
|------|------|
| `AsmStatement` | Top-level IR node: `Label`, `Directive`, `Instruction`, or `Empty`. |
| `AsmDirective` | Fully-typed directive variant (24 kinds, from `.section` to `.cfi_*`). |
| `Operand` | Operand of an instruction (20 variants covering every AArch64 addressing mode). |
| `SectionDirective` | Parsed `.section name, "flags", @type` triple. |
| `DataValue` | Data that can be an integer, a symbol, a symbol+offset, or a symbol difference. |
| `SizeExpr` | The expression in `.size sym, expr` -- either a constant or `.- sym`. |
| `SymbolKind` | From `.type`: `Function`, `Object`, `TlsObject`, `NoType`. |

### Operand Variants

The `Operand` enum models every AArch64 operand shape the codegen can emit:

```
Reg("x0")                          -- general / FP / SIMD register
Imm(42)                            -- immediate value (#42)
Symbol("printf")                   -- bare symbol reference
SymbolOffset("arr", 16)            -- symbol + constant
Mem { base, offset }               -- [base, #offset]
MemPreIndex { base, offset }       -- [base, #offset]!
MemPostIndex { base, offset }      -- [base], #offset
MemRegOffset { base, index, .. }   -- [base, Xm, extend #shift]
Modifier { kind, symbol }          -- :lo12:symbol, :got_lo12:symbol
ModifierOffset { kind, sym, off }  -- :lo12:symbol+offset
Shift { kind, amount }             -- lsl #N
Extend { kind, amount }            -- sxtw #N
Cond("eq")                         -- condition code
Barrier("ish")                     -- barrier option
Label(".LBB0_4")                   -- branch target
RegArrangement { reg, arr }        -- v0.16b (NEON arrangement)
RegLane { reg, elem_size, index }  -- v0.d[1] (NEON lane)
RegList(Vec<Operand>)              -- {v0.16b, v1.16b}
RegListIndexed { regs, index }     -- {v0.s, v1.s}[0] (NEON single-element)
```

### Parsing Algorithm

```
parse_asm(text)
  0. Pre-pass: expand_rept_blocks() -- flatten .rept/.endr into repeated lines
  for each line in text:
    1. Trim whitespace
    2. Strip comments (// style and @ GAS-ARM style)
    3. Split on ';' (GAS multi-statement separator, respecting strings)
    4. For each sub-statement:
       a. Try to match "name:" -> Label(name)
          - .L* prefixed names are recognized as local labels
          - Any text after the colon is recursively parsed
       b. Try to match "." prefix -> parse_directive()
          - 24 directive types, each with its own sub-parser
       c. Otherwise -> parse_instruction()
          - Split mnemonic from operand string
          - parse_operands() splits on ',' respecting [] and {} nesting
          - Each token -> parse_single_operand()
          - Post-pass: merge [base], #offset into MemPostIndex
```

### Supported Directives

| Category | Directives |
|----------|-----------|
| Sections | `.section`, `.text`, `.data`, `.bss`, `.rodata`, `.pushsection`, `.popsection`/`.previous` |
| Symbols | `.globl`/`.global`, `.weak`, `.hidden`, `.protected`, `.internal`, `.type`, `.size`, `.local`, `.comm`, `.set`/`.equ` |
| Alignment | `.align`, `.p2align` (power-of-2), `.balign` (byte count) |
| Data emission | `.byte`, `.short`/`.hword`/`.2byte`, `.long`/`.4byte`/`.word`, `.quad`/`.8byte`/`.xword`, `.zero`/`.space`, `.ascii`, `.asciz`/`.string` |
| Repetition | `.rept` count / `.endr` (expanded as a pre-pass before parsing) |
| CFI | `.cfi_startproc`, `.cfi_endproc`, `.cfi_def_cfa_offset`, `.cfi_offset`, and 12 more (all passed through as no-ops) |
| Ignored | `.file`, `.loc`, `.ident`, `.addrsig`, `.addrsig_sym`, `.build_attributes`, `.eabi_attribute` |

### Design Decisions (Parser)

- **Eager parsing**: Directives are fully parsed at parse time (not deferred).
  The `.section` flags string is decomposed into `SectionDirective`; `.type`
  maps to a `SymbolKind` enum; `.align` values are converted from power-of-2
  to byte counts immediately.

- **Comment stripping guards**: Both `//` and `@` are handled, but `@` is only
  treated as a comment character when it does not prefix known GAS type tags
  (`@object`, `@function`, `@progbits`, `@nobits`, `@tls_object`, `@note`).

- **Raw operand preservation**: Each `Instruction` stores both the parsed
  `Vec<Operand>` and the raw operand text string, allowing the encoder to fall
  back to text-level heuristics for unusual operand patterns.


---

## Stage 2: Instruction Encoder (`encoder.rs`)

### Purpose

Given a mnemonic string and a `Vec<Operand>`, produce the 4-byte (32-bit)
little-endian machine code word.  AArch64 has a fixed 32-bit instruction
width, which makes encoding straightforward compared to variable-length ISAs.

### Key Data Structures

| Type | Role |
|------|------|
| `EncodeResult` | Outcome of encoding one instruction. |
| `RelocType` | AArch64 ELF relocation types (17 variants). |
| `Relocation` | A relocation request: type + symbol + addend. |

The `EncodeResult` enum has four variants:

```
Word(u32)                        -- single fully-resolved instruction
WordWithReloc { word, reloc }    -- instruction needing a linker relocation
Words(Vec<u32>)                  -- multi-word sequence (e.g., movz+movk)
Skip                             -- pseudo-instruction; no code emitted
```

### Supported Instruction Categories

The encoder handles every instruction the compiler's AArch64 backend emits.
The dispatch table in `encode_instruction()` maps ~120 mnemonics:

| Category | Mnemonics |
|----------|-----------|
| **Data Processing** | `mov`, `movz`, `movk`, `movn`, `add`, `adds`, `sub`, `subs`, `and`, `orr`, `eor`, `ands`, `orn`, `bics`, `mul`, `madd`, `msub`, `smull`, `umull`, `smaddl`, `umaddl`, `mneg`, `udiv`, `sdiv`, `umulh`, `smulh`, `neg`, `negs`, `mvn`, `adc`, `adcs`, `sbc`, `sbcs` |
| **Shifts** | `lsl`, `lsr`, `asr`, `ror` |
| **Extensions** | `sxtw`, `sxth`, `sxtb`, `uxtw`, `uxth`, `uxtb` |
| **Compare** | `cmp`, `cmn`, `tst`, `ccmp` |
| **Conditional select** | `csel`, `csinc`, `csinv`, `csneg`, `cset`, `csetm` |
| **Branches** | `b`, `bl`, `br`, `blr`, `ret`, `cbz`, `cbnz`, `tbz`, `tbnz`, `b.eq`/`beq`, `b.ne`/`bne`, `b.lt`/`blt`, ... (all 16 condition codes, with and without dot) |
| **Loads/Stores** | `ldr`, `str`, `ldrb`, `strb`, `ldrh`, `strh`, `ldrsw`, `ldrsb`, `ldrsh`, `ldp`, `stp`, `ldnp`, `stnp`, `ldxr`, `stxr`, `ldxrb`, `stxrb`, `ldxrh`, `stxrh`, `ldaxr`, `stlxr`, `ldaxrb`, `stlxrb`, `ldaxrh`, `stlxrh`, `ldar`, `stlr`, `ldarb`, `stlrb`, `ldarh`, `stlrh` |
| **Address** | `adrp`, `adr` |
| **Floating point** | `fmov`, `fadd`, `fsub`, `fmul`, `fdiv`, `fneg`, `fabs`, `fsqrt`, `fcmp`, `fcvtzs`, `fcvtzu`, `fcvtas`, `fcvtau`, `fcvtns`, `fcvtnu`, `fcvtms`, `fcvtmu`, `fcvtps`, `fcvtpu`, `ucvtf`, `scvtf`, `fcvt` |
| **NEON/SIMD** | `cnt`, `uaddlv`, `cmeq`, `cmtst`, `uqsub`, `sqsub`, `ushr`, `sshr`, `shl`, `sli`, `ext`, `addv`, `umov`, `dup`, `ins`, `not`, `movi`, `bic`, `bsl`, `pmul`, `mla`, `mls`, `rev64`, `tbl`, `tbx`, `ld1`, `ld1r`, `ld2`, `ld3`, `ld4`, `st1`, `st2`, `st3`, `st4`, `uzp1`, `uzp2`, `zip1`, `zip2`, `eor3`, `pmull`, `pmull2`, `aese`, `aesd`, `aesmc`, `aesimc` |
| **System** | `nop`, `yield`, `clrex`, `dc`, `dmb`, `dsb`, `isb`, `mrs`, `msr`, `svc`, `brk` |
| **Bit manipulation** | `clz`, `cls`, `rbit`, `rev`, `rev16`, `rev32` |
| **CRC32** | `crc32b`, `crc32h`, `crc32w`, `crc32x`, `crc32cb`, `crc32ch`, `crc32cw`, `crc32cx` |
| **Prefetch** | `prfm` |

### Relocation Types Emitted

When an instruction references an external symbol (e.g., `bl printf` or
`adrp x0, :got:variable`), the encoder returns `WordWithReloc`.  The 17
relocation types cover the full AArch64 static-linking relocation model:

| Relocation | ELF Number | Usage |
|-----------|-----------|-------|
| `Call26` | 283 | `bl` (26-bit PC-relative call) |
| `Jump26` | 282 | `b` (26-bit PC-relative jump) |
| `AdrpPage21` | 275 | `adrp` (page-relative, bits [32:12]) |
| `AddAbsLo12` | 277 | `add :lo12:sym` (low 12 bits) |
| `Ldst8AbsLo12` | 278 | Load/store byte, low 12 |
| `Ldst16AbsLo12` | 284 | Load/store halfword, low 12 |
| `Ldst32AbsLo12` | 285 | Load/store word, low 12 |
| `Ldst64AbsLo12` | 286 | Load/store doubleword, low 12 |
| `Ldst128AbsLo12` | 299 | Load/store quadword, low 12 |
| `AdrGotPage21` | 311 | `adrp` via GOT |
| `Ld64GotLo12` | 312 | `ldr` from GOT entry |
| `TlsLeAddTprelHi12` | 549 | TLS Local Exec, high 12 |
| `TlsLeAddTprelLo12` | 550 | TLS Local Exec, low 12 |
| `CondBr19` | 280 | Conditional branch, 19-bit |
| `TstBr14` | 279 | Test-and-branch, 14-bit |
| `Abs64` | 257 | 64-bit absolute |
| `Abs32` | 258 | 32-bit absolute |
| `Prel32` | 261 | 32-bit PC-relative |

### Encoding Approach

1. **Register parsing**: `parse_reg_num()` converts textual register names
   (`x0`-`x30`, `w0`-`w30`, `sp`, `xzr`, `wzr`, `lr`, `d0`-`d31`,
   `s0`-`s31`, `q0`-`q31`, `v0`-`v31`) to 5-bit register numbers.

2. **Size inference**: The `sf` bit (bit 31) is set from the register name
   prefix: `x`/`sp`/`xzr` = 64-bit, `w`/`wsp`/`wzr` = 32-bit.

3. **Condition codes**: 16 codes (`eq`, `ne`, `cs`/`hs`, `cc`/`lo`, ...,
   `al`, `nv`) are mapped to their 4-bit encoding.

4. **Wide immediates**: `mov Xd, #large` generates a `movz`+`movk` sequence
   (up to 4 instructions for a 64-bit constant), returned as
   `EncodeResult::Words`.

5. **MOV special cases**: `mov` to/from `sp` encodes as `add Xd, Xn, #0`;
   register-to-register `mov` encodes as `orr Rd, xzr, Rm`.  NEON `mov`
   variants (lane insert, lane extract, element-to-element) are detected by
   operand type and encoded as `INS`/`UMOV`/`ORR` as appropriate.


---

## Stage 3: ELF Object Writer (`elf_writer.rs`)

### Purpose

Walk the `Vec<AsmStatement>`, accumulate section data, build the symbol table,
resolve local branches, and serialize everything into a valid ELF64
relocatable object file.

### Key Data Structures

| Type | Role |
|------|------|
| `ElfWriter` | The central state machine -- holds all sections, symbols, labels, pending relocations. |
| `Section` | A section being built: name, type, flags, data bytes, alignment, relocation list. |
| `ElfReloc` | A relocation entry: offset, type, symbol name, addend. |
| `ElfSymbol` | A symbol entry: name, value, size, binding, type, visibility, section. |
| `PendingReloc` | A relocation for a local branch label (resolved after all labels are known). |
| `PendingSymDiff` | A deferred symbol-difference expression (e.g., `.long .LBB3 - .Ljt_0`). |
| `StringTable` | Builder for NUL-terminated ELF string tables (`.strtab`, `.shstrtab`). |
| `SymEntry` | A finalized `Elf64_Sym` record ready for serialization. |

### ElfWriter State

```rust
pub struct ElfWriter {
    current_section: String,              // Active section name
    sections: HashMap<String, Section>,   // All sections by name
    section_order: Vec<String>,           // Insertion order (deterministic output)
    section_stack: Vec<String>,           // Stack for .pushsection/.popsection
    symbols: Vec<ElfSymbol>,              // Built symbol table
    labels: HashMap<String, (String, u64)>, // label -> (section, offset)
    pending_branch_relocs: Vec<PendingReloc>,  // Local branches to fix up
    pending_sym_diffs: Vec<PendingSymDiff>,     // Deferred A-B expressions
    global_symbols: HashMap<String, bool>,     // .globl declarations
    weak_symbols: HashMap<String, bool>,       // .weak declarations
    symbol_types: HashMap<String, u8>,         // .type declarations
    symbol_sizes: HashMap<String, u64>,        // .size declarations
    symbol_visibility: HashMap<String, u8>,    // .hidden/.protected/.internal
}
```

### Processing Algorithm

```
process_statements(statements):
  for each statement:
    Label(name)        -> record (section, offset) in labels map
    Directive(dir)     -> process_directive():
                           Section   -> ensure_section(), update current_section
                           PushSection -> push current_section onto section_stack, switch
                           PopSection  -> pop section_stack, restore current_section
                           Global    -> mark in global_symbols
                           Weak      -> mark in weak_symbols
                           Hidden/Protected/Internal -> mark visibility
                           SymbolType -> record in symbol_types
                           Size      -> compute and record in symbol_sizes
                           Align/Balign -> pad current section to alignment
                           Byte/Short/Long/Quad -> emit data bytes
                             (Long/Quad with symbols emit relocations)
                           Zero      -> emit fill bytes
                           Asciz/Ascii -> emit string bytes
                           Comm      -> create COMMON symbol
                           Cfi/Ignored -> no-op
    Instruction(m,ops) -> process_instruction():
                           call encode_instruction(m, ops)
                           Word       -> emit 4 bytes
                           WordWithReloc:
                             local (.L*) -> store in pending_branch_relocs
                             external    -> add_reloc() to section
                           Words      -> emit all 4-byte words
                           Skip       -> no-op

  resolve_sym_diffs():   resolve all A-B expressions
    same-section     -> patch data in place
    cross-section    -> emit R_AARCH64_PREL32 relocation

  resolve_local_branches():   resolve all .L* branch targets
    same-section     -> compute PC-relative offset, patch instruction word
      JUMP26/CALL26  -> encode imm26 field
      CONDBR19       -> encode imm19 field
      TSTBR14        -> encode imm14 field
    cross-section    -> emit relocation with section symbol + addend
```

### ELF File Layout

```
  +========================+  offset 0
  |  ELF64 Header (64 B)  |  e_machine=EM_AARCH64 (183)
  |                        |  e_type=ET_REL (1)
  +========================+
  |  Section Data          |  .text, .data, .rodata, .bss, etc.
  |  (aligned per section) |  Each section padded to its sh_addralign
  +========================+
  |  .rela sections        |  One per content section with relocations
  |  (8-byte aligned)      |  Each entry: 24 bytes (Elf64_Rela)
  +========================+
  |  .symtab               |  Symbol table (24 bytes per Elf64_Sym)
  |  (8-byte aligned)      |  Order: NULL, section syms, local, global
  +========================+
  |  .strtab               |  Symbol name strings
  +========================+
  |  .shstrtab             |  Section name strings
  +========================+
  |  Section Header Table  |  One Elf64_Shdr per section (64 bytes each)
  |  (8-byte aligned)      |  Order: NULL, content, rela, symtab,
  |                        |         strtab, shstrtab
  +========================+
```

### Symbol Table Construction

`build_symbol_table()` runs just before ELF serialization:

1. **Section symbols**: One `STT_SECTION` / `STB_LOCAL` symbol per content
   section.

2. **Defined symbols**: Every label recorded in `self.labels`, excluding
   `.L*` / `.l*` local labels.  Binding is determined from `global_symbols`,
   `weak_symbols`, or defaults to local.  Type and size come from
   `symbol_types` and `symbol_sizes`.

3. **Undefined symbols**: Every symbol referenced in relocations that has no
   definition.  These get `STB_GLOBAL` binding (or `STB_WEAK` if declared
   `.weak`).

4. **COMMON symbols**: Created by `.comm` directives with `SHN_COMMON` section
   index.

### Local Label and Data Relocation Resolution

Two resolution passes run after all statements are processed:

- **`resolve_local_data_relocs()`**: Rewrites relocations that reference `.L*`
  labels (which will not appear in the symbol table) to instead reference the
  section symbol plus the label's offset as addend.  This matches the behavior
  of GCC's assembler.

- **`resolve_sym_diffs()`**: Handles `.long .LA - .LB` style expressions.
  Same-section differences are computed and patched directly.  Cross-section
  differences produce `R_AARCH64_PREL32` relocations.


---

## Design Decisions and Trade-offs

### 1. Single-pass parsing, two-pass encoding

Parsing is single-pass and purely syntactic.  The ELF writer makes two
logical passes: a forward pass to collect all labels and emit code/data, then
backward resolution passes to fix up local branches and symbol differences.
This avoids the complexity of a full two-pass assembler while handling forward
references correctly.

### 2. Local branch resolution at assembly time

Same-section branches to `.L*` labels are resolved by the assembler itself,
producing fully-linked instruction words.  Only cross-section or external
symbol references generate relocations in the `.o` file.  This reduces linker
work and matches GCC's behavior.

### 3. No DWARF emission

CFI directives (`.cfi_startproc`, `.cfi_offset`, etc.) are parsed and
silently ignored.  The assembler does not emit `.eh_frame` or `.debug_*`
sections.  This is acceptable for the compiler's use case but means
stack unwinding and debugger support rely on the external assembler path.

### 4. Deterministic output

Section order is tracked via `section_order: Vec<String>` rather than relying
on `HashMap` iteration order.  This ensures identical input always produces
bit-identical output.

### 5. Fixed instruction width simplifies encoding

AArch64's uniform 32-bit instruction encoding means every instruction is
exactly 4 bytes.  There is no need for instruction-length calculation or
relaxation passes (unlike x86).  The only multi-word output is the
`movz`+`movk` sequence for wide immediates, returned as `EncodeResult::Words`.


---

## File Inventory

| File | Lines | Purpose |
|------|-------|---------|
| `mod.rs` | 221 | Public API: `assemble()` entry point, module declarations |
| `parser.rs` | 1,234 | Tokenizer and parser: text -> `Vec<AsmStatement>` |
| `encoder.rs` | 4,012 | Instruction encoder: mnemonic + operands -> `u32` machine code |
| `elf_writer.rs` | 797 | ELF object file writer: statements -> `.o` file on disk |
| **Total** | **6,264** | |
