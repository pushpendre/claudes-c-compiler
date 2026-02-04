# RISC-V 64-bit Assembler -- Design Document

## Overview

This module implements a complete, self-contained assembler for the RISC-V 64-bit
ISA (RV64GC). It translates textual assembly source -- as emitted by the compiler's
code-generation backend -- into relocatable ELF object files (`.o`). The assembler
is designed to be invoked in-process (no fork/exec of an external tool), which
removes a hard dependency on a host `as` binary and dramatically improves
compilation latency on cross-compilation setups.

The assembler supports the full RV64I base integer ISA, the M (multiply/divide),
A (atomics), F (single-precision float), D (double-precision float), and
C (compressed 16-bit) standard extensions.  It handles all standard assembler
directives, pseudo-instructions, relocation modifiers, numeric local labels,
and optional RV64C instruction compression.

### Capabilities at a glance

- Full RV64IMAFDC instruction encoding
- 40+ pseudo-instructions (li, la, call, tail, mv, not, negw, seqz, ...)
- All standard assembler directives (.text, .data, .globl, .align, .byte, ...)
- Relocation modifier parsing (%pcrel_hi, %pcrel_lo, %hi, %lo, %tprel_*, %got_pcrel_hi, ...)
- Automatic RV64C compressed instruction selection (post-encoding pass)
- Numeric local labels (1:, 1b, 1f) with forward/backward reference resolution
- Correct ELF object file emission with .symtab, .strtab, .rela.* sections

## Architecture / Pipeline

```
                          Assembly source (text)
                                  |
                                  v
                     +------------------------+
                     |     Parser (parser.rs) |
                     |  - Tokenize lines      |
                     |  - Parse operands       |
                     |  - Recognize directives |
                     +------------------------+
                                  |
                         Vec<ParsedLine>
                                  |
                                  v
                     +------------------------+
                     |   Encoder (encoder.rs) |
                     |  - Map mnemonic to ISA |
                     |  - Encode R/I/S/B/U/J  |
                     |  - Expand pseudos      |
                     |  - Emit relocations    |
                     +------------------------+
                                  |
                       EncodeResult + Relocation
                                  |
                                  v
                     +---------------------------+
                     | Assembler Core (mod.rs)   |
                     |  - Section management     |
                     |  - Label/symbol tracking   |
                     |  - Directive execution     |
                     |  - Branch reloc resolution |
                     +---------------------------+
                                  |
                                  v
                     +----------------------------+
                     | Compressor (compress.rs)   |
                     |  - Scan 32-bit encodings   |
                     |  - Replace with 16-bit RVC |
                     |  - Adjust offsets/relocs   |
                     +----------------------------+
                                  |
                                  v
                     +----------------------------+
                     | ELF Writer (elf_writer.rs) |
                     |  - ELF64 header            |
                     |  - Section headers         |
                     |  - .symtab / .strtab       |
                     |  - .rela.text / .rela.data |
                     +----------------------------+
                                  |
                                  v
                         Relocatable ELF .o file
```

## File Inventory

| File            | Lines | Role                                                    |
|-----------------|-------|---------------------------------------------------------|
| `mod.rs`        | ~30   | Top-level `Assembler` struct; public `assemble()` entry point, delegates to parser/encoder/compressor/ELF writer |
| `parser.rs`     | ~980  | Line tokenizer and operand parser; splits assembly text into structured `ParsedLine` records |
| `encoder.rs`    | ~1900 | Instruction encoder; maps every mnemonic to its binary encoding, handles pseudo-instruction expansion, relocation modifier parsing |
| `compress.rs`   | ~830  | Post-encoding RV64C compression pass; rewrites eligible 32-bit instructions to 16-bit compressed equivalents |
| `elf_writer.rs` | ~1600 | ELF object file serializer; section/symbol management, directive handling, local branch resolution, builds headers, symbol tables, relocation sections, and writes the final `.o` bytes |

## Key Data Structures

### `Assembler` (mod.rs)

The central state object that accumulates the results of the entire assembly
process.

```
Assembler {
    sections:              HashMap<String, Section>,   // ".text", ".data", ...
    section_order:         Vec<String>,                // insertion-order tracking
    current_section:       String,                     // currently active section
    section_stack:         Vec<String>,                // saved sections for .pushsection/.popsection
    labels:                HashMap<String, (String, u64)>,  // name -> (section, offset)
    numeric_labels:        HashMap<String, Vec<(String, u64)>>,  // "1" -> [(sec, off), ...]
    symbols:               Vec<ElfSymbol>,              // accumulated symbol table
    pending_branch_relocs: Vec<PendingBranchReloc>,     // unresolved local branches
    global_names:          HashSet<String>,             // names marked .globl
    weak_names:            HashSet<String>,              // names marked .weak
}
```

### `Section` (mod.rs)

Represents a single ELF section being built.

```
Section {
    data:           Vec<u8>,          // accumulated bytes
    sh_type:        u32,              // SHT_PROGBITS, SHT_NOBITS, ...
    sh_flags:       u64,              // SHF_ALLOC | SHF_WRITE | SHF_EXECINSTR
    sh_addralign:   u64,              // required alignment
    sh_entsize:     u64,              // entry size for fixed-size sections
    relocs:         Vec<ElfReloc>,    // pending relocations for this section
}
```

### `ParsedLine` (parser.rs)

The parser's output for a single source line.

```
ParsedLine {
    label:     Option<String>,     // label defined on this line, if any
    mnemonic:  Option<String>,     // instruction or directive name
    operands:  Vec<Operand>,       // parsed operand list
}
```

### `Operand` (parser.rs)

A tagged union covering every operand form the parser recognizes.

```
Operand::Reg(String)                  // register name: "x5", "a0", "sp"
Operand::Imm(i64)                     // immediate: 42, -7, 0xff
Operand::Symbol(String)               // symbol name: "printf", ".LC0"
Operand::Label(String)                // label reference (same as symbol)
Operand::MemRef { base, offset }      // memory: 8(sp), 0(a0)
Operand::Modifier { kind, symbol }    // %pcrel_hi(sym), %lo(sym), ...
```

### `EncodeResult` (encoder.rs)

The encoder returns one of several result variants depending on the instruction
class.

```
EncodeResult::Word(u32)                           // single 32-bit instruction
EncodeResult::WordWithReloc { word, reloc }       // instruction + relocation
EncodeResult::WordsWithRelocs(Vec<(u32, Option<Reloc>)>)  // multi-word sequence (e.g., call = auipc+jalr)
EncodeResult::Compressed(u16)                     // already-compressed instruction
EncodeResult::Raw(Vec<u8>)                        // raw bytes (e.g., .byte/.word)
```

### `Relocation` / `RelocType` (encoder.rs)

```
Relocation {
    reloc_type: RelocType,     // semantic relocation kind
    symbol:     String,        // target symbol name
    addend:     i64,           // constant addend
}

RelocType::Branch          -> R_RISCV_BRANCH     (B-type, 12-bit PC-rel)
RelocType::Jal             -> R_RISCV_JAL        (J-type, 20-bit PC-rel)
RelocType::CallPlt         -> R_RISCV_CALL_PLT   (AUIPC+JALR pair)
RelocType::PcrelHi20       -> R_RISCV_PCREL_HI20
RelocType::PcrelLo12I      -> R_RISCV_PCREL_LO12_I
RelocType::PcrelLo12S      -> R_RISCV_PCREL_LO12_S
RelocType::Hi20            -> R_RISCV_HI20
RelocType::Lo12I           -> R_RISCV_LO12_I
RelocType::Lo12S           -> R_RISCV_LO12_S
RelocType::TprelHi20       -> R_RISCV_TPREL_HI20
RelocType::TprelLo12I      -> R_RISCV_TPREL_LO12_I
RelocType::TprelLo12S      -> R_RISCV_TPREL_LO12_S
RelocType::TprelAdd        -> R_RISCV_TPREL_ADD
RelocType::GotPcrelHi20   -> R_RISCV_GOT_HI20
RelocType::Abs32           -> R_RISCV_32
RelocType::Abs64           -> R_RISCV_64
```

## Processing Algorithm Step by Step

### Step 1: Parsing (`parser.rs`)

The parser processes the assembly source line by line. For each line it:

1. Strips comments (everything after `#` or `;`).
2. Detects a leading label (any identifier followed by `:`). Numeric labels
   such as `1:` are recognized as re-definable local labels.
3. Identifies the mnemonic -- either an instruction name (`add`, `ld`, `beq`)
   or a directive (`.text`, `.globl`, `.quad`).
4. Parses the operand list, recognizing:
   - Integer and FP register names (x0-x31, a0-a7, s0-s11, t0-t6, f0-f31,
     fa0-fa7, fs0-fs11, ft0-ft11), including ABI aliases.
   - Immediates in decimal, hex (`0x`), octal (`0`), and binary (`0b`).
   - Memory references in the form `offset(base)`.
   - Relocation modifiers: `%pcrel_hi(sym)`, `%lo(sym)`, `%tprel_add(sym)`, etc.
   - Bare symbol references and label references (including numeric `1b`, `1f`).

### Step 2: Directive Execution (`mod.rs`)

The assembler core iterates over parsed lines and handles directives inline:

| Directive              | Effect                                           |
|------------------------|--------------------------------------------------|
| `.text` / `.data` / `.bss` / `.rodata` / `.section` | Switch or create a section |
| `.globl` / `.global`  | Mark symbol as globally visible                   |
| `.weak`               | Mark symbol as weak binding                        |
| `.type`               | Set symbol type (function/object)                  |
| `.size`               | Set symbol size                                    |
| `.byte` / `.half` / `.word` / `.quad` / `.8byte` | Emit literal data       |
| `.zero` / `.space`    | Emit N zero bytes                                  |
| `.string` / `.asciz` / `.ascii` | Emit string data (with/without NUL)     |
| `.align` / `.balign` / `.p2align` | Pad to alignment boundary              |
| `.equ` / `.set`       | Define a symbol with a constant value              |
| `.comm` / `.lcomm`    | Reserve common/local-common storage                |
| `.pushsection` / `.popsection` / `.previous` | Push/pop section stack (save and restore active section) |
| `.rept` / `.endr`           | Repeat enclosed lines N times (expanded during parsing) |
| `.option push/pop/rvc/norvc` | Control RVC compression                    |
| `.attribute` / `.file` / `.ident` | Metadata / ignored                      |
| `.cfi_*`              | Silently consumed (CFI info not emitted)           |
| `.addrsig` / `.addrsig_sym` | Address-significance (silently consumed)    |

### Step 3: Instruction Encoding (`encoder.rs`)

For each instruction mnemonic, the encoder:

1. Looks up the mnemonic in a master dispatch table. The table covers:
   - **R-type**: add, sub, sll, slt, sltu, xor, srl, sra, or, and, mul, div,
     rem, addw, subw, sllw, srlw, sraw, mulw, divw, remw, plus all
     variants (unsigned, word-width).
   - **I-type**: addi, slti, sltiu, xori, ori, andi, slli, srli, srai,
     addiw, slliw, srliw, sraiw, lb/lh/lw/ld/lbu/lhu/lwu, jalr.
   - **S-type**: sb, sh, sw, sd.
   - **B-type**: beq, bne, blt, bge, bltu, bgeu.
   - **U-type**: lui, auipc.
   - **J-type**: jal.
   - **Atomics (A-extension)**: lr.w/d, sc.w/d, amo{swap,add,and,or,xor,min,max,minu,maxu}.w/d
   - **Floating-point (F/D)**: fadd/fsub/fmul/fdiv/fsqrt, fmin/fmax,
     fcvt (all int/float conversions), fmv.x.w/d, fmv.w.x/d.x,
     fmadd/fmsub/fnmadd/fnmsub, feq/flt/fle, fclass, flw/fld/fsw/fsd.
   - **System**: ecall, ebreak, fence, fence.i, csrr/csrw/csrs/csrc.

2. Expands pseudo-instructions into their real instruction sequences:

   | Pseudo         | Expansion                                            |
   |----------------|------------------------------------------------------|
   | `li rd, imm`   | `lui + addi(w)` or single `addi`, up to 3-instruction sequences for 64-bit constants |
   | `mv rd, rs`    | `addi rd, rs, 0`                                     |
   | `not rd, rs`   | `xori rd, rs, -1`                                    |
   | `neg rd, rs`   | `sub rd, x0, rs`                                     |
   | `negw rd, rs`  | `subw rd, x0, rs`                                    |
   | `sext.w rd, rs`| `addiw rd, rs, 0`                                    |
   | `seqz rd, rs`  | `sltiu rd, rs, 1`                                    |
   | `snez rd, rs`  | `sltu rd, x0, rs`                                    |
   | `sltz rd, rs`  | `slt rd, rs, x0`                                     |
   | `sgtz rd, rs`  | `slt rd, x0, rs`                                     |
   | `beqz/bnez`    | `beq/bne rs, x0, label`                              |
   | `blez/bgez/...`| Corresponding `bge`/`blt` with x0                    |
   | `bgt/ble/bgtu/bleu` | Swapped-operand `blt`/`bge` variants            |
   | `j label`      | `jal x0, label`                                      |
   | `jr rs`        | `jalr x0, 0(rs)`                                     |
   | `ret`          | `jalr x0, 0(ra)`                                     |
   | `call sym`     | `auipc ra, %pcrel_hi(sym)` + `jalr ra, %pcrel_lo(sym)(ra)` |
   | `tail sym`     | `auipc t1, %pcrel_hi(sym)` + `jalr x0, %pcrel_lo(sym)(t1)` |
   | `la rd, sym`   | `auipc rd, ...` + `addi rd, rd, ...` (pcrel pair)    |
   | `lla rd, sym`  | Same as `la` (non-PIC)                                |
   | `nop`          | `addi x0, x0, 0`                                     |
   | `fmv.s/d`      | `fsgnj.s/d rd, rs, rs`                               |
   | `fabs.s/d`     | `fsgnjx.s/d rd, rs, rs`                              |
   | `fneg.s/d`     | `fsgnjn.s/d rd, rs, rs`                              |
   | `rdcycle/rdtime/rdinstret` | `csrrs rd, csr, x0`                    |
   | `csrr/csrw/csrs/csrc` | Expanded `csrrs`/`csrrw`/`csrrc` forms         |

3. Produces an `EncodeResult` containing the machine code bytes and any
   relocations required for symbol references.

The encoding functions for each instruction format follow the RISC-V ISA
specification exactly:

```
R-type:  [funct7 | rs2 | rs1 | funct3 |  rd  | opcode]
I-type:  [    imm[11:0]  | rs1 | funct3 |  rd  | opcode]
S-type:  [imm[11:5]| rs2 | rs1 | funct3 | imm[4:0] | opcode]
B-type:  [imm[12|10:5] | rs2 | rs1 | funct3 | imm[4:1|11] | opcode]
U-type:  [          imm[31:12]           |  rd  | opcode]
J-type:  [imm[20|10:1|11|19:12]         |  rd  | opcode]
```

### Step 4: Section Data Accumulation (`mod.rs`)

As instructions are encoded, the assembler appends the resulting bytes to the
current section's data buffer. For instructions with relocations:

- **Intra-section branches** (same section, label already defined or to be
  defined): recorded as `PendingBranchReloc` entries for later resolution.
- **External symbol references**: recorded as `ElfReloc` entries in the
  section's relocation list, to be emitted as `.rela.*` sections.

For multi-word expansions (e.g., `call` emitting AUIPC+JALR), the assembler
generates synthetic labels (`.Lpcrel_hiN`) so that `%pcrel_lo` relocations can
reference the AUIPC's PC, as required by the RISC-V ABI.

### Step 5: Local Branch Resolution (`mod.rs` -- `resolve_local_branches`)

Before ELF emission, the assembler resolves all pending intra-section branch
relocations:

1. For each `PendingBranchReloc`, it looks up the target label in the label
   table (handling numeric label `Nb`/`Nf` references via
   `resolve_numeric_label_ref`).
2. If the target is in the same section, it computes the PC-relative offset
   and patches the instruction word directly in the section data buffer,
   encoding the offset into the appropriate bit fields for the relocation type:
   - **R_RISCV_BRANCH** (B-type): 12-bit signed offset, bit-scattered
   - **R_RISCV_JAL** (J-type): 20-bit signed offset, bit-scattered
   - **R_RISCV_CALL_PLT**: patches both AUIPC (hi20) and JALR (lo12)
   - **R_RISCV_PCREL_HI20**: patches AUIPC upper 20 bits
   - **R_RISCV_PCREL_LO12_I/S**: patches load/store lower 12 bits
3. If the target is in a different section or undefined, the relocation is
   promoted to an external ELF relocation for the linker to resolve.

### Step 6: RV64C Compression (`compress.rs`)

After all instructions are encoded as 32-bit words, a compression pass scans
each `.text` section and replaces eligible instructions with their 16-bit RVC
equivalents. This pass is optional (controlled by `.option rvc`/`.option norvc`
directives; enabled by default).

The compressor works in a single linear scan:

1. Read each 32-bit instruction from the section data.
2. Check if it matches any compression pattern (see table below).
3. If compressible, emit 2 bytes instead of 4 and record the delta.
4. Adjust all relocation offsets for instructions that shifted.

The compression is done *after* local branch resolution but *before* ELF
emission, so external relocations get their offsets adjusted correctly.

#### Compression Patterns

The following 32-bit instructions are compressed when their operands satisfy
RVC constraints:

| 32-bit Instruction     | RVC Equivalent | Constraints                                          |
|------------------------|----------------|------------------------------------------------------|
| `addi rd, x0, imm`    | `c.li rd, imm` | rd != x0, imm fits in 6-bit signed                   |
| `addi rd, rd, imm`    | `c.addi rd, imm` | rd != x0, imm != 0, 6-bit signed                  |
| `addi rd, sp, imm`    | `c.addi4spn rd, imm` | rd in x8-x15, imm > 0, imm multiple of 4, fits in 8 unsigned bits |
| `addiw rd, rd, imm`   | `c.addiw rd, imm` | rd != x0, 6-bit signed                            |
| `addi sp, sp, imm`    | `c.addi16sp imm` | imm != 0, multiple of 16, fits in range             |
| `lui rd, imm`         | `c.lui rd, imm` | rd != x0, rd != x2, imm != 0, 6-bit range           |
| `slli rd, rd, shamt`  | `c.slli rd, shamt` | rd != x0, shamt != 0, 6-bit                      |
| `srli rd, rd, shamt`  | `c.srli rd, shamt` | rd in x8-x15, shamt != 0, 6-bit                  |
| `srai rd, rd, shamt`  | `c.srai rd, shamt` | rd in x8-x15, shamt != 0, 6-bit                  |
| `andi rd, rd, imm`    | `c.andi rd, imm` | rd in x8-x15, 6-bit signed                         |
| `add rd, x0, rs2`     | `c.mv rd, rs2` | rd != x0, rs2 != x0                                  |
| `add rd, rd, rs2`     | `c.add rd, rs2` | rd != x0, rs2 != x0                                 |
| `and rd, rd, rs2`     | `c.and rd, rs2` | rd in x8-x15, rs2 in x8-x15                         |
| `or rd, rd, rs2`      | `c.or rd, rs2` | rd in x8-x15, rs2 in x8-x15                          |
| `xor rd, rd, rs2`     | `c.xor rd, rs2` | rd in x8-x15, rs2 in x8-x15                         |
| `sub rd, rd, rs2`     | `c.sub rd, rs2` | rd in x8-x15, rs2 in x8-x15                         |
| `addw rd, rd, rs2`    | `c.addw rd, rs2` | rd in x8-x15, rs2 in x8-x15                        |
| `subw rd, rd, rs2`    | `c.subw rd, rs2` | rd in x8-x15, rs2 in x8-x15                        |
| `ld rd, off(rs1)`     | `c.ld rd, off(rs1)` | rd, rs1 in x8-x15, off mult of 8, fits 5 bits   |
| `lw rd, off(rs1)`     | `c.lw rd, off(rs1)` | rd, rs1 in x8-x15, off mult of 4, fits 5 bits   |
| `sd rs2, off(rs1)`    | `c.sd rs2, off(rs1)` | rs2, rs1 in x8-x15, off mult of 8, fits 5 bits |
| `sw rs2, off(rs1)`    | `c.sw rs2, off(rs1)` | rs2, rs1 in x8-x15, off mult of 4, fits 5 bits |
| `ld rd, off(sp)`      | `c.ldsp rd, off` | rd != x0, off mult of 8, fits 6 bits                 |
| `lw rd, off(sp)`      | `c.lwsp rd, off` | rd != x0, off mult of 4, fits 6 bits                 |
| `sd rs2, off(sp)`     | `c.sdsp rs2, off` | off mult of 8, fits 6 bits                          |
| `sw rs2, off(sp)`     | `c.swsp rs2, off` | off mult of 4, fits 6 bits                          |
| `jal x0, off`         | `c.j off` | offset fits in 11-bit signed                              |
| `jalr x1, 0(rs1)`     | `c.jalr rs1` | rs1 != x0                                             |
| `jalr x0, 0(rs1)`     | `c.jr rs1` | rs1 != x0                                               |
| `beq rs, x0, off`     | `c.beqz rs, off` | rs in x8-x15, offset fits in 8-bit signed           |
| `bne rs, x0, off`     | `c.bnez rs, off` | rs in x8-x15, offset fits in 8-bit signed           |
| `ebreak`              | `c.ebreak` | (always compressible)                                    |
| `addi x0, x0, 0`      | `c.nop` | (canonical NOP)                                           |

### Step 7: ELF Object Emission (`elf_writer.rs`)

The final step serializes the assembled state into a conformant ELF64 relocatable
object file. The layout is:

```
+----------------------------------+  offset 0
|  ELF Header (64 bytes)           |
|  - e_machine = EM_RISCV (243)    |
|  - e_flags = FLOAT_ABI_DOUBLE    |
|             | RVC                 |
+----------------------------------+
|  Section data                    |
|  (.text, .data, .rodata, .bss,   |
|   .sdata, .init_array, etc.)     |
|  (each aligned per sh_addralign) |
+----------------------------------+
|  .rela.text (relocation entries) |
|  .rela.data                      |
|  (24 bytes per entry: ELF64_Rela)|
+----------------------------------+
|  .symtab (symbol table)          |
|  (24 bytes per entry: ELF64_Sym) |
|  Ordering: NULL, section syms,   |
|            local syms, global    |
+----------------------------------+
|  .strtab (symbol string table)   |
+----------------------------------+
|  .shstrtab (section name strings)|
+----------------------------------+
|  Section header table            |
|  (64 bytes per header: Elf64_Shdr|
|  NULL, content sections,         |
|  .rela.*, .symtab, .strtab,      |
|  .shstrtab)                      |
+----------------------------------+
```

The writer performs several bookkeeping tasks:

- **Symbol table construction**: Local labels (`.L*`) are included only if
  they are referenced by a relocation (e.g., synthetic `%pcrel_lo` labels).
  Section symbols are emitted for every content section. The `sh_info` field
  of `.symtab` is set to the index of the first global symbol, per ELF spec.

- **Relocation entries**: Each `ElfReloc` is serialized as an `Elf64_Rela`
  entry (offset, r_info = symbol_index << 32 | type, addend). A companion
  `R_RISCV_RELAX` relocation is emitted alongside `PCREL_HI20`, `CALL_PLT`,
  `TPREL_*`, and `GOT_HI20` relocations to allow the linker to perform
  relaxation optimizations.

- **ELF flags**: `EF_RISCV_FLOAT_ABI_DOUBLE | EF_RISCV_RVC` (0x05),
  indicating the double-precision float ABI and the presence of compressed
  instructions.

## Key Design Decisions and Trade-offs

### 1. Post-encoding compression vs. direct compressed emission

The assembler first encodes all instructions as 32-bit words, then runs a
separate compression pass. This two-phase approach is simpler than trying to
emit compressed instructions inline during encoding, because:

- The compressor can examine each fully-formed 32-bit encoding and make a
  binary yes/no decision. The encoder does not need to be aware of RVC
  constraints at all.
- Relocation offset adjustment is localized to a single pass rather than
  being spread throughout the encoder.
- The approach is trivially correct: removing the compression pass produces
  a valid (if larger) object file.

The trade-off is a second scan over all `.text` section data and the need to
rebuild the relocation offset map. In practice the overhead is negligible
compared to parsing and encoding.

### 2. Eager local branch resolution

Branches to labels within the same section are resolved immediately in the
assembler (before ELF emission), rather than being emitted as relocations for
the linker. This reduces the number of relocations the linker must process and
produces smaller object files. The linker only sees cross-section and
cross-module symbol references.

### 3. Synthetic labels for PCREL_LO12

The RISC-V ABI requires that `%pcrel_lo` relocations reference the *AUIPC
instruction's address*, not the symbol directly. The assembler generates
synthetic labels (`.Lpcrel_hiN`) at each AUIPC site and makes the corresponding
LO12 relocation reference that label. The `build_symbol_table` pass in the
ELF writer ensures these synthetic labels appear in `.symtab` whenever they
are referenced by a `.rela.*` entry.

### 4. Numeric local labels

Numeric labels (`1:`, `2:`, etc.) can be redefined multiple times. Forward
references (`1f`) resolve to the *next* definition; backward references (`1b`)
resolve to the *most recent* definition. The assembler maintains a per-label
list of `(section, offset)` definitions and performs linear search at resolution
time. This is O(n) in the number of definitions of a particular numeric label,
but numeric labels are rarely defined more than a handful of times, so this is
not a practical concern.

### 5. In-process execution

The assembler runs entirely in-process, sharing the compiler's address space.
There is no serialization to text and back, no fork/exec of a system assembler.
This means:

- No dependency on a host RISC-V cross-assembler being installed.
- Faster compilation: no process spawning overhead.
- The compiler controls the exact assembly dialect and can rely on features
  without worrying about toolchain version skew.

### 6. Directive subset

The assembler implements the subset of GNU assembler directives that the
compiler's code-generation backend actually emits, plus common directives found
in CRT startup code. Rarely-used directives (`.macro`, `.irpc`, `.if/.endif`,
conditional assembly) are not implemented. This keeps the assembler simple and
focused. Directives that are not recognized are silently ignored to ensure
compatibility with assembly code that includes GAS-specific annotations.

### 7. No linker relaxation in the assembler

The assembler emits `R_RISCV_RELAX` hints alongside eligible relocations but
does not perform any relaxation itself. Relaxation (e.g., converting a
`lui+addi` pair to a single `addi` when the symbol is close to GP) is
intentionally left to the linker, which has full address layout information.
The assembler's job is to produce conservative, correct encodings.
