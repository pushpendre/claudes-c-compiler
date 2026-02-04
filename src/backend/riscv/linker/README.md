# RISC-V 64-bit Linker -- Design Document

## Overview

This module implements a self-contained static linker for RISC-V 64-bit ELF
targets. It reads relocatable object files (`.o`) and static archives (`.a`),
resolves symbols, merges sections, applies all RISC-V relocations, generates
PLT/GOT structures for dynamic library interop, and emits a complete
dynamically-linked (or optionally static) ELF executable.

The linker is invoked in-process by the compiler driver (no fork/exec of a
system `ld`), removing any dependency on a RISC-V cross-linker installation.
It is active by default (when the `gcc_linker` Cargo feature is not enabled).

### Capabilities at a glance

- Reads ELF64 relocatable objects and GNU `ar` archives
- Reads ELF shared libraries to discover dynamic symbols
- Handles GNU-style linker scripts embedded in `.so` wrappers
- Iterative archive member resolution (pull-in-on-demand)
- Full RISC-V relocation processing (30+ relocation types)
- PLT/GOT generation for function calls into shared libraries
- COPY relocations for data symbols imported from shared libraries
- TLS support (Local-Exec and Initial-Exec models; GD->LE relaxation for static binaries; PT_TLS segment)
- GNU RELRO segment for .dynamic, .got, init/fini arrays
- Linker-defined symbols (__global_pointer$, _start, _edata, __bss_start, ...)
- CRT startup object discovery and automatic linking of libc/libgcc
- Produces a conformant ELF64 executable with correct program and section headers

## Architecture / Pipeline

```
  .o files      .a archives      .so shared libs      -l/-L flags
      |               |                |                    |
      v               v                v                    v
+--------------------------------------------------------------------------+
|                      Phase 1: Input Collection                           |
|  - Read ELF objects and ar archives                                      |
|  - Discover CRT objects (crt1.o, crti.o, crtbegin.o, crtend.o, crtn.o)  |
|  - Parse -L library search paths and -l library names                    |
+--------------------------------------------------------------------------+
      |
      v
+--------------------------------------------------------------------------+
|                Phase 1b: Library Resolution                              |
|  - Scan shared libraries (.so) for exported symbols                      |
|  - Parse linker scripts embedded in .so stubs                            |
|  - Build shared_lib_syms: HashMap<name, DynSymbol>                       |
|  - Iteratively resolve .a archive members against undefined symbols      |
+--------------------------------------------------------------------------+
      |
      v
+--------------------------------------------------------------------------+
|                 Phase 2: Section Merging                                  |
|  - Group input sections by output name (.text.* -> .text, etc.)          |
|  - Concatenate data, respecting alignment                                |
|  - Track per-input-section mapping: (obj, sec) -> (merged, offset)       |
+--------------------------------------------------------------------------+
      |
      v
+--------------------------------------------------------------------------+
|              Phase 3: Symbol Table Construction                          |
|  - First pass: define global symbols from input objects                   |
|  - Handle SHN_ABS, SHN_COMMON (allocate in .bss), weak vs. strong       |
|  - Identify PLT symbols (undefined functions in shared libs)             |
|  - Identify COPY symbols (undefined data objects in shared libs)         |
|  - Identify GOT symbols (referenced via R_RISCV_GOT_HI20)               |
+--------------------------------------------------------------------------+
      |
      v
+--------------------------------------------------------------------------+
|              Phase 4: Address Layout                                     |
|  - Sort merged sections by canonical order                               |
|  - Compute sizes of generated sections (PLT, GOT, .dynamic, ...)        |
|  - Layout RX segment: headers, .interp, .gnu.hash, .dynsym, .dynstr,    |
|    .rela.plt, .plt, then user .text/.rodata/.eh_frame                    |
|  - Layout RW segment: init/fini arrays, .dynamic, .got (RELRO region),  |
|    then .got.plt, .data, .sdata, TLS sections, .bss                     |
|  - Assign virtual addresses and file offsets to all sections             |
+--------------------------------------------------------------------------+
      |
      v
+--------------------------------------------------------------------------+
|              Phase 5: Symbol Fixup                                       |
|  - Add section base vaddrs to all symbol values                          |
|  - Define linker-provided symbols (__global_pointer$, _edata, etc.)      |
|  - Patch dynsym entries for COPY-relocated symbols                       |
|  - Build local symbol vaddr map for relocation resolution                |
+--------------------------------------------------------------------------+
      |
      v
+--------------------------------------------------------------------------+
|              Phase 6: Relocation Application                             |
|  - Iterate all input relocations, mapped to merged sections              |
|  - Resolve each symbol to its final virtual address                      |
|  - Patch instruction/data bytes in merged section buffers                |
|  - Handle all 30+ RISC-V relocation types                                |
+--------------------------------------------------------------------------+
      |
      v
+--------------------------------------------------------------------------+
|        Phases 7-10: Build GOT, PLT, .rela.plt, .dynamic                 |
|  - Fill GOT entries with resolved symbol addresses                       |
|  - Generate PLT header (resolver stub) and per-symbol PLT entries        |
|  - Build .rela.plt with R_RISCV_JUMP_SLOT entries                        |
|  - Build .rela.dyn with R_RISCV_COPY entries                             |
|  - Build .dynamic section with DT_* tag/value pairs                      |
+--------------------------------------------------------------------------+
      |
      v
+--------------------------------------------------------------------------+
|        Phases 11-14: Final ELF Emission                                  |
|  - Build .eh_frame_hdr (minimal, for basic execution)                    |
|  - Find entry point (_start or main)                                     |
|  - Write ELF header, program headers, section data, section headers      |
|  - Set file permissions to 0755                                          |
+--------------------------------------------------------------------------+
      |
      v
                    ELF Executable
```

## File Inventory

| File          | Lines | Role                                                          |
|---------------|-------|---------------------------------------------------------------|
| `mod.rs`      | ~13   | Module declaration; re-exports `link_to_executable` as the public API |
| `link.rs`     | ~2570 | Core linker implementation: all 14 phases from input reading through ELF emission |
| `elf_read.rs` | ~67   | Re-exports shared linker_common types; delegates parsing to shared infrastructure |

## Key Data Structures

### `ElfObject` (linker_common via elf_read.rs)

A fully parsed relocatable ELF64 object file. This type is shared with the
x86 and ARM backends via `linker_common::Elf64Object`.

```
ElfObject {
    sections:      Vec<Elf64Section>,          // parsed section headers
    symbols:       Vec<Elf64Symbol>,           // full symbol table
    section_data:  Vec<Vec<u8>>,               // raw data for each section (empty for NOBITS)
    relocations:   Vec<Vec<Elf64Rela>>,        // relocs indexed by target section
    source_name:   String,                     // source file name
}
```

### `Elf64Section` (linker_common)

A single section header from an input object file.

```
Elf64Section {
    name:       String,    // e.g., ".text", ".rodata", ".rela.text"
    sh_type:    u32,       // SHT_PROGBITS, SHT_NOBITS, SHT_RELA, ...
    flags:      u64,       // SHF_ALLOC | SHF_WRITE | SHF_EXECINSTR | SHF_TLS
    size:       u64,       // section size in bytes
    addralign:  u64,       // required alignment
    entsize:    u64,       // entry size for uniform-entry sections
    link:       u32,       // linked section index (e.g., .symtab -> .strtab)
    info:       u32,       // extra info (e.g., .rela.text -> .text section index)
}
```

### `Elf64Symbol` (linker_common)

A parsed ELF symbol table entry. Binding, type, and visibility are accessed
via methods: `binding()`, `sym_type()`, `visibility()`.

```
Elf64Symbol {
    name:   String,   // symbol name from .strtab
    value:  u64,      // st_value (offset within section, or alignment for COMMON)
    size:   u64,      // st_size
    info:   u8,       // packed binding (>>4) and type (&0xf)
    other:  u8,       // packed visibility (&0x3)
    shndx:  u16,      // section index, or SHN_UNDEF / SHN_ABS / SHN_COMMON
}
```

### `Elf64Rela` (linker_common)

A single relocation entry from a `.rela.*` section.

```
Elf64Rela {
    offset:     u64,     // byte offset within the target section
    rela_type:  u32,     // R_RISCV_* relocation type
    sym_idx:    u32,     // index into the object's symbol table
    addend:     i64,     // explicit addend (Elf64_Rela format)
}
```

### `DynSymbol` (linker_common)

Information about a symbol exported by a shared library.

```
DynSymbol {
    name:   String,    // symbol name
    info:   u8,        // packed type; use sym_type() method for STT_FUNC/STT_OBJECT
    size:   u64,       // st_size (needed for COPY relocation allocation)
}
```

### `MergedSection` (link.rs)

A merged output section containing data concatenated from multiple input sections.

```
MergedSection {
    name:      String,    // output section name (".text", ".data", ...)
    sh_type:   u32,       // SHT_PROGBITS or SHT_NOBITS
    sh_flags:  u64,       // union of all contributing input section flags
    data:      Vec<u8>,   // concatenated section data
    vaddr:     u64,       // assigned virtual address (set in Phase 4)
    align:     u64,       // maximum alignment across all contributing sections
}
```

### `GlobalSym` (link.rs)

Represents a global symbol's resolved state during linking.

```
GlobalSym {
    value:       u64,            // virtual address (or in-section offset before fixup)
    size:        u64,
    binding:     u8,             // STB_GLOBAL / STB_WEAK
    sym_type:    u8,             // STT_FUNC / STT_OBJECT / STT_NOTYPE
    visibility:  u8,
    defined:     bool,           // true if defined in an input object
    needs_plt:   bool,           // true if this is a PLT-resolved dynamic function
    plt_idx:     usize,          // index into the PLT entry array
    got_offset:  Option<u64>,    // offset within .got (if this symbol has a GOT entry)
    section_idx: Option<usize>,  // index into merged_sections (if defined locally)
}
```

### `InputSecRef` (link.rs, local to link_to_executable)

Tracks the mapping from an input section to its position in a merged section.

```
InputSecRef {
    obj_idx:           usize,   // index into input_objs
    sec_idx:           usize,   // section index within that object
    merged_sec_idx:    usize,   // index into merged_sections
    offset_in_merged:  u64,     // byte offset within the merged section
}
```

## Processing Algorithm Step by Step

### Phase 1: Input Collection

1. Parse command-line flags to determine linking mode (`-static`, `-nostdlib`).
2. Auto-discover CRT directories by searching well-known paths for
   `crt1.o`, `crti.o`, `crtbegin.o`, etc. The search covers both native and
   cross-compiler install layouts:
   - `/usr/lib/gcc-cross/riscv64-linux-gnu/{14..8}/`
   - `/usr/lib/gcc/riscv64-linux-gnu/{14..8}/`
   - `/usr/riscv64-linux-gnu/lib/`
   - `/usr/lib/riscv64-linux-gnu/`
3. Unless `-nostdlib`, prepend CRT startup objects (`crt1.o`, `crti.o`,
   `crtbegin.o`) and append CRT finalization objects (`crtend.o`, `crtn.o`).
4. Read each input file. If it starts with `!<arch>\n`, parse as a GNU `ar`
   archive (extracting all ELF members). If it starts with `\x7fELF`, parse
   as a single object file. Other files (e.g., linker scripts) are silently
   skipped.
5. Parse `-L` paths and `-l` library names from user arguments, including
   `-Wl,` pass-through syntax.
6. Add default library search paths and default libraries (`-lgcc -lgcc_eh -lc -lm`
   for static linking, `-lgcc -lgcc_s -lc -lm` for dynamic linking).

### Phase 1b: Library Resolution

The linker builds a set of defined and undefined symbols from the initial
input objects. Then:

1. **Shared library discovery**: For each `-l` library, search for `lib<name>.so`
   in the library search paths. If the `.so` file is actually a GNU linker
   script (common for libc.so), parse the script text to find the real shared
   object paths and any non-shared archive references (e.g., `libc_nonshared.a`).
   Read the `.dynsym` section of each real shared library to collect all
   exported symbol names, types, and sizes.

2. **Archive group-loading**: All archives are iterated in an outer group
   loop until no new objects are added, handling circular cross-archive
   dependencies (e.g., `libm -> libgcc -> libc -> libgcc`). Within each
   archive, a multi-pass iterative algorithm pulls in members on demand:
   ```
   # Outer group loop (cross-archive)
   loop {
       prev_count = input_objs.len()
       for each archive:
           # Inner per-archive loop
           loop {
               added_any = false
               for each unprocessed archive member:
                   if member defines any currently-undefined symbol:
                       add member to input_objs
                       update defined_syms and undefined_syms
                       added_any = true
               if !added_any: break
       if input_objs.len() == prev_count: break
   }
   ```
   This handles both intra-archive transitive dependencies and cross-archive
   circular dependencies.

3. **SONAME discovery**: For each `-l` library, the linker searches the
   library directory for versioned shared object names (e.g., `libc.so.6`)
   to use as the DT_NEEDED entry, preferring the shortest versioned name.

### Phase 2: Section Merging

Input sections are grouped into output sections by name prefix:

| Input Section Pattern          | Output Section   |
|--------------------------------|------------------|
| `.text`, `.text.*`             | `.text`          |
| `.rodata`, `.rodata.*`         | `.rodata`        |
| `.data`, `.data.*`, `.data.rel.ro*` | `.data`    |
| `.tdata`, `.tdata.*`           | `.tdata`         |
| `.tbss`, `.tbss.*`             | `.tbss`          |
| `.bss`, `.bss.*`               | `.bss`           |
| `.sdata`, `.sdata.*`           | `.sdata`         |
| `.sbss`, `.sbss.*`             | `.sbss`          |
| `.init_array`, `.init_array.*` | `.init_array`    |
| `.fini_array`, `.fini_array.*` | `.fini_array`    |
| `.preinit_array`, `.preinit_array.*` | `.preinit_array` |
| `.eh_frame`                    | `.eh_frame`      |
| `.riscv.attributes`            | `.riscv.attributes` (first instance only) |

Non-allocatable sections (debug info, comment sections) are silently dropped,
as are `.note.GNU-stack` sections. Group sections (`SHT_GROUP`), symbol tables,
string tables, and relocation sections are consumed structurally but not
merged as content sections.

For each input section that maps to a merged section:
1. Align the current end of the merged section to the input section's alignment.
2. Append the input section's data.
3. Record the `(obj_idx, sec_idx) -> (merged_idx, offset)` mapping for
   relocation fixup.
4. Update the merged section's flags (union of SHF_WRITE, SHF_ALLOC,
   SHF_EXECINSTR, SHF_TLS) and alignment (maximum).

### Phase 3: Symbol Table Construction

The linker builds a global symbol table (`HashMap<String, GlobalSym>`)
in two passes:

**First pass -- define symbols**: For each non-local symbol in each input
object:
- If `section_idx == SHN_UNDEF`: register as undefined (placeholder entry).
- If `section_idx == SHN_ABS`: define as absolute symbol.
- If `section_idx == SHN_COMMON`: allocate space in `.bss` (aligned to
  `st_value`, sized to `st_size`). If multiple COMMON definitions exist,
  the largest size wins.
- Otherwise: compute the symbol's in-section offset using the section mapping,
  mark as defined. A global definition always overrides a weak one; a weak
  definition never overrides a global one.

**Second pass -- classify dynamic symbols**:
- Undefined function symbols that appear in shared library exports are marked
  `needs_plt = true` and assigned a PLT index.
- Undefined data symbols (`STT_OBJECT`) from shared libraries are recorded
  for COPY relocation (allocated in `.bss` at link time).
- Symbols referenced via `R_RISCV_GOT_HI20`, `R_RISCV_TLS_GOT_HI20`, or
  `R_RISCV_TLS_GD_HI20` relocations are collected for GOT slot allocation.

### Phase 4: Address Layout

The linker lays out the executable image in two LOAD segments:

```
+================================================================+
|  LOAD Segment 0 (RX -- Read + Execute)                         |
|----------------------------------------------------------------|
|  ELF Header (64 bytes)                                         |
|  Program Headers (56 bytes x N)                                |
|  .interp          (path to ld-linux-riscv64-lp64d.so.1)        |
|  .gnu.hash        (GNU hash table for .dynsym)                 |
|  .dynsym          (dynamic symbol table)                       |
|  .dynstr          (dynamic string table)                       |
|  .gnu.version     (symbol versioning, may be empty)            |
|  .gnu.version_r   (version requirements, may be empty)         |
|  .rela.dyn        (COPY relocations for data imports)          |
|  .rela.plt        (JUMP_SLOT relocations for PLT)              |
|  .plt             (Procedure Linkage Table)                    |
|  .text            (merged executable code)                     |
|  .rodata          (merged read-only data)                      |
|  .eh_frame        (exception handling frames)                  |
+================================================================+
  --- page boundary (0x1000 alignment) ---
+================================================================+
|  LOAD Segment 1 (RW -- Read + Write)                           |
|----------------------------------------------------------------|
|  RELRO region (mprotected to RO after startup):                |
|    .preinit_array  (pre-initialization function pointers)      |
|    .init_array     (initialization function pointers)          |
|    .fini_array     (finalization function pointers)            |
|    .dynamic        (dynamic linker control block)              |
|    .got            (Global Offset Table for pcrel GOT refs)    |
|  End of RELRO                                                  |
|  .got.plt          (GOT entries for PLT lazy binding)          |
|  .data             (merged initialized data)                   |
|  .sdata            (small initialized data)                    |
|  .tdata            (initialized thread-local data)             |
|  .tbss             (uninitialized thread-local data, no file)  |
|  .bss              (uninitialized data, no file space)         |
|  (COPY relocation allocations appended to .bss)                |
+================================================================+
```

The base virtual address is `0x10000`. The page size is `0x1000` (4 KiB).
File offsets and virtual addresses are kept congruent modulo page size, as
required for mmap-based loading.

The canonical section ordering within each segment is controlled by a priority
function:

| Priority | Sections                                      |
|----------|-----------------------------------------------|
| 100      | `.text`                                       |
| 150      | Other executable sections                     |
| 200      | `.rodata`                                     |
| 250      | `.eh_frame_hdr`                               |
| 260      | `.eh_frame`                                   |
| 300      | Other read-only sections                      |
| 500      | `.preinit_array`                               |
| 510      | `.init_array`                                  |
| 520      | `.fini_array`                                  |
| 600      | `.data` and other writable sections            |
| 650      | `.sdata`                                       |
| 700      | `.bss`, `.sbss`                                |

### Phase 5: Symbol Fixup

After layout, every global symbol's `value` is adjusted by adding its merged
section's base virtual address.  Then the linker defines a comprehensive set
of linker-provided symbols:

| Symbol                    | Value                                         |
|---------------------------|-----------------------------------------------|
| `__global_pointer$`       | `.sdata` + 0x800 (RISC-V GP convention)        |
| `_GLOBAL_OFFSET_TABLE_`   | Start of `.got.plt`                            |
| `_DYNAMIC`                | Start of `.dynamic`                            |
| `_edata`                  | End of `.sdata` (or `.data`)                   |
| `__bss_start`             | Start of `.bss`                                |
| `_end` / `__BSS_END__`    | End of `.bss`                                  |
| `__SDATA_BEGIN__`          | Start of `.sdata`                              |
| `__DATA_BEGIN__` / `data_start` / `__data_start` | Start of `.data`    |
| `__dso_handle`            | Start of `.data`                               |
| `_IO_stdin_used`          | Start of `.rodata`                             |
| `__init_array_start/end`  | Bounds of `.init_array`                        |
| `__fini_array_start/end`  | Bounds of `.fini_array`                        |
| `__preinit_array_start/end` | Bounds of `.preinit_array`                   |
| `__ehdr_start`            | Base address (0x10000)                         |
| `__rela_iplt_start/end`   | 0 (no IPLT support)                            |
| Various weak symbols      | 0 (ITM, pthread, GCC personality, Unwind, ...) |

The linker also builds the local symbol virtual address map (`local_sym_vaddrs`)
that maps each `(obj_idx, sym_idx)` to its final virtual address, used for
resolving relocations that reference local symbols.

### Phase 6: Relocation Application

This is the largest phase. For every relocation in every input object, the
linker:

1. Maps the relocation's target section + offset to the merged section and
   its adjusted offset.
2. Resolves the referenced symbol to a virtual address `S`, considering:
   - Section symbols: use the section's merged base address.
   - Global symbols: look up in `global_syms`; PLT symbols use their PLT
     entry address for `CALL_PLT` relocations.
   - Local symbols: look up in `local_sym_vaddrs`.
3. Computes the relocation value and patches the bytes in the merged section
   data buffer.

#### Supported Relocation Types

| Type                    | Code | Calculation          | Instruction Format     |
|-------------------------|------|----------------------|------------------------|
| `R_RISCV_32`            | 1    | S + A                | 4-byte data            |
| `R_RISCV_64`            | 2    | S + A                | 8-byte data            |
| `R_RISCV_BRANCH`        | 16   | S + A - P            | B-type (13-bit)        |
| `R_RISCV_JAL`           | 17   | S + A - P            | J-type (21-bit)        |
| `R_RISCV_CALL_PLT`      | 19   | S + A - P            | AUIPC+JALR pair        |
| `R_RISCV_GOT_HI20`      | 20   | G + A - P            | U-type (AUIPC)         |
| `R_RISCV_TLS_GOT_HI20`  | 21   | G + A - P            | U-type (AUIPC)         |
| `R_RISCV_TLS_GD_HI20`   | 22   | G + A - P (relaxed to LE in static) | U-type (AUIPC→LUI) |
| `R_RISCV_PCREL_HI20`    | 23   | S + A - P            | U-type (AUIPC)         |
| `R_RISCV_PCREL_LO12_I`  | 24   | lo12(hi20_target)    | I-type                 |
| `R_RISCV_PCREL_LO12_S`  | 25   | lo12(hi20_target)    | S-type                 |
| `R_RISCV_HI20`          | 26   | (S + A + 0x800) >> 12| U-type (LUI)           |
| `R_RISCV_LO12_I`        | 27   | (S + A) & 0xFFF      | I-type                 |
| `R_RISCV_LO12_S`        | 28   | (S + A) & 0xFFF      | S-type                 |
| `R_RISCV_TPREL_HI20`    | 29   | (S+A-TP+0x800)>>12   | U-type (LUI)           |
| `R_RISCV_TPREL_LO12_I`  | 30   | (S + A - TP) & 0xFFF | I-type                 |
| `R_RISCV_TPREL_LO12_S`  | 31   | (S + A - TP) & 0xFFF | S-type                 |
| `R_RISCV_TPREL_ADD`     | 32   | (no-op hint)         | --                     |
| `R_RISCV_ADD8`           | 33   | *P += S + A          | 1-byte data            |
| `R_RISCV_ADD16`          | 34   | *P += S + A          | 2-byte data            |
| `R_RISCV_ADD32`          | 35   | *P += S + A          | 4-byte data            |
| `R_RISCV_ADD64`          | 36   | *P += S + A          | 8-byte data            |
| `R_RISCV_SUB8`           | 37   | *P -= S + A          | 1-byte data            |
| `R_RISCV_SUB16`          | 38   | *P -= S + A          | 2-byte data            |
| `R_RISCV_SUB32`          | 39   | *P -= S + A          | 4-byte data            |
| `R_RISCV_SUB64`          | 40   | *P -= S + A          | 8-byte data            |
| `R_RISCV_RELAX`          | 51   | (skip -- hint only)  | --                     |
| `R_RISCV_SUB6`           | 52   | *P[5:0] -= (S+A)     | 6-bit sub-byte field   |
| `R_RISCV_SET6`           | 53   | *P[5:0] = (S+A)      | 6-bit sub-byte field   |
| `R_RISCV_SET8`           | 54   | *P = (S + A)         | 1-byte data            |
| `R_RISCV_SET16`          | 55   | *P = (S + A)         | 2-byte data            |
| `R_RISCV_SET32`          | 56   | *P = (S + A)         | 4-byte data            |
| `R_RISCV_32_PCREL`       | 57   | S + A - P            | 4-byte data            |

Legend: S = symbol value, A = addend, P = relocation site address,
G = GOT entry address, TP = thread pointer (TLS base).

#### PCREL_LO12 Resolution

`R_RISCV_PCREL_LO12_I` and `R_RISCV_PCREL_LO12_S` are unusual: their symbol
operand references the *AUIPC instruction* (not the data symbol). The linker
must locate the `PCREL_HI20` or `GOT_HI20` relocation at that AUIPC address,
re-compute the full PC-relative offset from the original hi20 relocation's
symbol, and extract the low 12 bits. This is handled by the `find_hi20_value`
helper, which scans the current section's relocations for a matching hi20
relocation at the AUIPC virtual address.

### Phases 7-10: Dynamic Linking Support

#### PLT Generation (Phase 8)

The PLT uses the standard RISC-V lazy binding layout:

**PLT[0] -- Resolver Stub** (32 bytes):
```asm
auipc   t2, %pcrel_hi(.got.plt)      # t2 = GOT.PLT page
sub     t1, t1, t3                    # compute PLT entry offset
ld      t3, %pcrel_lo(1b)(t2)        # t3 = resolver address (GOT.PLT[0])
addi    t1, t1, -(header_size + 12)  # adjust offset
addi    t0, t2, %pcrel_lo(1b)        # t0 = GOT.PLT base
srli    t1, t1, 1                    # byte offset -> index
ld      t0, 8(t0)                    # t0 = link_map (GOT.PLT[1])
jr      t3                           # jump to resolver
```

**PLT[N] -- Per-Symbol Entry** (16 bytes):
```asm
auipc   t3, %pcrel_hi(GOT.PLT[N+2])
ld      t3, %pcrel_lo(1b)(t3)       # load target address from GOT.PLT
jalr    t1, t3                       # jump (t1 saves return for PLT[0])
nop                                  # padding to 16 bytes
```

#### GOT Layout

```
.got      : One 8-byte entry per GOT_HI20-referenced symbol, filled with
            the symbol's resolved virtual address.

.got.plt  : [0] = 0 (reserved for dynamic linker's resolver address)
            [1] = 0 (reserved for link_map pointer)
            [2..N+2] = PLT[0] address (lazy binding: initial GOT.PLT entries
                       point back to the resolver stub; the dynamic linker
                       overwrites them on first call)
```

#### .dynamic Section (Phase 10)

The `.dynamic` section contains tag/value pairs consumed by the dynamic linker:

| Tag              | Value                              |
|------------------|------------------------------------|
| DT_NEEDED        | One entry per shared library       |
| DT_PREINIT_ARRAY | Address and size of .preinit_array |
| DT_INIT_ARRAY    | Address and size of .init_array    |
| DT_FINI_ARRAY    | Address and size of .fini_array    |
| DT_GNU_HASH      | Address of .gnu.hash               |
| DT_STRTAB        | Address of .dynstr                 |
| DT_SYMTAB        | Address of .dynsym                 |
| DT_STRSZ         | Size of .dynstr                    |
| DT_SYMENT        | 24 (size of Elf64_Sym)             |
| DT_DEBUG         | 0 (filled by ld.so at runtime)     |
| DT_PLTGOT        | Address of .got.plt                |
| DT_PLTRELSZ      | Size of .rela.plt                  |
| DT_PLTREL        | DT_RELA (7)                        |
| DT_JMPREL        | Address of .rela.plt               |
| DT_RELA          | Address of .rela.dyn               |
| DT_RELASZ        | Combined size of rela sections     |
| DT_RELAENT       | 24 (size of Elf64_Rela)            |
| DT_NULL          | Terminator                         |

### Phase 14: ELF Emission

The final phase writes the complete ELF executable:

1. **ELF Header**: 64 bytes, `e_type = ET_EXEC`, `e_machine = EM_RISCV`,
   `e_flags = 0x05` (RVC + double-float ABI), `e_entry` set to `_start`
   (or `main`, or start of `.text`).

2. **Program Headers** (10 or 11 entries):
   - `PT_PHDR`: self-referencing program header table
   - `PT_INTERP`: path to dynamic linker
   - `PT_RISCV_ATTRIBUTES`: RISC-V architecture attributes
   - `PT_LOAD` (RX): read-execute segment
   - `PT_LOAD` (RW): read-write segment
   - `PT_DYNAMIC`: dynamic section
   - `PT_NOTE`: note section (stub)
   - `PT_GNU_EH_FRAME`: exception handling frame header (.eh_frame_hdr with binary search table)
   - `PT_GNU_STACK`: stack permissions (RW, no execute)
   - `PT_GNU_RELRO`: RELRO boundary for .dynamic/.got/init arrays
   - `PT_TLS`: thread-local storage segment (only if TLS sections exist)

3. **Segment Data**: Written in order -- RX segment contents, then RW segment
   contents, then `.riscv.attributes`, each padded to their file offsets.

4. **Section Headers**: A minimal set covering all generated and merged
   sections, plus `.shstrtab`.

5. **Permissions**: The output file is set to mode 0755 on Unix.

## Key Design Decisions and Trade-offs

### 1. Monolithic single-pass architecture

The entire linker is implemented in a single function (`link_to_executable`)
with clearly labeled phases. This was a deliberate choice for simplicity and
debuggability -- the linker's state flows linearly from input to output with
no callbacks, trait objects, or complex control flow. The trade-off is that the
function is large (~2500 lines), but it reads top-to-bottom as a pipeline.

### 2. In-memory linking

All input objects are fully parsed into memory (`Vec<ElfObject>`) before any
processing begins. Section merging happens by copying data into new `Vec<u8>`
buffers. This is simple and fast for the object file sizes typical of a C
compiler, but would not scale to very large link jobs (millions of lines of
code). For this compiler's use case, the approach is more than adequate.

### 3. Iterative archive resolution

Archive members are pulled in via a multi-pass algorithm rather than a
single-pass topological sort. Each pass scans all remaining archive members
and pulls in any that define a currently-undefined symbol. This handles
circular dependencies between archive members naturally, at the cost of
potentially O(N^2) scans for pathological cases. In practice, convergence
is fast (2-3 passes for typical C programs with libc).

### 4. Limited linker relaxation

The linker recognizes `R_RISCV_RELAX` relocations but does not perform general
relaxation optimizations (e.g., converting `auipc+jalr` to `jal`, or `auipc+addi`
to `addi` when the target is within GP range). All instructions retain their
original encoding width, avoiding the complexity of iterative section resizing.

The one exception is **TLS GD→LE relaxation** for static binaries. When
`R_RISCV_TLS_GD_HI20` relocations are encountered in a static link, the General
Dynamic TLS access sequence (`auipc`/`addi`/`call __tls_get_addr`) is rewritten
to Local Exec (`lui %tprel_hi`/`addi %tprel_lo`/`add a0,a0,tp` + `nop`). This
is required because `__tls_get_addr` depends on dynamic linker state that does
not exist in static binaries. The relaxation is performed in a pre-pass that
collects GD auipc addresses and their associated `__tls_get_addr` call sites,
then rewrites the instructions during the main relocation application phase.

### 5. COPY relocations for data imports

When a program references a data object defined in a shared library (e.g.,
`stdout`), the linker allocates space in `.bss` and emits an `R_RISCV_COPY`
relocation. The dynamic linker copies the object's initial value from the
shared library into the executable's `.bss` at startup. This is the standard
approach for non-PIE executables and avoids the complexity of GOT-indirect
data access.

### 6. Minimal .gnu.hash

The linker generates a minimal `.gnu.hash` table with a single zero-entry
bloom filter (accept-all). This is functionally correct -- the dynamic linker
will always fall through to a linear symbol search -- but does not provide the
O(1) lookup performance of a properly computed hash table. For executables
with a small number of dynamic symbols (typical of C programs), this has
negligible performance impact and avoids the complexity of computing proper
DJB hash chains.

### 7. RELRO design

The `.got` section (for `GOT_HI20` references) is placed *inside* the RELRO
region, so it becomes read-only after startup. The `.got.plt` section is placed
*outside* RELRO because it must remain writable for lazy PLT resolution. This
matches the standard RELRO layout used by GNU ld and provides hardening against
GOT overwrite attacks without breaking lazy binding.

### 8. Entry point discovery

The linker looks for `_start` first, then `main`, then falls back to the
start of `.text`. This handles both standard C programs (which define `_start`
in `crt1.o`) and minimal programs that only define `main`. The fallback to
`.text` ensures the executable is always runnable even in unusual configurations.

### 9. No PIE support

The linker produces `ET_EXEC` (fixed-address) executables, not `ET_DYN`
(position-independent executables). This simplifies address layout and
relocation processing. PIE support would require GOT-indirect access for all
global data and function pointers, which is a significant increase in
complexity for a feature not required by the compiler's current use cases.

### 10. Silent tolerance of missing inputs

Missing CRT objects and unresolvable shared libraries are silently skipped
rather than treated as fatal errors. This allows the linker to produce
output in degraded environments (e.g., a cross-compilation sysroot with
incomplete library packages). The resulting executable may not be fully
functional, but the linker does not block the build.
