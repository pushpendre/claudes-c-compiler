//! x86-64 instruction encoder.
//!
//! Encodes parsed x86-64 instructions into machine code bytes.
//! Handles REX prefixes, ModR/M, SIB, and displacement encoding.

use super::parser::*;

/// Relocation entry for the linker to resolve.
#[derive(Debug, Clone)]
pub struct Relocation {
    /// Offset within the section where the relocation applies.
    pub offset: u64,
    /// Symbol name to resolve.
    pub symbol: String,
    /// Relocation type (ELF R_X86_64_* constants).
    pub reloc_type: u32,
    /// Addend for the relocation.
    pub addend: i64,
}

// ELF x86-64 relocation types
#[allow(dead_code)]
pub const R_X86_64_NONE: u32 = 0;
pub const R_X86_64_64: u32 = 1;
pub const R_X86_64_PC32: u32 = 2;
#[allow(dead_code)]
pub const R_X86_64_GOT32: u32 = 3;
pub const R_X86_64_PLT32: u32 = 4;
pub const R_X86_64_32: u32 = 10;
pub const R_X86_64_32S: u32 = 11;
pub const R_X86_64_GOTPCREL: u32 = 9;
pub const R_X86_64_TPOFF32: u32 = 23;
pub const R_X86_64_GOTTPOFF: u32 = 22;
#[allow(dead_code)]
pub const R_X86_64_TPOFF64: u32 = 18;
// Internal-only: 8-bit PC-relative relocation for jrcxz/loop (never emitted to ELF)
pub const R_X86_64_PC8_INTERNAL: u32 = 0x8000_0001;

/// Register encoding (3-bit register number in ModR/M and SIB).
fn reg_num(name: &str) -> Option<u8> {
    match name {
        "al" | "ax" | "eax" | "rax" | "xmm0" | "st" | "st(0)" | "mm0" | "es" | "ymm0" => Some(0),
        "cl" | "cx" | "ecx" | "rcx" | "xmm1" | "st(1)" | "mm1" | "cs" | "ymm1" => Some(1),
        "dl" | "dx" | "edx" | "rdx" | "xmm2" | "st(2)" | "mm2" | "ss" | "ymm2" => Some(2),
        "bl" | "bx" | "ebx" | "rbx" | "xmm3" | "st(3)" | "mm3" | "ds" | "ymm3" => Some(3),
        "ah" | "spl" | "sp" | "esp" | "rsp" | "xmm4" | "st(4)" | "mm4" | "fs" | "ymm4" => Some(4),
        "ch" | "bpl" | "bp" | "ebp" | "rbp" | "xmm5" | "st(5)" | "mm5" | "gs" | "ymm5" => Some(5),
        "dh" | "sil" | "si" | "esi" | "rsi" | "xmm6" | "st(6)" | "mm6" | "ymm6" => Some(6),
        "bh" | "dil" | "di" | "edi" | "rdi" | "xmm7" | "st(7)" | "mm7" | "ymm7" => Some(7),
        "r8b" | "r8w" | "r8d" | "r8" | "xmm8" | "ymm8" => Some(0),
        "r9b" | "r9w" | "r9d" | "r9" | "xmm9" | "ymm9" => Some(1),
        "r10b" | "r10w" | "r10d" | "r10" | "xmm10" | "ymm10" => Some(2),
        "r11b" | "r11w" | "r11d" | "r11" | "xmm11" | "ymm11" => Some(3),
        "r12b" | "r12w" | "r12d" | "r12" | "xmm12" | "ymm12" => Some(4),
        "r13b" | "r13w" | "r13d" | "r13" | "xmm13" | "ymm13" => Some(5),
        "r14b" | "r14w" | "r14d" | "r14" | "xmm14" | "ymm14" => Some(6),
        "r15b" | "r15w" | "r15d" | "r15" | "xmm15" | "ymm15" => Some(7),
        _ => None,
    }
}

/// Is this an MMX register?
fn is_mmx(name: &str) -> bool {
    name.starts_with("mm") && !name.starts_with("mmx")
        && name.len() <= 3
        && name.as_bytes().get(2).is_some_and(|c| c.is_ascii_digit())
}

/// Is this a segment register?
fn is_segment_reg(name: &str) -> bool {
    matches!(name, "es" | "cs" | "ss" | "ds" | "fs" | "gs")
}

fn is_control_reg(name: &str) -> bool {
    matches!(name, "cr0" | "cr2" | "cr3" | "cr4" | "cr8")
}

fn control_reg_num(name: &str) -> Option<u8> {
    match name {
        "cr0" => Some(0),
        "cr2" => Some(2),
        "cr3" => Some(3),
        "cr4" => Some(4),
        "cr8" => Some(8),
        _ => None,
    }
}

/// Is this a YMM register?
fn is_ymm(name: &str) -> bool {
    name.starts_with("ymm")
}

/// Does this register need the REX.B/R/X extension bit?
fn needs_rex_ext(name: &str) -> bool {
    name.starts_with("r8") || name.starts_with("r9") || name.starts_with("r10")
        || name.starts_with("r11") || name.starts_with("r12") || name.starts_with("r13")
        || name.starts_with("r14") || name.starts_with("r15")
        || name.starts_with("xmm8") || name.starts_with("xmm9")
        || name.starts_with("xmm10") || name.starts_with("xmm11")
        || name.starts_with("xmm12") || name.starts_with("xmm13")
        || name.starts_with("xmm14") || name.starts_with("xmm15")
        || name.starts_with("ymm8") || name.starts_with("ymm9")
        || name.starts_with("ymm10") || name.starts_with("ymm11")
        || name.starts_with("ymm12") || name.starts_with("ymm13")
        || name.starts_with("ymm14") || name.starts_with("ymm15")
}

/// Does this register need the VEX.B extension bit? Same as REX ext but for VEX-encoded instructions.
fn needs_vex_ext(name: &str) -> bool {
    needs_rex_ext(name)
}

/// Is this a 64-bit GP register?
fn is_reg64(name: &str) -> bool {
    matches!(name, "rax" | "rcx" | "rdx" | "rbx" | "rsp" | "rbp" | "rsi" | "rdi"
        | "r8" | "r9" | "r10" | "r11" | "r12" | "r13" | "r14" | "r15")
}

/// Is this a 32-bit GP register?
#[allow(dead_code)]
fn is_reg32(name: &str) -> bool {
    matches!(name, "eax" | "ecx" | "edx" | "ebx" | "esp" | "ebp" | "esi" | "edi"
        | "r8d" | "r9d" | "r10d" | "r11d" | "r12d" | "r13d" | "r14d" | "r15d")
}

/// Is this a 16-bit GP register?
#[allow(dead_code)]
fn is_reg16(name: &str) -> bool {
    matches!(name, "ax" | "cx" | "dx" | "bx" | "sp" | "bp" | "si" | "di"
        | "r8w" | "r9w" | "r10w" | "r11w" | "r12w" | "r13w" | "r14w" | "r15w")
}

/// Is this an 8-bit GP register?
#[allow(dead_code)]
fn is_reg8(name: &str) -> bool {
    matches!(name, "al" | "cl" | "dl" | "bl" | "ah" | "ch" | "dh" | "bh"
        | "spl" | "bpl" | "sil" | "dil"
        | "r8b" | "r9b" | "r10b" | "r11b" | "r12b" | "r13b" | "r14b" | "r15b")
}

/// Does this 8-bit register require REX prefix for access (spl, bpl, sil, dil)?
fn is_rex_required_8bit(name: &str) -> bool {
    matches!(name, "spl" | "bpl" | "sil" | "dil")
}

/// Is this an XMM register?
fn is_xmm(name: &str) -> bool {
    name.starts_with("xmm")
}

/// Is this an XMM or YMM register?
fn is_xmm_or_ymm(name: &str) -> bool {
    name.starts_with("xmm") || name.starts_with("ymm")
}

/// Is this an x87 FPU register?
#[allow(dead_code)]
fn is_st(name: &str) -> bool {
    name == "st" || name.starts_with("st(")
}

/// Get the operand-size suffix character for a register name.
/// Returns 'q' for 64-bit, 'l' for 32-bit, 'w' for 16-bit, 'b' for 8-bit.
fn register_size_suffix(name: &str) -> Option<char> {
    if is_reg64(name) { return Some('q'); }
    if is_reg32(name) { return Some('l'); }
    if is_reg16(name) { return Some('w'); }
    if is_reg8(name) { return Some('b'); }
    None
}

/// Set of base mnemonics that accept AT&T size suffixes (b/w/l/q).
/// Only these mnemonics will have suffixes inferred from operand types.
const SUFFIXABLE_MNEMONICS: &[&str] = &[
    "mov", "add", "sub", "and", "or", "xor", "cmp", "test",
    "push", "pop", "lea",
    "shl", "shr", "sar", "rol", "ror",
    "inc", "dec", "neg", "not",
    "imul", "mul", "div", "idiv",
    "adc", "sbb",
    "xchg", "cmpxchg", "xadd", "bswap",
    "bsf", "bsr",
];

/// Infer the AT&T size suffix for an unsuffixed mnemonic from its operands.
///
/// Hand-written assembly (e.g., musl's .s files) often omits the size suffix
/// when it can be inferred from register operands. For example:
///   push %rax  -> pushq %rax
///   mov %edi,%ecx -> movl %edi,%ecx
///   and $0x3f,%ecx -> andl $0x3f,%ecx
///
/// Only mnemonics in the SUFFIXABLE_MNEMONICS whitelist are candidates for
/// suffix inference. All others are returned as-is.
fn infer_suffix(mnemonic: &str, ops: &[Operand]) -> String {
    // Check if this is a known suffixable mnemonic
    if !SUFFIXABLE_MNEMONICS.contains(&mnemonic) {
        return mnemonic.to_string();
    }

    // For shift/rotate instructions, infer size from the *destination* (last) operand,
    // not %cl (the first operand). E.g., "shl %cl, %edx" should become "shll", not "shlb".
    let is_shift = matches!(mnemonic, "shl" | "shr" | "sar" | "rol" | "ror");
    if is_shift && ops.len() == 2 {
        if let Operand::Register(r) = &ops[1] {
            if let Some(suffix) = register_size_suffix(&r.name) {
                return format!("{}{}", mnemonic, suffix);
            }
        }
    }

    // Find the first register operand to determine size
    for op in ops {
        if let Operand::Register(r) = op {
            if let Some(suffix) = register_size_suffix(&r.name) {
                return format!("{}{}", mnemonic, suffix);
            }
        }
    }

    // No register operand found - return as-is
    mnemonic.to_string()
}

/// Get operand size from mnemonic suffix.
fn mnemonic_size_suffix(mnemonic: &str) -> Option<u8> {
    // Handle mnemonics that don't follow the simple suffix pattern
    match mnemonic {
        "cltq" | "cqto" | "cltd" | "cdq" | "cqo" | "ret" | "nop" | "ud2"
        | "endbr64" | "pause" | "mfence" | "lfence" | "sfence" | "clflush"
        | "syscall" | "sysenter" | "cpuid" | "rdtsc" | "rdtscp" | "rdpmc"
        | "clc" | "stc" | "cli" | "sti" | "cld" | "std" | "sahf" | "lahf" | "fninit" | "fwait" | "wait" | "fnstcw" | "fstcw"
        | "fld1" | "fldl2e" | "fldlg2" | "fldln2" | "fldz" | "fldpi" | "fldl2t"
        | "fabs" | "fsqrt" | "frndint" | "f2xm1" | "fscale" | "fpatan" | "fprem" | "fprem1"
        | "fyl2x" | "fyl2xp1" | "fptan" | "fsin" | "fcos" | "fxtract" | "fnclex" | "fxch"
        | "fadd" | "fmul" | "fsub" | "fdiv" | "fnstenv" | "fldenv" | "fnstsw"
        | "ldmxcsr" | "stmxcsr" | "wbinvd" | "invd" | "rdsspq" | "rdsspd"
        | "pushf" | "pushfq" | "popf" | "popfq" | "int3"
        | "movsq" | "stosq" | "movsw" | "stosw" | "lodsb" | "lodsw" | "lodsd" | "lodsq"
        | "scasb" | "scasw" | "scasd" | "scasq" | "cmpsb" | "cmpsw" | "cmpsd" | "cmpsq"
        | "insb" | "insw" | "insd" | "insl" | "outsb" | "outsw" | "outsd" | "outsl"
        | "inb" | "inw" | "inl" | "outb" | "outw" | "outl" => return None,
        _ => {}
    }

    let last = mnemonic.as_bytes().last()?;
    match last {
        b'b' => Some(1),
        b'w' => Some(2),
        b'l' | b'd' => Some(4),
        b'q' => Some(8),
        _ => None,
    }
}

/// Instruction encoding context.
pub struct InstructionEncoder {
    /// Output bytes.
    pub bytes: Vec<u8>,
    /// Relocations generated during encoding.
    pub relocations: Vec<Relocation>,
    /// Current offset within the section.
    pub offset: u64,
}

impl InstructionEncoder {
    pub fn new() -> Self {
        InstructionEncoder {
            bytes: Vec::new(),
            relocations: Vec::new(),
            offset: 0,
        }
    }

    /// Encode a single instruction and append bytes.
    pub fn encode(&mut self, instr: &Instruction) -> Result<(), String> {
        let start_len = self.bytes.len();

        // Handle prefix
        if let Some(ref prefix) = instr.prefix {
            match prefix.as_str() {
                "lock" => self.bytes.push(0xF0),
                "rep" | "repz" | "repe" => self.bytes.push(0xF3),
                "repnz" | "repne" => self.bytes.push(0xF2),
                "notrack" => self.bytes.push(0x3E),
                _ => return Err(format!("unknown prefix: {}", prefix)),
            }
        }

        let result = self.encode_mnemonic(instr);

        if result.is_ok() {
            self.offset += (self.bytes.len() - start_len) as u64;
        }

        result
    }

    /// Main mnemonic dispatch.
    fn encode_mnemonic(&mut self, instr: &Instruction) -> Result<(), String> {
        // Infer suffix for unsuffixed mnemonics (e.g., push -> pushq, mov -> movl)
        // This enables assembling hand-written .s files that omit AT&T size suffixes.
        let suffixed = infer_suffix(&instr.mnemonic, &instr.operands);
        let mnemonic = suffixed.as_str();
        let ops = &instr.operands;

        match mnemonic {
            // Data movement
            "movq" => {
                // Check if any operand is an XMM register - route to SSE movq
                let has_xmm = ops.iter().any(|op| matches!(op, Operand::Register(r) if is_xmm(&r.name)));
                let has_mmx = ops.iter().any(|op| matches!(op, Operand::Register(r) if is_mmx(&r.name)));
                let has_seg = ops.iter().any(|op| matches!(op, Operand::Register(r) if is_segment_reg(&r.name)));
                let has_cr = ops.iter().any(|op| matches!(op, Operand::Register(r) if is_control_reg(&r.name)));
                if has_xmm {
                    self.encode_movq_xmm(ops)
                } else if has_mmx {
                    self.encode_mmx_movq(ops)
                } else if has_seg {
                    self.encode_mov_seg(ops)
                } else if has_cr {
                    self.encode_mov_cr(ops)
                } else {
                    self.encode_mov(ops, 8)
                }
            }
            "movl" => {
                if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_segment_reg(&r.name))) {
                    self.encode_mov_seg(ops)
                } else if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_control_reg(&r.name))) {
                    self.encode_mov_cr(ops)
                } else {
                    self.encode_mov(ops, 4)
                }
            }
            "movw" => {
                if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_segment_reg(&r.name))) {
                    self.encode_mov_seg(ops)
                } else {
                    self.encode_mov(ops, 2)
                }
            }
            "movb" => self.encode_mov(ops, 1),
            "movabsq" | "movabs" => self.encode_movabs(ops),
            "movslq" => self.encode_movsx(ops, 4, 8),
            "movsbq" => self.encode_movsx(ops, 1, 8),
            "movswq" => self.encode_movsx(ops, 2, 8),
            "movsbl" => self.encode_movsx(ops, 1, 4),
            "movswl" => self.encode_movsx(ops, 2, 4),
            "movzbq" | "movzbl" => self.encode_movzx(ops, 1, if mnemonic == "movzbq" { 8 } else { 4 }),
            "movzwq" | "movzwl" => self.encode_movzx(ops, 2, if mnemonic == "movzwq" { 8 } else { 4 }),

            // LEA
            "leaq" => self.encode_lea(ops, 8),
            "leal" => self.encode_lea(ops, 4),

            // Stack ops
            "pushq" => self.encode_push(ops),
            "popq" => self.encode_pop(ops),

            // Arithmetic
            "addq" | "addl" | "addw" | "addb" => self.encode_alu(ops, mnemonic, 0),
            "orq" | "orl" | "orw" | "orb" => self.encode_alu(ops, mnemonic, 1),
            "adcq" | "adcl" => self.encode_alu(ops, mnemonic, 2),
            "sbbq" | "sbbl" => self.encode_alu(ops, mnemonic, 3),
            "andq" | "andl" | "andw" | "andb" => self.encode_alu(ops, mnemonic, 4),
            "subq" | "subl" | "subw" | "subb" => self.encode_alu(ops, mnemonic, 5),
            "xorq" | "xorl" | "xorw" | "xorb" => self.encode_alu(ops, mnemonic, 6),
            "cmpq" | "cmpl" | "cmpw" | "cmpb" => self.encode_alu(ops, mnemonic, 7),
            "testq" | "testl" | "testw" | "testb" => self.encode_test(ops, mnemonic),

            // Multiply/divide
            "imulq" => self.encode_imul(ops, 8),
            "imull" => self.encode_imul(ops, 4),
            "mulq" => self.encode_unary_rm(ops, 4, 8),
            "divq" => self.encode_unary_rm(ops, 6, 8),
            "divl" => self.encode_unary_rm(ops, 6, 4),
            "idivq" => self.encode_unary_rm(ops, 7, 8),
            "idivl" => self.encode_unary_rm(ops, 7, 4),

            // Unary
            "negq" => self.encode_unary_rm(ops, 3, 8),
            "negl" => self.encode_unary_rm(ops, 3, 4),
            "notq" => self.encode_unary_rm(ops, 2, 8),
            "notl" => self.encode_unary_rm(ops, 2, 4),
            "incq" => self.encode_inc_dec(ops, 0, 8),
            "incl" => self.encode_inc_dec(ops, 0, 4),
            "incw" => self.encode_inc_dec(ops, 0, 2),
            "incb" => self.encode_inc_dec(ops, 0, 1),
            "decq" => self.encode_inc_dec(ops, 1, 8),
            "decl" => self.encode_inc_dec(ops, 1, 4),
            "decw" => self.encode_inc_dec(ops, 1, 2),
            "decb" => self.encode_inc_dec(ops, 1, 1),

            // Shifts
            "shlq" | "shll" | "shlw" | "shlb" => self.encode_shift(ops, mnemonic, 4),
            "shrq" | "shrl" | "shrw" | "shrb" => self.encode_shift(ops, mnemonic, 5),
            "sarq" | "sarl" | "sarw" | "sarb" => self.encode_shift(ops, mnemonic, 7),
            "rolq" | "roll" | "rolw" | "rolb" => self.encode_shift(ops, mnemonic, 0),
            "rorq" | "rorl" | "rorw" | "rorb" => self.encode_shift(ops, mnemonic, 1),

            // Double-precision shifts
            "shldq" => self.encode_double_shift(ops, 0xA4, 8),
            "shrdq" => self.encode_double_shift(ops, 0xAC, 8),

            // Sign extension
            "cltq" => { self.bytes.extend_from_slice(&[0x48, 0x98]); Ok(()) }
            "cqto" | "cqo" => { self.bytes.extend_from_slice(&[0x48, 0x99]); Ok(()) }
            "cltd" | "cdq" => { self.bytes.push(0x99); Ok(()) }

            // Byte swap
            "bswapl" => self.encode_bswap(ops, 4),
            "bswapq" => self.encode_bswap(ops, 8),

            // Bit operations
            "lzcntl" | "lzcntq" | "lzcntw" | "tzcntl" | "tzcntq" | "tzcntw"
            | "popcntl" | "popcntq" | "popcntw" => {
                self.encode_bit_count(ops, mnemonic)
            }
            "bsfl" | "bsfq" | "bsrl" | "bsrq" => self.encode_bsf_bsr(ops, mnemonic),
            "btq" | "btl" | "btsq" | "btsl" | "btrq" | "btrl" | "btcq" | "btcl" => {
                self.encode_bt(ops, mnemonic)
            }

            // Conditional set
            "sete" | "setz" | "setne" | "setnz" | "setl" | "setle" | "setg" | "setge"
            | "setb" | "setc" | "setbe" | "seta" | "setae" | "setnc" | "setnb" | "setnp" | "setp"
            | "sets" | "setns" | "seto" | "setno"
            | "setnae" | "setnbe" | "setnge" | "setng" | "setnle" | "setnl"
            | "setpe" | "setpo" => self.encode_setcc(ops, mnemonic),

            // Conditional move
            "cmoveq" | "cmovneq" | "cmovlq" | "cmovleq" | "cmovgq" | "cmovgeq"
            | "cmovbq" | "cmovbeq" | "cmovaq" | "cmovaeq"
            | "cmovel" | "cmovnel" | "cmovll" | "cmovlel" | "cmovgl" | "cmovgel"
            | "cmovbl" | "cmovbel" | "cmoval" | "cmovael"
            | "cmovew" | "cmovnew" | "cmovlw" | "cmovlew" | "cmovgw" | "cmovgew"
            | "cmovbw" | "cmovbew" | "cmovaw" | "cmovaew" => self.encode_cmovcc(ops, mnemonic),

            // Jumps (jmpq is a common AT&T alias for jmp on x86-64)
            "jmp" | "jmpq" => self.encode_jmp(ops),
            "je" | "jz" | "jne" | "jnz" | "jl" | "jle" | "jg" | "jge"
            | "jb" | "jbe" | "ja" | "jae" | "js" | "jns" | "jo" | "jno" | "jp" | "jnp"
            | "jc" | "jnc" => {
                self.encode_jcc(ops, mnemonic)
            }

            // Call/return (callq/retq are common AT&T aliases on x86-64)
            "call" | "callq" => self.encode_call(ops),
            "ret" | "retq" => {
                if ops.is_empty() {
                    self.bytes.push(0xC3);
                } else if let Some(Operand::Immediate(ImmediateValue::Integer(val))) = ops.first() {
                    // ret $imm16 - pop return address and deallocate imm16 bytes
                    self.bytes.push(0xC2);
                    self.bytes.extend_from_slice(&(*val as u16).to_le_bytes());
                } else {
                    return Err("unsupported ret operand".to_string());
                }
                Ok(())
            }

            // Standalone prefix bytes (used when prefix and instruction are on separate
            // lines or separated by ; in inline asm, e.g., "lock ; cmpxchg %r, %m")
            "lock" if ops.is_empty() => { self.bytes.push(0xF0); Ok(()) }
            "rep" | "repe" | "repz" if ops.is_empty() => { self.bytes.push(0xF3); Ok(()) }
            "repne" | "repnz" if ops.is_empty() => { self.bytes.push(0xF2); Ok(()) }

            // System instructions
            "syscall" => { self.bytes.extend_from_slice(&[0x0F, 0x05]); Ok(()) }
            "sysenter" => { self.bytes.extend_from_slice(&[0x0F, 0x34]); Ok(()) }
            "cpuid" => { self.bytes.extend_from_slice(&[0x0F, 0xA2]); Ok(()) }
            "rdtsc" => { self.bytes.extend_from_slice(&[0x0F, 0x31]); Ok(()) }
            "rdtscp" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xF9]); Ok(()) }
            "wbinvd" => { self.bytes.extend_from_slice(&[0x0F, 0x09]); Ok(()) }
            "invd" => { self.bytes.extend_from_slice(&[0x0F, 0x08]); Ok(()) }

            // No-ops and misc
            "nop" => { self.bytes.push(0x90); Ok(()) }
            "hlt" => { self.bytes.push(0xF4); Ok(()) }
            "leave" | "leaveq" => { self.bytes.push(0xC9); Ok(()) }
            "ud2" => { self.bytes.extend_from_slice(&[0x0F, 0x0B]); Ok(()) }
            "endbr64" => { self.bytes.extend_from_slice(&[0xF3, 0x0F, 0x1E, 0xFA]); Ok(()) }
            // CET shadow stack: rdsspq %r64, rdsspd %r32
            "rdsspq" => {
                if let Some(Operand::Register(reg)) = ops.first() {
                    let rm = reg_num(&reg.name).ok_or_else(|| format!("unknown register: {}", reg.name))?;
                    self.bytes.push(0xF3);
                    // REX.W prefix (with REX.B if needed for r8-r15)
                    let rex_b = if needs_rex_ext(&reg.name) { 1 } else { 0 };
                    self.bytes.push(0x48 | rex_b);
                    self.bytes.extend_from_slice(&[0x0F, 0x1E]);
                    self.bytes.push(0xC8 | rm);
                    Ok(())
                } else {
                    Err("rdsspq requires a 64-bit register operand".to_string())
                }
            }
            "rdsspd" => {
                if let Some(Operand::Register(reg)) = ops.first() {
                    let rm = reg_num(&reg.name).ok_or_else(|| format!("unknown register: {}", reg.name))?;
                    self.bytes.push(0xF3);
                    if needs_rex_ext(&reg.name) {
                        self.bytes.push(0x41);
                    }
                    self.bytes.extend_from_slice(&[0x0F, 0x1E]);
                    self.bytes.push(0xC8 | rm);
                    Ok(())
                } else {
                    Err("rdsspd requires a 32-bit register operand".to_string())
                }
            }
            "pause" => { self.bytes.extend_from_slice(&[0xF3, 0x90]); Ok(()) }
            "mfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF0]); Ok(()) }
            "lfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xE8]); Ok(()) }
            "sfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF8]); Ok(()) }
            "clflush" => self.encode_clflush(ops),

            // Direction flag
            "cld" => { self.bytes.push(0xFC); Ok(()) }
            "std" => { self.bytes.push(0xFD); Ok(()) }

            // String ops
            "movsb" => { self.bytes.push(0xA4); Ok(()) }
            "movsd" if ops.is_empty() => { self.bytes.push(0xA5); Ok(()) }
            "movsq" => { self.bytes.extend_from_slice(&[0x48, 0xA5]); Ok(()) }
            "stosb" => { self.bytes.push(0xAA); Ok(()) }
            "stosd" => { self.bytes.push(0xAB); Ok(()) }
            "stosq" => { self.bytes.extend_from_slice(&[0x48, 0xAB]); Ok(()) }
            "lodsb" => { self.bytes.push(0xAC); Ok(()) }
            "lodsd" => { self.bytes.push(0xAD); Ok(()) }
            "lodsq" => { self.bytes.extend_from_slice(&[0x48, 0xAD]); Ok(()) }
            "scasb" => { self.bytes.push(0xAE); Ok(()) }
            "scasd" => { self.bytes.push(0xAF); Ok(()) }
            "scasq" => { self.bytes.extend_from_slice(&[0x48, 0xAF]); Ok(()) }
            "cmpsb" => { self.bytes.push(0xA6); Ok(()) }
            "cmpsd" if ops.is_empty() => { self.bytes.push(0xA7); Ok(()) }
            "cmpsq" => { self.bytes.extend_from_slice(&[0x48, 0xA7]); Ok(()) }

            // I/O string ops
            "insb" => { self.bytes.push(0x6C); Ok(()) }
            "insw" => { self.bytes.extend_from_slice(&[0x66, 0x6D]); Ok(()) }
            "insd" | "insl" => { self.bytes.push(0x6D); Ok(()) }
            "outsb" => { self.bytes.push(0x6E); Ok(()) }
            "outsw" => { self.bytes.extend_from_slice(&[0x66, 0x6F]); Ok(()) }
            "outsd" | "outsl" => { self.bytes.push(0x6F); Ok(()) }

            // Port I/O instructions
            "outb" | "outw" | "outl" => self.encode_out(ops, mnemonic),
            "inb" | "inw" | "inl" => self.encode_in(ops, mnemonic),

            // Atomic exchange
            "xchgb" | "xchgw" | "xchgl" | "xchgq" => self.encode_xchg(ops, mnemonic),

            // Lock-prefixed atomics (already have the lock prefix from the prefix handling)
            "cmpxchgb" | "cmpxchgw" | "cmpxchgl" | "cmpxchgq" => self.encode_cmpxchg(ops, mnemonic),
            "xaddb" | "xaddw" | "xaddl" | "xaddq" => self.encode_xadd(ops, mnemonic),

            // SSE/SSE2 floating-point
            "movss" => self.encode_sse_rr_rm(ops, &[0xF3, 0x0F, 0x10], &[0xF3, 0x0F, 0x11]),
            "movsd" => self.encode_sse_rr_rm(ops, &[0xF2, 0x0F, 0x10], &[0xF2, 0x0F, 0x11]),
            "movd" => self.encode_movd(ops),
            "movdqu" => self.encode_sse_rr_rm(ops, &[0xF3, 0x0F, 0x6F], &[0xF3, 0x0F, 0x7F]),
            "movupd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x10], &[0x66, 0x0F, 0x11]),

            "addsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x58]),
            "subsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5C]),
            "mulsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x59]),
            "divsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5E]),
            "addss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x58]),
            "subss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5C]),
            "mulss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x59]),
            "divss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5E]),
            "sqrtsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x51]),
            "sqrtss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x51]),
            "sqrtps" => self.encode_sse_op(ops, &[0x0F, 0x51]),
            "sqrtpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x51]),
            "rsqrtss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x52]),
            "rsqrtps" => self.encode_sse_op(ops, &[0x0F, 0x52]),
            "rcpss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x53]),
            "rcpps" => self.encode_sse_op(ops, &[0x0F, 0x53]),
            "ucomisd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x2E]),
            "ucomiss" => self.encode_sse_op(ops, &[0x0F, 0x2E]),
            "xorpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x57]),
            "xorps" => self.encode_sse_op(ops, &[0x0F, 0x57]),
            "andpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x54]),
            "andps" => self.encode_sse_op(ops, &[0x0F, 0x54]),

            // SSE conversions
            "cvtsd2ss" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5A]),
            "cvtss2sd" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5A]),
            "cvtsi2sdq" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF2, 0x0F, 0x2A], 8),
            "cvtsi2ssq" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF3, 0x0F, 0x2A], 8),
            "cvttsd2siq" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2C], 8),
            "cvttss2siq" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2C], 8),
            "cvtsd2siq" | "cvtsd2si" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2D], 8),
            "cvtss2siq" | "cvtss2si" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2D], 8),
            "cvtsd2sil" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2D], 4),
            "cvtss2sil" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2D], 4),
            "cvttsd2sil" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2C], 4),
            "cvttss2sil" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2C], 4),
            "cvtsi2sdl" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF2, 0x0F, 0x2A], 4),
            "cvtsi2ssl" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF3, 0x0F, 0x2A], 4),

            // SSE2 integer SIMD
            "paddw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFD]),
            "psubw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF9]),
            "paddd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFE]),
            "psubd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFA]),
            "pmulhw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE5]),
            "pmaddwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF5]),
            "pcmpgtw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x65]),
            "pcmpgtb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x64]),
            "packssdw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6B]),
            "packuswb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x67]),
            "punpcklbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x60]),
            "punpckhbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x68]),
            "punpcklwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x61]),
            "punpckhwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x69]),
            "pmovmskb" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x66, 0x0F, 0xD7], 4),

            // Additional SSE2 packed integer operations
            "pcmpeqb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x74]),
            "pcmpeqd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x76]),
            "pcmpeqw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x75]),
            "pand" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDB]),
            "pandn" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDF]),
            "por" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEB]),
            "pxor" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEF]),
            "psubusb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD8]),
            "psubusw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD9]),
            "psubsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE8]),
            "psubsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE9]),
            "paddusb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDC]),
            "paddusw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDD]),
            "paddsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEC]),
            "paddsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xED]),
            "pmuludq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF4]),
            "pmullw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD5]),
            "pmulld" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x40]),
            "pminub" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDA]),
            "pmaxub" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDE]),
            "pminsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x39]),
            "pmaxsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3D]),
            "pavgb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE0]),
            "pavgw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE3]),
            "psadbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF6]),
            "paddq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD4]),
            "psubq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFB]),
            "punpcklqdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6C]),
            "punpckhqdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6D]),
            "punpckldq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x62]),
            "punpckhdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6A]),

            // SSE packed float operations
            "addpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x58]),
            "subpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5C]),
            "mulpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x59]),
            "divpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5E]),
            "addps" => self.encode_sse_op(ops, &[0x0F, 0x58]),
            "subps" => self.encode_sse_op(ops, &[0x0F, 0x5C]),
            "mulps" => self.encode_sse_op(ops, &[0x0F, 0x59]),
            "divps" => self.encode_sse_op(ops, &[0x0F, 0x5E]),
            "orpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x56]),
            "orps" => self.encode_sse_op(ops, &[0x0F, 0x56]),
            "andnpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x55]),
            "andnps" => self.encode_sse_op(ops, &[0x0F, 0x55]),
            "haddpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x7C]),
            "hsubpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x7D]),
            "haddps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x7C]),
            "hsubps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x7D]),
            "addsubpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD0]),
            "addsubps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0xD0]),
            "minsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5D]),
            "maxsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5F]),
            "minss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5D]),
            "maxss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5F]),

            // Additional SSE data movement
            "movaps" => self.encode_sse_rr_rm(ops, &[0x0F, 0x28], &[0x0F, 0x29]),
            "movdqa" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x6F], &[0x66, 0x0F, 0x7F]),
            "movlpd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x12], &[0x66, 0x0F, 0x13]),
            "movhpd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x16], &[0x66, 0x0F, 0x17]),
            "movlps" => self.encode_sse_rr_rm(ops, &[0x0F, 0x12], &[0x0F, 0x13]),
            "movhps" => self.encode_sse_rr_rm(ops, &[0x0F, 0x16], &[0x0F, 0x17]),

            // SSE shifts with immediate
            "psllw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF1], 6, &[0x66, 0x0F, 0x71]),
            "psrlw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD1], 2, &[0x66, 0x0F, 0x71]),
            "psraw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xE1], 4, &[0x66, 0x0F, 0x71]),
            "pslld" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF2], 6, &[0x66, 0x0F, 0x72]),
            "psrld" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD2], 2, &[0x66, 0x0F, 0x72]),
            "psrad" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xE2], 4, &[0x66, 0x0F, 0x72]),
            "psllq" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF3], 6, &[0x66, 0x0F, 0x73]),
            "psrlq" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD3], 2, &[0x66, 0x0F, 0x73]),
            "pslldq" => self.encode_sse_shift_imm_only(ops, 7, &[0x66, 0x0F, 0x73]),
            "psrldq" => self.encode_sse_shift_imm_only(ops, 3, &[0x66, 0x0F, 0x73]),

            // SSE shuffles
            "pshufd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x70]),
            "pshuflw" => self.encode_sse_op_imm8(ops, &[0xF2, 0x0F, 0x70]),
            "pshufhw" => self.encode_sse_op_imm8(ops, &[0xF3, 0x0F, 0x70]),

            // SSE insert/extract
            "pinsrw" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0xC4], false),
            // TODO: pextrw memory dest needs SSE4.1 opcode 66 0F 3A 15, not legacy 66 0F C5
            "pextrw" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0xC5], false),
            "pinsrd" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x22], false),
            "pextrd" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x16], false),
            "pinsrb" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x20], false),
            "pextrb" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x14], false),
            "pinsrq" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x22], true),
            "pextrq" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x16], true),

            // AES-NI
            "aesenc" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDC]),
            "aesenclast" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDD]),
            "aesdec" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDE]),
            "aesdeclast" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDF]),
            "aesimc" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDB]),
            "aeskeygenassist" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0xDF]),

            // PCLMULQDQ
            "pclmulqdq" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x44]),

            // Non-temporal stores
            "movnti" | "movntil" => self.encode_movnti(ops),
            "movntiq" => self.encode_movnti_q(ops),
            "movntdq" => self.encode_sse_store(ops, &[0x66, 0x0F, 0xE7]),
            "movntpd" => self.encode_sse_store(ops, &[0x66, 0x0F, 0x2B]),

            // CRC32
            "crc32b" => self.encode_crc32(ops, 1),
            "crc32w" => self.encode_crc32(ops, 2),
            "crc32l" => self.encode_crc32(ops, 4),
            "crc32q" => self.encode_crc32(ops, 8),

            // x87 FPU
            "fldt" => self.encode_x87_mem(ops, &[0xDB], 5),
            "fstpt" => self.encode_x87_mem(ops, &[0xDB], 7),
            "fldl" => self.encode_x87_mem(ops, &[0xDD], 0),
            "flds" => self.encode_x87_mem(ops, &[0xD9], 0),
            "fstpl" => self.encode_x87_mem(ops, &[0xDD], 3),
            "fstps" => self.encode_x87_mem(ops, &[0xD9], 3),
            "fildq" | "fildll" => self.encode_x87_mem(ops, &[0xDF], 5),
            "fisttpq" | "fisttpll" => self.encode_x87_mem(ops, &[0xDD], 1),
            "fistpq" | "fistpll" => self.encode_x87_mem(ops, &[0xDF], 7),
            "faddp" => { self.bytes.extend_from_slice(&[0xDE, 0xC1]); Ok(()) }
            // Note: AT&T syntax swaps the meaning of fsub/fsubr and fdiv/fdivr
            // relative to Intel mnemonics for the *p (pop) forms.
            // GAS: fsubp = DE E1, fsubrp = DE E9, fdivp = DE F1, fdivrp = DE F9
            "fsubp" => { self.bytes.extend_from_slice(&[0xDE, 0xE1]); Ok(()) }
            "fsubrp" => { self.bytes.extend_from_slice(&[0xDE, 0xE9]); Ok(()) }
            "fmulp" => { self.bytes.extend_from_slice(&[0xDE, 0xC9]); Ok(()) }
            "fdivp" => { self.bytes.extend_from_slice(&[0xDE, 0xF1]); Ok(()) }
            "fdivrp" => { self.bytes.extend_from_slice(&[0xDE, 0xF9]); Ok(()) }
            "fchs" => { self.bytes.extend_from_slice(&[0xD9, 0xE0]); Ok(()) }
            "fcomip" => self.encode_fcomip(ops),
            "fucomip" => self.encode_fucomip(ops),
            "fld" => self.encode_fld_st(ops),
            "fstp" => self.encode_fstp_st(ops),

            // ---- Flag manipulation ----
            "clc" => { self.bytes.push(0xF8); Ok(()) }
            "stc" => { self.bytes.push(0xF9); Ok(()) }
            "cli" => { self.bytes.push(0xFA); Ok(()) }
            "sti" => { self.bytes.push(0xFB); Ok(()) }
            "cmc" => { self.bytes.push(0xF5); Ok(()) }
            "sahf" => { self.bytes.push(0x9E); Ok(()) }
            "lahf" => { self.bytes.push(0x9F); Ok(()) }
            "pushf" | "pushfq" => { self.bytes.push(0x9C); Ok(()) }
            "popf" | "popfq" => { self.bytes.push(0x9D); Ok(()) }

            // ---- System instructions ----
            "int3" => { self.bytes.push(0xCC); Ok(()) }
            "int" => {
                if let Some(Operand::Immediate(ImmediateValue::Integer(n))) = ops.first() {
                    if *n == 3 {
                        self.bytes.push(0xCC);
                    } else {
                        self.bytes.push(0xCD);
                        self.bytes.push(*n as u8);
                    }
                    Ok(())
                } else {
                    Err("int requires immediate operand".to_string())
                }
            }
            "sldt" => self.encode_sldt(ops),
            "str" if ops.len() == 1 => self.encode_str_insn(ops),
            "fninit" => { self.bytes.extend_from_slice(&[0xDB, 0xE3]); Ok(()) }
            "fwait" | "wait" => { self.bytes.push(0x9B); Ok(()) }
            "fnstcw" | "fstcw" => {
                // fstcw = fwait + fnstcw (wait prefix before the instruction)
                if mnemonic == "fstcw" {
                    self.bytes.push(0x9B); // FWAIT
                }
                self.encode_x87_mem(ops, &[0xD9], 7)
            }
            "fldcw" => self.encode_x87_mem(ops, &[0xD9], 5),

            // ---- String operations (additional sizes) ----
            "movsw" => { self.bytes.extend_from_slice(&[0x66, 0xA5]); Ok(()) }
            "stosw" => { self.bytes.extend_from_slice(&[0x66, 0xAB]); Ok(()) }
            "stosl" => { self.bytes.push(0xAB); Ok(()) }
            "movsl" => { self.bytes.push(0xA5); Ok(()) }
            "lodsw" => { self.bytes.extend_from_slice(&[0x66, 0xAD]); Ok(()) }
            "scasw" => { self.bytes.extend_from_slice(&[0x66, 0xAF]); Ok(()) }
            "cmpsw" => { self.bytes.extend_from_slice(&[0x66, 0xA7]); Ok(()) }

            // ---- Bit scan (16-bit) ----
            "bsfw" => self.encode_bit_scan(ops, mnemonic, 0xBC),
            "bsrw" => self.encode_bit_scan(ops, mnemonic, 0xBD),

            // ---- Unsigned multiply (single-operand) ----
            "mull" => self.encode_unary_rm(ops, 4, 4),
            "mulw" => {
                self.bytes.push(0x66);
                self.encode_unary_rm(ops, 4, 2)
            }

            // ---- Rotate through carry ----
            "rclq" | "rcll" | "rclw" | "rclb" => self.encode_shift(ops, mnemonic, 2),
            "rcrq" | "rcrl" | "rcrw" | "rcrb" => self.encode_shift(ops, mnemonic, 3),

            // ---- Bit test operations (16-bit) ----
            "btw" => self.encode_bt(ops, mnemonic),
            "btsw" => self.encode_bt(ops, mnemonic),
            "btrw" => self.encode_bt(ops, mnemonic),
            "btcw" => self.encode_bt(ops, mnemonic),

            // ---- 32-bit double shifts ----
            "shldl" => self.encode_double_shift(ops, 0xA4, 4),
            "shrdl" => self.encode_double_shift(ops, 0xAC, 4),

            // ---- Additional unary sizes ----
            "notb" => self.encode_unary_rm(ops, 2, 1),
            "notw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 2, 2) }
            "negb" => self.encode_unary_rm(ops, 3, 1),
            "negw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 3, 2) }

            // ---- Additional conditional branches ----
            "jrcxz" => {
                if ops.len() != 1 { return Err("jrcxz requires 1 operand".to_string()); }
                match &ops[0] {
                    Operand::Label(label) => {
                        // jrcxz uses short jump only (E3 rel8)
                        self.bytes.push(0xE3);
                        self.add_relocation(label, R_X86_64_PC8_INTERNAL, -1);
                        self.bytes.push(0); // placeholder for rel8
                        Ok(())
                    }
                    _ => Err("jrcxz requires label".to_string()),
                }
            }
            "loop" => {
                if ops.len() != 1 { return Err("loop requires 1 operand".to_string()); }
                match &ops[0] {
                    Operand::Label(label) => {
                        self.bytes.push(0xE2);
                        self.add_relocation(label, R_X86_64_PC8_INTERNAL, -1);
                        self.bytes.push(0); // placeholder for rel8
                        Ok(())
                    }
                    _ => Err("loop requires label".to_string()),
                }
            }

            // ---- cmpxchg16b ----
            "cmpxchg16b" => {
                if ops.len() != 1 { return Err("cmpxchg16b requires 1 operand".to_string()); }
                match &ops[0] {
                    Operand::Memory(mem) => {
                        self.emit_rex_rm(8, "", mem); // REX.W
                        self.bytes.extend_from_slice(&[0x0F, 0xC7]);
                        self.encode_modrm_mem(1, mem)
                    }
                    _ => Err("cmpxchg16b requires memory operand".to_string()),
                }
            }

            // ---- MMX instructions ----
            "emms" => { self.bytes.extend_from_slice(&[0x0F, 0x77]); Ok(()) }

            // ---- SSE: palignr, pshufb ----
            "palignr" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0F]),
            "pshufb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x00]),

            // ---- SSE: shufps, shufpd, movapd, unpcklpd, unpckhpd, movups ----
            "shufps" => self.encode_sse_op_imm8(ops, &[0x0F, 0xC6]),
            "shufpd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0xC6]),
            "movapd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x28], &[0x66, 0x0F, 0x29]),
            "movups" => self.encode_sse_rr_rm(ops, &[0x0F, 0x10], &[0x0F, 0x11]),
            "unpcklpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x14]),
            "unpckhpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x15]),
            "unpcklps" => self.encode_sse_op(ops, &[0x0F, 0x14]),
            "unpckhps" => self.encode_sse_op(ops, &[0x0F, 0x15]),
            "movmskpd" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x66, 0x0F, 0x50], 4),
            "movmskps" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x0F, 0x50], 4),
            "movntps" => self.encode_sse_store(ops, &[0x0F, 0x2B]),

            // ---- SSE4.1 ----
            // blendv instructions use xmm0 as implicit mask; 3-op form names it explicitly
            "blendvpd" => { let ops2 = if ops.len() == 3 { &ops[1..] } else { ops }; self.encode_sse_op(ops2, &[0x66, 0x0F, 0x38, 0x15]) }
            "blendvps" => { let ops2 = if ops.len() == 3 { &ops[1..] } else { ops }; self.encode_sse_op(ops2, &[0x66, 0x0F, 0x38, 0x14]) }
            "pblendvb" => { let ops2 = if ops.len() == 3 { &ops[1..] } else { ops }; self.encode_sse_op(ops2, &[0x66, 0x0F, 0x38, 0x10]) }
            "roundsd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0B]),
            "roundss" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0A]),
            "roundpd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x09]),
            "roundps" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x08]),
            "pblendw" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0E]),
            "blendpd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0D]),
            "blendps" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0C]),
            "dpps" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x40]),
            "dppd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x41]),
            "ptest" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x17]),
            "pminsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x38]),
            "pminuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3A]),
            "pmaxsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3C]),
            "pmaxuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3E]),
            "pminud" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3B]),
            "pmaxud" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3F]),
            "pminsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEA]),
            "pmaxsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEE]),
            "phminposuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x41]),
            "packusdw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x2B]),
            "packsswb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x63]),
            "movhlps" => self.encode_sse_op(ops, &[0x0F, 0x12]),
            "movlhps" => self.encode_sse_op(ops, &[0x0F, 0x16]),
            "movddup" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x12]),
            "movshdup" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x16]),
            "movsldup" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x12]),
            "cvtps2pd" => self.encode_sse_op(ops, &[0x0F, 0x5A]),
            "cvtpd2ps" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5A]),
            "cvtdq2ps" => self.encode_sse_op(ops, &[0x0F, 0x5B]),
            "cvtps2dq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5B]),
            "cvttps2dq" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5B]),
            "cvtdq2pd" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0xE6]),
            "cvtpd2dq" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0xE6]),

            // SSE4.1 zero/sign extension
            "pmovzxbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x30]),
            "pmovzxbd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x31]),
            "pmovzxbq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x32]),
            "pmovzxwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x33]),
            "pmovzxwq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x34]),
            "pmovzxdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x35]),
            "pmovsxbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x20]),
            "pmovsxbd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x21]),
            "pmovsxbq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x22]),
            "pmovsxwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x23]),
            "pmovsxwq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x24]),
            "pmovsxdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x25]),
            "pcmpgtq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x37]),

            // SSSE3 instructions
            "pabsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1C]),
            "pabsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1D]),
            "pabsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1E]),
            "phaddw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x01]),
            "phaddd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x02]),
            "phsubw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x05]),
            "phsubd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x06]),
            "pmulhrsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x0B]),

            // ---- AVX instructions (VEX-encoded) ----
            "vmovdqa" => self.encode_avx_mov(ops, 0x6F, 0x7F, true),
            "vmovdqu" => self.encode_avx_mov(ops, 0x6F, 0x7F, false),
            "vmovaps" => self.encode_avx_mov_np(ops, 0x28, 0x29, false),
            "vmovapd" => self.encode_avx_mov_np(ops, 0x28, 0x29, true),
            "vmovups" => self.encode_avx_mov_np(ops, 0x10, 0x11, false),
            "vmovupd" => self.encode_avx_mov_np(ops, 0x10, 0x11, true),
            "vbroadcastss" => self.encode_avx_broadcast(ops, &[0x18]),
            "vbroadcastsd" => self.encode_avx_broadcast(ops, &[0x19]),
            "vpand" => self.encode_avx_3op(ops, 0xDB, true),
            "vpandn" => self.encode_avx_3op(ops, 0xDF, true),
            "vpor" => self.encode_avx_3op(ops, 0xEB, true),
            "vpxor" => self.encode_avx_3op(ops, 0xEF, true),
            "vpaddd" => self.encode_avx_3op(ops, 0xFE, true),
            "vpsubd" => self.encode_avx_3op(ops, 0xFA, true),
            "vpcmpeqd" => self.encode_avx_3op(ops, 0x76, true),
            "vpcmpeqb" => self.encode_avx_3op(ops, 0x74, true),
            "vpcmpeqw" => self.encode_avx_3op(ops, 0x75, true),
            "vpcmpgtb" => self.encode_avx_3op(ops, 0x64, true),
            "vpcmpgtw" => self.encode_avx_3op(ops, 0x65, true),
            "vpcmpgtd" => self.encode_avx_3op(ops, 0x66, true),
            "vpaddb" => self.encode_avx_3op(ops, 0xFC, true),
            "vpaddw" => self.encode_avx_3op(ops, 0xFD, true),
            "vpsubb" => self.encode_avx_3op(ops, 0xF8, true),
            "vpsubw" => self.encode_avx_3op(ops, 0xF9, true),
            "vaddpd" => self.encode_avx_3op(ops, 0x58, true),
            "vsubpd" => self.encode_avx_3op(ops, 0x5C, true),
            "vmulpd" => self.encode_avx_3op(ops, 0x59, true),
            "vdivpd" => self.encode_avx_3op(ops, 0x5E, true),
            "vaddps" => self.encode_avx_3op_np(ops, 0x58),
            "vsubps" => self.encode_avx_3op_np(ops, 0x5C),
            "vmulps" => self.encode_avx_3op_np(ops, 0x59),
            "vdivps" => self.encode_avx_3op_np(ops, 0x5E),
            "vxorpd" => self.encode_avx_3op(ops, 0x57, true),
            "vxorps" => self.encode_avx_3op_np(ops, 0x57),
            "vandpd" => self.encode_avx_3op(ops, 0x54, true),
            "vandps" => self.encode_avx_3op_np(ops, 0x54),
            "vandnpd" => self.encode_avx_3op(ops, 0x55, true),
            "vandnps" => self.encode_avx_3op_np(ops, 0x55),
            "vorpd" => self.encode_avx_3op(ops, 0x56, true),
            "vorps" => self.encode_avx_3op_np(ops, 0x56),
            "vpshufd" => self.encode_avx_shuffle(ops, 0x70, true),
            "vpshufb" => self.encode_avx_3op_38(ops, 0x00, true),
            "vpalignr" => self.encode_avx_3op_3a_imm8(ops, 0x0F, true),
            "vblendps" => self.encode_avx_3op_3a_imm8(ops, 0x0C, true),
            "vblendpd" => self.encode_avx_3op_3a_imm8(ops, 0x0D, true),
            "vinserti128" => self.encode_avx_3op_3a_imm8(ops, 0x38, true),
            "vextracti128" => self.encode_avx_extract_imm8(ops, 0x39, true),
            "vpabsb" => self.encode_avx_2op_38(ops, 0x1C, true),
            "vpabsw" => self.encode_avx_2op_38(ops, 0x1D, true),
            "vpabsd" => self.encode_avx_2op_38(ops, 0x1E, true),
            "vpmovmskb" => self.encode_avx_extract_gp(ops, 0xD7, true),
            "vmovd" => self.encode_avx_movd(ops),
            "vmovq" => self.encode_avx_movq(ops),
            "vpunpcklbw" => self.encode_avx_3op(ops, 0x60, true),
            "vpunpckhbw" => self.encode_avx_3op(ops, 0x68, true),
            "vpunpcklwd" => self.encode_avx_3op(ops, 0x61, true),
            "vpunpckhwd" => self.encode_avx_3op(ops, 0x69, true),
            "vpunpckldq" => self.encode_avx_3op(ops, 0x62, true),
            "vpunpckhdq" => self.encode_avx_3op(ops, 0x6A, true),
            "vpunpcklqdq" => self.encode_avx_3op(ops, 0x6C, true),
            "vpunpckhqdq" => self.encode_avx_3op(ops, 0x6D, true),
            "vpmullw" => self.encode_avx_3op(ops, 0xD5, true),
            "vpmulld" => self.encode_avx_3op_38(ops, 0x40, true),
            "vpmuludq" => self.encode_avx_3op(ops, 0xF4, true),
            "vpsllw" => self.encode_avx_shift(ops, 0xF1, 6, 0x71, true),
            "vpslld" => self.encode_avx_shift(ops, 0xF2, 6, 0x72, true),
            "vpsllq" => self.encode_avx_shift(ops, 0xF3, 6, 0x73, true),
            "vpsrlw" => self.encode_avx_shift(ops, 0xD1, 2, 0x71, true),
            "vpsrld" => self.encode_avx_shift(ops, 0xD2, 2, 0x72, true),
            "vpsrlq" => self.encode_avx_shift(ops, 0xD3, 2, 0x73, true),
            "vpsraw" => self.encode_avx_shift(ops, 0xE1, 4, 0x71, true),
            "vpsrad" => self.encode_avx_shift(ops, 0xE2, 4, 0x72, true),

            // AVX2 integer comparisons (additional)
            "vpcmpeqq" => self.encode_avx_3op_38(ops, 0x29, true),
            "vpcmpgtq" => self.encode_avx_3op_38(ops, 0x37, true),

            // AVX2 broadcast
            "vpbroadcastb" => self.encode_avx_broadcast(ops, &[0x78]),
            "vpbroadcastw" => self.encode_avx_broadcast(ops, &[0x79]),
            "vpbroadcastd" => self.encode_avx_broadcast(ops, &[0x58]),
            "vpbroadcastq" => self.encode_avx_broadcast(ops, &[0x59]),

            // AVX2 permute
            "vperm2i128" => self.encode_avx_3op_3a_imm8(ops, 0x46, true),
            "vperm2f128" => self.encode_avx_3op_3a_imm8(ops, 0x06, true),
            "vpermd" => self.encode_avx_3op_38(ops, 0x36, true),
            "vpermq" => self.encode_avx_shuffle_3a_w1(ops, 0x00, true),

            // AVX blend (additional)
            "vpblendd" => self.encode_avx_3op_3a_imm8(ops, 0x02, true),
            "vblendvps" => self.encode_avx_4op_3a(ops, 0x4A, true),
            "vblendvpd" => self.encode_avx_4op_3a(ops, 0x4B, true),
            "vpblendvb" => self.encode_avx_4op_3a(ops, 0x4C, true),

            // AVX2 broadcast from 128-bit memory
            "vbroadcasti128" => self.encode_avx_broadcast(ops, &[0x5A]),

            // AVX AES-NI
            "vaesenc" => self.encode_avx_3op_38(ops, 0xDC, true),
            "vaesenclast" => self.encode_avx_3op_38(ops, 0xDD, true),
            "vaesdec" => self.encode_avx_3op_38(ops, 0xDE, true),
            "vaesdeclast" => self.encode_avx_3op_38(ops, 0xDF, true),

            // AVX PCLMULQDQ
            "vpclmulqdq" => self.encode_avx_3op_3a_imm8(ops, 0x44, true),

            // Additional AVX packed integer
            "vpminub" => self.encode_avx_3op(ops, 0xDA, true),
            "vpmaxub" => self.encode_avx_3op(ops, 0xDE, true),
            "vpminsd" => self.encode_avx_3op_38(ops, 0x39, true),
            "vpmaxsd" => self.encode_avx_3op_38(ops, 0x3D, true),
            "vpminsb" => self.encode_avx_3op_38(ops, 0x38, true),
            "vpmaxsb" => self.encode_avx_3op_38(ops, 0x3C, true),
            "vpminuw" => self.encode_avx_3op_38(ops, 0x3A, true),
            "vpmaxuw" => self.encode_avx_3op_38(ops, 0x3E, true),
            "vpminsw" => self.encode_avx_3op(ops, 0xEA, true),
            "vpmaxsw" => self.encode_avx_3op(ops, 0xEE, true),
            "vpminud" => self.encode_avx_3op_38(ops, 0x3B, true),
            "vpmaxud" => self.encode_avx_3op_38(ops, 0x3F, true),
            "vpavgb" => self.encode_avx_3op(ops, 0xE0, true),
            "vpavgw" => self.encode_avx_3op(ops, 0xE3, true),
            "vpsadbw" => self.encode_avx_3op(ops, 0xF6, true),
            "vpmaddwd" => self.encode_avx_3op(ops, 0xF5, true),
            "vpmulhw" => self.encode_avx_3op(ops, 0xE5, true),
            "vpsubusb" => self.encode_avx_3op(ops, 0xD8, true),
            "vpsubusw" => self.encode_avx_3op(ops, 0xD9, true),
            "vpackuswb" => self.encode_avx_3op(ops, 0x67, true),
            "vpackssdw" => self.encode_avx_3op(ops, 0x6B, true),
            "vpackusdw" => self.encode_avx_3op_38(ops, 0x2B, true),
            "vpacksswb" => self.encode_avx_3op(ops, 0x63, true),
            "vpaddusb" => self.encode_avx_3op(ops, 0xDC, true),
            "vpaddusw" => self.encode_avx_3op(ops, 0xDD, true),
            "vpaddsb" => self.encode_avx_3op(ops, 0xEC, true),
            "vpaddsw" => self.encode_avx_3op(ops, 0xED, true),
            "vpsubsb" => self.encode_avx_3op(ops, 0xE8, true),
            "vpsubsw" => self.encode_avx_3op(ops, 0xE9, true),

            // AVX float misc (vshufps/vshufpd are in 0F map, not 0F3A)
            "vshufps" => self.encode_avx_3op_0f_imm8(ops, 0xC6, false),
            "vshufpd" => self.encode_avx_3op_0f_imm8(ops, 0xC6, true),
            "vunpcklpd" => self.encode_avx_3op(ops, 0x14, true),
            "vunpckhpd" => self.encode_avx_3op(ops, 0x15, true),
            "vunpcklps" => self.encode_avx_3op_np(ops, 0x14),
            "vunpckhps" => self.encode_avx_3op_np(ops, 0x15),
            "vmovlhps" => self.encode_avx_3op_np(ops, 0x16),
            "vmovhlps" => self.encode_avx_3op_np(ops, 0x12),
            "vmovddup" => self.encode_avx_2op_0f(ops, 0x12, 3),  // VEX.F2.0F 12 /r
            "vmovshdup" => self.encode_avx_2op_0f(ops, 0x16, 2), // VEX.F3.0F 16 /r
            "vmovsldup" => self.encode_avx_2op_0f(ops, 0x12, 2), // VEX.F3.0F 12 /r

            // AVX comparison with immediate (vcmpps/vcmppd/vcmpss/vcmpsd)
            "vcmpps" => self.encode_avx_3op_0f_imm8(ops, 0xC2, false),
            "vcmppd" => self.encode_avx_3op_0f_imm8(ops, 0xC2, true),
            "vcmpss" => self.encode_avx_cmp_scalar(ops, 0xC2, 2), // VEX.NDS.LIG.F3.0F C2 /r ib
            "vcmpsd" => self.encode_avx_cmp_scalar(ops, 0xC2, 3), // VEX.NDS.LIG.F2.0F C2 /r ib

            // AVX scalar float operations (VEX.NDS.LIG.F3/F2.0F)
            "vmovss" => self.encode_avx_scalar_mov(ops, 0x10, 0x11, 2), // F3 prefix
            "vmovsd" if !ops.is_empty() => self.encode_avx_scalar_mov(ops, 0x10, 0x11, 3), // F2 prefix
            "vaddss" => self.encode_avx_scalar_3op(ops, 0x58, 2),   // VEX.NDS.LIG.F3.0F 58
            "vsubss" => self.encode_avx_scalar_3op(ops, 0x5C, 2),   // VEX.NDS.LIG.F3.0F 5C
            "vmulss" => self.encode_avx_scalar_3op(ops, 0x59, 2),   // VEX.NDS.LIG.F3.0F 59
            "vdivss" => self.encode_avx_scalar_3op(ops, 0x5E, 2),   // VEX.NDS.LIG.F3.0F 5E
            "vaddsd" => self.encode_avx_scalar_3op(ops, 0x58, 3),   // VEX.NDS.LIG.F2.0F 58
            "vsubsd" => self.encode_avx_scalar_3op(ops, 0x5C, 3),   // VEX.NDS.LIG.F2.0F 5C
            "vmulsd" => self.encode_avx_scalar_3op(ops, 0x59, 3),   // VEX.NDS.LIG.F2.0F 59
            "vdivsd" => self.encode_avx_scalar_3op(ops, 0x5E, 3),   // VEX.NDS.LIG.F2.0F 5E
            "vmaxps" => self.encode_avx_3op_np(ops, 0x5F),
            "vminps" => self.encode_avx_3op_np(ops, 0x5D),
            "vmaxpd" => self.encode_avx_3op(ops, 0x5F, true),
            "vminpd" => self.encode_avx_3op(ops, 0x5D, true),
            "vmaxss" => self.encode_avx_scalar_3op(ops, 0x5F, 2),
            "vminss" => self.encode_avx_scalar_3op(ops, 0x5D, 2),
            "vmaxsd" => self.encode_avx_scalar_3op(ops, 0x5F, 3),
            "vminsd" => self.encode_avx_scalar_3op(ops, 0x5D, 3),

            // AVX extract/permute
            "vextractf128" => self.encode_avx_extract_imm8(ops, 0x19, true), // VEX.256.66.0F3A 19 /r ib
            "vpermilps" => self.encode_avx_shuffle_3a(ops, 0x04, true),      // VEX.256.66.0F3A 04 /r ib (imm form)
            "vpermilpd" => self.encode_avx_shuffle_3a(ops, 0x05, true),      // VEX.256.66.0F3A 05 /r ib (imm form)

            // AVX conversions
            "vcvtps2dq" => self.encode_avx_2op_0f(ops, 0x5B, 1),        // VEX.128/256.66.0F 5B /r
            "vcvtdq2ps" => self.encode_avx_2op_0f(ops, 0x5B, 0),        // VEX.128/256.0F 5B /r
            "vcvttps2dq" => self.encode_avx_2op_0f(ops, 0x5B, 2),       // VEX.128/256.F3.0F 5B /r

            // AVX insert/extract
            "vpinsrb" => self.encode_avx_insert_gp(ops, 0x20, true),
            "vpinsrw" => self.encode_avx_insert_gp_0f(ops, 0xC4, true),
            "vpinsrd" => self.encode_avx_insert_gp(ops, 0x22, true),
            "vpinsrq" if ops.len() == 4 => {
                // Already handled above, but catch the 4-op form
                self.encode_avx_insert_gp_w1(ops, 0x22, true)
            }
            "vpextrb" => self.encode_avx_extract_byte(ops, 0x14, true),
            "vpextrd" => self.encode_avx_extract_byte(ops, 0x16, true),
            "vpextrw" => self.encode_avx_extract_byte(ops, 0x15, true),

            // ---- AVX-512 (EVEX-encoded) ----
            "vpxord" => self.encode_evex_3op(ops, 0xEF, 1, 0),  // EVEX.NDS.{128,256,512}.66.0F.W0 EF /r
            "vpxorq" => self.encode_evex_3op(ops, 0xEF, 1, 1),  // EVEX.NDS.{128,256,512}.66.0F.W1 EF /r
            "vpandd" => self.encode_evex_3op(ops, 0xDB, 1, 0),  // EVEX.NDS.66.0F.W0 DB /r
            "vpandq" => self.encode_evex_3op(ops, 0xDB, 1, 1),  // EVEX.NDS.66.0F.W1 DB /r
            "vpord" => self.encode_evex_3op(ops, 0xEB, 1, 0),   // EVEX.NDS.66.0F.W0 EB /r
            "vporq" => self.encode_evex_3op(ops, 0xEB, 1, 1),   // EVEX.NDS.66.0F.W1 EB /r

            // ---- BMI2 instructions (VEX-encoded, GPR) ----
            "shrxq" => self.encode_bmi2_shift(ops, 0xF7, 3, 1), // F2.0F38.W1
            "shrxl" => self.encode_bmi2_shift(ops, 0xF7, 3, 0), // F2.0F38.W0
            "shlxq" => self.encode_bmi2_shift(ops, 0xF7, 1, 1), // 66.0F38.W1
            "shlxl" => self.encode_bmi2_shift(ops, 0xF7, 1, 0), // 66.0F38.W0
            "sarxq" => self.encode_bmi2_shift(ops, 0xF7, 2, 1), // F3.0F38.W1
            "sarxl" => self.encode_bmi2_shift(ops, 0xF7, 2, 0), // F3.0F38.W0
            "rorxq" => self.encode_bmi2_rorx(ops, 1),            // F2.0F3A.W1 F0 /r imm8
            "rorxl" => self.encode_bmi2_rorx(ops, 0),            // F2.0F3A.W0 F0 /r imm8
            "bzhiq" => self.encode_bmi2_shift(ops, 0xF5, 0, 1), // NP.0F38.W1
            "bzhil" => self.encode_bmi2_shift(ops, 0xF5, 0, 0), // NP.0F38.W0
            "pextq" => self.encode_bmi2_shift(ops, 0xF5, 2, 1), // F3.0F38.W1
            "pextl" => self.encode_bmi2_shift(ops, 0xF5, 2, 0), // F3.0F38.W0
            "pdepq" => self.encode_bmi2_shift(ops, 0xF5, 3, 1), // F2.0F38.W1
            "pdepl" => self.encode_bmi2_shift(ops, 0xF5, 3, 0), // F2.0F38.W0
            "mulxq" => self.encode_bmi2_shift(ops, 0xF6, 3, 1), // F2.0F38.W1
            "mulxl" => self.encode_bmi2_shift(ops, 0xF6, 3, 0), // F2.0F38.W0
            "andnq" => self.encode_bmi_andn(ops, 1), // NP.0F38.W1
            "andnl" => self.encode_bmi_andn(ops, 0), // NP.0F38.W0
            "bextrl" | "bextrq" => {
                let w = if mnemonic == "bextrq" { 1 } else { 0 };
                self.encode_bmi2_shift(ops, 0xF7, 0, w)          // NP.0F38.Wx
            }
            // BMI2 suffix-less forms: infer 32/64-bit from destination register
            "shrx" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF7, 3, w) }
            "shlx" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF7, 1, w) }
            "sarx" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF7, 2, w) }
            "rorx" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_rorx(ops, w) }
            "bzhi" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF5, 0, w) }
            "pext" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF5, 2, w) }
            "pdep" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF5, 3, w) }
            "mulx" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF6, 3, w) }
            "andn" => { let w = self.bmi2_infer_w(ops); self.encode_bmi_andn(ops, w) }
            "bextr" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF7, 0, w) }

            // ---- Suffix-less forms (infer size from operands) ----
            // These are commonly emitted by inline asm
            "push" => self.encode_push(ops),
            "pop" => self.encode_pop(ops),
            "mov" => self.encode_suffixless_mov(ops),
            "add" => self.encode_suffixless_alu(ops, 0),
            "sub" => self.encode_suffixless_alu(ops, 5),
            "xor" => self.encode_suffixless_alu(ops, 6),
            "and" => self.encode_suffixless_alu(ops, 4),
            "or" => self.encode_suffixless_alu(ops, 1),
            "cmp" => self.encode_suffixless_alu(ops, 7),
            "test" => self.encode_suffixless_test(ops),
            "shr" => self.encode_suffixless_shift(ops, 5),
            "shl" | "sal" => self.encode_suffixless_shift(ops, 4),
            "sar" => self.encode_suffixless_shift(ops, 7),
            "inc" => self.encode_suffixless_unary(ops, 0),
            "dec" => self.encode_suffixless_unary(ops, 1),
            "neg" => self.encode_suffixless_unary(ops, 3),
            "not" => self.encode_suffixless_unary(ops, 2),
            "lea" => {
                // Infer size from destination register
                if ops.len() == 2 {
                    if let Operand::Register(dst) = &ops[1] {
                        let size = infer_reg_size(&dst.name);
                        return self.encode_lea(ops, size);
                    }
                }
                Err("unsupported lea operands".to_string())
            }
            "xchg" => {
                if ops.len() == 2 {
                    let size = infer_operand_size_from_pair(&ops[0], &ops[1]);
                    let suffix_mnemonic = match size {
                        1 => "xchgb", 2 => "xchgw", 4 => "xchgl", _ => "xchgq",
                    };
                    self.encode_xchg(ops, suffix_mnemonic)
                } else {
                    Err("xchg requires 2 operands".to_string())
                }
            }
            "imul" => self.encode_imul(ops, 8), // default to 64-bit for suffix-less
            "mul" => {
                let size = if let Some(Operand::Register(r)) = ops.first() { infer_reg_size(&r.name) } else { 8 };
                if size == 2 { self.bytes.push(0x66); }
                self.encode_unary_rm(ops, 4, size)
            }
            "div" => {
                let size = if let Some(Operand::Register(r)) = ops.first() { infer_reg_size(&r.name) } else { 8 };
                if size == 2 { self.bytes.push(0x66); }
                self.encode_unary_rm(ops, 6, size)
            }
            "idiv" => {
                let size = if let Some(Operand::Register(r)) = ops.first() { infer_reg_size(&r.name) } else { 8 };
                if size == 2 { self.bytes.push(0x66); }
                self.encode_unary_rm(ops, 7, size)
            }
            "bswap" => {
                let size = if let Some(Operand::Register(r)) = ops.first() {
                    infer_reg_size(&r.name)
                } else { 8 };
                self.encode_bswap(ops, size)
            }
            "bsf" => self.encode_bit_scan(ops, "bsfq", 0xBC),
            "bsr" => self.encode_bit_scan(ops, "bsrq", 0xBD),
            "cmovzq" | "cmovnzq" | "cmovsq" | "cmovnsq" | "cmovpq" | "cmovnpq" => self.encode_cmovcc(ops, mnemonic),
            "cmovzl" | "cmovnzl" | "cmovsl" | "cmovnsl" | "cmovpl" | "cmovnpl" => self.encode_cmovcc(ops, mnemonic),
            "cmovzw" | "cmovnzw" | "cmovsw" | "cmovnsw" | "cmovpw" | "cmovnpw" => self.encode_cmovcc(ops, mnemonic),

            // Suffix-less cmov (infer from operand size)
            "cmovz" | "cmovnz" | "cmovs" | "cmovns" | "cmovp" | "cmovnp"
            | "cmove" | "cmovne" | "cmovl" | "cmovle" | "cmovg" | "cmovge"
            | "cmovb" | "cmovbe" | "cmova" | "cmovae"
            | "cmovc" | "cmovnc" | "cmovno" | "cmovo"
            | "cmovna" | "cmovnb" | "cmovnbe" | "cmovnge" | "cmovng"
            | "cmovnle" | "cmovnl" | "cmovpe" | "cmovpo" | "cmovnae" => {
                if ops.len() == 2 {
                    let size = infer_operand_size_from_pair(&ops[0], &ops[1]);
                    let suffix = match size {
                        8 => "q",
                        4 => "l",
                        2 => "w",
                        _ => "l",
                    };
                    let new_mnemonic = format!("{}{}", mnemonic, suffix);
                    self.encode_cmovcc(ops, &new_mnemonic)
                } else {
                    Err("cmov requires 2 operands".to_string())
                }
            }

            // Additional cmov variants with size suffix
            "cmovcq" | "cmovncq" | "cmovnaq" | "cmovnbeq"
            | "cmovngeq" | "cmovngq" | "cmovnleq" | "cmovnlq"
            | "cmovnoq" | "cmovnaeq" | "cmovnbq" | "cmovoq" | "cmovpeq" | "cmovpoq"
            | "cmovcl" | "cmovncl" | "cmovnal" | "cmovnbel"
            | "cmovngel" | "cmovngl" | "cmovnlel" | "cmovnll"
            | "cmovnol" | "cmovnael" | "cmovnbl" | "cmovol" | "cmovpel" | "cmovpol"
            | "cmovcw" | "cmovncw" | "cmovnaw" | "cmovnbew"
            | "cmovngew" | "cmovngw" | "cmovnlew" | "cmovnlw"
            | "cmovnow" | "cmovnaew" | "cmovnbw" | "cmovow" | "cmovpew" | "cmovpow" => self.encode_cmovcc(ops, mnemonic),

            // Additional set instructions
            "setcc" => self.encode_setcc(ops, "setc"),

            // ---- imul with suffix forms we haven't caught ----
            "imulw" => self.encode_imul(ops, 2),

            // ---- Additional ADC/SBB sizes ----
            "adcw" | "adcb" => self.encode_alu(ops, mnemonic, 2),
            "sbbw" | "sbbb" => self.encode_alu(ops, mnemonic, 3),

            // ---- divw, divb, idivw, idivb, mulb ----
            "divw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 6, 2) }
            "divb" => self.encode_unary_rm(ops, 6, 1),
            "idivw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 7, 2) }
            "idivb" => self.encode_unary_rm(ops, 7, 1),
            "mulb" => self.encode_unary_rm(ops, 4, 1),

            // ---- CBW/CWDE/CWD ----
            "cbw" => { self.bytes.extend_from_slice(&[0x66, 0x98]); Ok(()) }
            "cwd" => { self.bytes.extend_from_slice(&[0x66, 0x99]); Ok(()) }

            // ---- AVX: vzeroupper ----
            "vzeroupper" => {
                // VEX.128.0F.WIG 77
                self.emit_vex(false, false, false, 1, 0, 0, 0, 0);
                self.bytes.push(0x77);
                Ok(())
            }
            "vzeroall" => {
                // VEX.256.0F.WIG 77
                self.emit_vex(false, false, false, 1, 0, 0, 1, 0);
                self.bytes.push(0x77);
                Ok(())
            }

            // Additional AVX operations
            "vpaddq" => self.encode_avx_3op(ops, 0xD4, true),
            "vpsubq" => self.encode_avx_3op(ops, 0xFB, true),
            "vpsrldq" => {
                // VEX.NDD.128/256.66.0F 73 /3 ib
                if ops.len() != 3 { return Err("vpsrldq requires 3 operands".to_string()); }
                match (&ops[0], &ops[1], &ops[2]) {
                    (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let l = if is_ymm(&src.name) || is_ymm(&dst.name) { 1 } else { 0 };
                        let b = needs_vex_ext(&src.name);
                        let dst_num_full = reg_num(&dst.name).ok_or("bad register")? | (if needs_vex_ext(&dst.name) { 8 } else { 0 });
                        self.emit_vex(false, false, b, 1, 0, dst_num_full, l, 1);
                        self.bytes.push(0x73);
                        self.bytes.push(self.modrm(3, 3, src_num));
                        self.bytes.push(*imm as u8);
                        Ok(())
                    }
                    _ => Err("unsupported vpsrldq operands".to_string()),
                }
            }
            "vpslldq" => {
                if ops.len() != 3 { return Err("vpslldq requires 3 operands".to_string()); }
                match (&ops[0], &ops[1], &ops[2]) {
                    (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let l = if is_ymm(&src.name) || is_ymm(&dst.name) { 1 } else { 0 };
                        let b = needs_vex_ext(&src.name);
                        let dst_num_full = reg_num(&dst.name).ok_or("bad register")? | (if needs_vex_ext(&dst.name) { 8 } else { 0 });
                        self.emit_vex(false, false, b, 1, 0, dst_num_full, l, 1);
                        self.bytes.push(0x73);
                        self.bytes.push(self.modrm(3, 7, src_num));
                        self.bytes.push(*imm as u8);
                        Ok(())
                    }
                    _ => Err("unsupported vpslldq operands".to_string()),
                }
            }
            "vptest" => {
                // VEX.128.66.0F38 17 /r
                if ops.len() != 2 { return Err("vptest requires 2 operands".to_string()); }
                match (&ops[0], &ops[1]) {
                    (Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        let l = if is_ymm(&src.name) || is_ymm(&dst.name) { 1 } else { 0 };
                        let r = needs_vex_ext(&dst.name);
                        let b = needs_vex_ext(&src.name);
                        self.emit_vex(r, false, b, 2, 0, 0, l, 1);
                        self.bytes.push(0x17);
                        self.bytes.push(self.modrm(3, dst_num, src_num));
                        Ok(())
                    }
                    _ => Err("unsupported vptest operands".to_string()),
                }
            }
            "vpextrq" => {
                // VEX.128.66.0F3A.W1 16 /r ib
                if ops.len() != 3 { return Err("vpextrq requires 3 operands".to_string()); }
                match (&ops[0], &ops[1], &ops[2]) {
                    (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        let r = needs_vex_ext(&src.name);
                        let b = needs_vex_ext(&dst.name);
                        self.emit_vex(r, false, b, 3, 1, 0, 0, 1);
                        self.bytes.push(0x16);
                        self.bytes.push(self.modrm(3, src_num, dst_num));
                        self.bytes.push(*imm as u8);
                        Ok(())
                    }
                    _ => Err("unsupported vpextrq operands".to_string()),
                }
            }
            "vpinsrq" => {
                // VEX.128.66.0F3A.W1 22 /r ib
                if ops.len() != 4 { return Err("vpinsrq requires 4 operands".to_string()); }
                match (&ops[0], &ops[1], &ops[2], &ops[3]) {
                    (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        let r = needs_vex_ext(&dst.name);
                        let b = needs_vex_ext(&src.name);
                        let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                        self.emit_vex(r, false, b, 3, 1, vvvv_enc, 0, 1);
                        self.bytes.push(0x22);
                        self.bytes.push(self.modrm(3, dst_num, src_num));
                        self.bytes.push(*imm as u8);
                        Ok(())
                    }
                    _ => Err("unsupported vpinsrq operands".to_string()),
                }
            }

            // ---- Suffix-less shrd/shld ----
            "shrd" => {
                if ops.len() == 3 {
                    let size = match &ops[2] {
                        Operand::Register(r) => infer_reg_size(&r.name),
                        _ => 8,
                    };
                    let opcode = 0xACu8;
                    self.encode_double_shift(ops, opcode, size)
                } else {
                    Err("shrd requires 3 operands".to_string())
                }
            }
            "shld" => {
                if ops.len() == 3 {
                    let size = match &ops[2] {
                        Operand::Register(r) => infer_reg_size(&r.name),
                        _ => 8,
                    };
                    let opcode = 0xA4u8;
                    self.encode_double_shift(ops, opcode, size)
                } else {
                    Err("shld requires 3 operands".to_string())
                }
            }

            // ---- Suffix-less adc/sbb ----
            "adc" => self.encode_suffixless_alu(ops, 2),
            "sbb" => self.encode_suffixless_alu(ops, 3),

            // ---- Additional x87 ----
            "fistpl" => self.encode_x87_mem(ops, &[0xDB], 3),
            "fistl" => self.encode_x87_mem(ops, &[0xDB], 2),
            "fildl" => self.encode_x87_mem(ops, &[0xDB], 0),
            "filds" => self.encode_x87_mem(ops, &[0xDF], 0),
            "fistps" => self.encode_x87_mem(ops, &[0xDF], 3),
            "fisttpl" => self.encode_x87_mem(ops, &[0xDB], 1),
            "fdivl" => self.encode_x87_mem(ops, &[0xDC], 6),
            "fdivs" => self.encode_x87_mem(ops, &[0xD8], 6),
            "fmull" => self.encode_x87_mem(ops, &[0xDC], 1),
            "fmuls" => self.encode_x87_mem(ops, &[0xD8], 1),
            "fsubl" => self.encode_x87_mem(ops, &[0xDC], 4),
            "fsubs" => self.encode_x87_mem(ops, &[0xD8], 4),
            "faddl" => self.encode_x87_mem(ops, &[0xDC], 0),
            "fadds" => self.encode_x87_mem(ops, &[0xD8], 0),
            // fdivrp with 2 operands is handled by the no-operand form above

            // ---- lock cmpxchg8b ----
            "cmpxchg8b" => {
                if ops.len() != 1 { return Err("cmpxchg8b requires 1 operand".to_string()); }
                match &ops[0] {
                    Operand::Memory(mem) => {
                        self.emit_rex_rm(0, "", mem);
                        self.bytes.extend_from_slice(&[0x0F, 0xC7]);
                        self.encode_modrm_mem(1, mem)
                    }
                    _ => Err("cmpxchg8b requires memory operand".to_string()),
                }
            }

            // ---- MMX paddb ----
            "paddb" if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_mmx(&r.name))) => {
                // MMX form: 0F FC /r
                if ops.len() != 2 { return Err("paddb requires 2 operands".to_string()); }
                match (&ops[0], &ops[1]) {
                    (Operand::Register(src), Operand::Register(dst)) if is_mmx(&src.name) && is_mmx(&dst.name) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        self.bytes.extend_from_slice(&[0x0F, 0xFC]);
                        self.bytes.push(self.modrm(3, dst_num, src_num));
                        Ok(())
                    }
                    (Operand::Memory(mem), Operand::Register(dst)) if is_mmx(&dst.name) => {
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        self.bytes.extend_from_slice(&[0x0F, 0xFC]);
                        self.encode_modrm_mem(dst_num, mem)
                    }
                    _ => Err("unsupported mmx paddb operands".to_string()),
                }
            }
            "paddb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFC]),

            // ---- Additional suffixless forms ----
            "bt" => self.encode_bt(ops, "btq"),
            "bts" => self.encode_bt(ops, "btsq"),
            "btr" => self.encode_bt(ops, "btrq"),
            "btc" => self.encode_bt(ops, "btcq"),
            "rcl" => self.encode_suffixless_shift(ops, 2),
            "rcr" => self.encode_suffixless_shift(ops, 3),
            "rol" => self.encode_suffixless_shift(ops, 0),
            "ror" => self.encode_suffixless_shift(ops, 1),

            // ---- rep/lock with nothing or partial ----
            "rep" => {
                // rep; by itself just emits the prefix (for "rep; nop" = pause)
                self.bytes.push(0xF3);
                Ok(())
            }
            "lock" => {
                // lock; by itself just emits the prefix byte
                self.bytes.push(0xF0);
                Ok(())
            }

            // 1-operand shift forms are now handled in encode_shift directly

            // ---- Additional x87 forms from upstream ----
            "fsts" => self.encode_x87_mem(ops, &[0xD9], 2),

            // x87 constants
            "fld1" => { self.bytes.extend_from_slice(&[0xD9, 0xE8]); Ok(()) }
            "fldl2e" => { self.bytes.extend_from_slice(&[0xD9, 0xEA]); Ok(()) }
            "fldlg2" => { self.bytes.extend_from_slice(&[0xD9, 0xEC]); Ok(()) }
            "fldln2" => { self.bytes.extend_from_slice(&[0xD9, 0xED]); Ok(()) }
            "fldz" => { self.bytes.extend_from_slice(&[0xD9, 0xEE]); Ok(()) }
            "fldpi" => { self.bytes.extend_from_slice(&[0xD9, 0xEB]); Ok(()) }
            "fldl2t" => { self.bytes.extend_from_slice(&[0xD9, 0xE9]); Ok(()) }

            // x87 arithmetic (zero-operand forms operate on st(0),st(1))
            "fabs" => { self.bytes.extend_from_slice(&[0xD9, 0xE1]); Ok(()) }
            "fsqrt" => { self.bytes.extend_from_slice(&[0xD9, 0xFA]); Ok(()) }
            "frndint" => { self.bytes.extend_from_slice(&[0xD9, 0xFC]); Ok(()) }
            "f2xm1" => { self.bytes.extend_from_slice(&[0xD9, 0xF0]); Ok(()) }
            "fscale" => { self.bytes.extend_from_slice(&[0xD9, 0xFD]); Ok(()) }
            "fpatan" => { self.bytes.extend_from_slice(&[0xD9, 0xF3]); Ok(()) }
            "fprem" => { self.bytes.extend_from_slice(&[0xD9, 0xF8]); Ok(()) }
            "fprem1" => { self.bytes.extend_from_slice(&[0xD9, 0xF5]); Ok(()) }
            "fyl2x" => { self.bytes.extend_from_slice(&[0xD9, 0xF1]); Ok(()) }
            "fyl2xp1" => { self.bytes.extend_from_slice(&[0xD9, 0xF9]); Ok(()) }
            "fptan" => { self.bytes.extend_from_slice(&[0xD9, 0xF2]); Ok(()) }
            "fsin" => { self.bytes.extend_from_slice(&[0xD9, 0xFE]); Ok(()) }
            "fcos" => { self.bytes.extend_from_slice(&[0xD9, 0xFF]); Ok(()) }
            "fxtract" => { self.bytes.extend_from_slice(&[0xD9, 0xF4]); Ok(()) }

            // x87 register-register operations
            "fadd" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xC0),
            "fmul" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xC8),
            "fsub" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xE0),
            "fdiv" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xF0),

            // x87 exchange
            "fxch" => self.encode_fxch(ops),

            // x87 control word / environment
            "fnclex" => { self.bytes.extend_from_slice(&[0xDB, 0xE2]); Ok(()) }
            "fnstenv" => self.encode_x87_mem(ops, &[0xD9], 6),
            "fldenv" => self.encode_x87_mem(ops, &[0xD9], 4),
            "fnstsw" => {
                // fnstsw %ax -> DF E0
                // fnstsw mem -> DD /7
                if let Some(Operand::Register(r)) = ops.first() {
                    if r.name == "ax" {
                        self.bytes.extend_from_slice(&[0xDF, 0xE0]);
                        return Ok(());
                    }
                }
                self.encode_x87_mem(ops, &[0xDD], 7)
            }

            // SSE MXCSR control register
            "ldmxcsr" => self.encode_sse_mem_only(ops, &[0x0F, 0xAE], 2),
            "stmxcsr" => self.encode_sse_mem_only(ops, &[0x0F, 0xAE], 3),

            // Prefetch instructions (0F 18 /0-3)
            "prefetcht0" => self.encode_sse_mem_only(ops, &[0x0F, 0x18], 1),
            "prefetcht1" => self.encode_sse_mem_only(ops, &[0x0F, 0x18], 2),
            "prefetcht2" => self.encode_sse_mem_only(ops, &[0x0F, 0x18], 3),
            "prefetchnta" => self.encode_sse_mem_only(ops, &[0x0F, 0x18], 0),
            "prefetchw" => self.encode_sse_mem_only(ops, &[0x0F, 0x0D], 1),

            // RDRAND/RDSEED
            "rdrand" | "rdrandl" | "rdrandq" => self.encode_rdrand(ops, mnemonic, 0xC7, 6),
            "rdseed" | "rdseedl" | "rdseedq" => self.encode_rdrand(ops, mnemonic, 0xC7, 7),

            // ---- Instructions from i686 encoder for cohesion ----

            // SSE ordered compare (comisd/comiss)
            "comisd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x2F]),
            "comiss" => self.encode_sse_op(ops, &[0x0F, 0x2F]),

            // SSE compare with immediate (cmpsd/cmpss/cmppd/cmpps as SSE ops)
            "cmpsd" => self.encode_sse_op_imm8(ops, &[0xF2, 0x0F, 0xC2]),
            "cmpss" => self.encode_sse_op_imm8(ops, &[0xF3, 0x0F, 0xC2]),
            "cmppd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0xC2]),
            "cmpps" => self.encode_sse_op_imm8(ops, &[0x0F, 0xC2]),

            // x87 additional from i686
            "ftst" => { self.bytes.extend_from_slice(&[0xD9, 0xE4]); Ok(()) }
            "fxam" => { self.bytes.extend_from_slice(&[0xD9, 0xE5]); Ok(()) }
            "fucomi" => self.encode_fucomi(ops),
            "fstl" => self.encode_x87_mem(ops, &[0xDD], 2),

            // x87 reverse memory arithmetic
            "fsubrl" => self.encode_x87_mem(ops, &[0xDC], 5),
            "fdivrl" => self.encode_x87_mem(ops, &[0xDC], 7),
            "fsubrs" => self.encode_x87_mem(ops, &[0xD8], 5),
            "fdivrs" => self.encode_x87_mem(ops, &[0xD8], 7),

            // Data movement: byte-to-word extensions
            "movsbw" => self.encode_movsx(ops, 1, 2),
            "movzbw" => self.encode_movzx(ops, 1, 2),

            // SSE packed integer (missing from x86)
            "pcmpgtd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x66]),
            "pmulhuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE4]),
            "psubb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF8]),

            // System instructions
            "rdmsr" => { self.bytes.extend_from_slice(&[0x0F, 0x32]); Ok(()) }
            "wrmsr" => { self.bytes.extend_from_slice(&[0x0F, 0x30]); Ok(()) }
            "xgetbv" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xD0]); Ok(()) }
            "xsetbv" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xD1]); Ok(()) }
            "rdpmc" => { self.bytes.extend_from_slice(&[0x0F, 0x33]); Ok(()) }
            "verw" => self.encode_verw(ops),
            "lsl" => self.encode_lsl(ops),

            // System table instructions
            "sgdt" | "sidt" | "lgdt" | "lidt" => self.encode_system_table(ops, mnemonic),

            _ if mnemonic.starts_with("set") => {
                // Handle setXXb forms (e.g., setcb = setc with byte suffix)
                self.encode_setcc(ops, mnemonic)
            }

            // SSE comparison pseudo-ops: cmpXXps/pd/ss/sd -> cmpps/pd/ss/sd $imm
            _ if self.try_encode_sse_cmp_pseudo(ops, mnemonic)?.is_some() => Ok(()),

            // AVX comparison pseudo-ops: vcmpXXps/pd/ss/sd -> vcmpps/pd/ss/sd $imm
            _ if self.try_encode_avx_cmp_pseudo(ops, mnemonic)?.is_some() => Ok(()),

            _ => {
                // TODO: many more instructions to handle
                Err(format!("unhandled instruction: {} {:?}", mnemonic, ops))
            }
        }
    }

    // ---- Encoding helpers ----

    /// Build a REX prefix byte.
    fn rex(&self, w: bool, r: bool, x: bool, b: bool) -> u8 {
        let mut rex = 0x40u8;
        if w { rex |= 0x08; }
        if r { rex |= 0x04; }
        if x { rex |= 0x02; }
        if b { rex |= 0x01; }
        rex
    }

    /// Encode ModR/M byte.
    fn modrm(&self, mod_: u8, reg: u8, rm: u8) -> u8 {
        (mod_ << 6) | ((reg & 7) << 3) | (rm & 7)
    }

    /// Encode SIB byte.
    fn sib(&self, scale: u8, index: u8, base: u8) -> u8 {
        let scale_bits = match scale {
            1 => 0,
            2 => 1,
            4 => 2,
            8 => 3,
            _ => 0,
        };
        (scale_bits << 6) | ((index & 7) << 3) | (base & 7)
    }

    /// Emit REX prefix if needed for reg-reg operation.
    fn emit_rex_rr(&mut self, size: u8, reg: &str, rm: &str) {
        let w = size == 8;
        let r = needs_rex_ext(reg);
        let b = needs_rex_ext(rm);
        let need_rex = w || r || b || is_rex_required_8bit(reg) || is_rex_required_8bit(rm);
        if need_rex {
            self.bytes.push(self.rex(w, r, false, b));
        }
    }

    /// Emit segment override prefix (0x64 for %fs, 0x65 for %gs) if present.
    /// Must be emitted before any operand-size override, REX prefix, or opcode.
    // TODO: emit_segment_prefix is currently only called in mov (reg-mem, mem-reg) and ALU ops.
    // Other instruction families that accept memory operands should also call this.
    fn emit_segment_prefix(&mut self, mem: &MemoryOperand) -> Result<(), String> {
        if let Some(ref seg) = mem.segment {
            match seg.as_str() {
                "fs" => self.bytes.push(0x64),
                "gs" => self.bytes.push(0x65),
                _ => return Err(format!("unsupported segment override: %{}", seg)),
            }
        }
        Ok(())
    }

    /// Emit REX prefix for a memory operand where 'reg' is the reg field.
    fn emit_rex_rm(&mut self, size: u8, reg: &str, mem: &MemoryOperand) {
        let w = size == 8;
        let r = needs_rex_ext(reg);
        let b = mem.base.as_ref().is_some_and(|b| needs_rex_ext(&b.name));
        let x = mem.index.as_ref().is_some_and(|i| needs_rex_ext(&i.name));
        let need_rex = w || r || b || x || is_rex_required_8bit(reg);
        if need_rex {
            self.bytes.push(self.rex(w, r, x, b));
        }
    }

    /// Emit REX prefix for unary operation on register.
    fn emit_rex_unary(&mut self, size: u8, rm: &str) {
        let w = size == 8;
        let b = needs_rex_ext(rm);
        let need_rex = w || b || is_rex_required_8bit(rm);
        if need_rex {
            self.bytes.push(self.rex(w, false, false, b));
        }
    }

    /// Encode ModR/M + SIB + displacement for a memory operand.
    /// Returns the bytes to append. `reg_field` is the /r value (3 bits).
    fn encode_modrm_mem(&mut self, reg_field: u8, mem: &MemoryOperand) -> Result<(), String> {
        let base = mem.base.as_ref();
        let index = mem.index.as_ref();

        // RIP-relative addressing
        if let Some(base_reg) = base {
            if base_reg.name == "rip" {
                // ModR/M: mod=00, rm=101 (RIP-relative)
                self.bytes.push(self.modrm(0, reg_field, 5));
                // 32-bit displacement (will be filled by relocation)
                match &mem.displacement {
                    Displacement::Symbol(sym) => {
                        self.add_relocation(sym, R_X86_64_PC32, -4);
                        self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                    }
                    Displacement::SymbolAddend(sym, addend) => {
                        self.add_relocation(sym, R_X86_64_PC32, *addend - 4);
                        self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                    }
                    Displacement::SymbolPlusOffset(sym, offset) => {
                        self.add_relocation(sym, R_X86_64_PC32, *offset - 4);
                        self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                    }
                    Displacement::SymbolMod(sym, modifier) => {
                        let reloc_type = match modifier.as_str() {
                            "GOTPCREL" => R_X86_64_GOTPCREL,
                            "GOTTPOFF" => R_X86_64_GOTTPOFF,
                            "PLT" => R_X86_64_PLT32,
                            _ => R_X86_64_PC32,
                        };
                        self.add_relocation(sym, reloc_type, -4);
                        self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                    }
                    Displacement::Integer(val) => {
                        self.bytes.extend_from_slice(&(*val as i32).to_le_bytes());
                    }
                    Displacement::None => {
                        self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                    }
                }
                return Ok(());
            }
        }

        // Handle symbol displacements that need relocations.
        // We defer emitting the relocation until after the ModR/M and SIB bytes
        // so the relocation offset correctly points to the displacement bytes.
        let (disp_val, has_symbol, deferred_reloc) = match &mem.displacement {
            Displacement::None => (0i64, false, None),
            Displacement::Integer(v) => (*v, false, None),
            Displacement::Symbol(sym) => {
                (0i64, true, Some((sym.clone(), R_X86_64_32S, 0i64)))
            }
            Displacement::SymbolAddend(sym, addend) => {
                (0i64, true, Some((sym.clone(), R_X86_64_32S, *addend)))
            }
            Displacement::SymbolPlusOffset(sym, offset) => {
                (0i64, true, Some((sym.clone(), R_X86_64_32S, *offset)))
            }
            Displacement::SymbolMod(sym, modifier) => {
                let reloc_type = match modifier.as_str() {
                    "TPOFF" => R_X86_64_TPOFF32,
                    "GOTPCREL" => R_X86_64_GOTPCREL,
                    _ => R_X86_64_32S,
                };
                (0i64, true, Some((sym.clone(), reloc_type, 0i64)))
            }
        };

        // No base register - need SIB with no-base encoding
        if base.is_none() && index.is_none() {
            // Direct memory reference - mod=00, rm=100 (SIB), SIB: base=101 (no base)
            self.bytes.push(self.modrm(0, reg_field, 4));
            self.bytes.push(self.sib(1, 4, 5)); // index=100 (none), base=101 (disp32)
            if let Some((sym, reloc_type, addend)) = deferred_reloc {
                self.add_relocation(&sym, reloc_type, addend);
            }
            self.bytes.extend_from_slice(&(disp_val as i32).to_le_bytes());
            return Ok(());
        }

        let base_reg = base.map(|r| &r.name as &str).unwrap_or("");
        let base_num = if !base_reg.is_empty() { reg_num(base_reg).unwrap_or(0) } else { 5 };

        // Determine if we need SIB
        let need_sib = index.is_some()
            || (base_num & 7) == 4  // RSP/R12 always need SIB
            || base.is_none();

        // Determine displacement size
        let (mod_bits, disp_size) = if has_symbol {
            (2, 4) // always use disp32 for symbols
        } else if disp_val == 0 && (base_num & 7) != 5 {
            // No displacement (RBP/R13 always need at least disp8)
            (0, 0)
        } else if (-128..=127).contains(&disp_val) {
            (1, 1) // disp8
        } else {
            (2, 4) // disp32
        };

        if need_sib {
            let idx = index.as_ref();
            let idx_num = idx.map(|r| reg_num(&r.name).unwrap_or(4)).unwrap_or(4); // 4 = no index
            let scale = mem.scale.unwrap_or(1);

            if base.is_none() {
                // No base - disp32 with SIB
                self.bytes.push(self.modrm(0, reg_field, 4));
                self.bytes.push(self.sib(scale, idx_num, 5));
                if let Some((sym, reloc_type, addend)) = deferred_reloc {
                    self.add_relocation(&sym, reloc_type, addend);
                }
                self.bytes.extend_from_slice(&(disp_val as i32).to_le_bytes());
            } else {
                self.bytes.push(self.modrm(mod_bits, reg_field, 4));
                self.bytes.push(self.sib(scale, idx_num, base_num));
                if let Some((sym, reloc_type, addend)) = deferred_reloc {
                    self.add_relocation(&sym, reloc_type, addend);
                }
                match disp_size {
                    0 => {}
                    1 => self.bytes.push(disp_val as u8),
                    4 => self.bytes.extend_from_slice(&(disp_val as i32).to_le_bytes()),
                    _ => unreachable!(),
                }
            }
        } else {
            self.bytes.push(self.modrm(mod_bits, reg_field, base_num));
            if let Some((sym, reloc_type, addend)) = deferred_reloc {
                self.add_relocation(&sym, reloc_type, addend);
            }
            match disp_size {
                0 => {}
                1 => self.bytes.push(disp_val as u8),
                4 => self.bytes.extend_from_slice(&(disp_val as i32).to_le_bytes()),
                _ => unreachable!(),
            }
        }

        Ok(())
    }

    /// Add a relocation relative to current position.
    fn add_relocation(&mut self, symbol: &str, reloc_type: u32, addend: i64) {
        self.relocations.push(Relocation {
            offset: self.offset + self.bytes.len() as u64 - (self.offset), // adjusted in caller
            symbol: symbol.to_string(),
            reloc_type,
            addend,
        });
    }

    // ---- Instruction-specific encoders ----

    fn encode_mov(&mut self, ops: &[Operand], size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("mov requires 2 operands, got {}", ops.len()));
        }

        match (&ops[0], &ops[1]) {
            // mov $imm, %reg
            (Operand::Immediate(imm), Operand::Register(dst)) => {
                self.encode_mov_imm_reg(imm, dst, size)
            }
            // mov %reg, %reg
            (Operand::Register(src), Operand::Register(dst)) => {
                self.encode_mov_rr(src, dst, size)
            }
            // mov mem, %reg
            (Operand::Memory(mem), Operand::Register(dst)) => {
                self.encode_mov_mem_reg(mem, dst, size)
            }
            // mov %reg, mem
            (Operand::Register(src), Operand::Memory(mem)) => {
                self.encode_mov_reg_mem(src, mem, size)
            }
            // mov $imm, mem
            (Operand::Immediate(imm), Operand::Memory(mem)) => {
                self.encode_mov_imm_mem(imm, mem, size)
            }
            _ => Err("unsupported mov operand combination".to_string()),
        }
    }

    fn encode_mov_imm_reg(&mut self, imm: &ImmediateValue, dst: &Register, size: u8) -> Result<(), String> {
        let dst_num = reg_num(&dst.name).ok_or_else(|| format!("bad register: {}", dst.name))?;

        match imm {
            ImmediateValue::Integer(val) => {
                let val = *val;
                if size == 8 {
                    // For 64-bit: if value fits in signed 32-bit, use movq $imm32, %reg (sign-extended)
                    if val >= i32::MIN as i64 && val <= i32::MAX as i64 {
                        self.emit_rex_unary(8, &dst.name);
                        self.bytes.push(0xC7);
                        self.bytes.push(self.modrm(3, 0, dst_num));
                        self.bytes.extend_from_slice(&(val as i32).to_le_bytes());
                    } else {
                        // Need movabsq for 64-bit immediate
                        let b = needs_rex_ext(&dst.name);
                        self.bytes.push(self.rex(true, false, false, b));
                        self.bytes.push(0xB8 + (dst_num & 7));
                        self.bytes.extend_from_slice(&val.to_le_bytes());
                    }
                } else if size == 4 {
                    if needs_rex_ext(&dst.name) {
                        self.bytes.push(self.rex(false, false, false, true));
                    }
                    self.bytes.push(0xC7);
                    self.bytes.push(self.modrm(3, 0, dst_num));
                    self.bytes.extend_from_slice(&(val as i32).to_le_bytes());
                } else if size == 2 {
                    self.bytes.push(0x66); // operand size prefix
                    if needs_rex_ext(&dst.name) {
                        self.bytes.push(self.rex(false, false, false, true));
                    }
                    self.bytes.push(0xC7);
                    self.bytes.push(self.modrm(3, 0, dst_num));
                    self.bytes.extend_from_slice(&(val as i16).to_le_bytes());
                } else {
                    // 8-bit
                    if needs_rex_ext(&dst.name) || is_rex_required_8bit(&dst.name) {
                        self.bytes.push(self.rex(false, false, false, needs_rex_ext(&dst.name)));
                    }
                    self.bytes.push(0xC6);
                    self.bytes.push(self.modrm(3, 0, dst_num));
                    self.bytes.push(val as u8);
                }
            }
            ImmediateValue::Symbol(sym) => {
                // movq $symbol, %reg - load address
                if size == 8 {
                    self.emit_rex_unary(8, &dst.name);
                    self.bytes.push(0xC7);
                    self.bytes.push(self.modrm(3, 0, dst_num));
                    self.add_relocation(sym, R_X86_64_32S, 0);
                    self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                } else {
                    self.emit_rex_unary(size, &dst.name);
                    self.bytes.push(0xC7);
                    self.bytes.push(self.modrm(3, 0, dst_num));
                    self.add_relocation(sym, R_X86_64_32, 0);
                    self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                }
            }
            ImmediateValue::SymbolMod(_, _) | ImmediateValue::SymbolDiff(_, _) => {
                Err("unsupported immediate type for mov".to_string())?
            }
        }
        Ok(())
    }

    fn encode_mov_rr(&mut self, src: &Register, dst: &Register, size: u8) -> Result<(), String> {
        let src_num = reg_num(&src.name).ok_or_else(|| format!("bad register: {}", src.name))?;
        let dst_num = reg_num(&dst.name).ok_or_else(|| format!("bad register: {}", dst.name))?;

        if size == 2 {
            self.bytes.push(0x66);
        }
        self.emit_rex_rr(size, &src.name, &dst.name);
        if size == 1 {
            self.bytes.push(0x88);
        } else {
            self.bytes.push(0x89);
        }
        self.bytes.push(self.modrm(3, src_num, dst_num));
        Ok(())
    }

    fn encode_mov_mem_reg(&mut self, mem: &MemoryOperand, dst: &Register, size: u8) -> Result<(), String> {
        let dst_num = reg_num(&dst.name).ok_or_else(|| format!("bad register: {}", dst.name))?;

        // Handle segment prefix
        self.emit_segment_prefix(mem)?;

        if size == 2 {
            self.bytes.push(0x66);
        }
        self.emit_rex_rm(size, &dst.name, mem);
        if size == 1 {
            self.bytes.push(0x8A);
        } else {
            self.bytes.push(0x8B);
        }
        self.encode_modrm_mem(dst_num, mem)
    }

    fn encode_mov_reg_mem(&mut self, src: &Register, mem: &MemoryOperand, size: u8) -> Result<(), String> {
        let src_num = reg_num(&src.name).ok_or_else(|| format!("bad register: {}", src.name))?;

        self.emit_segment_prefix(mem)?;

        if size == 2 {
            self.bytes.push(0x66);
        }
        self.emit_rex_rm(size, &src.name, mem);
        if size == 1 {
            self.bytes.push(0x88);
        } else {
            self.bytes.push(0x89);
        }
        self.encode_modrm_mem(src_num, mem)
    }

    fn encode_mov_imm_mem(&mut self, imm: &ImmediateValue, mem: &MemoryOperand, size: u8) -> Result<(), String> {
        if size == 2 {
            self.bytes.push(0x66);
        }
        // Use an empty string for REX calculation since the reg field is /0
        self.emit_rex_rm(if size == 8 { 8 } else { size }, "", mem);
        if size == 1 {
            self.bytes.push(0xC6);
        } else {
            self.bytes.push(0xC7);
        }
        self.encode_modrm_mem(0, mem)?;

        match imm {
            ImmediateValue::Integer(val) => {
                match size {
                    1 => self.bytes.push(*val as u8),
                    2 => self.bytes.extend_from_slice(&(*val as i16).to_le_bytes()),
                    4 | 8 => self.bytes.extend_from_slice(&(*val as i32).to_le_bytes()),
                    _ => unreachable!(),
                }
            }
            _ => return Err("unsupported immediate for mov to memory".to_string()),
        }
        Ok(())
    }

    fn encode_movabs(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movabsq requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let b = needs_rex_ext(&dst.name);
                self.bytes.push(self.rex(true, false, false, b));
                self.bytes.push(0xB8 + (dst_num & 7));
                self.bytes.extend_from_slice(&val.to_le_bytes());
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Symbol(sym)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let b = needs_rex_ext(&dst.name);
                self.bytes.push(self.rex(true, false, false, b));
                self.bytes.push(0xB8 + (dst_num & 7));
                self.add_relocation(sym, R_X86_64_64, 0);
                self.bytes.extend_from_slice(&[0u8; 8]);
                Ok(())
            }
            _ => Err("unsupported movabsq operands".to_string()),
        }
    }

    fn encode_movsx(&mut self, ops: &[Operand], src_size: u8, dst_size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movsx requires 2 operands".to_string());
        }

        let opcode = match (src_size, dst_size) {
            (1, _) => vec![0x0F, 0xBE],   // movsbq/movsbl/movsbw
            (2, _) => vec![0x0F, 0xBF],   // movswq/movswl
            (4, 8) => vec![0x63],          // movslq (movsxd)
            _ => return Err(format!("unsupported movsx combination: {} -> {}", src_size, dst_size)),
        };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                // 16-bit destination needs operand-size override prefix
                if dst_size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rr(dst_size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                if dst_size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(dst_size, &dst.name, mem);
                self.bytes.extend_from_slice(&opcode);
                self.encode_modrm_mem(dst_num, mem)?;
            }
            _ => return Err("unsupported movsx operands".to_string()),
        }
        Ok(())
    }

    fn encode_movzx(&mut self, ops: &[Operand], src_size: u8, dst_size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movzx requires 2 operands".to_string());
        }

        let opcode = match src_size {
            1 => vec![0x0F, 0xB6],  // movzbl/movzbq/movzbw
            2 => vec![0x0F, 0xB7],  // movzwl/movzwq
            _ => return Err(format!("unsupported movzx src size: {}", src_size)),
        };

        // Note: movzbl zero-extends to 64 bits implicitly (32-bit op clears upper 32)
        // So we use size=4 for REX calculation unless dst is an extended register needing REX.B
        let rex_size = if dst_size == 8 { 8 } else { 4 };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                // 16-bit destination needs operand-size override prefix
                if dst_size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rr(rex_size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                if dst_size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(rex_size, &dst.name, mem);
                self.bytes.extend_from_slice(&opcode);
                self.encode_modrm_mem(dst_num, mem)?;
            }
            _ => return Err("unsupported movzx operands".to_string()),
        }
        Ok(())
    }

    fn encode_lea(&mut self, ops: &[Operand], size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("lea requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.push(0x8D);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("lea requires memory source and register destination".to_string()),
        }
    }

    fn encode_push(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("push requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                if needs_rex_ext(&reg.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.push(0x50 + (num & 7));
                Ok(())
            }
            Operand::Immediate(ImmediateValue::Integer(val)) => {
                if *val >= -128 && *val <= 127 {
                    self.bytes.push(0x6A);
                    self.bytes.push(*val as u8);
                } else {
                    self.bytes.push(0x68);
                    self.bytes.extend_from_slice(&(*val as i32).to_le_bytes());
                }
                Ok(())
            }
            Operand::Memory(mem) => {
                self.emit_rex_rm(0, "", mem);
                self.bytes.push(0xFF);
                self.encode_modrm_mem(6, mem)
            }
            _ => Err("unsupported push operand".to_string()),
        }
    }

    fn encode_pop(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("pop requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                if needs_rex_ext(&reg.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.push(0x58 + (num & 7));
                Ok(())
            }
            Operand::Memory(mem) => {
                // pop to memory: 8F /0
                self.emit_rex_rm(0, "", mem);
                self.bytes.push(0x8F);
                self.encode_modrm_mem(0, mem)
            }
            _ => Err("unsupported pop operand".to_string()),
        }
    }

    /// Encode ALU operations (add/or/adc/sbb/and/sub/xor/cmp).
    /// `alu_op` is the operation number (0-7).
    fn encode_alu(&mut self, ops: &[Operand], mnemonic: &str, alu_op: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }

        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);

        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(dst)) => {
                let val = *val;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;

                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_unary(size, &dst.name);

                if size == 1 {
                    // 8-bit ALU with imm8
                    self.bytes.push(0x80);
                    self.bytes.push(self.modrm(3, alu_op, dst_num));
                    self.bytes.push(val as u8);
                } else if (-128..=127).contains(&val) {
                    // Sign-extended imm8
                    self.bytes.push(0x83);
                    self.bytes.push(self.modrm(3, alu_op, dst_num));
                    self.bytes.push(val as u8);
                } else {
                    // imm32
                    if dst_num == 0 && !needs_rex_ext(&dst.name) {
                        // Special short form: op eax/rax, imm32
                        self.bytes.push(if size == 1 { 0x04 } else { 0x05 } + alu_op * 8);
                    } else {
                        self.bytes.push(0x81);
                        self.bytes.push(self.modrm(3, alu_op, dst_num));
                    }
                    if size == 2 {
                        self.bytes.extend_from_slice(&(val as i16).to_le_bytes());
                    } else {
                        self.bytes.extend_from_slice(&(val as i32).to_le_bytes());
                    }
                }
                Ok(())
            }
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;

                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rr(size, &src.name, &dst.name);
                self.bytes.push(if size == 1 { 0x00 } else { 0x01 } + alu_op * 8);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.emit_segment_prefix(mem)?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.push(if size == 1 { 0x02 } else { 0x03 } + alu_op * 8);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                self.emit_segment_prefix(mem)?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.push(if size == 1 { 0x00 } else { 0x01 } + alu_op * 8);
                self.encode_modrm_mem(src_num, mem)
            }
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Memory(mem)) => {
                let val = *val;
                self.emit_segment_prefix(mem)?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, "", mem);

                if size == 1 {
                    self.bytes.push(0x80);
                    self.encode_modrm_mem(alu_op, mem)?;
                    self.bytes.push(val as u8);
                } else if (-128..=127).contains(&val) {
                    self.bytes.push(0x83);
                    self.encode_modrm_mem(alu_op, mem)?;
                    self.bytes.push(val as u8);
                } else {
                    self.bytes.push(0x81);
                    self.encode_modrm_mem(alu_op, mem)?;
                    if size == 2 {
                        self.bytes.extend_from_slice(&(val as i16).to_le_bytes());
                    } else {
                        self.bytes.extend_from_slice(&(val as i32).to_le_bytes());
                    }
                }
                Ok(())
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    fn encode_test(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }

        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rr(size, &src.name, &dst.name);
                self.bytes.push(if size == 1 { 0x84 } else { 0x85 });
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(dst)) => {
                let val = *val;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_unary(size, &dst.name);

                if size == 1 {
                    if dst_num == 0 && !needs_rex_ext(&dst.name) {
                        self.bytes.push(0xA8);
                    } else {
                        self.bytes.push(0xF6);
                        self.bytes.push(self.modrm(3, 0, dst_num));
                    }
                    self.bytes.push(val as u8);
                } else {
                    if dst_num == 0 && !needs_rex_ext(&dst.name) {
                        self.bytes.push(0xA9);
                    } else {
                        self.bytes.push(0xF7);
                        self.bytes.push(self.modrm(3, 0, dst_num));
                    }
                    if size == 2 {
                        self.bytes.extend_from_slice(&(val as i16).to_le_bytes());
                    } else {
                        self.bytes.extend_from_slice(&(val as i32).to_le_bytes());
                    }
                }
                Ok(())
            }
            // test %reg, mem -> TEST mem, reg (AT&T: src=reg, dst=mem)
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                self.emit_segment_prefix(mem)?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.push(if size == 1 { 0x84 } else { 0x85 });
                self.encode_modrm_mem(src_num, mem)
            }
            // test $imm, mem
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Memory(mem)) => {
                let val = *val;
                self.emit_segment_prefix(mem)?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, "", mem);
                self.bytes.push(if size == 1 { 0xF6 } else { 0xF7 });
                self.encode_modrm_mem(0, mem)?;
                if size == 1 {
                    self.bytes.push(val as u8);
                } else if size == 2 {
                    self.bytes.extend_from_slice(&(val as i16).to_le_bytes());
                } else {
                    self.bytes.extend_from_slice(&(val as i32).to_le_bytes());
                }
                Ok(())
            }
            _ => Err("unsupported test operands".to_string()),
        }
    }

    fn encode_imul(&mut self, ops: &[Operand], size: u8) -> Result<(), String> {
        match ops.len() {
            1 => {
                // One-operand form: imul r/m (result in rdx:rax)
                self.encode_unary_rm(ops, 5, size)
            }
            2 => {
                // Two-operand form: imul src, dst  OR  imul $imm, dst (shorthand for imul $imm, dst, dst)
                match (&ops[0], &ops[1]) {
                    (Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        self.emit_rex_rr(size, &dst.name, &src.name);
                        self.bytes.extend_from_slice(&[0x0F, 0xAF]);
                        self.bytes.push(self.modrm(3, dst_num, src_num));
                        Ok(())
                    }
                    (Operand::Memory(mem), Operand::Register(dst)) => {
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        self.emit_rex_rm(size, &dst.name, mem);
                        self.bytes.extend_from_slice(&[0x0F, 0xAF]);
                        self.encode_modrm_mem(dst_num, mem)
                    }
                    (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(dst)) => {
                        // 2-operand imul $imm, %reg is same as 3-operand imul $imm, %reg, %reg
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        self.emit_rex_rr(size, &dst.name, &dst.name);
                        if *val >= -128 && *val <= 127 {
                            self.bytes.push(0x6B);
                            self.bytes.push(self.modrm(3, dst_num, dst_num));
                            self.bytes.push(*val as u8);
                        } else {
                            self.bytes.push(0x69);
                            self.bytes.push(self.modrm(3, dst_num, dst_num));
                            self.bytes.extend_from_slice(&(*val as i32).to_le_bytes());
                        }
                        Ok(())
                    }
                    _ => Err("unsupported imul operands".to_string()),
                }
            }
            3 => {
                // Three-operand form: imul $imm, src, dst
                match (&ops[0], &ops[1], &ops[2]) {
                    (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        self.emit_rex_rr(size, &dst.name, &src.name);
                        if *val >= -128 && *val <= 127 {
                            self.bytes.push(0x6B);
                            self.bytes.push(self.modrm(3, dst_num, src_num));
                            self.bytes.push(*val as u8);
                        } else {
                            self.bytes.push(0x69);
                            self.bytes.push(self.modrm(3, dst_num, src_num));
                            self.bytes.extend_from_slice(&(*val as i32).to_le_bytes());
                        }
                        Ok(())
                    }
                    _ => Err("unsupported imul operands".to_string()),
                }
            }
            _ => Err("imul requires 1-3 operands".to_string()),
        }
    }

    fn encode_unary_rm(&mut self, ops: &[Operand], op_ext: u8, size: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("unary op requires 1 operand".to_string());
        }
        // inc (op_ext=0) and dec (op_ext=1) use FE/FF, not F6/F7
        let base_opcode = if op_ext <= 1 {
            if size == 1 { 0xFE } else { 0xFF }
        } else if size == 1 { 0xF6 } else { 0xF7 };
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                self.emit_rex_unary(size, &reg.name);
                self.bytes.push(base_opcode);
                self.bytes.push(self.modrm(3, op_ext, num));
                Ok(())
            }
            Operand::Memory(mem) => {
                self.emit_rex_rm(size, "", mem);
                self.bytes.push(base_opcode);
                self.encode_modrm_mem(op_ext, mem)
            }
            _ => Err("unsupported unary operand".to_string()),
        }
    }

    /// Encode INC/DEC using Group 5 opcode (0xFE/0xFF), not Group 3 (0xF6/0xF7).
    fn encode_inc_dec(&mut self, ops: &[Operand], op_ext: u8, size: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("inc/dec requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_unary(size, &reg.name);
                self.bytes.push(if size == 1 { 0xFE } else { 0xFF });
                self.bytes.push(self.modrm(3, op_ext, num));
                Ok(())
            }
            Operand::Memory(mem) => {
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, "", mem);
                self.bytes.push(if size == 1 { 0xFE } else { 0xFF });
                self.encode_modrm_mem(op_ext, mem)
            }
            _ => Err("unsupported inc/dec operand".to_string()),
        }
    }

    fn encode_shift(&mut self, ops: &[Operand], mnemonic: &str, shift_op: u8) -> Result<(), String> {
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);

        // Handle 1-operand form: shift by 1 implicitly
        if ops.len() == 1 {
            match &ops[0] {
                Operand::Register(dst) => {
                    let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                    if size == 2 { self.bytes.push(0x66); }
                    self.emit_rex_unary(size, &dst.name);
                    self.bytes.push(if size == 1 { 0xD0 } else { 0xD1 });
                    self.bytes.push(self.modrm(3, shift_op, dst_num));
                    return Ok(());
                }
                Operand::Memory(mem) => {
                    if size == 2 { self.bytes.push(0x66); }
                    self.emit_rex_rm(size, "", mem);
                    self.bytes.push(if size == 1 { 0xD0 } else { 0xD1 });
                    return self.encode_modrm_mem(shift_op, mem);
                }
                _ => return Err(format!("unsupported {} operand", mnemonic)),
            }
        }

        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }

        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(count)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let count = *count as u8;

                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_unary(size, &dst.name);

                if count == 1 {
                    self.bytes.push(if size == 1 { 0xD0 } else { 0xD1 });
                    self.bytes.push(self.modrm(3, shift_op, dst_num));
                } else {
                    self.bytes.push(if size == 1 { 0xC0 } else { 0xC1 });
                    self.bytes.push(self.modrm(3, shift_op, dst_num));
                    self.bytes.push(count);
                }
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(count)), Operand::Memory(mem)) => {
                let count = *count as u8;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, "", mem);
                if count == 1 {
                    self.bytes.push(if size == 1 { 0xD0 } else { 0xD1 });
                    self.encode_modrm_mem(shift_op, mem)
                } else {
                    self.bytes.push(if size == 1 { 0xC0 } else { 0xC1 });
                    self.encode_modrm_mem(shift_op, mem)?;
                    self.bytes.push(count);
                    Ok(())
                }
            }
            (Operand::Register(cl), Operand::Register(dst)) if cl.name == "cl" => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_unary(size, &dst.name);
                self.bytes.push(if size == 1 { 0xD2 } else { 0xD3 });
                self.bytes.push(self.modrm(3, shift_op, dst_num));
                Ok(())
            }
            (Operand::Register(cl), Operand::Memory(mem)) if cl.name == "cl" => {
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, "", mem);
                self.bytes.push(if size == 1 { 0xD2 } else { 0xD3 });
                self.encode_modrm_mem(shift_op, mem)
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    fn encode_double_shift(&mut self, ops: &[Operand], opcode: u8, size: u8) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("double shift requires 3 operands".to_string());
        }

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(count)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.emit_rex_rr(size, &src.name, &dst.name);
                self.bytes.extend_from_slice(&[0x0F, opcode]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                self.bytes.push(*count as u8);
                Ok(())
            }
            (Operand::Register(cl), Operand::Register(src), Operand::Register(dst)) if cl.name == "cl" => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.emit_rex_rr(size, &src.name, &dst.name);
                self.bytes.extend_from_slice(&[0x0F, opcode + 1]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            _ => Err("unsupported double shift operands".to_string()),
        }
    }

    fn encode_bswap(&mut self, ops: &[Operand], size: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("bswap requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                self.emit_rex_unary(size, &reg.name);
                self.bytes.extend_from_slice(&[0x0F, 0xC8 + (num & 7)]);
                Ok(())
            }
            _ => Err("bswap requires register operand".to_string()),
        }
    }

    fn encode_bit_count(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }

        let (prefix, opcode) = match mnemonic {
            "lzcntl" | "lzcntq" | "lzcntw" => (0xF3u8, [0x0F, 0xBD]),
            "tzcntl" | "tzcntq" | "tzcntw" => (0xF3, [0x0F, 0xBC]),
            "popcntl" | "popcntq" | "popcntw" => (0xF3, [0x0F, 0xB8]),
            _ => return Err(format!("unknown bit count: {}", mnemonic)),
        };

        let size = mnemonic_size_suffix(mnemonic).unwrap_or(4);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); } // operand-size override for 16-bit
                self.bytes.push(prefix);
                self.emit_rex_rr(size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(prefix);
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.extend_from_slice(&opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    fn encode_setcc(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("setcc requires 1 operand".to_string());
        }

        let cc_str = &mnemonic[3..];
        // Try the condition code as-is first, then strip trailing 'b' suffix
        let cc = cc_from_mnemonic(cc_str).or_else(|_| {
            if let Some(stripped) = cc_str.strip_suffix('b') {
                cc_from_mnemonic(stripped)
            } else {
                Err(format!("unknown condition code: {}", cc_str))
            }
        })?;

        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                if needs_rex_ext(&reg.name) || is_rex_required_8bit(&reg.name) {
                    self.bytes.push(self.rex(false, false, false, needs_rex_ext(&reg.name)));
                }
                self.bytes.extend_from_slice(&[0x0F, 0x90 + cc]);
                self.bytes.push(self.modrm(3, 0, num));
                Ok(())
            }
            Operand::Memory(mem) => {
                self.emit_rex_rm(1, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0x90 + cc]);
                self.encode_modrm_mem(0, mem)
            }
            _ => Err("setcc requires register or memory operand".to_string()),
        }
    }

    fn encode_cmovcc(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("cmovcc requires 2 operands".to_string());
        }

        // Extract condition code: strip "cmov" prefix and size suffix
        let without_prefix = &mnemonic[4..];
        let (cc_str, size) = if let Some(stripped) = without_prefix.strip_suffix('q') {
            (stripped, 8u8)
        } else if let Some(stripped) = without_prefix.strip_suffix('l') {
            (stripped, 4u8)
        } else if let Some(stripped) = without_prefix.strip_suffix('w') {
            (stripped, 2u8)
        } else {
            (without_prefix, 8u8) // default to 64-bit
        };
        let cc = cc_from_mnemonic(cc_str)?;

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rr(size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, 0x40 + cc]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0x40 + cc]);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported cmov operands".to_string()),
        }
    }

    fn encode_jmp(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("jmp requires 1 operand".to_string());
        }

        match &ops[0] {
            Operand::Label(label) => {
                // Near jump with 32-bit displacement (will be resolved by linker/relocator)
                self.bytes.push(0xE9);
                self.add_relocation(label, R_X86_64_PC32, -4);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            Operand::Indirect(inner) => {
                match inner.as_ref() {
                    Operand::Register(reg) => {
                        let num = reg_num(&reg.name).ok_or("bad register")?;
                        if needs_rex_ext(&reg.name) {
                            self.bytes.push(self.rex(false, false, false, true));
                        }
                        self.bytes.push(0xFF);
                        self.bytes.push(self.modrm(3, 4, num));
                        Ok(())
                    }
                    Operand::Memory(mem) => {
                        self.emit_rex_rm(0, "", mem);
                        self.bytes.push(0xFF);
                        self.encode_modrm_mem(4, mem)
                    }
                    _ => Err("unsupported indirect jmp target".to_string()),
                }
            }
            _ => Err("unsupported jmp operand".to_string()),
        }
    }

    fn encode_jcc(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("jcc requires 1 operand".to_string());
        }

        let cc = cc_from_mnemonic(&mnemonic[1..])?;

        match &ops[0] {
            Operand::Label(label) => {
                // Near jcc with 32-bit displacement
                self.bytes.extend_from_slice(&[0x0F, 0x80 + cc]);
                self.add_relocation(label, R_X86_64_PC32, -4);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            _ => Err("jcc requires label operand".to_string()),
        }
    }

    fn encode_call(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("call requires 1 operand".to_string());
        }

        match &ops[0] {
            Operand::Label(label) => {
                self.bytes.push(0xE8);
                // Use PLT32 for external function calls (linker will resolve)
                let reloc_type = R_X86_64_PLT32;
                let sym = label.trim_end_matches("@PLT");
                self.add_relocation(sym, reloc_type, -4);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            Operand::Indirect(inner) => {
                match inner.as_ref() {
                    Operand::Register(reg) => {
                        let num = reg_num(&reg.name).ok_or("bad register")?;
                        if needs_rex_ext(&reg.name) {
                            self.bytes.push(self.rex(false, false, false, true));
                        }
                        self.bytes.push(0xFF);
                        self.bytes.push(self.modrm(3, 2, num));
                        Ok(())
                    }
                    Operand::Memory(mem) => {
                        // call *disp(%base) - FF /2 with memory operand
                        self.emit_rex_rm(0, "", mem);
                        self.bytes.push(0xFF);
                        self.encode_modrm_mem(2, mem)
                    }
                    _ => Err("unsupported indirect call target".to_string()),
                }
            }
            _ => Err("unsupported call operand".to_string()),
        }
    }

    fn encode_xchg(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("xchg requires 2 operands".to_string());
        }
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.push(if size == 1 { 0x86 } else { 0x87 });
                self.encode_modrm_mem(src_num, mem)
            }
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rr(size, &src.name, &dst.name);
                self.bytes.push(if size == 1 { 0x86 } else { 0x87 });
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            _ => Err("unsupported xchg operands".to_string()),
        }
    }

    fn encode_cmpxchg(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("cmpxchg requires 2 operands".to_string());
        }
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, if size == 1 { 0xB0 } else { 0xB1 }]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported cmpxchg operands".to_string()),
        }
    }

    fn encode_xadd(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("xadd requires 2 operands".to_string());
        }
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, if size == 1 { 0xC0 } else { 0xC1 }]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported xadd operands".to_string()),
        }
    }

    /// Encode OUT instruction: outb/outw/outl
    /// AT&T syntax: outb %al, %dx  OR  outb %al, $imm8
    fn encode_out(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        let size: u8 = match mnemonic {
            "outb" => 1,
            "outw" => 2,
            "outl" => 4,
            _ => return Err(format!("unknown out mnemonic: {}", mnemonic)),
        };

        // Handle zero-operand form (implicit operands)
        if ops.is_empty() {
            if size == 2 { self.bytes.push(0x66); }
            self.bytes.push(if size == 1 { 0xEE } else { 0xEF });
            return Ok(());
        }

        if ops.len() != 2 {
            return Err(format!("{} requires 0 or 2 operands", mnemonic));
        }

        // AT&T: outb %al, %dx  (src=al, dst=dx)
        // AT&T: outb %al, $imm8  (src=al, dst=imm)
        match (&ops[0], &ops[1]) {
            (Operand::Register(_src), Operand::Register(_dst)) => {
                // outb %al, %dx  =>  0xEE (byte) or 0xEF (word/dword)
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xEE } else { 0xEF });
                Ok(())
            }
            (Operand::Register(_src), Operand::Immediate(ImmediateValue::Integer(val))) => {
                // outb %al, $imm8  =>  0xE6 ib (byte) or 0xE7 ib (word/dword)
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xE6 } else { 0xE7 });
                self.bytes.push(*val as u8);
                Ok(())
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    /// Encode IN instruction: inb/inw/inl
    /// AT&T syntax: inb %dx, %al  OR  inb $imm8, %al
    fn encode_in(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        let size: u8 = match mnemonic {
            "inb" => 1,
            "inw" => 2,
            "inl" => 4,
            _ => return Err(format!("unknown in mnemonic: {}", mnemonic)),
        };

        // Handle zero-operand form (implicit operands)
        if ops.is_empty() {
            if size == 2 { self.bytes.push(0x66); }
            self.bytes.push(if size == 1 { 0xEC } else { 0xED });
            return Ok(());
        }

        if ops.len() != 2 {
            return Err(format!("{} requires 0 or 2 operands", mnemonic));
        }

        // AT&T: inb %dx, %al  (src=dx, dst=al)
        // AT&T: inb $imm8, %al  (src=imm, dst=al)
        match (&ops[0], &ops[1]) {
            (Operand::Register(_src), Operand::Register(_dst)) => {
                // inb %dx, %al  =>  0xEC (byte) or 0xED (word/dword)
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xEC } else { 0xED });
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(_dst)) => {
                // inb $imm8, %al  =>  0xE4 ib (byte) or 0xE5 ib (word/dword)
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xE4 } else { 0xE5 });
                self.bytes.push(*val as u8);
                Ok(())
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    fn encode_clflush(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("clflush requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0xAE]);
                self.encode_modrm_mem(7, mem)
            }
            _ => Err("clflush requires memory operand".to_string()),
        }
    }

    /// Encode VERW instruction: 0F 00 /5
    fn encode_verw(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("verw requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0x00]);
                self.encode_modrm_mem(5, mem)
            }
            Operand::Register(reg) => {
                let rm = reg_num(&reg.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0x00]);
                self.bytes.push(self.modrm(3, 5, rm));
                Ok(())
            }
            _ => Err("verw requires memory or register operand".to_string()),
        }
    }

    /// Encode MOV to/from control register: 0F 20 /r (read) or 0F 22 /r (write)
    fn encode_mov_cr(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("mov cr requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(cr), Operand::Register(gp)) if is_control_reg(&cr.name) => {
                // movq %crN, %rax  =>  0F 20 ModR/M
                let cr_num = control_reg_num(&cr.name).ok_or("bad control register")?;
                let gp_num = reg_num(&gp.name).ok_or("bad register")?;
                // Need REX prefix for r8-r15 or for cr8
                let mut rex = 0u8;
                if cr_num >= 8 { rex |= 0x44; } // REX.R
                if needs_rex_ext(&gp.name) { rex |= 0x41; } // REX.B
                if rex != 0 { self.bytes.push(rex); }
                self.bytes.extend_from_slice(&[0x0F, 0x20]);
                self.bytes.push(self.modrm(3, cr_num & 7, gp_num));
                Ok(())
            }
            (Operand::Register(gp), Operand::Register(cr)) if is_control_reg(&cr.name) => {
                // movq %rax, %crN  =>  0F 22 ModR/M
                let cr_num = control_reg_num(&cr.name).ok_or("bad control register")?;
                let gp_num = reg_num(&gp.name).ok_or("bad register")?;
                let mut rex = 0u8;
                if cr_num >= 8 { rex |= 0x44; } // REX.R
                if needs_rex_ext(&gp.name) { rex |= 0x41; } // REX.B
                if rex != 0 { self.bytes.push(rex); }
                self.bytes.extend_from_slice(&[0x0F, 0x22]);
                self.bytes.push(self.modrm(3, cr_num & 7, gp_num));
                Ok(())
            }
            _ => Err("unsupported mov cr operands".to_string()),
        }
    }

    /// Encode SGDT/SIDT/LGDT/LIDT: 0F 01 /N (memory operand)
    fn encode_system_table(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 1 {
            return Err(format!("{} requires 1 operand", mnemonic));
        }
        let reg_ext = match mnemonic {
            "sgdt" => 0,
            "sidt" => 1,
            "lgdt" => 2,
            "lidt" => 3,
            _ => return Err(format!("unknown system table instruction: {}", mnemonic)),
        };
        match &ops[0] {
            Operand::Memory(mem) => {
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0x01]);
                self.encode_modrm_mem(reg_ext, mem)
            }
            _ => Err(format!("{} requires memory operand", mnemonic)),
        }
    }

    /// Encode LSL (Load Segment Limit): 0F 03 /r
    fn encode_lsl(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("lsl requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                // 16-bit operand size prefix
                if src.name.starts_with('w') || src.name.len() == 2 && src.name.ends_with('x') || src.name.ends_with('i') && !src.name.starts_with('e') && !src.name.starts_with('r') {
                    // Heuristic: check if it's 16-bit register
                    let is_16 = matches!(src.name.as_str(), "ax"|"bx"|"cx"|"dx"|"si"|"di"|"sp"|"bp");
                    if is_16 {
                        self.bytes.push(0x66);
                    }
                }
                self.emit_rex_rr(4, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, 0x03]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.emit_rex_rm(4, &dst.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0x03]);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported lsl operands".to_string()),
        }
    }

    // ---- SSE/SSE2 encoding helpers ----

    /// Encode SSE instruction: load (xmm<-xmm/mem) or store (xmm->mem).
    fn encode_sse_rr_rm(&mut self, ops: &[Operand], load_opcode: &[u8], store_opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE mov requires 2 operands".to_string());
        }

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) && is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                // Emit mandatory prefix bytes (e.g. 0x66, 0xF2, 0xF3) before REX.
                // The prefix ends where the 0x0F escape byte begins.
                let prefix_len = load_opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &load_opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rr(0, &dst.name, &src.name);
                // Emit remaining opcode bytes (0x0F + opcode byte)
                self.bytes.extend_from_slice(&load_opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = load_opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &load_opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rm(0, &dst.name, mem);
                self.bytes.extend_from_slice(&load_opcode[prefix_len..]);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let prefix_len = store_opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &store_opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rm(0, &src.name, mem);
                self.bytes.extend_from_slice(&store_opcode[prefix_len..]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported SSE mov operands".to_string()),
        }
    }

    /// Encode a standard SSE operation: op xmm/mem, xmm
    fn encode_sse_op(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE op requires 2 operands".to_string());
        }

        // Check if operands are MMX registers - if so, skip the 0x66 prefix
        let use_mmx = match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => is_mmx(&src.name) || is_mmx(&dst.name),
            (Operand::Memory(_), Operand::Register(dst)) => is_mmx(&dst.name),
            (Operand::Register(src), Operand::Memory(_)) => is_mmx(&src.name),
            _ => false,
        };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                if !use_mmx {
                    for &b in &opcode[..prefix_len] {
                        self.bytes.push(b);
                    }
                }
                if !use_mmx {
                    self.emit_rex_rr(0, &dst.name, &src.name);
                }
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                if !use_mmx {
                    for &b in &opcode[..prefix_len] {
                        self.bytes.push(b);
                    }
                }
                if !use_mmx {
                    self.emit_rex_rm(0, &dst.name, mem);
                }
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported SSE op operands".to_string()),
        }
    }

    fn encode_sse_store(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE store requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rm(0, &src.name, mem);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported SSE store operands".to_string()),
        }
    }

    fn encode_sse_op_imm8(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("SSE op+imm8 requires 3 operands".to_string());
        }
        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rr(0, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported SSE op+imm8 operands".to_string()),
        }
    }

    fn encode_movd(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movd requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&dst.name) => {
                // GP -> XMM: 66 0F 6E /r
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                self.emit_rex_rr(0, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, 0x6E]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) => {
                // XMM -> GP: 66 0F 7E /r
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                self.emit_rex_rr(0, &src.name, &dst.name);
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm(&dst.name) => {
                // mem -> XMM: 66 0F 6E /r
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                self.emit_rex_rm(0, &dst.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0x6E]);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                // XMM -> mem: 66 0F 7E /r
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                self.emit_rex_rm(0, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported movd operands".to_string()),
        }
    }

    fn encode_movnti(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movnti requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let size = if is_reg64(&src.name) { 8 } else { 4 };
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0xC3]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported movnti operands".to_string()),
        }
    }

    fn encode_movnti_q(&mut self, ops: &[Operand]) -> Result<(), String> {
        // movntiq: 0F C3 /r with REX.W (always 64-bit)
        if ops.len() != 2 {
            return Err("movntiq requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.emit_rex_rm(8, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0xC3]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported movntiq operands".to_string()),
        }
    }

    fn encode_rdrand(&mut self, ops: &[Operand], mnemonic: &str, opcode2: u8, reg_ext: u8) -> Result<(), String> {
        if ops.len() != 1 { return Err("rdrand requires 1 operand".to_string()); }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                let size = if mnemonic.ends_with('q') { 8 } else if mnemonic.ends_with('l') { 4 } else { infer_reg_size(&reg.name) };
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_unary(size, &reg.name);
                self.bytes.extend_from_slice(&[0x0F, opcode2]);
                self.bytes.push(self.modrm(3, reg_ext, num));
                Ok(())
            }
            _ => Err("rdrand requires register operand".to_string()),
        }
    }

    fn encode_sldt(&mut self, ops: &[Operand]) -> Result<(), String> {
        // sldt %reg: 0F 00 /0
        if ops.len() != 1 { return Err("sldt requires 1 operand".to_string()); }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                let size = infer_reg_size(&reg.name);
                if size == 2 { self.bytes.push(0x66); }
                if size == 8 {
                    self.bytes.push(self.rex(true, false, false, needs_rex_ext(&reg.name)));
                } else if needs_rex_ext(&reg.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.extend_from_slice(&[0x0F, 0x00]);
                self.bytes.push(self.modrm(3, 0, num));
                Ok(())
            }
            Operand::Memory(mem) => {
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0x00]);
                self.encode_modrm_mem(0, mem)
            }
            _ => Err("unsupported sldt operand".to_string()),
        }
    }

    fn encode_str_insn(&mut self, ops: &[Operand]) -> Result<(), String> {
        // str %reg: 0F 00 /1
        if ops.len() != 1 { return Err("str requires 1 operand".to_string()); }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                let size = infer_reg_size(&reg.name);
                if size == 2 { self.bytes.push(0x66); }
                if size == 8 {
                    self.bytes.push(self.rex(true, false, false, needs_rex_ext(&reg.name)));
                } else if needs_rex_ext(&reg.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.extend_from_slice(&[0x0F, 0x00]);
                self.bytes.push(self.modrm(3, 1, num));
                Ok(())
            }
            _ => Err("unsupported str operand".to_string()),
        }
    }

    fn encode_sse_cvt_gp_to_xmm(&mut self, ops: &[Operand], opcode: &[u8], gp_size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("cvt requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                // Need REX.W for 64-bit source
                self.emit_rex_rr(gp_size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            _ => Err("unsupported cvt operands".to_string()),
        }
    }

    fn encode_sse_cvt_xmm_to_gp(&mut self, ops: &[Operand], opcode: &[u8], gp_size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("cvt requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rr(gp_size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            _ => Err("unsupported cvt operands".to_string()),
        }
    }

    fn encode_sse_shift(&mut self, ops: &[Operand], reg_opcode: &[u8], imm_ext: u8, imm_opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE shift requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = imm_opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &imm_opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                if needs_rex_ext(&dst.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.extend_from_slice(&imm_opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, imm_ext, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) => {
                self.encode_sse_op(&[ops[0].clone(), ops[1].clone()], reg_opcode)
            }
            _ => Err("unsupported SSE shift operands".to_string()),
        }
    }

    fn encode_sse_shift_imm_only(&mut self, ops: &[Operand], imm_ext: u8, opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE shift requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                if needs_rex_ext(&dst.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, imm_ext, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported SSE shift operands".to_string()),
        }
    }

    fn encode_sse_insert(&mut self, ops: &[Operand], opcode: &[u8], rex_w: bool) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("pinsrX requires 3 operands".to_string());
        }
        // pinsrX $imm, r/m, xmm
        let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                let size = if rex_w { 8 } else { 0 };
                self.emit_rex_rr(size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                let size = if rex_w { 8 } else { 0 };
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported pinsrX operands".to_string()),
        }
    }

    fn encode_sse_extract(&mut self, ops: &[Operand], opcode: &[u8], rex_w: bool) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("pextrX requires 3 operands".to_string());
        }
        // pextrX $imm, xmm, r/m
        let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                let size = if rex_w { 8 } else { 0 };
                self.emit_rex_rr(size, &src.name, &dst.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                let size = if rex_w { 8 } else { 0 };
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.encode_modrm_mem(src_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported pextrX operands".to_string()),
        }
    }

    // ---- MOVQ with XMM registers ----

    /// Encode movq with XMM register operands.
    /// Forms:
    ///   movq %gp64, %xmm  -> 66 REX.W 0F 6E /r (GP -> XMM)
    ///   movq %xmm, %gp64  -> 66 REX.W 0F 7E /r (XMM -> GP)
    ///   movq mem, %xmm    -> F3 0F 7E /r (load 64-bit from memory)
    ///   movq %xmm, mem    -> 66 0F D6 /r (store 64-bit to memory)
    ///   movq %xmm, %xmm   -> F3 0F 7E /r (xmm-to-xmm)
    fn encode_movq_xmm(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movq requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            // movq %gp64, %xmm -> 66 REX.W 0F 6E /r
            (Operand::Register(src), Operand::Register(dst)) if !is_xmm(&src.name) && is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                // REX.W with proper R (dst xmm ext) and B (src gp ext)
                let r = needs_rex_ext(&dst.name);
                let b = needs_rex_ext(&src.name);
                self.bytes.push(self.rex(true, r, false, b));
                self.bytes.extend_from_slice(&[0x0F, 0x6E]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // movq %xmm, %gp64 -> 66 REX.W 0F 7E /r
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) && !is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                // REX.W with proper R (src xmm ext) and B (dst gp ext)
                let r = needs_rex_ext(&src.name);
                let b = needs_rex_ext(&dst.name);
                self.bytes.push(self.rex(true, r, false, b));
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            // movq %xmm, %xmm -> F3 0F 7E /r
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) && is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0xF3);
                self.emit_rex_rr(0, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // movq mem, %xmm -> F3 0F 7E /r (load)
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0xF3);
                self.emit_rex_rm(0, &dst.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.encode_modrm_mem(dst_num, mem)
            }
            // movq %xmm, mem -> 66 0F D6 /r (store)
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                self.emit_rex_rm(0, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0xD6]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported movq operand combination".to_string()),
        }
    }

    // ---- CRC32 encoding ----

    /// Encode CRC32 instruction.
    /// crc32b: F2 0F 38 F0 /r (8-bit source)
    /// crc32w: 66 F2 0F 38 F1 /r (16-bit source)
    /// crc32l: F2 0F 38 F1 /r (32-bit source)
    /// crc32q: F2 REX.W 0F 38 F1 /r (64-bit source)
    fn encode_crc32(&mut self, ops: &[Operand], src_size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("crc32 requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;

                if src_size == 2 {
                    self.bytes.push(0x66); // operand size prefix for 16-bit
                }
                self.bytes.push(0xF2); // mandatory prefix

                // REX prefix
                let w = src_size == 8;
                let r = needs_rex_ext(&dst.name);
                let b = needs_rex_ext(&src.name);
                let need_rex = w || r || b || (src_size == 1 && is_rex_required_8bit(&src.name));
                if need_rex {
                    self.bytes.push(self.rex(w, r, false, b));
                }

                self.bytes.extend_from_slice(&[0x0F, 0x38]);
                if src_size == 1 {
                    self.bytes.push(0xF0);
                } else {
                    self.bytes.push(0xF1);
                }
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;

                if src_size == 2 {
                    self.bytes.push(0x66);
                }
                self.bytes.push(0xF2);

                let w = src_size == 8;
                let r = needs_rex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_rex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_rex_ext(&i.name));
                let need_rex = w || r || b_ext || x;
                if need_rex {
                    self.bytes.push(self.rex(w, r, x, b_ext));
                }

                self.bytes.extend_from_slice(&[0x0F, 0x38]);
                if src_size == 1 {
                    self.bytes.push(0xF0);
                } else {
                    self.bytes.push(0xF1);
                }
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported crc32 operands".to_string()),
        }
    }

    // ---- SSE memory-only encoding (ldmxcsr, stmxcsr) ----

    /// Encode an SSE instruction with a single memory operand and a ModR/M extension.
    /// Format: [opcode bytes] ModR/M(ext, mem)
    /// Used for ldmxcsr (0F AE /2) and stmxcsr (0F AE /3).
    fn encode_sse_mem_only(&mut self, ops: &[Operand], opcode: &[u8], ext: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("SSE mem-only op requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.emit_segment_prefix(mem)?;
                // No REX.W needed for 32-bit memory operations
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(ext, mem)
            }
            _ => Err("SSE mem-only op requires memory operand".to_string()),
        }
    }

    // ---- BSF/BSR encoding ----

    /// Encode BSF (Bit Scan Forward) or BSR (Bit Scan Reverse).
    /// bsfl/bsfq: 0F BC r/m -> reg
    /// bsrl/bsrq: 0F BD r/m -> reg
    fn encode_bsf_bsr(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);
        let opcode_byte = if mnemonic.starts_with("bsf") { 0xBC } else { 0xBD };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.emit_rex_rr(size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, opcode_byte]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.emit_segment_prefix(mem)?;
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.extend_from_slice(&[0x0F, opcode_byte]);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    // ---- BT/BTS/BTR/BTC encoding ----

    /// Encode bit test instructions (bt, bts, btr, btc).
    fn encode_bt(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);
        // Determine which bt variant
        let base = &mnemonic[..mnemonic.len()-1]; // strip size suffix
        let (reg_opcode, imm_ext) = match base {
            "bt"  => (0xA3u8, 4u8),
            "bts" => (0xAB, 5),
            "btr" => (0xB3, 6),
            "btc" => (0xBB, 7),
            _ => return Err(format!("unknown bt variant: {}", mnemonic)),
        };

        match (&ops[0], &ops[1]) {
            // bt $imm, reg/mem
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.emit_rex_unary(size, &dst.name);
                self.bytes.extend_from_slice(&[0x0F, 0xBA]);
                self.bytes.push(self.modrm(3, imm_ext, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem)) => {
                self.emit_segment_prefix(mem)?;
                self.emit_rex_rm(size, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0xBA]);
                self.encode_modrm_mem(imm_ext, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            // bt %reg, reg/mem
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.emit_rex_rr(size, &src.name, &dst.name);
                self.bytes.extend_from_slice(&[0x0F, reg_opcode]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                self.emit_segment_prefix(mem)?;
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, reg_opcode]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    // ---- x87 FPU encoding ----

    fn encode_x87_mem(&mut self, ops: &[Operand], opcode: &[u8], ext: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("x87 mem op requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                // x87 instructions don't use REX.W
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(ext, mem)
            }
            _ => Err("x87 mem op requires memory operand".to_string()),
        }
    }

    fn encode_fcomip(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 2 {
            // fcomip %st(N), %st
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDF, 0xF0 + n]);
                    Ok(())
                }
                _ => Err("fcomip requires st register".to_string()),
            }
        } else if ops.is_empty() {
            self.bytes.extend_from_slice(&[0xDF, 0xF1]);
            Ok(())
        } else {
            Err("fcomip requires 0 or 2 operands".to_string())
        }
    }

    fn encode_fucomi(&mut self, ops: &[Operand]) -> Result<(), String> {
        // fucomi %st(i), %st -> DB E8+i (0 or 2 operands; 1-operand also accepted)
        if ops.len() == 2 || ops.len() == 1 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDB, 0xE8 + n]);
                    Ok(())
                }
                _ => Err("fucomi requires st register".to_string()),
            }
        } else if ops.is_empty() {
            self.bytes.extend_from_slice(&[0xDB, 0xE9]); // default st(1)
            Ok(())
        } else {
            Err("fucomi requires 0-2 operands".to_string())
        }
    }

    fn encode_fucomip(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 2 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDF, 0xE8 + n]);
                    Ok(())
                }
                _ => Err("fucomip requires st register".to_string()),
            }
        } else if ops.is_empty() {
            self.bytes.extend_from_slice(&[0xDF, 0xE9]);
            Ok(())
        } else {
            Err("fucomip requires 0 or 2 operands".to_string())
        }
    }

    fn encode_fld_st(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("fld requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let n = parse_st_num(&reg.name)?;
                self.bytes.extend_from_slice(&[0xD9, 0xC0 + n]);
                Ok(())
            }
            _ => Err("fld requires st register".to_string()),
        }
    }

    fn encode_fstp_st(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("fstp requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let n = parse_st_num(&reg.name)?;
                self.bytes.extend_from_slice(&[0xDD, 0xD8 + n]);
                Ok(())
            }
            _ => Err("fstp requires st register".to_string()),
        }
    }

    // ---- Bit scan (BSF/BSR) ----

    fn encode_bit_scan(&mut self, ops: &[Operand], mnemonic: &str, opcode2: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rr(size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, opcode2]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.extend_from_slice(&[0x0F, opcode2]);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }


    // ---- Segment register moves ----

    fn encode_mov_seg(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("mov seg requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            // mov %seg, %reg
            (Operand::Register(src), Operand::Register(dst)) if is_segment_reg(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad seg register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                // 8C /r - MOV r/m16, Sreg
                if is_reg64(&dst.name) {
                    self.emit_rex_unary(8, &dst.name);
                } else if needs_rex_ext(&dst.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.push(0x8C);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            // mov %reg, %seg
            (Operand::Register(src), Operand::Register(dst)) if is_segment_reg(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad seg register")?;
                // 8E /r - MOV Sreg, r/m16
                if needs_rex_ext(&src.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.push(0x8E);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // mov %seg, mem
            (Operand::Register(src), Operand::Memory(mem)) if is_segment_reg(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad seg register")?;
                // 8C /r - MOV r/m16, Sreg (store segment to memory)
                self.emit_rex_rm(0, "", mem);
                self.bytes.push(0x8C);
                self.encode_modrm_mem(src_num, mem)
            }
            // mov mem, %seg
            (Operand::Memory(mem), Operand::Register(dst)) if is_segment_reg(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad seg register")?;
                // 8E /r - MOV Sreg, r/m16 (load segment from memory)
                self.emit_rex_rm(0, "", mem);
                self.bytes.push(0x8E);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported mov seg operands".to_string()),
        }
    }

    // ---- MMX movq ----

    fn encode_mmx_movq(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("mmx movq requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            // movq %mm, %mm
            (Operand::Register(src), Operand::Register(dst)) if is_mmx(&src.name) && is_mmx(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0x6F]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // movq mem, %mm
            (Operand::Memory(mem), Operand::Register(dst)) if is_mmx(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0x6F]);
                self.encode_modrm_mem(dst_num, mem)
            }
            // movq %mm, mem
            (Operand::Register(src), Operand::Memory(mem)) if is_mmx(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0x7F]);
                self.encode_modrm_mem(src_num, mem)
            }
            // movq %gp64, %mm -> 0F 6E (with REX.W)
            (Operand::Register(src), Operand::Register(dst)) if !is_mmx(&src.name) && is_mmx(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let b = needs_rex_ext(&src.name);
                self.bytes.push(self.rex(true, false, false, b));
                self.bytes.extend_from_slice(&[0x0F, 0x6E]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // movq %mm, %gp64 -> 0F 7E (with REX.W)
            (Operand::Register(src), Operand::Register(dst)) if is_mmx(&src.name) && !is_mmx(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let b = needs_rex_ext(&dst.name);
                self.bytes.push(self.rex(true, false, false, b));
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            _ => Err("unsupported mmx movq operands".to_string()),
        }
    }

    // ---- Suffix-less instruction helpers ----

    fn encode_suffixless_mov(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("mov requires 2 operands".to_string());
        }
        // Check for segment registers
        if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_segment_reg(&r.name))) {
            return self.encode_mov_seg(ops);
        }
        let size = infer_operand_size_from_pair(&ops[0], &ops[1]);
        self.encode_mov(ops, size)
    }

    fn encode_suffixless_alu(&mut self, ops: &[Operand], alu_op: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("ALU op requires 2 operands".to_string());
        }
        let size = infer_operand_size_from_pair(&ops[0], &ops[1]);
        let suffix = match size { 1 => "b", 2 => "w", 4 => "l", _ => "q" };
        let op_name = match alu_op { 0 => "add", 1 => "or", 2 => "adc", 3 => "sbb", 4 => "and", 5 => "sub", 6 => "xor", 7 => "cmp", _ => "?" };
        let mnemonic = format!("{}{}", op_name, suffix);
        self.encode_alu(ops, &mnemonic, alu_op)
    }

    fn encode_suffixless_test(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("test requires 2 operands".to_string());
        }
        let size = infer_operand_size_from_pair(&ops[0], &ops[1]);
        let suffix = match size { 1 => "b", 2 => "w", 4 => "l", _ => "q" };
        let mnemonic = format!("test{}", suffix);
        self.encode_test(ops, &mnemonic)
    }

    fn encode_suffixless_shift(&mut self, ops: &[Operand], shift_op: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("shift requires 2 operands".to_string());
        }
        // Size comes from dst (second) operand
        let size = match &ops[1] {
            Operand::Register(r) => infer_reg_size(&r.name),
            _ => 8,
        };
        let op_name = match shift_op { 4 => "shl", 5 => "shr", 7 => "sar", 0 => "rol", 1 => "ror", 2 => "rcl", 3 => "rcr", _ => "?" };
        let suffix = match size { 1 => "b", 2 => "w", 4 => "l", _ => "q" };
        let mnemonic = format!("{}{}", op_name, suffix);
        self.encode_shift(ops, &mnemonic, shift_op)
    }

    fn encode_suffixless_unary(&mut self, ops: &[Operand], op_ext: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("unary op requires 1 operand".to_string());
        }
        let size = match &ops[0] {
            Operand::Register(r) => infer_reg_size(&r.name),
            _ => 8,
        };
        if size == 2 { self.bytes.push(0x66); }
        self.encode_unary_rm(ops, op_ext, size)
    }

    // ---- VEX encoding helpers for AVX ----

    /// Emit a 2-byte or 3-byte VEX prefix.
    /// pp: 0=none, 1=66, 2=F3, 3=F2
    /// mm: 1=0F, 2=0F38, 3=0F3A
    /// w: 0 or 1 (VEX.W)
    /// vvvv: complement of source register number (15 - reg_num, or 15 if none)
    /// l: 0=128, 1=256
    /// r, x, b: VEX extension bits (inverted from REX)
    fn emit_vex(&mut self, r: bool, x: bool, b: bool, mm: u8, w: u8, vvvv: u8, l: u8, pp: u8) {
        let r_bit = if r { 0 } else { 1 };
        let x_bit = if x { 0 } else { 1 };
        let b_bit = if b { 0 } else { 1 };
        let vvvv_inv = (!vvvv) & 0xF;

        // Use 2-byte VEX if possible: mm=1, w=0, x=0, b=0
        if mm == 1 && w == 0 && !x && !b {
            self.bytes.push(0xC5);
            let byte2 = (r_bit << 7) | (vvvv_inv << 3) | (l << 2) | pp;
            self.bytes.push(byte2);
        } else {
            // 3-byte VEX
            self.bytes.push(0xC4);
            let byte1 = (r_bit << 7) | (x_bit << 6) | (b_bit << 5) | mm;
            let byte2 = (w << 7) | (vvvv_inv << 3) | (l << 2) | pp;
            self.bytes.push(byte1);
            self.bytes.push(byte2);
        }
    }

    /// Emit EVEX 4-byte prefix.
    /// Parameters match VEX but with additional EVEX-specific fields.
    /// ll: 00=128, 01=256, 10=512
    /// TODO: z (merge-masking) and aaa (opmask register k1-k7) are not yet used.
    /// TODO: r_prime and v_prime are passed as false; zmm16-zmm31 won't encode correctly.
    fn emit_evex(&mut self, r: bool, x: bool, b: bool, r_prime: bool, mm: u8, w: u8, vvvv: u8, v_prime: bool, pp: u8, ll: u8, _z: bool, _aaa: u8) {
        let r_bit = if r { 0u8 } else { 1 };
        let x_bit = if x { 0u8 } else { 1 };
        let b_bit = if b { 0u8 } else { 1 };
        let r_prime_bit = if r_prime { 0u8 } else { 1 };
        let vvvv_inv = (!vvvv) & 0xF;
        let v_prime_bit = if v_prime { 0u8 } else { 1 };

        self.bytes.push(0x62); // EVEX prefix indicator

        // Byte 1: R X B R' 0 0 mm
        let byte1 = (r_bit << 7) | (x_bit << 6) | (b_bit << 5) | (r_prime_bit << 4) | mm;
        self.bytes.push(byte1);

        // Byte 2: W vvvv 1 pp
        let byte2 = (w << 7) | (vvvv_inv << 3) | (1 << 2) | pp;
        self.bytes.push(byte2);

        // Byte 3: z L'L b V' aaa
        let byte3 = (ll << 5) | (v_prime_bit << 3);
        self.bytes.push(byte3);
    }

    /// Determine EVEX L'L from operands: 00=128(xmm), 01=256(ymm), 10=512(zmm)
    fn evex_ll_from_ops(&self, ops: &[Operand]) -> u8 {
        for op in ops {
            if let Operand::Register(r) = op {
                let name = r.name.to_lowercase();
                if name.starts_with("zmm") { return 0b10; }
                if name.starts_with("ymm") { return 0b01; }
            }
        }
        0b00 // default to 128-bit
    }

    /// Encode EVEX 3-operand instruction (e.g., vpxord, vpandd, etc.)
    /// Operands in AT&T order: src, vvvv, dst
    fn encode_evex_3op(&mut self, ops: &[Operand], opcode: u8, pp: u8, w: u8) -> Result<(), String> {
        if ops.len() != 3 { return Err("EVEX 3-op requires 3 operands".to_string()); }
        let ll = self.evex_ll_from_ops(ops);

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                // mm=1 (0F map)
                self.emit_evex(r, false, b, false, 1, w, vvvv_enc, false, pp, ll, false, 0);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(vvvv), Operand::Register(dst)) => {
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_evex(r, x, b_ext, false, 1, w, vvvv_enc, false, pp, ll, false, 0);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported EVEX 3-op operands".to_string()),
        }
    }

    /// Determine VEX L (vector length) from operand: 0=128(xmm), 1=256(ymm)
    fn vex_l_from_ops(&self, ops: &[Operand]) -> u8 {
        for op in ops {
            match op {
                Operand::Register(r) if is_ymm(&r.name) => return 1,
                _ => {}
            }
        }
        0
    }

    /// Encode AVX vmovdqa/vmovdqu (load/store with 66/F3 prefix)
    fn encode_avx_mov(&mut self, ops: &[Operand], load_op: u8, store_op: u8, is_66: bool) -> Result<(), String> {
        if ops.len() != 2 { return Err("AVX mov requires 2 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if is_66 { 1 } else { 2 }; // 66 -> pp=1, F3 -> pp=2

        match (&ops[0], &ops[1]) {
            // load: mem/reg -> xmm/ymm
            (Operand::Register(src), Operand::Register(dst)) if is_xmm_or_ymm(&src.name) && is_xmm_or_ymm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 1, 0, 0, l, pp);
                self.bytes.push(load_op);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm_or_ymm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 1, 0, 0, l, pp);
                self.bytes.push(load_op);
                self.encode_modrm_mem(dst_num, mem)
            }
            // store: xmm/ymm -> mem
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm_or_ymm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let r = needs_vex_ext(&src.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 1, 0, 0, l, pp);
                self.bytes.push(store_op);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported AVX mov operands".to_string()),
        }
    }

    /// Encode AVX vmovaps/vmovapd/vmovups/vmovupd (no mandatory prefix, or 66 prefix)
    fn encode_avx_mov_np(&mut self, ops: &[Operand], load_op: u8, store_op: u8, is_66: bool) -> Result<(), String> {
        if ops.len() != 2 { return Err("AVX mov requires 2 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if is_66 { 1 } else { 0 };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm_or_ymm(&src.name) && is_xmm_or_ymm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 1, 0, 0, l, pp);
                self.bytes.push(load_op);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm_or_ymm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 1, 0, 0, l, pp);
                self.bytes.push(load_op);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm_or_ymm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let r = needs_vex_ext(&src.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 1, 0, 0, l, pp);
                self.bytes.push(store_op);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported AVX mov operands".to_string()),
        }
    }

    /// Encode AVX 3-operand instruction with 66 prefix (or no prefix): op src, vvvv, dst
    /// Format: VEX.NDS.128/256.66.0F opcode /r
    fn encode_avx_3op(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 3 { return Err("AVX 3-op requires 3 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2]) {
            // src_reg, vvvv_reg, dst_reg
            (Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 1, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // mem, vvvv_reg, dst_reg
            (Operand::Memory(mem), Operand::Register(vvvv), Operand::Register(dst)) => {
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, x, b_ext, 1, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported AVX 3-op operands".to_string()),
        }
    }

    /// Encode AVX 3-operand with no 66 prefix
    fn encode_avx_3op_np(&mut self, ops: &[Operand], opcode: u8) -> Result<(), String> {
        self.encode_avx_3op(ops, opcode, false)
    }

    /// Encode AVX 3-operand in 0F38 map
    fn encode_avx_3op_38(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 3 { return Err("AVX 3-op requires 3 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 2, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(vvvv), Operand::Register(dst)) => {
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, x, b_ext, 2, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported AVX 3-op operands".to_string()),
        }
    }

    /// Encode AVX 3-operand in 0F map (mm=1) with imm8 (vshufps, vshufpd, etc.)
    fn encode_avx_3op_0f_imm8(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 4 { return Err("AVX 3-op+imm8 requires 4 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2], &ops[3]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 1, 0, vvvv_enc, l, pp); // mm=1 (0F map)
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(vvvv), Operand::Register(dst)) => {
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, x, b_ext, 1, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX 3-op+imm8 operands".to_string()),
        }
    }

    /// Encode AVX 2-operand in 0F38 map (e.g., vpabsb src, dst with vvvv=0)
    fn encode_avx_2op_38(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 2 { return Err("AVX 2-op requires 2 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm_or_ymm(&src.name) && is_xmm_or_ymm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 2, 0, 0, l, pp);  // vvvv=0 for 2-operand
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm_or_ymm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 2, 0, 0, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported AVX 2-op operands".to_string()),
        }
    }

    /// Encode AVX 2-operand in 0F map (e.g., vmovddup, vmovshdup, vmovsldup)
    /// pp: 0=NP, 1=66, 2=F3, 3=F2
    fn encode_avx_2op_0f(&mut self, ops: &[Operand], opcode: u8, pp: u8) -> Result<(), String> {
        if ops.len() != 2 { return Err("AVX 2-op requires 2 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm_or_ymm(&src.name) && is_xmm_or_ymm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 1, 0, 0, l, pp);  // mm=1 (0F), vvvv=0
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm_or_ymm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 1, 0, 0, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported AVX 2-op operands".to_string()),
        }
    }

    /// Encode AVX scalar comparison (vcmpss/vcmpsd) with F3/F2 prefix
    /// pp: 2=F3 (vcmpss), 3=F2 (vcmpsd)
    fn encode_avx_cmp_scalar(&mut self, ops: &[Operand], opcode: u8, pp: u8) -> Result<(), String> {
        if ops.len() != 4 { return Err("AVX scalar cmp requires 4 operands (imm8, src, vvvv, dst)".to_string()); }
        let l = 0; // LIG, use 128-bit

        match (&ops[0], &ops[1], &ops[2], &ops[3]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 1, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(vvvv), Operand::Register(dst)) => {
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, x, b_ext, 1, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX scalar cmp operands".to_string()),
        }
    }

    /// Encode AVX scalar 3-operand instruction (e.g. vmulss, vaddss)
    /// pp: 2=F3 (single), 3=F2 (double)
    fn encode_avx_scalar_3op(&mut self, ops: &[Operand], opcode: u8, pp: u8) -> Result<(), String> {
        if ops.len() != 3 { return Err("AVX scalar 3-op requires 3 operands".to_string()); }
        let l = 0; // LIG - always 128-bit for scalar

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 1, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(vvvv), Operand::Register(dst)) => {
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, x, b_ext, 1, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported AVX scalar 3-op operands".to_string()),
        }
    }

    /// Encode AVX scalar move (vmovss/vmovsd) - handles both 2-op (load/store) and 3-op (merge) forms
    /// pp: 2=F3 (vmovss), 3=F2 (vmovsd)
    fn encode_avx_scalar_mov(&mut self, ops: &[Operand], load_op: u8, store_op: u8, pp: u8) -> Result<(), String> {
        match ops.len() {
            2 => {
                // 2-operand load/store form (no vvvv merge)
                match (&ops[0], &ops[1]) {
                    (Operand::Memory(mem), Operand::Register(dst)) if is_xmm_or_ymm(&dst.name) => {
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        let r = needs_vex_ext(&dst.name);
                        let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                        let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                        self.emit_vex(r, x, b_ext, 1, 0, 0, 0, pp); // vvvv=0, L=0
                        self.bytes.push(load_op);
                        self.encode_modrm_mem(dst_num, mem)
                    }
                    (Operand::Register(src), Operand::Memory(mem)) if is_xmm_or_ymm(&src.name) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let r = needs_vex_ext(&src.name);
                        let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                        let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                        self.emit_vex(r, x, b_ext, 1, 0, 0, 0, pp); // vvvv=0, L=0
                        self.bytes.push(store_op);
                        self.encode_modrm_mem(src_num, mem)
                    }
                    (Operand::Register(src), Operand::Register(dst)) if is_xmm_or_ymm(&src.name) && is_xmm_or_ymm(&dst.name) => {
                        // reg-reg: use load form
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        let r = needs_vex_ext(&dst.name);
                        let b = needs_vex_ext(&src.name);
                        self.emit_vex(r, false, b, 1, 0, 0, 0, pp);
                        self.bytes.push(load_op);
                        self.bytes.push(self.modrm(3, dst_num, src_num));
                        Ok(())
                    }
                    _ => Err("unsupported AVX scalar mov 2-op operands".to_string()),
                }
            }
            3 => {
                // 3-operand merge form (VEX.NDS)
                self.encode_avx_scalar_3op(ops, load_op, pp)
            }
            _ => Err("AVX scalar mov requires 2 or 3 operands".to_string()),
        }
    }

    /// Encode AVX shuffle in 0F3A map (e.g. vpermilps, vpermilpd with immediate)
    /// Format: VEX.128/256.66.0F3A opcode /r ib
    fn encode_avx_shuffle_3a(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 3 { return Err("AVX shuffle 3A requires 3 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 3, 0, 0, l, pp); // mm=3 (0F3A), vvvv=0
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 3, 0, 0, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX shuffle 3A operands".to_string()),
        }
    }

    /// Parse SSE comparison predicate from pseudo-op mnemonic.
    /// Returns (predicate, suffix) e.g. "cmpnleps" -> Some((6, "ps"))
    fn parse_sse_cmp_pseudo(mnemonic: &str) -> Option<(u8, &str)> {
        if !mnemonic.starts_with("cmp") { return None; }
        let rest = &mnemonic[3..];
        // Try to match a suffix (ps, pd, ss, sd)
        let suffixes = ["ps", "pd", "ss", "sd"];
        for suffix in &suffixes {
            if let Some(pred_str) = rest.strip_suffix(*suffix) {
                let pred = match pred_str {
                    "eq" => 0,
                    "lt" => 1,
                    "le" => 2,
                    "unord" => 3,
                    "neq" => 4,
                    "nlt" => 5,
                    "nle" => 6,
                    "ord" => 7,
                    _ => continue,
                };
                return Some((pred, suffix));
            }
        }
        None
    }

    /// Try to encode an SSE comparison pseudo-op (e.g. cmpnleps -> cmpps $6, src, dst)
    fn try_encode_sse_cmp_pseudo(&mut self, ops: &[Operand], mnemonic: &str) -> Result<Option<()>, String> {
        let (pred, suffix) = match Self::parse_sse_cmp_pseudo(mnemonic) {
            Some(v) => v,
            None => return Ok(None),
        };
        let opcode: &[u8] = match suffix {
            "ps" => &[0x0F, 0xC2],
            "pd" => &[0x66, 0x0F, 0xC2],
            "ss" => &[0xF3, 0x0F, 0xC2],
            "sd" => &[0xF2, 0x0F, 0xC2],
            _ => return Ok(None),
        };
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }
        // Encode as the base instruction with an implicit immediate predicate
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rr(0, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(pred);
                Ok(Some(()))
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rm(0, &dst.name, mem);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(pred);
                Ok(Some(()))
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    /// Parse AVX comparison predicate from pseudo-op mnemonic.
    /// Returns (predicate, suffix) e.g. "vcmpnleps" -> Some((6, "ps"))
    fn parse_avx_cmp_pseudo(mnemonic: &str) -> Option<(u8, &str)> {
        if !mnemonic.starts_with("vcmp") { return None; }
        let rest = &mnemonic[4..];
        let suffixes = ["ps", "pd", "ss", "sd"];
        for suffix in &suffixes {
            if let Some(pred_str) = rest.strip_suffix(*suffix) {
                let pred = match pred_str {
                    "eq" => 0,
                    "lt" => 1,
                    "le" => 2,
                    "unord" => 3,
                    "neq" => 4,
                    "nlt" => 5,
                    "nle" => 6,
                    "ord" => 7,
                    // AVX extended predicates (8-31)
                    "eq_uq" => 8,
                    "nge" => 9,
                    "ngt" => 10,
                    "false" => 11,
                    "neq_oq" => 12,
                    "ge" => 13,
                    "gt" => 14,
                    "true" => 15,
                    "eq_os" => 16,
                    "lt_oq" => 17,
                    "le_oq" => 18,
                    "unord_s" => 19,
                    "neq_us" => 20,
                    "nlt_uq" => 21,
                    "nle_uq" => 22,
                    "ord_s" => 23,
                    "eq_us" => 24,
                    "nge_uq" => 25,
                    "ngt_uq" => 26,
                    "false_os" => 27,
                    "neq_os" => 28,
                    "ge_oq" => 29,
                    "gt_oq" => 30,
                    "true_us" => 31,
                    _ => continue,
                };
                return Some((pred, suffix));
            }
        }
        None
    }

    /// Try to encode an AVX comparison pseudo-op (e.g. vcmpnleps -> vcmpps $6, src, vvvv, dst)
    fn try_encode_avx_cmp_pseudo(&mut self, ops: &[Operand], mnemonic: &str) -> Result<Option<()>, String> {
        let (pred, suffix) = match Self::parse_avx_cmp_pseudo(mnemonic) {
            Some(v) => v,
            None => return Ok(None),
        };
        // AVX pseudo-ops take 3 operands: src, vvvv, dst (no explicit immediate)
        if ops.len() != 3 {
            return Err(format!("{} requires 3 operands", mnemonic));
        }
        let (has_66, pp_scalar) = match suffix {
            "ps" => (false, None),
            "pd" => (true, None),
            "ss" => (false, Some(2u8)), // F3
            "sd" => (false, Some(3u8)), // F2
            _ => return Ok(None),
        };

        let l = self.vex_l_from_ops(ops);
        let pp = match pp_scalar {
            Some(p) => p,
            None => if has_66 { 1 } else { 0 },
        };

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 1, 0, vvvv_enc, l, pp);
                self.bytes.push(0xC2);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(pred);
                Ok(Some(()))
            }
            (Operand::Memory(mem), Operand::Register(vvvv), Operand::Register(dst)) => {
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, x, b_ext, 1, 0, vvvv_enc, l, pp);
                self.bytes.push(0xC2);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(pred);
                Ok(Some(()))
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    /// Encode AVX 3-operand in 0F3A map with imm8
    fn encode_avx_3op_3a_imm8(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 4 { return Err("AVX 3-op+imm8 requires 4 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2], &ops[3]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 3, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(vvvv), Operand::Register(dst)) => {
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, x, b_ext, 3, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX 3-op+imm8 operands".to_string()),
        }
    }

    /// Encode AVX vbroadcastss/vbroadcastsd
    fn encode_avx_broadcast(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 { return Err("vbroadcast requires 2 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);

        match (&ops[0], &ops[1]) {
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm_or_ymm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                // VEX.256.66.0F38 opcode /r
                self.emit_vex(r, x, b_ext, 2, 0, 0, l, 1);
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Register(dst)) if is_xmm_or_ymm(&src.name) && is_xmm_or_ymm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 2, 0, 0, l, 1);
                self.bytes.extend_from_slice(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            _ => Err("unsupported vbroadcast operands".to_string()),
        }
    }

    /// Encode AVX pshufd-like (imm8 + 2 register operands)
    fn encode_avx_shuffle(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 3 { return Err("AVX shuffle requires 3 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 1, 0, 0, l, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 1, 0, 0, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX shuffle operands".to_string()),
        }
    }

    /// Encode AVX vpmovmskb-like (xmm->gp)
    fn encode_avx_extract_gp(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 2 { return Err("AVX extract requires 2 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm_or_ymm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 1, 0, 0, l, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            _ => Err("unsupported AVX extract operands".to_string()),
        }
    }

    /// Encode AVX vmovd
    fn encode_avx_movd(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 { return Err("vmovd requires 2 operands".to_string()); }
        match (&ops[0], &ops[1]) {
            // GP -> XMM: VEX.128.66.0F 6E /r
            (Operand::Register(src), Operand::Register(dst)) if !is_xmm_or_ymm(&src.name) && is_xmm_or_ymm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 1, 0, 0, 0, 1);
                self.bytes.push(0x6E);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // XMM -> GP: VEX.128.66.0F 7E /r
            (Operand::Register(src), Operand::Register(dst)) if is_xmm_or_ymm(&src.name) && !is_xmm_or_ymm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&src.name);
                let b = needs_vex_ext(&dst.name);
                self.emit_vex(r, false, b, 1, 0, 0, 0, 1);
                self.bytes.push(0x7E);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            // mem -> XMM: VEX.128.66.0F 6E /r
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm_or_ymm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 1, 0, 0, 0, 1);
                self.bytes.push(0x6E);
                self.encode_modrm_mem(dst_num, mem)
            }
            // XMM -> mem: VEX.128.66.0F 7E /r
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm_or_ymm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let r = needs_vex_ext(&src.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 1, 0, 0, 0, 1);
                self.bytes.push(0x7E);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported vmovd operands".to_string()),
        }
    }

    /// Encode AVX vmovq
    fn encode_avx_movq(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 { return Err("vmovq requires 2 operands".to_string()); }
        match (&ops[0], &ops[1]) {
            // GP64 -> XMM: VEX.128.66.0F.W1 6E /r
            (Operand::Register(src), Operand::Register(dst)) if !is_xmm_or_ymm(&src.name) && is_xmm_or_ymm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 1, 1, 0, 0, 1);
                self.bytes.push(0x6E);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // XMM -> GP64: VEX.128.66.0F.W1 7E /r
            (Operand::Register(src), Operand::Register(dst)) if is_xmm_or_ymm(&src.name) && !is_xmm_or_ymm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&src.name);
                let b = needs_vex_ext(&dst.name);
                self.emit_vex(r, false, b, 1, 1, 0, 0, 1);
                self.bytes.push(0x7E);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            // XMM -> XMM: VEX.128.F3.0F 7E /r
            (Operand::Register(src), Operand::Register(dst)) if is_xmm_or_ymm(&src.name) && is_xmm_or_ymm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 1, 0, 0, 0, 2);
                self.bytes.push(0x7E);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // mem -> XMM: VEX.128.F3.0F 7E /r
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm_or_ymm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 1, 0, 0, 0, 2);
                self.bytes.push(0x7E);
                self.encode_modrm_mem(dst_num, mem)
            }
            // XMM -> mem: VEX.128.66.0F D6 /r
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm_or_ymm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let r = needs_vex_ext(&src.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 1, 0, 0, 0, 1);
                self.bytes.push(0xD6);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported vmovq operands".to_string()),
        }
    }

    /// Encode AVX shift instructions (imm8 form or xmm form)
    fn encode_avx_shift(&mut self, ops: &[Operand], reg_op: u8, imm_ext: u8, imm_op: u8, has_66: bool) -> Result<(), String> {
        let pp = if has_66 { 1 } else { 0 };
        if ops.len() == 3 {
            match (&ops[0], &ops[1], &ops[2]) {
                // $imm, %xmm_src, %xmm_dst  (immediate shift, dst = vvvv)
                (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                    let src_num = reg_num(&src.name).ok_or("bad register")?;
                    let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                    let l = if is_ymm(&src.name) || is_ymm(&dst.name) { 1 } else { 0 };
                    let b = needs_vex_ext(&src.name);
                    let vvvv_enc = dst_num | (if needs_vex_ext(&dst.name) { 8 } else { 0 });
                    self.emit_vex(false, false, b, 1, 0, vvvv_enc, l, pp);
                    self.bytes.push(imm_op);
                    self.bytes.push(self.modrm(3, imm_ext, src_num));
                    self.bytes.push(*imm as u8);
                    Ok(())
                }
                // %xmm_count, %xmm_src(vvvv), %xmm_dst
                (Operand::Register(count), Operand::Register(vvvv), Operand::Register(dst)) if is_xmm_or_ymm(&count.name) => {
                    let count_num = reg_num(&count.name).ok_or("bad register")?;
                    let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                    let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                    let l = if is_ymm(&vvvv.name) || is_ymm(&dst.name) { 1 } else { 0 };
                    let r = needs_vex_ext(&dst.name);
                    let b = needs_vex_ext(&count.name);
                    let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                    self.emit_vex(r, false, b, 1, 0, vvvv_enc, l, pp);
                    self.bytes.push(reg_op);
                    self.bytes.push(self.modrm(3, dst_num, count_num));
                    Ok(())
                }
                _ => Err("unsupported AVX shift operands".to_string()),
            }
        } else {
            Err("AVX shift requires 3 operands".to_string())
        }
    }

    /// Encode x87 register-register arithmetic (fadd, fmul, fsub, fdiv).
    ///
    /// In AT&T syntax:
    ///   fadd %st(i), %st    -> D8 (base_st0 + i)  -- st(0) = st(0) op st(i)
    ///   fadd %st, %st(i)    -> DC (base_sti + i)  -- st(i) = st(i) op st(0)
    ///   fadd %st(i)         -> D8 (base_st0 + i)  -- shorthand for fadd %st(i), %st
    ///
    /// `opcode_st0` = D8 (reg field in modrm for st(0) as dest)
    /// `opcode_sti` = DC (reg field in modrm for st(i) as dest)
    /// `base_modrm` = base for the modrm second byte (e.g., 0xC0 for fadd)
    fn encode_x87_arith_reg(&mut self, ops: &[Operand], opcode_st0: u8, opcode_sti: u8, base_modrm: u8) -> Result<(), String> {
        match ops.len() {
            0 => {
                // Default: fadd %st(1), %st (i.e., st(0) = st(0) op st(1))
                self.bytes.extend_from_slice(&[opcode_st0, base_modrm + 1]);
                Ok(())
            }
            1 => {
                // fadd %st(i) -> st(0) = st(0) op st(i)
                match &ops[0] {
                    Operand::Register(reg) => {
                        let n = parse_st_num(&reg.name)?;
                        self.bytes.extend_from_slice(&[opcode_st0, base_modrm + n]);
                        Ok(())
                    }
                    _ => Err("x87 arith requires st register operand".to_string()),
                }
            }
            2 => {
                // Two operands: fadd %st(i), %st or fadd %st, %st(i)
                match (&ops[0], &ops[1]) {
                    (Operand::Register(src), Operand::Register(dst)) => {
                        let src_n = parse_st_num(&src.name)?;
                        let dst_n = parse_st_num(&dst.name)?;
                        if dst_n == 0 {
                            // fadd %st(i), %st -> D8 (base + i)
                            self.bytes.extend_from_slice(&[opcode_st0, base_modrm + src_n]);
                        } else if src_n == 0 {
                            // fadd %st, %st(i) -> DC (base + i)
                            // Note: for fsub/fdiv, the DC form uses reversed base
                            // DC E0+i for fsubr, DC E8+i for fsub (swapped!)
                            // But base_modrm is from the D8 encoding perspective.
                            // The DC form for reverse direction:
                            // fadd: DC C0+i, fmul: DC C8+i, fsub: DC E8+i, fdiv: DC F8+i
                            // (fsub/fdiv swap: D8 E0 = fsub st(i),st; DC E8 = fsub st,st(i))
                            let dc_modrm = match base_modrm {
                                0xC0 => 0xC0, // fadd
                                0xC8 => 0xC8, // fmul
                                0xE0 => 0xE8, // fsub -> fsubr encoding in DC
                                0xF0 => 0xF8, // fdiv -> fdivr encoding in DC
                                _ => base_modrm,
                            };
                            self.bytes.extend_from_slice(&[opcode_sti, dc_modrm + dst_n]);
                        } else {
                            return Err("x87 arith: one operand must be st(0)".to_string());
                        }
                        Ok(())
                    }
                    _ => Err("x87 arith requires st register operands".to_string()),
                }
            }
            _ => Err("x87 arith requires 0-2 operands".to_string()),
        }
    }

    /// Encode fxch (exchange st(0) with st(i)).
    fn encode_fxch(&mut self, ops: &[Operand]) -> Result<(), String> {
        let n = match ops.len() {
            0 => 1, // fxch defaults to st(1)
            1 => {
                match &ops[0] {
                    Operand::Register(reg) => parse_st_num(&reg.name)?,
                    _ => return Err("fxch requires st register".to_string()),
                }
            }
            _ => return Err("fxch requires 0 or 1 operand".to_string()),
        };
        self.bytes.extend_from_slice(&[0xD9, 0xC8 + n]);
        Ok(())
    }

    /// Infer BMI2 W bit (0=32-bit, 1=64-bit) from destination register.
    fn bmi2_infer_w(&self, ops: &[Operand]) -> u8 {
        // Check destination (last operand) for register size
        if let Some(Operand::Register(r)) = ops.last() {
            if is_reg64(&r.name) { return 1; }
        }
        // Check other register operands
        for op in ops {
            if let Operand::Register(r) = op {
                if is_reg64(&r.name) { return 1; }
                if is_reg32(&r.name) { return 0; }
            }
        }
        1 // default to 64-bit
    }

    /// Encode AVX extract with imm8 (vextracti128, vextractf128)
    /// Format: VEX.256.66.0F3A opcode /r ib
    /// AT&T: $imm8, %src_ymm, %dst_xmm/mem
    fn encode_avx_extract_imm8(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 3 { return Err("AVX extract requires 3 operands".to_string()); }
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&src.name);
                let b = needs_vex_ext(&dst.name);
                self.emit_vex(r, false, b, 3, 0, 0, 1, pp); // L=1 (256-bit source)
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let r = needs_vex_ext(&src.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 3, 0, 0, 1, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(src_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX extract operands".to_string()),
        }
    }

    /// Encode AVX shuffle in 0F3A map with W=1 (vpermq, vpermpd)
    /// Format: VEX.256.66.0F3A.W1 opcode /r ib
    fn encode_avx_shuffle_3a_w1(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 3 { return Err("AVX permq requires 3 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 3, 1, 0, l, pp); // W=1
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 3, 1, 0, l, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX permq operands".to_string()),
        }
    }

    /// Encode AVX 4-operand instruction in 0F3A map (vblendvps, vblendvpd, vpblendvb)
    /// AT&T: $imm/mask, src, vvvv, dst -> actually: src_mask, src, vvvv, dst
    /// Intel: dst, vvvv, src, mask_reg
    /// VEX.NDS.128/256.66.0F3A opcode /r /is4
    fn encode_avx_4op_3a(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 4 { return Err("AVX 4-op requires 4 operands".to_string()); }
        let l = self.vex_l_from_ops(ops);
        let pp = if has_66 { 1 } else { 0 };

        // AT&T: %mask, %src, %vvvv, %dst
        match (&ops[0], &ops[1], &ops[2], &ops[3]) {
            (Operand::Register(mask), Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let mask_num = reg_num(&mask.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 3, 0, vvvv_enc, l, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                // is4: mask register encoded in imm8[7:4]
                let mask_full = mask_num | (if needs_vex_ext(&mask.name) { 8 } else { 0 });
                self.bytes.push((mask_full & 0xF) << 4);
                Ok(())
            }
            _ => Err("unsupported AVX 4-op operands".to_string()),
        }
    }

    /// Encode AVX insert from GP register (vpinsrb, vpinsrd) via 0F3A map
    /// AT&T: $imm8, %gp/%mem, %xmm_vvvv, %xmm_dst
    fn encode_avx_insert_gp(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 4 { return Err("AVX insert requires 4 operands".to_string()); }
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2], &ops[3]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 3, 0, vvvv_enc, 0, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(vvvv), Operand::Register(dst)) => {
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, x, b_ext, 3, 0, vvvv_enc, 0, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX insert operands".to_string()),
        }
    }

    /// Encode AVX insert via 0F map (vpinsrw)
    fn encode_avx_insert_gp_0f(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 4 { return Err("AVX insert requires 4 operands".to_string()); }
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2], &ops[3]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 1, 0, vvvv_enc, 0, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX insert operands".to_string()),
        }
    }

    /// Encode AVX insert with W=1 (vpinsrq)
    fn encode_avx_insert_gp_w1(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 4 { return Err("AVX insert requires 4 operands".to_string()); }
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2], &ops[3]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                self.emit_vex(r, false, b, 3, 1, vvvv_enc, 0, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX insert operands".to_string()),
        }
    }

    /// Encode AVX extract byte/dword (vpextrb, vpextrd) via 0F3A map
    fn encode_avx_extract_byte(&mut self, ops: &[Operand], opcode: u8, has_66: bool) -> Result<(), String> {
        if ops.len() != 3 { return Err("AVX extract requires 3 operands".to_string()); }
        let pp = if has_66 { 1 } else { 0 };

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&src.name);
                let b = needs_vex_ext(&dst.name);
                self.emit_vex(r, false, b, 3, 0, 0, 0, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let r = needs_vex_ext(&src.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 3, 0, 0, 0, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(src_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported AVX extract operands".to_string()),
        }
    }

    /// Encode BMI2 3-operand GPR instructions (shrxq, shlxq, sarxq, etc.).
    ///
    /// AT&T syntax: `shrxq %src_shift, %r/m, %dst`
    ///   ops[0] = shift count register → VEX.vvvv
    ///   ops[1] = source r/m → ModRM r/m
    ///   ops[2] = destination → ModRM reg
    ///
    /// All use 0F38 map (mm=2). pp selects prefix: 0=NP, 1=66, 2=F3, 3=F2.
    fn encode_bmi2_shift(&mut self, ops: &[Operand], opcode: u8, pp: u8, w: u8) -> Result<(), String> {
        if ops.len() != 3 { return Err("BMI2 instruction requires 3 operands".to_string()); }

        let vvvv_reg = match &ops[0] {
            Operand::Register(r) => r,
            _ => return Err("BMI2: first operand must be register".to_string()),
        };
        let vvvv_num = reg_num(&vvvv_reg.name).ok_or("bad vvvv register")?;
        let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv_reg.name) { 8 } else { 0 });

        match (&ops[1], &ops[2]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 2, w, vvvv_enc, 0, pp);
                self.bytes.push(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 2, w, vvvv_enc, 0, pp);
                self.bytes.push(opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("BMI2: unsupported operand combination".to_string()),
        }
    }

    /// Encode BMI1 ANDN: andnl %src2, %src1, %dst → dst = ~src1 & src2
    /// AT&T operand order: ops[0]=src2(r/m), ops[1]=src1(vvvv), ops[2]=dst(reg)
    fn encode_bmi_andn(&mut self, ops: &[Operand], w: u8) -> Result<(), String> {
        if ops.len() != 3 { return Err("andn requires 3 operands".to_string()); }

        // ops[1] = src1 → VEX.vvvv
        let vvvv_reg = match &ops[1] {
            Operand::Register(r) => r,
            _ => return Err("andn: second operand must be register".to_string()),
        };
        let vvvv_num = reg_num(&vvvv_reg.name).ok_or("bad vvvv register")?;
        let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv_reg.name) { 8 } else { 0 });

        match (&ops[0], &ops[2]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                self.emit_vex(r, false, b, 2, w, vvvv_enc, 0, 0); // NP.0F38
                self.bytes.push(0xF2);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 2, w, vvvv_enc, 0, 0);
                self.bytes.push(0xF2);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported andn operands".to_string()),
        }
    }

    /// Encode BMI2 rorx (rotate right logical with immediate).
    /// AT&T syntax: `rorxq $imm8, %r/m, %dst`
    ///   ops[0] = immediate
    ///   ops[1] = source r/m
    ///   ops[2] = destination
    fn encode_bmi2_rorx(&mut self, ops: &[Operand], w: u8) -> Result<(), String> {
        if ops.len() != 3 { return Err("rorx requires 3 operands".to_string()); }

        let imm = match &ops[0] {
            Operand::Immediate(ImmediateValue::Integer(val)) => *val as u8,
            _ => return Err("rorx: first operand must be immediate".to_string()),
        };

        match (&ops[1], &ops[2]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                let r = needs_vex_ext(&dst.name);
                let b = needs_vex_ext(&src.name);
                // VEX.LZ.F2.0F3A.Wx F0 /r imm8 (mm=3 for 0F3A, pp=3 for F2)
                self.emit_vex(r, false, b, 3, w, 0, 0, 3);
                self.bytes.push(0xF0);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(imm);
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                let r = needs_vex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_vex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_vex_ext(&i.name));
                self.emit_vex(r, x, b_ext, 3, w, 0, 0, 3);
                self.bytes.push(0xF0);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(imm);
                Ok(())
            }
            _ => Err("rorx: unsupported operand combination".to_string()),
        }
    }
}

/// Infer register size in bytes from register name.
fn infer_reg_size(name: &str) -> u8 {
    if is_reg64(name) { 8 }
    else if is_reg32(name) { 4 }
    else if is_reg16(name) { 2 }
    else if is_reg8(name) { 1 }
    else if is_xmm(name) { 16 }
    else if is_ymm(name) { 32 }
    else { 8 } // mmx and other registers default to 8
}

/// Infer operand size from a pair of operands for suffix-less instructions.
fn infer_operand_size_from_pair(op1: &Operand, op2: &Operand) -> u8 {
    // Try to infer from register operands
    for op in [op1, op2] {
        if let Operand::Register(r) = op {
            if is_segment_reg(&r.name) { continue; }
            if is_reg64(&r.name) { return 8; }
            if is_reg32(&r.name) { return 4; }
            if is_reg16(&r.name) { return 2; }
            if is_reg8(&r.name) { return 1; }
        }
    }
    // Default to 64-bit
    8
}

/// Parse x87 register number: "st(0)" -> 0, "st" -> 0, "st(1)" -> 1, etc.
fn parse_st_num(name: &str) -> Result<u8, String> {
    if name == "st" || name == "st(0)" {
        return Ok(0);
    }
    if name.starts_with("st(") && name.ends_with(')') {
        let n: u8 = name[3..name.len()-1].parse()
            .map_err(|_| format!("bad st register: {}", name))?;
        if n > 7 {
            return Err(format!("st register out of range: {}", name));
        }
        return Ok(n);
    }
    Err(format!("not an st register: {}", name))
}

/// Map condition code suffix to encoding.
fn cc_from_mnemonic(cc_str: &str) -> Result<u8, String> {
    match cc_str {
        "o" => Ok(0),
        "no" => Ok(1),
        "b" | "c" | "nae" => Ok(2),
        "nb" | "nc" | "ae" => Ok(3),
        "e" | "z" => Ok(4),
        "ne" | "nz" => Ok(5),
        "be" | "na" => Ok(6),
        "nbe" | "a" => Ok(7),
        "s" => Ok(8),
        "ns" => Ok(9),
        "p" | "pe" => Ok(10),
        "np" | "po" => Ok(11),
        "l" | "nge" => Ok(12),
        "nl" | "ge" => Ok(13),
        "le" | "ng" => Ok(14),
        "nle" | "g" => Ok(15),
        _ => Err(format!("unknown condition code: {}", cc_str)),
    }
}
