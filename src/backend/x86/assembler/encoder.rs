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

/// Register encoding (3-bit register number in ModR/M and SIB).
fn reg_num(name: &str) -> Option<u8> {
    match name {
        "al" | "ax" | "eax" | "rax" | "xmm0" | "st" | "st(0)" => Some(0),
        "cl" | "cx" | "ecx" | "rcx" | "xmm1" | "st(1)" => Some(1),
        "dl" | "dx" | "edx" | "rdx" | "xmm2" | "st(2)" => Some(2),
        "bl" | "bx" | "ebx" | "rbx" | "xmm3" | "st(3)" => Some(3),
        "ah" | "spl" | "sp" | "esp" | "rsp" | "xmm4" | "st(4)" => Some(4),
        "ch" | "bpl" | "bp" | "ebp" | "rbp" | "xmm5" | "st(5)" => Some(5),
        "dh" | "sil" | "si" | "esi" | "rsi" | "xmm6" | "st(6)" => Some(6),
        "bh" | "dil" | "di" | "edi" | "rdi" | "xmm7" | "st(7)" => Some(7),
        "r8b" | "r8w" | "r8d" | "r8" | "xmm8" => Some(0),
        "r9b" | "r9w" | "r9d" | "r9" | "xmm9" => Some(1),
        "r10b" | "r10w" | "r10d" | "r10" | "xmm10" => Some(2),
        "r11b" | "r11w" | "r11d" | "r11" | "xmm11" => Some(3),
        "r12b" | "r12w" | "r12d" | "r12" | "xmm12" => Some(4),
        "r13b" | "r13w" | "r13d" | "r13" | "xmm13" => Some(5),
        "r14b" | "r14w" | "r14d" | "r14" | "xmm14" => Some(6),
        "r15b" | "r15w" | "r15d" | "r15" | "xmm15" => Some(7),
        _ => None,
    }
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

/// Is this an x87 FPU register?
#[allow(dead_code)]
fn is_st(name: &str) -> bool {
    name == "st" || name.starts_with("st(")
}

/// Get operand size from mnemonic suffix.
fn mnemonic_size_suffix(mnemonic: &str) -> Option<u8> {
    // Handle mnemonics that don't follow the simple suffix pattern
    match mnemonic {
        "cltq" | "cqto" | "cltd" | "cdq" | "cqo" | "ret" | "nop" | "ud2"
        | "endbr64" | "pause" | "mfence" | "lfence" | "sfence" | "clflush"
        | "syscall" | "sysenter" | "cpuid" | "rdtsc" | "rdtscp" => return None,
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
        let mnemonic = instr.mnemonic.as_str();
        let ops = &instr.operands;

        match mnemonic {
            // Data movement
            "movq" => {
                // Check if any operand is an XMM register - route to SSE movq
                let has_xmm = ops.iter().any(|op| matches!(op, Operand::Register(r) if is_xmm(&r.name)));
                if has_xmm {
                    self.encode_movq_xmm(ops)
                } else {
                    self.encode_mov(ops, 8)
                }
            }
            "movl" => self.encode_mov(ops, 4),
            "movw" => self.encode_mov(ops, 2),
            "movb" => self.encode_mov(ops, 1),
            "movabsq" => self.encode_movabs(ops),
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
            "incq" => self.encode_unary_rm(ops, 0, 8),
            "incl" => self.encode_unary_rm(ops, 0, 4),
            "decq" => self.encode_unary_rm(ops, 1, 8),
            "decl" => self.encode_unary_rm(ops, 1, 4),

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
            "lzcntl" | "lzcntq" | "tzcntl" | "tzcntq" | "popcntl" | "popcntq" => {
                self.encode_bit_count(ops, mnemonic)
            }

            // Conditional set
            "sete" | "setz" | "setne" | "setnz" | "setl" | "setle" | "setg" | "setge"
            | "setb" | "setc" | "setbe" | "seta" | "setae" | "setnc" | "setnp" | "setp"
            | "sets" | "setns" | "seto" | "setno" => self.encode_setcc(ops, mnemonic),

            // Conditional move
            "cmoveq" | "cmovneq" | "cmovlq" | "cmovleq" | "cmovgq" | "cmovgeq"
            | "cmovbq" | "cmovbeq" | "cmovaq" | "cmovaeq"
            | "cmovel" | "cmovnel" | "cmovll" | "cmovlel" | "cmovgl" | "cmovgel"
            | "cmovbl" | "cmovbel" | "cmoval" | "cmovael" => self.encode_cmovcc(ops, mnemonic),

            // Jumps
            "jmp" => self.encode_jmp(ops),
            "je" | "jz" | "jne" | "jnz" | "jl" | "jle" | "jg" | "jge"
            | "jb" | "jbe" | "ja" | "jae" | "js" | "jns" | "jo" | "jno" | "jp" | "jnp" => {
                self.encode_jcc(ops, mnemonic)
            }

            // Call/return
            "call" => self.encode_call(ops),
            "ret" => { self.bytes.push(0xC3); Ok(()) }

            // No-ops and misc
            "nop" => { self.bytes.push(0x90); Ok(()) }
            "ud2" => { self.bytes.extend_from_slice(&[0x0F, 0x0B]); Ok(()) }
            "endbr64" => { self.bytes.extend_from_slice(&[0xF3, 0x0F, 0x1E, 0xFA]); Ok(()) }
            "pause" => { self.bytes.extend_from_slice(&[0xF3, 0x90]); Ok(()) }
            "mfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF0]); Ok(()) }
            "lfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xE8]); Ok(()) }
            "sfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF8]); Ok(()) }
            "clflush" => self.encode_clflush(ops),

            // String ops
            "movsb" => { self.bytes.push(0xA4); Ok(()) }
            "movsd" if ops.is_empty() => { self.bytes.push(0xA5); Ok(()) }
            "stosb" => { self.bytes.push(0xAA); Ok(()) }
            "stosd" => { self.bytes.push(0xAB); Ok(()) }

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
            "minsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5D]),
            "maxsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5F]),
            "minss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5D]),
            "maxss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5F]),

            // Additional SSE data movement
            "movaps" => self.encode_sse_rr_rm(ops, &[0x0F, 0x28], &[0x0F, 0x29]),
            "movdqa" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x6F], &[0x66, 0x0F, 0x7F]),
            "movlpd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x12], &[0x66, 0x0F, 0x13]),
            "movhpd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x16], &[0x66, 0x0F, 0x17]),

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
            "pinsrw" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0xC4]),
            "pextrw" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0xC5]),
            "pinsrd" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x22]),
            "pextrd" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x16]),
            "pinsrb" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x20]),
            "pextrb" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x14]),
            "pinsrq" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x22]),
            "pextrq" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x16]),

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
            "movnti" => self.encode_movnti(ops),
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
            "fildq" => self.encode_x87_mem(ops, &[0xDF], 5),
            "fisttpq" => self.encode_x87_mem(ops, &[0xDD], 1),
            "faddp" => { self.bytes.extend_from_slice(&[0xDE, 0xC1]); Ok(()) }
            "fsubrp" => { self.bytes.extend_from_slice(&[0xDE, 0xE1]); Ok(()) }
            "fmulp" => { self.bytes.extend_from_slice(&[0xDE, 0xC9]); Ok(()) }
            "fdivrp" => { self.bytes.extend_from_slice(&[0xDE, 0xF1]); Ok(()) }
            "fchs" => { self.bytes.extend_from_slice(&[0xD9, 0xE0]); Ok(()) }
            "fcomip" => self.encode_fcomip(ops),
            "fucomip" => self.encode_fucomip(ops),
            "fld" => self.encode_fld_st(ops),
            "fstp" => self.encode_fstp_st(ops),

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

    /// Emit REX prefix for a memory operand where 'reg' is the reg field.
    fn emit_rex_rm(&mut self, size: u8, reg: &str, mem: &MemoryOperand) {
        let w = size == 8;
        let r = needs_rex_ext(reg);
        let b = mem.base.as_ref().map_or(false, |b| needs_rex_ext(&b.name));
        let x = mem.index.as_ref().map_or(false, |i| needs_rex_ext(&i.name));
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

        // Handle symbol displacements that need relocations
        let (disp_val, has_symbol) = match &mem.displacement {
            Displacement::None => (0i64, false),
            Displacement::Integer(v) => (*v, false),
            Displacement::Symbol(sym) => {
                self.add_relocation(sym, R_X86_64_32S, 0);
                (0i64, true)
            }
            Displacement::SymbolMod(sym, modifier) => {
                let reloc_type = match modifier.as_str() {
                    "TPOFF" => R_X86_64_TPOFF32,
                    "GOTPCREL" => R_X86_64_GOTPCREL,
                    _ => R_X86_64_32S,
                };
                self.add_relocation(sym, reloc_type, 0);
                (0i64, true)
            }
        };

        // No base register - need SIB with no-base encoding
        if base.is_none() && index.is_none() {
            // Direct memory reference - mod=00, rm=100 (SIB), SIB: base=101 (no base)
            self.bytes.push(self.modrm(0, reg_field, 4));
            self.bytes.push(self.sib(1, 4, 5)); // index=100 (none), base=101 (disp32)
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
        } else if disp_val >= -128 && disp_val <= 127 {
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
                self.bytes.extend_from_slice(&(disp_val as i32).to_le_bytes());
            } else {
                self.bytes.push(self.modrm(mod_bits, reg_field, 4));
                self.bytes.push(self.sib(scale, idx_num, base_num));
                match disp_size {
                    0 => {}
                    1 => self.bytes.push(disp_val as u8),
                    4 => self.bytes.extend_from_slice(&(disp_val as i32).to_le_bytes()),
                    _ => unreachable!(),
                }
            }
        } else {
            self.bytes.push(self.modrm(mod_bits, reg_field, base_num));
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
            _ => Err(format!("unsupported mov operand combination")),
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
            ImmediateValue::SymbolMod(_, _) => {
                Err(format!("unsupported immediate type for mov"))?
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
        if let Some(ref seg) = mem.segment {
            match seg.as_str() {
                "fs" => self.bytes.push(0x64),
                "gs" => self.bytes.push(0x65),
                _ => return Err(format!("unsupported segment: {}", seg)),
            }
        }

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

        if let Some(ref seg) = mem.segment {
            match seg.as_str() {
                "fs" => self.bytes.push(0x64),
                "gs" => self.bytes.push(0x65),
                _ => return Err(format!("unsupported segment: {}", seg)),
            }
        }

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
            (1, _) => vec![0x0F, 0xBE],   // movsbq/movsbl
            (2, _) => vec![0x0F, 0xBF],   // movswq/movswl
            (4, 8) => vec![0x63],          // movslq (movsxd)
            _ => return Err(format!("unsupported movsx combination: {} -> {}", src_size, dst_size)),
        };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.emit_rex_rr(dst_size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.emit_rex_rm(dst_size, &dst.name, mem);
                self.bytes.extend_from_slice(&opcode);
                self.encode_modrm_mem(dst_num, mem)?;
            }
            _ => return Err("unsupported movsx operands".to_string()),
        }
        Ok(())
    }

    fn encode_movzx(&mut self, ops: &[Operand], src_size: u8, _dst_size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movzx requires 2 operands".to_string());
        }

        let opcode = match src_size {
            1 => vec![0x0F, 0xB6],  // movzbl/movzbq
            2 => vec![0x0F, 0xB7],  // movzwl/movzwq
            _ => return Err(format!("unsupported movzx src size: {}", src_size)),
        };

        // Note: movzbl zero-extends to 64 bits implicitly (32-bit op clears upper 32)
        // So we use size=4 for REX calculation unless dst is an extended register needing REX.B
        let rex_size = if _dst_size == 8 { 8 } else { 4 };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                // movzbl uses 32-bit destination but we need REX if either operand is extended
                self.emit_rex_rr(rex_size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
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
                } else if val >= -128 && val <= 127 {
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
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.push(if size == 1 { 0x02 } else { 0x03 } + alu_op * 8);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.push(if size == 1 { 0x00 } else { 0x01 } + alu_op * 8);
                self.encode_modrm_mem(src_num, mem)
            }
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Memory(mem)) => {
                let val = *val;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, "", mem);

                if size == 1 {
                    self.bytes.push(0x80);
                    self.encode_modrm_mem(alu_op, mem)?;
                    self.bytes.push(val as u8);
                } else if val >= -128 && val <= 127 {
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
            _ => Err(format!("unsupported test operands")),
        }
    }

    fn encode_imul(&mut self, ops: &[Operand], size: u8) -> Result<(), String> {
        match ops.len() {
            1 => {
                // One-operand form: imul r/m (result in rdx:rax)
                self.encode_unary_rm(ops, 5, size)
            }
            2 => {
                // Two-operand form: imul src, dst
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
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                self.emit_rex_unary(size, &reg.name);
                self.bytes.push(if size == 1 { 0xF6 } else { 0xF7 });
                self.bytes.push(self.modrm(3, op_ext, num));
                Ok(())
            }
            Operand::Memory(mem) => {
                self.emit_rex_rm(size, "", mem);
                self.bytes.push(if size == 1 { 0xF6 } else { 0xF7 });
                self.encode_modrm_mem(op_ext, mem)
            }
            _ => Err("unsupported unary operand".to_string()),
        }
    }

    fn encode_shift(&mut self, ops: &[Operand], mnemonic: &str, shift_op: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }

        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);

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
            (Operand::Register(cl), Operand::Register(dst)) if cl.name == "cl" => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_unary(size, &dst.name);
                self.bytes.push(if size == 1 { 0xD2 } else { 0xD3 });
                self.bytes.push(self.modrm(3, shift_op, dst_num));
                Ok(())
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
            "lzcntl" | "lzcntq" => (0xF3u8, [0x0F, 0xBD]),
            "tzcntl" | "tzcntq" => (0xF3, [0x0F, 0xBC]),
            "popcntl" | "popcntq" => (0xF3, [0x0F, 0xB8]),
            _ => return Err(format!("unknown bit count: {}", mnemonic)),
        };

        let size = mnemonic_size_suffix(mnemonic).unwrap_or(4);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(prefix);
                self.emit_rex_rr(size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    fn encode_setcc(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("setcc requires 1 operand".to_string());
        }

        let cc = cc_from_mnemonic(&mnemonic[3..])?;

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
            _ => Err("setcc requires register operand".to_string()),
        }
    }

    fn encode_cmovcc(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("cmovcc requires 2 operands".to_string());
        }

        // Extract condition code: strip "cmov" prefix and size suffix
        let without_prefix = &mnemonic[4..];
        let (cc_str, size) = if without_prefix.ends_with('q') {
            (&without_prefix[..without_prefix.len()-1], 8u8)
        } else if without_prefix.ends_with('l') {
            (&without_prefix[..without_prefix.len()-1], 4u8)
        } else {
            (without_prefix, 8u8) // default to 64-bit
        };
        let cc = cc_from_mnemonic(cc_str)?;

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.emit_rex_rr(size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, 0x40 + cc]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
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
                let reloc_type = if label.ends_with("@PLT") {
                    R_X86_64_PLT32
                } else {
                    R_X86_64_PLT32 // Default to PLT32 for calls
                };
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
                // Emit prefix bytes (before REX)
                for &b in &load_opcode[..load_opcode.len()-1] {
                    if b == 0x0F || b >= 0x38 {
                        // Part of opcode, not prefix
                        break;
                    }
                    self.bytes.push(b);
                }
                self.emit_rex_rr(0, &dst.name, &src.name);
                // Emit remaining opcode bytes
                let prefix_len = load_opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
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
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rm(0, &dst.name, mem);
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

    fn encode_sse_insert(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("pinsrX requires 3 operands".to_string());
        }
        // pinsrX $imm, r/m, xmm
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
            _ => Err("unsupported pinsrX operands".to_string()),
        }
    }

    fn encode_sse_extract(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("pextrX requires 3 operands".to_string());
        }
        // pextrX $imm, xmm, r/m
        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rr(0, &src.name, &dst.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
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
                let b_ext = mem.base.as_ref().map_or(false, |b| needs_rex_ext(&b.name));
                let x = mem.index.as_ref().map_or(false, |i| needs_rex_ext(&i.name));
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
