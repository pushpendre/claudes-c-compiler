//! i686 (32-bit x86) instruction encoder.
//!
//! Encodes parsed i686 instructions into machine code bytes.
//! Similar to the x86-64 encoder but without REX prefixes and with
//! 32-bit default operand size. Uses R_386_* relocation types.

use crate::backend::x86::assembler::parser::*;

/// Relocation entry for the linker to resolve.
#[derive(Debug, Clone)]
pub struct Relocation {
    /// Offset within the section where the relocation applies.
    pub offset: u64,
    /// Symbol name to resolve.
    pub symbol: String,
    /// Relocation type (ELF R_386_* constants).
    pub reloc_type: u32,
    /// Addend for the relocation (used in RELA; for REL format, embedded in instruction).
    pub addend: i64,
}

// ELF i386 relocation types
pub const R_386_32: u32 = 1;
pub const R_386_PC32: u32 = 2;
#[allow(dead_code)]
pub const R_386_GOT32: u32 = 3;
pub const R_386_PLT32: u32 = 4;
#[allow(dead_code)]
pub const R_386_GOTOFF: u32 = 9;
#[allow(dead_code)]
pub const R_386_GOTPC: u32 = 10;
#[allow(dead_code)]
pub const R_386_TLS_LE_32: u32 = 37;
#[allow(dead_code)]
pub const R_386_TLS_IE: u32 = 15;

/// Register encoding (3-bit register number in ModR/M and SIB).
fn reg_num(name: &str) -> Option<u8> {
    match name {
        "al" | "ax" | "eax" | "xmm0" | "st" | "st(0)" => Some(0),
        "cl" | "cx" | "ecx" | "xmm1" | "st(1)" => Some(1),
        "dl" | "dx" | "edx" | "xmm2" | "st(2)" => Some(2),
        "bl" | "bx" | "ebx" | "xmm3" | "st(3)" => Some(3),
        "ah" | "sp" | "esp" | "xmm4" | "st(4)" => Some(4),
        "ch" | "bp" | "ebp" | "xmm5" | "st(5)" => Some(5),
        "dh" | "si" | "esi" | "xmm6" | "st(6)" => Some(6),
        "bh" | "di" | "edi" | "xmm7" | "st(7)" => Some(7),
        _ => None,
    }
}

/// Is this an XMM register?
fn is_xmm(name: &str) -> bool {
    name.starts_with("xmm")
}

/// Get operand size from mnemonic suffix.
fn mnemonic_size_suffix(mnemonic: &str) -> Option<u8> {
    match mnemonic {
        "cltd" | "cdq" | "ret" | "nop" | "ud2" | "pause"
        | "mfence" | "lfence" | "sfence" | "clflush"
        | "syscall" | "sysenter" | "cpuid" | "rdtsc" | "rdtscp" => return None,
        _ => {}
    }
    let last = mnemonic.as_bytes().last()?;
    match last {
        b'b' => Some(1),
        b'w' => Some(2),
        b'l' | b'd' => Some(4),
        // i686 shouldn't have 'q' suffix for GP instructions, but handle gracefully
        b'q' => Some(4),
        _ => None,
    }
}

/// Instruction encoding context for i686.
pub struct InstructionEncoder {
    /// Output bytes.
    pub bytes: Vec<u8>,
    /// Relocations generated during encoding.
    pub relocations: Vec<Relocation>,
    /// Current offset within the section.
    pub offset: u64,
    /// Whether we are in .code16gcc mode (16-bit real mode with 32-bit instructions).
    #[allow(dead_code)]
    pub code16gcc: bool,
}

impl InstructionEncoder {
    pub fn new() -> Self {
        InstructionEncoder {
            bytes: Vec::new(),
            relocations: Vec::new(),
            offset: 0,
            code16gcc: false,
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
            "movl" => self.encode_mov(ops, 4),
            "movw" => self.encode_mov(ops, 2),
            "movb" => self.encode_mov(ops, 1),
            "movsbl" => self.encode_movsx(ops, 1, 4),
            "movswl" => self.encode_movsx(ops, 2, 4),
            "movsbw" => self.encode_movsx(ops, 1, 2),
            "movzbl" => self.encode_movzx(ops, 1, 4),
            "movzwl" => self.encode_movzx(ops, 2, 4),
            "movzbw" => self.encode_movzx(ops, 1, 2),

            // LEA
            "leal" => self.encode_lea(ops, 4),

            // Stack ops (32-bit default)
            "pushl" => self.encode_push(ops),
            "popl" => self.encode_pop(ops),
            // Also handle pushw/popw for 16-bit variants
            "pushw" => self.encode_push16(ops),

            // Arithmetic
            "addl" | "addw" | "addb" => self.encode_alu(ops, mnemonic, 0),
            "orl" | "orw" | "orb" => self.encode_alu(ops, mnemonic, 1),
            "adcl" | "adcw" | "adcb" => self.encode_alu(ops, mnemonic, 2),
            "sbbl" | "sbbw" | "sbbb" => self.encode_alu(ops, mnemonic, 3),
            "andl" | "andw" | "andb" => self.encode_alu(ops, mnemonic, 4),
            "subl" | "subw" | "subb" => self.encode_alu(ops, mnemonic, 5),
            "xorl" | "xorw" | "xorb" => self.encode_alu(ops, mnemonic, 6),
            "cmpl" | "cmpw" | "cmpb" => self.encode_alu(ops, mnemonic, 7),
            "testl" | "testw" | "testb" => self.encode_test(ops, mnemonic),

            // Multiply/divide
            "imull" => self.encode_imul(ops, 4),
            "mull" => self.encode_unary_rm(ops, 4, 4),
            "divl" => self.encode_unary_rm(ops, 6, 4),
            "idivl" => self.encode_unary_rm(ops, 7, 4),

            // Unary
            "negl" => self.encode_unary_rm(ops, 3, 4),
            "negw" => self.encode_unary_rm(ops, 3, 2),
            "negb" => self.encode_unary_rm(ops, 3, 1),
            "notl" => self.encode_unary_rm(ops, 2, 4),
            "notw" => self.encode_unary_rm(ops, 2, 2),
            "notb" => self.encode_unary_rm(ops, 2, 1),
            "incl" => self.encode_inc_dec(ops, 0, 4),
            "incw" => self.encode_inc_dec(ops, 0, 2),
            "incb" => self.encode_inc_dec(ops, 0, 1),
            "decl" => self.encode_inc_dec(ops, 1, 4),
            "decw" => self.encode_inc_dec(ops, 1, 2),
            "decb" => self.encode_inc_dec(ops, 1, 1),

            // Shifts
            "shll" | "shlw" | "shlb" => self.encode_shift(ops, mnemonic, 4),
            "shrl" | "shrw" | "shrb" => self.encode_shift(ops, mnemonic, 5),
            "sarl" | "sarw" | "sarb" => self.encode_shift(ops, mnemonic, 7),
            "roll" | "rolw" | "rolb" => self.encode_shift(ops, mnemonic, 0),
            "rorl" | "rorw" | "rorb" => self.encode_shift(ops, mnemonic, 1),

            // Double-precision shifts
            "shldl" => self.encode_double_shift(ops, 0xA4, 4),
            "shrdl" => self.encode_double_shift(ops, 0xAC, 4),

            // Sign extension
            "cltd" | "cdq" => { self.bytes.push(0x99); Ok(()) }
            "cwtl" | "cwde" => { self.bytes.push(0x98); Ok(()) }
            "cbtw" | "cbw" => { self.bytes.extend_from_slice(&[0x66, 0x98]); Ok(()) }
            "cwtd" | "cwd" => { self.bytes.extend_from_slice(&[0x66, 0x99]); Ok(()) }

            // Byte swap
            "bswapl" => self.encode_bswap(ops),

            // Bit operations
            "lzcntl" | "tzcntl" | "popcntl" => self.encode_bit_count(ops, mnemonic),
            "bsrl" | "bsfl" => self.encode_bsr_bsf(ops, mnemonic),
            "btl" | "btsl" | "btrl" | "btcl" => self.encode_bt(ops, mnemonic),

            // Conditional set
            "sete" | "setz" | "setne" | "setnz" | "setl" | "setle" | "setg" | "setge"
            | "setb" | "setc" | "setbe" | "seta" | "setae" | "setnc" | "setnp" | "setp"
            | "sets" | "setns" | "seto" | "setno" => self.encode_setcc(ops, mnemonic),

            // Conditional move
            "cmovel" | "cmovnel" | "cmovll" | "cmovlel" | "cmovgl" | "cmovgel"
            | "cmovbl" | "cmovbel" | "cmoval" | "cmovael"
            | "cmovsl" | "cmovnsl" => self.encode_cmovcc(ops, mnemonic),

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
            "pause" => { self.bytes.extend_from_slice(&[0xF3, 0x90]); Ok(()) }
            "mfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF0]); Ok(()) }
            "lfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xE8]); Ok(()) }
            "sfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF8]); Ok(()) }
            "clflush" => self.encode_clflush(ops),
            "int" => self.encode_int(ops),

            // String ops
            "movsb" => { self.bytes.push(0xA4); Ok(()) }
            "movsl" if ops.is_empty() => { self.bytes.push(0xA5); Ok(()) }
            "stosb" => { self.bytes.push(0xAA); Ok(()) }
            "stosl" => { self.bytes.push(0xAB); Ok(()) }

            // Atomic exchange
            "xchgb" | "xchgw" | "xchgl" => self.encode_xchg(ops, mnemonic),

            // Lock-prefixed atomics
            "cmpxchgb" | "cmpxchgw" | "cmpxchgl" => self.encode_cmpxchg(ops, mnemonic),
            "xaddb" | "xaddw" | "xaddl" => self.encode_xadd(ops, mnemonic),

            // SSE/SSE2 floating-point
            "movss" => self.encode_sse_rr_rm(ops, &[0xF3, 0x0F, 0x10], &[0xF3, 0x0F, 0x11]),
            "movsd" => self.encode_sse_rr_rm(ops, &[0xF2, 0x0F, 0x10], &[0xF2, 0x0F, 0x11]),
            "movd" => self.encode_movd(ops),
            "movdqu" => self.encode_sse_rr_rm(ops, &[0xF3, 0x0F, 0x6F], &[0xF3, 0x0F, 0x7F]),
            "movupd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x10], &[0x66, 0x0F, 0x11]),
            "movaps" => self.encode_sse_rr_rm(ops, &[0x0F, 0x28], &[0x0F, 0x29]),
            "movdqa" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x6F], &[0x66, 0x0F, 0x7F]),

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
            "orpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x56]),
            "orps" => self.encode_sse_op(ops, &[0x0F, 0x56]),

            // SSE conversions (32-bit integer operand size for i686)
            "cvtsd2ss" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5A]),
            "cvtss2sd" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5A]),
            "cvtsi2sdl" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF2, 0x0F, 0x2A]),
            "cvtsi2ssl" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF3, 0x0F, 0x2A]),
            "cvttsd2sil" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2C]),
            "cvttss2sil" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2C]),

            // SSE packed integer
            "pshufd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x70]),
            "pxor" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEF]),
            "pand" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDB]),
            "por" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEB]),
            "pandn" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDF]),
            "pcmpeqb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x74]),
            "pcmpeqd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x76]),
            "pcmpeqw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x75]),
            "pmovmskb" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x66, 0x0F, 0xD7]),

            // SSE shifts
            "pslld" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF2], 6, &[0x66, 0x0F, 0x72]),
            "psrld" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD2], 2, &[0x66, 0x0F, 0x72]),
            "psrad" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xE2], 4, &[0x66, 0x0F, 0x72]),
            "psllq" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF3], 6, &[0x66, 0x0F, 0x73]),
            "psrlq" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD3], 2, &[0x66, 0x0F, 0x73]),

            // x87 FPU
            "fldt" => self.encode_x87_mem(ops, &[0xDB], 5),
            "fstpt" => self.encode_x87_mem(ops, &[0xDB], 7),
            "fldl" => self.encode_x87_mem(ops, &[0xDD], 0),
            "flds" => self.encode_x87_mem(ops, &[0xD9], 0),
            "fstpl" => self.encode_x87_mem(ops, &[0xDD], 3),
            "fstps" => self.encode_x87_mem(ops, &[0xD9], 3),
            "fstl" => self.encode_x87_mem(ops, &[0xDD], 2),
            "fsts" => self.encode_x87_mem(ops, &[0xD9], 2),
            "fildl" => self.encode_x87_mem(ops, &[0xDB], 0),
            "fildq" => self.encode_x87_mem(ops, &[0xDF], 5),
            "filds" => self.encode_x87_mem(ops, &[0xDF], 0),
            "fistpl" => self.encode_x87_mem(ops, &[0xDB], 3),
            "fisttpq" => self.encode_x87_mem(ops, &[0xDD], 1),
            "fisttpl" => self.encode_x87_mem(ops, &[0xDB], 1),
            "faddp" => { self.bytes.extend_from_slice(&[0xDE, 0xC1]); Ok(()) }
            "fsubrp" => { self.bytes.extend_from_slice(&[0xDE, 0xE1]); Ok(()) }
            "fsubp" => { self.bytes.extend_from_slice(&[0xDE, 0xE9]); Ok(()) }
            "fmulp" => { self.bytes.extend_from_slice(&[0xDE, 0xC9]); Ok(()) }
            "fdivrp" => { self.bytes.extend_from_slice(&[0xDE, 0xF1]); Ok(()) }
            "fdivp" => { self.bytes.extend_from_slice(&[0xDE, 0xF9]); Ok(()) }
            "fchs" => { self.bytes.extend_from_slice(&[0xD9, 0xE0]); Ok(()) }
            "fabs" => { self.bytes.extend_from_slice(&[0xD9, 0xE1]); Ok(()) }
            "fcomip" => self.encode_fcomip(ops),
            "fucomip" => self.encode_fucomip(ops),
            "fld" => self.encode_fld_st(ops),
            "fstp" => self.encode_fstp_st(ops),
            "fxch" => self.encode_fxch(ops),
            "faddl" => self.encode_x87_mem(ops, &[0xDC], 0),
            "fadds" => self.encode_x87_mem(ops, &[0xD8], 0),
            "fsubl" => self.encode_x87_mem(ops, &[0xDC], 4),
            "fsubs" => self.encode_x87_mem(ops, &[0xD8], 4),
            "fmull" => self.encode_x87_mem(ops, &[0xDC], 1),
            "fmuls" => self.encode_x87_mem(ops, &[0xD8], 1),
            "fdivl" => self.encode_x87_mem(ops, &[0xDC], 6),
            "fdivs" => self.encode_x87_mem(ops, &[0xD8], 6),
            "fsubrl" => self.encode_x87_mem(ops, &[0xDC], 5),
            "fdivrl" => self.encode_x87_mem(ops, &[0xDC], 7),

            _ => {
                Err(format!("unhandled i686 instruction: {} {:?}", mnemonic, ops))
            }
        }
    }

    // ---- Encoding helpers (no REX for i686) ----

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

    /// Encode ModR/M + SIB + displacement for a memory operand.
    fn encode_modrm_mem(&mut self, reg_field: u8, mem: &MemoryOperand) -> Result<(), String> {
        let base = mem.base.as_ref();
        let index = mem.index.as_ref();

        // Handle symbol displacements that need relocations
        let (disp_val, has_symbol) = match &mem.displacement {
            Displacement::None => (0i64, false),
            Displacement::Integer(v) => (*v, false),
            Displacement::Symbol(sym) => {
                self.add_relocation(sym, R_386_32, 0);
                (0i64, true)
            }
            Displacement::SymbolAddend(sym, addend) => {
                self.add_relocation(sym, R_386_32, *addend);
                (0i64, true)
            }
            Displacement::SymbolMod(sym, modifier) => {
                let reloc_type = match modifier.as_str() {
                    "GOT" => R_386_GOT32,
                    "GOTOFF" => R_386_GOTOFF,
                    "PLT" => R_386_PLT32,
                    "GOTPC" => R_386_GOTPC,
                    // TODO: TLS relocations
                    _ => R_386_32,
                };
                self.add_relocation(sym, reloc_type, 0);
                (0i64, true)
            }
        };

        // No base register - need SIB with no-base encoding or direct displacement
        if base.is_none() && index.is_none() {
            // Direct memory reference - mod=00, rm=101 (disp32)
            self.bytes.push(self.modrm(0, reg_field, 5));
            self.bytes.extend_from_slice(&(disp_val as i32).to_le_bytes());
            return Ok(());
        }

        let base_reg = base.map(|r| &r.name as &str).unwrap_or("");
        let base_num = if !base_reg.is_empty() { reg_num(base_reg).unwrap_or(0) } else { 5 };

        // Determine if we need SIB
        let need_sib = index.is_some()
            || (base_num & 7) == 4  // ESP always needs SIB
            || base.is_none();

        // Determine displacement size
        let (mod_bits, disp_size) = if has_symbol {
            (2, 4) // always use disp32 for symbols
        } else if disp_val == 0 && (base_num & 7) != 5 {
            // No displacement (EBP always needs at least disp8)
            (0, 0)
        } else if disp_val >= -128 && disp_val <= 127 {
            (1, 1) // disp8
        } else {
            (2, 4) // disp32
        };

        if need_sib {
            let idx = index.as_ref();
            let idx_num = idx.map(|r| reg_num(&r.name).unwrap_or(4)).unwrap_or(4);
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
            offset: self.offset + self.bytes.len() as u64 - self.offset,
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
            (Operand::Immediate(imm), Operand::Register(dst)) => {
                self.encode_mov_imm_reg(imm, dst, size)
            }
            (Operand::Register(src), Operand::Register(dst)) => {
                self.encode_mov_rr(src, dst, size)
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                self.encode_mov_mem_reg(mem, dst, size)
            }
            (Operand::Register(src), Operand::Memory(mem)) => {
                self.encode_mov_reg_mem(src, mem, size)
            }
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
                if size == 4 {
                    // movl $imm32, %reg - use compact B8+rd encoding
                    self.bytes.push(0xB8 + dst_num);
                    self.bytes.extend_from_slice(&(val as i32).to_le_bytes());
                } else if size == 2 {
                    self.bytes.push(0x66);
                    self.bytes.push(0xB8 + dst_num);
                    self.bytes.extend_from_slice(&(val as i16).to_le_bytes());
                } else {
                    // 8-bit
                    self.bytes.push(0xB0 + dst_num);
                    self.bytes.push(val as u8);
                }
            }
            ImmediateValue::Symbol(sym) => {
                if size == 4 {
                    self.bytes.push(0xB8 + dst_num);
                    self.add_relocation(sym, R_386_32, 0);
                    self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                } else {
                    return Err("symbol immediate only supported for 32-bit mov".to_string());
                }
            }
            ImmediateValue::SymbolMod(_, _) => {
                return Err("unsupported immediate type for mov".to_string());
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
                    4 => self.bytes.extend_from_slice(&(*val as i32).to_le_bytes()),
                    _ => unreachable!(),
                }
            }
            ImmediateValue::Symbol(sym) => {
                if size == 4 {
                    self.add_relocation(sym, R_386_32, 0);
                    self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                } else {
                    return Err("symbol immediate only supported for 32-bit mov to memory".to_string());
                }
            }
            _ => return Err("unsupported immediate for mov to memory".to_string()),
        }
        Ok(())
    }

    fn encode_movsx(&mut self, ops: &[Operand], src_size: u8, dst_size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movsx requires 2 operands".to_string());
        }

        if dst_size == 2 { self.bytes.push(0x66); }

        let opcode = match src_size {
            1 => vec![0x0F, 0xBE],
            2 => vec![0x0F, 0xBF],
            _ => return Err(format!("unsupported movsx src size: {}", src_size)),
        };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.bytes.extend_from_slice(&opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
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

        if dst_size == 2 { self.bytes.push(0x66); }

        let opcode = match src_size {
            1 => vec![0x0F, 0xB6],
            2 => vec![0x0F, 0xB7],
            _ => return Err(format!("unsupported movzx src size: {}", src_size)),
        };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.bytes.extend_from_slice(&opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.bytes.extend_from_slice(&opcode);
                self.encode_modrm_mem(dst_num, mem)?;
            }
            _ => return Err("unsupported movzx operands".to_string()),
        }
        Ok(())
    }

    fn encode_lea(&mut self, ops: &[Operand], _size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("lea requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
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
                self.bytes.push(0x50 + num);
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
            Operand::Immediate(ImmediateValue::Symbol(sym)) => {
                self.bytes.push(0x68);
                self.add_relocation(sym, R_386_32, 0);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            Operand::Memory(mem) => {
                self.bytes.push(0xFF);
                self.encode_modrm_mem(6, mem)
            }
            _ => Err("unsupported push operand".to_string()),
        }
    }

    fn encode_push16(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("pushw requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Immediate(ImmediateValue::Integer(val)) => {
                self.bytes.push(0x66);
                if *val >= -128 && *val <= 127 {
                    self.bytes.push(0x6A);
                    self.bytes.push(*val as u8);
                } else {
                    self.bytes.push(0x68);
                    self.bytes.extend_from_slice(&(*val as i16).to_le_bytes());
                }
                Ok(())
            }
            _ => Err("unsupported pushw operand".to_string()),
        }
    }

    fn encode_pop(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("pop requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                self.bytes.push(0x58 + num);
                Ok(())
            }
            _ => Err("unsupported pop operand".to_string()),
        }
    }

    fn encode_alu(&mut self, ops: &[Operand], mnemonic: &str, alu_op: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }

        let size = mnemonic_size_suffix(mnemonic).unwrap_or(4);

        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(dst)) => {
                let val = *val;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;

                if size == 2 { self.bytes.push(0x66); }

                if size == 1 {
                    self.bytes.push(0x80);
                    self.bytes.push(self.modrm(3, alu_op, dst_num));
                    self.bytes.push(val as u8);
                } else if val >= -128 && val <= 127 {
                    self.bytes.push(0x83);
                    self.bytes.push(self.modrm(3, alu_op, dst_num));
                    self.bytes.push(val as u8);
                } else {
                    if dst_num == 0 {
                        // Short form: op eax, imm32
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
            (Operand::Immediate(ImmediateValue::Symbol(sym)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                if dst_num == 0 {
                    self.bytes.push(0x05 + alu_op * 8);
                } else {
                    self.bytes.push(0x81);
                    self.bytes.push(self.modrm(3, alu_op, dst_num));
                }
                self.add_relocation(sym, R_386_32, 0);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;

                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0x00 } else { 0x01 } + alu_op * 8);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0x02 } else { 0x03 } + alu_op * 8);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0x00 } else { 0x01 } + alu_op * 8);
                self.encode_modrm_mem(src_num, mem)
            }
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Memory(mem)) => {
                let val = *val;
                if size == 2 { self.bytes.push(0x66); }

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

        let size = mnemonic_size_suffix(mnemonic).unwrap_or(4);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0x84 } else { 0x85 });
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(dst)) => {
                let val = *val;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                if size == 2 { self.bytes.push(0x66); }

                if size == 1 {
                    if dst_num == 0 {
                        self.bytes.push(0xA8);
                    } else {
                        self.bytes.push(0xF6);
                        self.bytes.push(self.modrm(3, 0, dst_num));
                    }
                    self.bytes.push(val as u8);
                } else {
                    if dst_num == 0 {
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
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Memory(mem)) => {
                let val = *val;
                if size == 2 { self.bytes.push(0x66); }
                if size == 1 {
                    self.bytes.push(0xF6);
                } else {
                    self.bytes.push(0xF7);
                }
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
            1 => self.encode_unary_rm(ops, 5, size),
            2 => {
                match (&ops[0], &ops[1]) {
                    (Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        self.bytes.extend_from_slice(&[0x0F, 0xAF]);
                        self.bytes.push(self.modrm(3, dst_num, src_num));
                        Ok(())
                    }
                    (Operand::Memory(mem), Operand::Register(dst)) => {
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        self.bytes.extend_from_slice(&[0x0F, 0xAF]);
                        self.encode_modrm_mem(dst_num, mem)
                    }
                    _ => Err("unsupported imul operands".to_string()),
                }
            }
            3 => {
                match (&ops[0], &ops[1], &ops[2]) {
                    (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
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
                    (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Memory(mem), Operand::Register(dst)) => {
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        if *val >= -128 && *val <= 127 {
                            self.bytes.push(0x6B);
                            self.encode_modrm_mem(dst_num, mem)?;
                            self.bytes.push(*val as u8);
                        } else {
                            self.bytes.push(0x69);
                            self.encode_modrm_mem(dst_num, mem)?;
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
        if size == 2 { self.bytes.push(0x66); }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                self.bytes.push(if size == 1 { 0xF6 } else { 0xF7 });
                self.bytes.push(self.modrm(3, op_ext, num));
                Ok(())
            }
            Operand::Memory(mem) => {
                self.bytes.push(if size == 1 { 0xF6 } else { 0xF7 });
                self.encode_modrm_mem(op_ext, mem)
            }
            _ => Err("unsupported unary operand".to_string()),
        }
    }

    /// Encode inc/dec instructions.
    /// In 32-bit mode, inc/dec have compact single-byte encodings for 32-bit registers:
    ///   inc: 0x40+reg, dec: 0x48+reg
    /// For memory operands or byte/word sizes, use opcode 0xFE (byte) / 0xFF (word/dword)
    /// with modrm extension /0 (inc) or /1 (dec).
    fn encode_inc_dec(&mut self, ops: &[Operand], op_ext: u8, size: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("inc/dec requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                if size == 4 {
                    // Use compact single-byte encoding: 0x40+reg (inc) or 0x48+reg (dec)
                    let base = if op_ext == 0 { 0x40 } else { 0x48 };
                    self.bytes.push(base + num);
                } else if size == 2 {
                    // 16-bit: operand size prefix + 0x40+reg (inc) or 0x48+reg (dec)
                    self.bytes.push(0x66);
                    let base = if op_ext == 0 { 0x40 } else { 0x48 };
                    self.bytes.push(base + num);
                } else {
                    // 8-bit: use 0xFE /0 (inc) or 0xFE /1 (dec) with modrm
                    self.bytes.push(0xFE);
                    self.bytes.push(self.modrm(3, op_ext, num));
                }
                Ok(())
            }
            Operand::Memory(mem) => {
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xFE } else { 0xFF });
                self.encode_modrm_mem(op_ext, mem)
            }
            _ => Err("unsupported inc/dec operand".to_string()),
        }
    }

    fn encode_shift(&mut self, ops: &[Operand], mnemonic: &str, shift_op: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }

        let size = mnemonic_size_suffix(mnemonic).unwrap_or(4);

        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(count)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let count = *count as u8;

                if size == 2 { self.bytes.push(0x66); }

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
                self.bytes.push(if size == 1 { 0xD2 } else { 0xD3 });
                self.bytes.push(self.modrm(3, shift_op, dst_num));
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(count)), Operand::Memory(mem)) => {
                let count = *count as u8;
                if size == 2 { self.bytes.push(0x66); }
                if count == 1 {
                    self.bytes.push(if size == 1 { 0xD0 } else { 0xD1 });
                    self.encode_modrm_mem(shift_op, mem)?;
                } else {
                    self.bytes.push(if size == 1 { 0xC0 } else { 0xC1 });
                    self.encode_modrm_mem(shift_op, mem)?;
                    self.bytes.push(count);
                }
                Ok(())
            }
            (Operand::Register(cl), Operand::Memory(mem)) if cl.name == "cl" => {
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xD2 } else { 0xD3 });
                self.encode_modrm_mem(shift_op, mem)
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    fn encode_double_shift(&mut self, ops: &[Operand], opcode: u8, _size: u8) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("double shift requires 3 operands".to_string());
        }

        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(count)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, opcode]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                self.bytes.push(*count as u8);
                Ok(())
            }
            (Operand::Register(cl), Operand::Register(src), Operand::Register(dst)) if cl.name == "cl" => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, opcode + 1]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            _ => Err("unsupported double shift operands".to_string()),
        }
    }

    fn encode_bswap(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("bswap requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0xC8 + num]);
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
            "lzcntl" => (0xF3u8, [0x0F, 0xBD]),
            "tzcntl" => (0xF3, [0x0F, 0xBC]),
            "popcntl" => (0xF3, [0x0F, 0xB8]),
            _ => return Err(format!("unknown bit count: {}", mnemonic)),
        };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(prefix);
                self.bytes.extend_from_slice(&opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    fn encode_bsr_bsf(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }

        let opcode = match mnemonic {
            "bsrl" => [0x0F, 0xBD],
            "bsfl" => [0x0F, 0xBC],
            _ => return Err(format!("unknown bit scan: {}", mnemonic)),
        };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    fn encode_bt(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }

        let (opcode_rr, ext) = match mnemonic {
            "btl" => (0xA3u8, 4u8),
            "btsl" => (0xAB, 5),
            "btrl" => (0xB3, 6),
            "btcl" => (0xBB, 7),
            _ => return Err(format!("unknown bt instruction: {}", mnemonic)),
        };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, opcode_rr]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0xBA]);
                self.bytes.push(self.modrm(3, ext, dst_num));
                self.bytes.push(*val as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Memory(mem)) => {
                self.bytes.extend_from_slice(&[0x0F, 0xBA]);
                self.encode_modrm_mem(ext, mem)?;
                self.bytes.push(*val as u8);
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
                self.bytes.extend_from_slice(&[0x0F, 0x90 + cc]);
                self.bytes.push(self.modrm(3, 0, num));
                Ok(())
            }
            Operand::Memory(mem) => {
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

        let without_prefix = &mnemonic[4..];
        let cc_str = &without_prefix[..without_prefix.len()-1]; // strip 'l' suffix
        let cc = cc_from_mnemonic(cc_str)?;

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0x40 + cc]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
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
                self.bytes.push(0xE9);
                self.add_relocation(label, R_386_PC32, -4);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            Operand::Indirect(inner) => {
                match inner.as_ref() {
                    Operand::Register(reg) => {
                        let num = reg_num(&reg.name).ok_or("bad register")?;
                        self.bytes.push(0xFF);
                        self.bytes.push(self.modrm(3, 4, num));
                        Ok(())
                    }
                    Operand::Memory(mem) => {
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
                self.bytes.extend_from_slice(&[0x0F, 0x80 + cc]);
                self.add_relocation(label, R_386_PC32, -4);
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
                let reloc_type = if label.ends_with("@PLT") {
                    R_386_PLT32
                } else {
                    R_386_PC32
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
                        self.bytes.push(0xFF);
                        self.bytes.push(self.modrm(3, 2, num));
                        Ok(())
                    }
                    Operand::Memory(mem) => {
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
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(4);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0x86 } else { 0x87 });
                self.encode_modrm_mem(src_num, mem)
            }
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
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
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(4);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
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
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(4);

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
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
                self.bytes.extend_from_slice(&[0x0F, 0xAE]);
                self.encode_modrm_mem(7, mem)
            }
            _ => Err("clflush requires memory operand".to_string()),
        }
    }

    fn encode_int(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("int requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Immediate(ImmediateValue::Integer(val)) => {
                if *val == 3 {
                    self.bytes.push(0xCC);
                } else {
                    self.bytes.push(0xCD);
                    self.bytes.push(*val as u8);
                }
                Ok(())
            }
            _ => Err("int requires immediate operand".to_string()),
        }
    }

    // ---- SSE encoding helpers (same opcodes, no REX) ----

    fn encode_sse_rr_rm(&mut self, ops: &[Operand], load_opcode: &[u8], store_opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE mov requires 2 operands".to_string());
        }

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) && is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(load_opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(load_opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(store_opcode);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported SSE mov operands".to_string()),
        }
    }

    fn encode_sse_op(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE op requires 2 operands".to_string());
        }

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported SSE op operands".to_string()),
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
                self.bytes.extend_from_slice(opcode);
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
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&dst.name) && !is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x66, 0x0F, 0x6E]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) && !is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x66, 0x0F, 0x7E]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x66, 0x0F, 0x6E]);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x66, 0x0F, 0x7E]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported movd operands".to_string()),
        }
    }

    fn encode_sse_cvt_gp_to_xmm(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("cvt requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported cvt operands".to_string()),
        }
    }

    fn encode_sse_cvt_xmm_to_gp(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("cvt requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            _ => Err("unsupported cvt operands".to_string()),
        }
    }

    fn encode_sse_shift(&mut self, ops: &[Operand], _reg_opcode: &[u8], imm_ext: u8, imm_opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE shift requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(imm_opcode);
                self.bytes.push(self.modrm(3, imm_ext, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) => {
                self.encode_sse_op(&[ops[0].clone(), ops[1].clone()], _reg_opcode)
            }
            _ => Err("unsupported SSE shift operands".to_string()),
        }
    }

    // ---- x87 FPU encoding (identical to x86-64, no REX needed) ----

    fn encode_x87_mem(&mut self, ops: &[Operand], opcode: &[u8], ext: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("x87 mem op requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(ext, mem)
            }
            _ => Err("x87 mem op requires memory operand".to_string()),
        }
    }

    fn encode_fcomip(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 2 {
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

    fn encode_fxch(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("fxch requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let n = parse_st_num(&reg.name)?;
                self.bytes.extend_from_slice(&[0xD9, 0xC8 + n]);
                Ok(())
            }
            _ => Err("fxch requires st register".to_string()),
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
