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
    /// For symbol difference expressions (A - B): the subtracted symbol.
    pub diff_symbol: Option<String>,
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
pub const R_386_TLS_LE_32: u32 = 37;
#[allow(dead_code)]
pub const R_386_TLS_IE: u32 = 15;
pub const R_386_TLS_GD: u32 = 18;
pub const R_386_TLS_LDM: u32 = 19;
pub const R_386_TLS_LDO_32: u32 = 32;
#[allow(dead_code)]
pub const R_386_TLS_GOTIE: u32 = 16;
pub const R_386_32S: u32 = 38; // R_386_TLS_LE (negative offset from TP)

/// Register encoding (3-bit register number in ModR/M and SIB).
fn reg_num(name: &str) -> Option<u8> {
    match name {
        "al" | "ax" | "eax" | "xmm0" | "mm0" | "st" | "st(0)" | "ymm0" => Some(0),
        "cl" | "cx" | "ecx" | "xmm1" | "mm1" | "st(1)" | "ymm1" => Some(1),
        "dl" | "dx" | "edx" | "xmm2" | "mm2" | "st(2)" | "ymm2" => Some(2),
        "bl" | "bx" | "ebx" | "xmm3" | "mm3" | "st(3)" | "ymm3" => Some(3),
        "ah" | "sp" | "esp" | "xmm4" | "mm4" | "st(4)" | "ymm4" => Some(4),
        "ch" | "bp" | "ebp" | "xmm5" | "mm5" | "st(5)" | "ymm5" => Some(5),
        "dh" | "si" | "esi" | "xmm6" | "mm6" | "st(6)" | "ymm6" => Some(6),
        "bh" | "di" | "edi" | "xmm7" | "mm7" | "st(7)" | "ymm7" => Some(7),
        _ => None,
    }
}

/// Get segment register number (es=0, cs=1, ss=2, ds=3, fs=4, gs=5).
fn seg_reg_num(name: &str) -> Option<u8> {
    match name {
        "es" => Some(0),
        "cs" => Some(1),
        "ss" => Some(2),
        "ds" => Some(3),
        "fs" => Some(4),
        "gs" => Some(5),
        _ => None,
    }
}

/// Is this a segment register?
fn is_segment_reg(name: &str) -> bool {
    matches!(name, "es" | "cs" | "ss" | "ds" | "fs" | "gs")
}

/// Is this a control register?
fn is_control_reg(name: &str) -> bool {
    matches!(name, "cr0" | "cr2" | "cr3" | "cr4")
}

/// Get control register number.
fn control_reg_num(name: &str) -> Option<u8> {
    match name {
        "cr0" => Some(0),
        "cr2" => Some(2),
        "cr3" => Some(3),
        "cr4" => Some(4),
        _ => None,
    }
}

/// Is this an XMM register?
fn is_xmm(name: &str) -> bool {
    name.starts_with("xmm")
}

/// Is this an MM (MMX) register?
fn is_mm(name: &str) -> bool {
    name.starts_with("mm") && !name.starts_with("mmx")
}

/// Infer operand size from register name for unsuffixed instructions.
fn reg_size(name: &str) -> u8 {
    match name {
        "al" | "ah" | "bl" | "bh" | "cl" | "ch" | "dl" | "dh" => 1,
        "ax" | "bx" | "cx" | "dx" | "sp" | "bp" | "si" | "di" => 2,
        "es" | "cs" | "ss" | "ds" | "fs" | "gs" => 2,
        _ => 4, // eax, ebx, etc. default to 32-bit on i686
    }
}

/// Get operand size from mnemonic suffix.
fn mnemonic_size_suffix(mnemonic: &str) -> Option<u8> {
    match mnemonic {
        "cltd" | "cdq" | "ret" | "nop" | "ud2" | "pause"
        | "mfence" | "lfence" | "sfence" | "clflush"
        | "ldmxcsr" | "stmxcsr"
        | "syscall" | "sysenter" | "cpuid" | "rdtsc" | "rdtscp" | "xgetbv"
        // Base ALU/shift mnemonics whose last letter is NOT a size suffix
        | "sub" | "sbb" | "add" | "and" | "shl" | "rol" | "xadd"
        | "insb" | "insw" | "insl" | "outsb" | "outsw" | "outsl"
        | "outb" | "outw" | "outl" | "inb" | "inw" | "inl"
        | "verw" | "lsl" | "sgdt" | "sidt" | "lgdt" | "lidt"
        | "sgdtl" | "sidtl" | "lgdtl" | "lidtl"
        | "wbinvd" | "invlpg" | "rdpmc"
        | "ljmpl" | "ljmpw" | "ljmp" | "lret" | "lretl" | "lretq" => return None,
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
            // Unsuffixed mov from inline asm - infer size from operands
            "mov" => self.encode_mov_infer_size(ops),
            "movsbl" => self.encode_movsx(ops, 1, 4),
            "movswl" => self.encode_movsx(ops, 2, 4),
            "movsbw" => self.encode_movsx(ops, 1, 2),
            "movzbl" => self.encode_movzx(ops, 1, 4),
            "movzwl" => self.encode_movzx(ops, 2, 4),
            "movzbw" => self.encode_movzx(ops, 1, 2),

            // LEA
            "leal" | "lea" => self.encode_lea(ops, 4),

            // Stack ops (32-bit default)
            "pushl" | "push" => self.encode_push(ops),
            "popl" | "pop" => self.encode_pop(ops),
            // Also handle pushw/popw for 16-bit variants
            "pushw" => self.encode_push16(ops),
            "popw" => self.encode_pop16(ops),

            // Arithmetic
            "addl" | "addw" | "addb" | "add" => self.encode_alu(ops, mnemonic, 0),
            "orl" | "orw" | "orb" | "or" => self.encode_alu(ops, mnemonic, 1),
            "adcl" | "adcw" | "adcb" | "adc" => self.encode_alu(ops, mnemonic, 2),
            "sbbl" | "sbbw" | "sbbb" | "sbb" => self.encode_alu(ops, mnemonic, 3),
            "andl" | "andw" | "andb" | "and" => self.encode_alu(ops, mnemonic, 4),
            "subl" | "subw" | "subb" | "sub" => self.encode_alu(ops, mnemonic, 5),
            "xorl" | "xorw" | "xorb" | "xor" => self.encode_alu(ops, mnemonic, 6),
            "cmpl" | "cmpw" | "cmpb" | "cmp" => self.encode_alu(ops, mnemonic, 7),
            "testl" | "testw" | "testb" | "test" => self.encode_test(ops, mnemonic),

            // Multiply/divide
            "imull" | "imul" => self.encode_imul(ops, 4),
            "mull" | "mul" => self.encode_unary_rm(ops, 4, 4),
            "divl" | "div" => self.encode_unary_rm(ops, 6, 4),
            "idivl" | "idiv" => self.encode_unary_rm(ops, 7, 4),

            // Unary
            "negl" | "neg" => self.encode_unary_rm(ops, 3, 4),
            "negw" => self.encode_unary_rm(ops, 3, 2),
            "negb" => self.encode_unary_rm(ops, 3, 1),
            "notl" | "not" => self.encode_unary_rm(ops, 2, 4),
            "notw" => self.encode_unary_rm(ops, 2, 2),
            "notb" => self.encode_unary_rm(ops, 2, 1),
            "incl" | "inc" => self.encode_inc_dec(ops, 0, 4),
            "incw" => self.encode_inc_dec(ops, 0, 2),
            "incb" => self.encode_inc_dec(ops, 0, 1),
            "decl" | "dec" => self.encode_inc_dec(ops, 1, 4),
            "decw" => self.encode_inc_dec(ops, 1, 2),
            "decb" => self.encode_inc_dec(ops, 1, 1),

            // Shifts
            "shll" | "shlw" | "shlb" | "shl" => self.encode_shift(ops, mnemonic, 4),
            "shrl" | "shrw" | "shrb" | "shr" => self.encode_shift(ops, mnemonic, 5),
            "sarl" | "sarw" | "sarb" | "sar" => self.encode_shift(ops, mnemonic, 7),
            "roll" | "rolw" | "rolb" | "rol" => self.encode_shift(ops, mnemonic, 0),
            "rorl" | "rorw" | "rorb" | "ror" => self.encode_shift(ops, mnemonic, 1),

            // Double-precision shifts
            "shldl" | "shld" => self.encode_double_shift(ops, 0xA4, 4),
            "shrdl" | "shrd" => self.encode_double_shift(ops, 0xAC, 4),

            // Sign extension
            "cltd" | "cdq" => { self.bytes.push(0x99); Ok(()) }
            "cwtl" | "cwde" => { self.bytes.push(0x98); Ok(()) }
            "cbtw" | "cbw" => { self.bytes.extend_from_slice(&[0x66, 0x98]); Ok(()) }
            "cwtd" | "cwd" => { self.bytes.extend_from_slice(&[0x66, 0x99]); Ok(()) }

            // Byte swap
            "bswapl" | "bswap" => self.encode_bswap(ops),

            // Bit operations
            "lzcntl" | "tzcntl" | "popcntl" => self.encode_bit_count(ops, mnemonic),
            "bsrl" | "bsfl" | "bsr" | "bsf" => self.encode_bsr_bsf(ops, mnemonic),
            "bsrw" | "bsfw" => self.encode_bsr_bsf_16(ops, mnemonic),
            "btl" | "btsl" | "btrl" | "btcl" | "bt" | "bts" | "btr" | "btc" => self.encode_bt(ops, mnemonic),

            // Conditional set
            "sete" | "setz" | "setne" | "setnz" | "setl" | "setle" | "setg" | "setge"
            | "setb" | "setc" | "setbe" | "seta" | "setae" | "setnc" | "setnp" | "setp"
            | "sets" | "setns" | "seto" | "setno" => self.encode_setcc(ops, mnemonic),

            // Conditional move
            "cmovel" | "cmovnel" | "cmovll" | "cmovlel" | "cmovgl" | "cmovgel"
            | "cmovbl" | "cmovbel" | "cmoval" | "cmovael"
            | "cmovsl" | "cmovnsl" | "cmovzl" | "cmovnzl" | "cmovpl" | "cmovnpl"
            | "cmovol" | "cmovnol" | "cmovcl" | "cmovncl"
            | "cmovew" | "cmovnew" | "cmovlw" | "cmovlew" | "cmovgw" | "cmovgew"
            | "cmovbw" | "cmovbew" | "cmovaw" | "cmovaew"
            | "cmovsw" | "cmovnsw" | "cmovzw" | "cmovnzw" | "cmovpw" | "cmovnpw"
            | "cmovow" | "cmovnow" | "cmovcw" | "cmovncw"
            | "cmove" | "cmovne" | "cmovl" | "cmovle" | "cmovg" | "cmovge"
            | "cmovb" | "cmovbe" | "cmova" | "cmovae"
            | "cmovs" | "cmovns" | "cmovz" | "cmovnz" | "cmovp" | "cmovnp"
            | "cmovo" | "cmovno" | "cmovc" | "cmovnc" => self.encode_cmovcc(ops, mnemonic),

            // Jumps
            "jmp" => self.encode_jmp(ops),
            "je" | "jz" | "jne" | "jnz" | "jl" | "jle" | "jg" | "jge"
            | "jb" | "jbe" | "ja" | "jae" | "js" | "jns" | "jo" | "jno" | "jp" | "jnp"
            | "jc" | "jnc" => {
                self.encode_jcc(ops, mnemonic)
            }
            // jecxz/jcxz - short jump only (no long form)
            "jecxz" | "jcxz" => {
                if ops.len() != 1 {
                    return Err("jecxz requires 1 operand".to_string());
                }
                match &ops[0] {
                    Operand::Label(_) => {
                        // E3 cb - Jump short if ECX register is 0
                        self.bytes.extend_from_slice(&[0xE3, 0x00]);
                        Ok(())
                    }
                    _ => Err("jecxz requires label operand".to_string()),
                }
            }
            // loop - short jump only (dec ECX, jump if non-zero)
            "loop" => {
                if ops.len() != 1 {
                    return Err("loop requires 1 operand".to_string());
                }
                match &ops[0] {
                    Operand::Label(_) => {
                        // E2 cb - Dec ECX; jump short if ECX != 0
                        self.bytes.extend_from_slice(&[0xE2, 0x00]);
                        Ok(())
                    }
                    _ => Err("loop requires label operand".to_string()),
                }
            }

            // Call/return
            "call" => self.encode_call(ops),
            "ret" => {
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
            // Far jump
            "ljmpl" | "ljmpw" | "ljmp" => self.encode_ljmp(ops),
            // Far return
            "lret" | "lretl" => {
                if ops.is_empty() {
                    self.bytes.push(0xCB);
                } else if let Some(Operand::Immediate(ImmediateValue::Integer(val))) = ops.first() {
                    self.bytes.push(0xCA);
                    self.bytes.extend_from_slice(&(*val as u16).to_le_bytes());
                } else {
                    return Err("unsupported lret operand".to_string());
                }
                Ok(())
            }
            // 64-bit far return (in .code64 sections, encoded with REX.W prefix)
            "lretq" => {
                self.bytes.extend_from_slice(&[0x48, 0xCB]); // REX.W + lret
                Ok(())
            }

            // No-ops and misc
            "nop" => { self.bytes.push(0x90); Ok(()) }
            "ud2" => { self.bytes.extend_from_slice(&[0x0F, 0x0B]); Ok(()) }
            "pause" => { self.bytes.extend_from_slice(&[0xF3, 0x90]); Ok(()) }
            "mfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF0]); Ok(()) }
            "lfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xE8]); Ok(()) }
            "sfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF8]); Ok(()) }
            "clflush" => self.encode_clflush(ops),
            "ldmxcsr" => self.encode_sse_mem_only(ops, 2),
            "stmxcsr" => self.encode_sse_mem_only(ops, 3),
            "int" => self.encode_int(ops),
            "cpuid" => { self.bytes.extend_from_slice(&[0x0F, 0xA2]); Ok(()) }
            "rdtsc" => { self.bytes.extend_from_slice(&[0x0F, 0x31]); Ok(()) }
            "rdtscp" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xF9]); Ok(()) }
            "xgetbv" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xD0]); Ok(()) }
            "syscall" => { self.bytes.extend_from_slice(&[0x0F, 0x05]); Ok(()) }
            "sysenter" => { self.bytes.extend_from_slice(&[0x0F, 0x34]); Ok(()) }
            "hlt" => { self.bytes.push(0xF4); Ok(()) }
            "emms" => { self.bytes.extend_from_slice(&[0x0F, 0x77]); Ok(()) }
            "cmpxchg8b" => self.encode_cmpxchg8b(ops),
            "rdmsr" => { self.bytes.extend_from_slice(&[0x0F, 0x32]); Ok(()) }
            "wrmsr" => { self.bytes.extend_from_slice(&[0x0F, 0x30]); Ok(()) }
            "rdpmc" => { self.bytes.extend_from_slice(&[0x0F, 0x33]); Ok(()) }
            "wbinvd" => { self.bytes.extend_from_slice(&[0x0F, 0x09]); Ok(()) }
            "invlpg" => self.encode_invlpg(ops),
            "verw" => self.encode_verw(ops),
            "lsl" => self.encode_lsl(ops),
            "sgdt" | "sgdtl" | "sidt" | "sidtl" | "lgdt" | "lgdtl" | "lidt" | "lidtl" => self.encode_system_table(ops, mnemonic),

            // Standalone prefix mnemonics (e.g. from "rep; nop" split on semicolon)
            "lock" if ops.is_empty() => { self.bytes.push(0xF0); Ok(()) }
            "rep" | "repe" | "repz" if ops.is_empty() => { self.bytes.push(0xF3); Ok(()) }
            "repnz" | "repne" if ops.is_empty() => { self.bytes.push(0xF2); Ok(()) }

            // String ops
            "movsb" => { self.bytes.push(0xA4); Ok(()) }
            "movsl" if ops.is_empty() => { self.bytes.push(0xA5); Ok(()) }
            "stosb" => { self.bytes.push(0xAA); Ok(()) }
            "stosl" => { self.bytes.push(0xAB); Ok(()) }
            "cmpsb" => { self.bytes.push(0xA6); Ok(()) }
            "cmpsl" => { self.bytes.push(0xA7); Ok(()) }
            "scasb" => { self.bytes.push(0xAE); Ok(()) }
            "scasl" => { self.bytes.push(0xAF); Ok(()) }
            "lodsb" => { self.bytes.push(0xAC); Ok(()) }
            "lodsl" => { self.bytes.push(0xAD); Ok(()) }

            // I/O string ops
            "insb" => { self.bytes.push(0x6C); Ok(()) }
            "insw" => { self.bytes.extend_from_slice(&[0x66, 0x6D]); Ok(()) }
            "insl" => { self.bytes.push(0x6D); Ok(()) }
            "outsb" => { self.bytes.push(0x6E); Ok(()) }
            "outsw" => { self.bytes.extend_from_slice(&[0x66, 0x6F]); Ok(()) }
            "outsl" => { self.bytes.push(0x6F); Ok(()) }

            // Port I/O instructions
            "outb" | "outw" | "outl" => self.encode_out(ops, mnemonic),
            "inb" | "inw" | "inl" => self.encode_in(ops, mnemonic),

            // Atomic exchange
            "xchgb" | "xchgw" | "xchgl" | "xchg" => self.encode_xchg(ops, mnemonic),

            // Lock-prefixed atomics
            "cmpxchgb" | "cmpxchgw" | "cmpxchgl" | "cmpxchg" => self.encode_cmpxchg(ops, mnemonic),
            "xaddb" | "xaddw" | "xaddl" | "xadd" => self.encode_xadd(ops, mnemonic),

            // SSE/SSE2 floating-point
            "movss" => self.encode_sse_rr_rm(ops, &[0xF3, 0x0F, 0x10], &[0xF3, 0x0F, 0x11]),
            "movsd" => self.encode_sse_rr_rm(ops, &[0xF2, 0x0F, 0x10], &[0xF2, 0x0F, 0x11]),
            "movd" => self.encode_movd(ops),
            "movq" => self.encode_movq(ops),
            "movdqu" => self.encode_sse_rr_rm(ops, &[0xF3, 0x0F, 0x6F], &[0xF3, 0x0F, 0x7F]),
            "movupd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x10], &[0x66, 0x0F, 0x11]),
            "movups" => self.encode_sse_rr_rm(ops, &[0x0F, 0x10], &[0x0F, 0x11]),
            "movaps" => self.encode_sse_rr_rm(ops, &[0x0F, 0x28], &[0x0F, 0x29]),
            "movdqa" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x6F], &[0x66, 0x0F, 0x7F]),
            "movlps" => self.encode_sse_op(ops, &[0x0F, 0x12]),
            "movhps" => self.encode_sse_op(ops, &[0x0F, 0x16]),
            "movlpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x12]),
            "movhpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x16]),

            // Non-temporal stores
            "movnti" | "movntil" => self.encode_movnti(ops),
            "movntdq" => self.encode_sse_store_only(ops, &[0x66, 0x0F, 0xE7]),

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
            "maxsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5F]),
            "maxss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5F]),
            "minsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5D]),
            "minss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5D]),
            "ucomisd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x2E]),
            "ucomiss" => self.encode_sse_op(ops, &[0x0F, 0x2E]),
            "comisd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x2F]),
            "comiss" => self.encode_sse_op(ops, &[0x0F, 0x2F]),
            "xorpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x57]),
            "xorps" => self.encode_sse_op(ops, &[0x0F, 0x57]),
            "andpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x54]),
            "andps" => self.encode_sse_op(ops, &[0x0F, 0x54]),
            "andnpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x55]),
            "andnps" => self.encode_sse_op(ops, &[0x0F, 0x55]),
            "orpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x56]),
            "orps" => self.encode_sse_op(ops, &[0x0F, 0x56]),
            "unpcklps" => self.encode_sse_op(ops, &[0x0F, 0x14]),
            "unpckhps" => self.encode_sse_op(ops, &[0x0F, 0x15]),
            "unpcklpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x14]),
            "unpckhpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x15]),
            "shufps" => self.encode_sse_op_imm8(ops, &[0x0F, 0xC6]),
            "shufpd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0xC6]),
            "cmpsd" => self.encode_sse_op_imm8(ops, &[0xF2, 0x0F, 0xC2]),
            "cmpss" => self.encode_sse_op_imm8(ops, &[0xF3, 0x0F, 0xC2]),
            "cmppd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0xC2]),
            "cmpps" => self.encode_sse_op_imm8(ops, &[0x0F, 0xC2]),
            "pclmulqdq" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x44]),

            // AES-NI
            "aesenc" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDC]),
            "aesenclast" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDD]),
            "aesdec" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDE]),
            "aesdeclast" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDF]),
            "aesimc" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDB]),
            "aeskeygenassist" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0xDF]),

            // SSE conversions (32-bit integer operand size for i686)
            "cvtsd2ss" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5A]),
            "cvtss2sd" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5A]),
            "cvtsi2sdl" | "cvtsi2sd" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF2, 0x0F, 0x2A]),
            "cvtsi2ssl" | "cvtsi2ss" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF3, 0x0F, 0x2A]),
            "cvttsd2sil" | "cvttsd2si" | "cvtsd2sil" | "cvtsd2si" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2C]),
            "cvttss2sil" | "cvttss2si" | "cvtss2sil" | "cvtss2si" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2C]),
            "cvtps2pd" => self.encode_sse_op(ops, &[0x0F, 0x5A]),
            "cvtpd2ps" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5A]),
            "cvtdq2ps" => self.encode_sse_op(ops, &[0x0F, 0x5B]),
            "cvtps2dq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5B]),
            "cvttps2dq" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5B]),
            "cvtdq2pd" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0xE6]),
            "cvtpd2dq" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0xE6]),

            // SSE packed integer
            "pshufd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x70]),
            "pshuflw" => self.encode_sse_op_imm8(ops, &[0xF2, 0x0F, 0x70]),
            "pshufhw" => self.encode_sse_op_imm8(ops, &[0xF3, 0x0F, 0x70]),
            "pxor" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEF]),
            "pand" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDB]),
            "por" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEB]),
            "pandn" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDF]),
            "pcmpeqb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x74]),
            "pcmpeqd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x76]),
            "pcmpeqw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x75]),
            "pcmpgtb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x64]),
            "pcmpgtd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x66]),
            "pcmpgtw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x65]),
            "pmovmskb" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x66, 0x0F, 0xD7]),
            "movmskps" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x0F, 0x50]),
            "movmskpd" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x66, 0x0F, 0x50]),

            // SSE packed arithmetic
            "paddb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFC]),
            "paddw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFD]),
            "paddd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFE]),
            "paddq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD4]),
            "psubb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF8]),
            "psubw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF9]),
            "psubd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFA]),
            "psubq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFB]),
            "pmullw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD5]),
            "pmulld" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x40]),
            "pmulhw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE5]),
            "pmulhuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE4]),
            "pmuludq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF4]),
            "paddusb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDC]),
            "paddusw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDD]),
            "psubusb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD8]),
            "psubusw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD9]),
            "paddsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEC]),
            "paddsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xED]),
            "psubsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE8]),
            "psubsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE9]),
            "pmaxub" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDE]),
            "pmaxsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEE]),
            "pminub" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDA]),
            "pminsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEA]),
            "pavgb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE0]),
            "pavgw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE3]),
            "psadbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF6]),
            "pmaddwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF5]),
            "pabsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1C]),
            "pabsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1D]),
            "pabsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1E]),

            // SSE pack/unpack
            "punpcklbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x60]),
            "punpcklwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x61]),
            "punpckldq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x62]),
            "punpcklqdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6C]),
            "punpckhbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x68]),
            "punpckhwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x69]),
            "punpckhdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6A]),
            "punpckhqdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6D]),
            "packsswb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x63]),
            "packssdw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6B]),
            "packuswb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x67]),
            "packusdw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x2B]),

            // SSE insert/extract
            "pextrw" => self.encode_pextrw(ops),
            "pinsrw" => self.encode_pinsrw(ops),

            // SSE shifts
            "pslld" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF2], 6, &[0x66, 0x0F, 0x72]),
            "psrld" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD2], 2, &[0x66, 0x0F, 0x72]),
            "psrad" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xE2], 4, &[0x66, 0x0F, 0x72]),
            "psllq" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF3], 6, &[0x66, 0x0F, 0x73]),
            "psrlq" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD3], 2, &[0x66, 0x0F, 0x73]),
            "psllw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF1], 6, &[0x66, 0x0F, 0x71]),
            "psrlw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD1], 2, &[0x66, 0x0F, 0x71]),
            "psraw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xE1], 4, &[0x66, 0x0F, 0x71]),
            // pslldq/psrldq: byte shifts (only immediate form)
            "pslldq" => self.encode_sse_byte_shift(ops, 7),
            "psrldq" => self.encode_sse_byte_shift(ops, 3),

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
            "fistpq" => self.encode_x87_mem(ops, &[0xDF], 7),
            "fisttpq" => self.encode_x87_mem(ops, &[0xDD], 1),
            "fisttpl" => self.encode_x87_mem(ops, &[0xDB], 1),
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
            "fabs" => { self.bytes.extend_from_slice(&[0xD9, 0xE1]); Ok(()) }
            "fsqrt" => { self.bytes.extend_from_slice(&[0xD9, 0xFA]); Ok(()) }
            "fsin" => { self.bytes.extend_from_slice(&[0xD9, 0xFE]); Ok(()) }
            "fcos" => { self.bytes.extend_from_slice(&[0xD9, 0xFF]); Ok(()) }
            "fpatan" => { self.bytes.extend_from_slice(&[0xD9, 0xF3]); Ok(()) }
            "fptan" => { self.bytes.extend_from_slice(&[0xD9, 0xF2]); Ok(()) }
            "fprem" => { self.bytes.extend_from_slice(&[0xD9, 0xF8]); Ok(()) }
            "fprem1" => { self.bytes.extend_from_slice(&[0xD9, 0xF5]); Ok(()) }
            "frndint" => { self.bytes.extend_from_slice(&[0xD9, 0xFC]); Ok(()) }
            "fscale" => { self.bytes.extend_from_slice(&[0xD9, 0xFD]); Ok(()) }
            "f2xm1" => { self.bytes.extend_from_slice(&[0xD9, 0xF0]); Ok(()) }
            "fyl2x" => { self.bytes.extend_from_slice(&[0xD9, 0xF1]); Ok(()) }
            "fyl2xp1" => { self.bytes.extend_from_slice(&[0xD9, 0xF9]); Ok(()) }
            "fld1" => { self.bytes.extend_from_slice(&[0xD9, 0xE8]); Ok(()) }
            "fldl2e" => { self.bytes.extend_from_slice(&[0xD9, 0xEA]); Ok(()) }
            "fldl2t" => { self.bytes.extend_from_slice(&[0xD9, 0xE9]); Ok(()) }
            "fldlg2" => { self.bytes.extend_from_slice(&[0xD9, 0xEC]); Ok(()) }
            "fldln2" => { self.bytes.extend_from_slice(&[0xD9, 0xED]); Ok(()) }
            "fldpi" => { self.bytes.extend_from_slice(&[0xD9, 0xEB]); Ok(()) }
            "fldz" => { self.bytes.extend_from_slice(&[0xD9, 0xEE]); Ok(()) }
            "fnstsw" => self.encode_fnstsw(ops),
            "fnstcw" => self.encode_x87_mem(ops, &[0xD9], 7),
            "fldcw" => self.encode_x87_mem(ops, &[0xD9], 5),
            "fwait" | "wait" => { self.bytes.push(0x9B); Ok(()) }
            "fnclex" => { self.bytes.extend_from_slice(&[0xDB, 0xE2]); Ok(()) }
            "fninit" => { self.bytes.extend_from_slice(&[0xDB, 0xE3]); Ok(()) }
            "ftst" => { self.bytes.extend_from_slice(&[0xD9, 0xE4]); Ok(()) }
            "fxam" => { self.bytes.extend_from_slice(&[0xD9, 0xE5]); Ok(()) }
            "fcomip" => self.encode_fcomip(ops),
            "fucomip" => self.encode_fucomip(ops),
            "fucomi" => self.encode_fucomi(ops),
            "fucomp" => self.encode_fucomp(ops),
            "fucom" => self.encode_fucom(ops),
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
            "fsubrs" => self.encode_x87_mem(ops, &[0xD8], 5),
            "fdivrs" => self.encode_x87_mem(ops, &[0xD8], 7),

            // x87 register-register arithmetic (fadd/fmul/fsub/fdiv with st(i) operands)
            "fadd" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xC0),
            "fmul" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xC8),
            "fsub" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xE0),
            "fdiv" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xF0),

            // x87 additional
            "fxtract" => { self.bytes.extend_from_slice(&[0xD9, 0xF4]); Ok(()) }
            "fnstenv" => self.encode_x87_mem(ops, &[0xD9], 6),
            "fldenv" => self.encode_x87_mem(ops, &[0xD9], 4),
            "fistl" => self.encode_x87_mem(ops, &[0xDB], 2),
            "fistps" => self.encode_x87_mem(ops, &[0xDF], 3),
            "fildll" => self.encode_x87_mem(ops, &[0xDF], 5),
            "fisttpll" => self.encode_x87_mem(ops, &[0xDD], 1),
            "fistpll" => self.encode_x87_mem(ops, &[0xDF], 7),
            "fstcw" => {
                // fstcw = fwait + fnstcw
                self.bytes.push(0x9B); // FWAIT
                self.encode_x87_mem(ops, &[0xD9], 7)
            }

            // Flag manipulation
            "cld" => { self.bytes.push(0xFC); Ok(()) }
            "std" => { self.bytes.push(0xFD); Ok(()) }
            "clc" => { self.bytes.push(0xF8); Ok(()) }
            "stc" => { self.bytes.push(0xF9); Ok(()) }
            "cmc" => { self.bytes.push(0xF5); Ok(()) }
            "cli" => { self.bytes.push(0xFA); Ok(()) }
            "sti" => { self.bytes.push(0xFB); Ok(()) }
            "sahf" => { self.bytes.push(0x9E); Ok(()) }
            "lahf" => { self.bytes.push(0x9F); Ok(()) }
            "pushf" | "pushfl" => { self.bytes.push(0x9C); Ok(()) }
            "popf" | "popfl" => { self.bytes.push(0x9D); Ok(()) }

            // Leave (stack frame teardown)
            "leave" => { self.bytes.push(0xC9); Ok(()) }

            // int3 (explicit breakpoint mnemonic)
            "int3" => { self.bytes.push(0xCC); Ok(()) }

            // Endbr32 (CET)
            "endbr32" => { self.bytes.extend_from_slice(&[0xF3, 0x0F, 0x1E, 0xFB]); Ok(()) }

            // SSE packed float arithmetic
            "addpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x58]),
            "subpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5C]),
            "mulpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x59]),
            "divpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5E]),
            "addps" => self.encode_sse_op(ops, &[0x0F, 0x58]),
            "subps" => self.encode_sse_op(ops, &[0x0F, 0x5C]),
            "mulps" => self.encode_sse_op(ops, &[0x0F, 0x59]),
            "divps" => self.encode_sse_op(ops, &[0x0F, 0x5E]),

            // SSE3 horizontal operations
            "haddpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x7C]),
            "hsubpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x7D]),
            "haddps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x7C]),
            "hsubps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x7D]),
            "addsubpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD0]),
            "addsubps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0xD0]),

            // SSSE3
            "palignr" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0F]),
            "pshufb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x00]),
            "phaddw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x01]),
            "phaddd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x02]),
            "phsubw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x05]),
            "phsubd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x06]),
            "pmulhrsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x0B]),

            // SSE4.1 blend
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

            // SSE4.1 test / min-max
            "ptest" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x17]),
            "pminsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x38]),
            "pmaxsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3C]),
            "pminuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3A]),
            "pmaxuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3E]),
            "pminud" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3B]),
            "pmaxud" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3F]),
            "pminsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x39]),
            "pmaxsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3D]),
            "phminposuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x41]),

            // SSE4.1 insert/extract (32-bit and byte)
            "pinsrd" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x22]),
            "pextrd" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x16]),
            "pinsrb" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x20]),
            "pextrb" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x14]),

            // SSE4.1/SSE4.2 packed integer extensions
            "pcmpgtq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x37]),

            // SSE4.1 zero/sign extend
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

            // SSE data movement (missing from i686)
            "movapd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x28], &[0x66, 0x0F, 0x29]),
            "movhlps" => self.encode_sse_op(ops, &[0x0F, 0x12]),
            "movlhps" => self.encode_sse_op(ops, &[0x0F, 0x16]),
            "movddup" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x12]),
            "movshdup" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x16]),
            "movsldup" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x12]),
            "movntps" => self.encode_sse_store_only(ops, &[0x0F, 0x2B]),
            "movntpd" => self.encode_sse_store_only(ops, &[0x66, 0x0F, 0x2B]),

            // Prefetch instructions
            "prefetcht0" => self.encode_prefetch(ops, 1),
            "prefetcht1" => self.encode_prefetch(ops, 2),
            "prefetcht2" => self.encode_prefetch(ops, 3),
            "prefetchnta" => self.encode_prefetch(ops, 0),
            "prefetchw" => self.encode_prefetch_0f0d(ops, 1),

            // Rotate through carry
            "rclb" | "rclw" | "rcll" | "rcl" => self.encode_shift(ops, mnemonic, 2),
            "rcrb" | "rcrw" | "rcrl" | "rcr" => self.encode_shift(ops, mnemonic, 3),

            // 16-bit string operations
            "movsw" => { self.bytes.extend_from_slice(&[0x66, 0xA5]); Ok(()) }
            "stosw" => { self.bytes.extend_from_slice(&[0x66, 0xAB]); Ok(()) }
            "lodsw" => { self.bytes.extend_from_slice(&[0x66, 0xAD]); Ok(()) }
            "scasw" => { self.bytes.extend_from_slice(&[0x66, 0xAF]); Ok(()) }
            "cmpsw" => { self.bytes.extend_from_slice(&[0x66, 0xA7]); Ok(()) }

            // Additional multiply/divide sizes
            "mulb" => self.encode_unary_rm(ops, 4, 1),
            "mulw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 4, 2) }
            "divw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 6, 2) }
            "divb" => self.encode_unary_rm(ops, 6, 1),
            "idivw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 7, 2) }
            "idivb" => self.encode_unary_rm(ops, 7, 1),
            "imulw" => self.encode_imul(ops, 2),

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
    ///
    /// For i686 REL relocations, the relocation offset must point to the
    /// displacement field (where the addend is embedded), not to the ModR/M
    /// byte. So we defer add_relocation() until after emitting ModR/M and SIB.
    /// Emit segment override prefix if the memory operand has a segment.
    fn emit_segment_prefix(&mut self, mem: &MemoryOperand) {
        if let Some(ref seg) = mem.segment {
            match seg.as_str() {
                "fs" => self.bytes.push(0x64),
                "gs" => self.bytes.push(0x65),
                "es" => self.bytes.push(0x26),
                "cs" => self.bytes.push(0x2E),
                "ss" => self.bytes.push(0x36),
                "ds" => self.bytes.push(0x3E),
                _ => {}
            }
        }
    }

    fn encode_modrm_mem(&mut self, reg_field: u8, mem: &MemoryOperand) -> Result<(), String> {
        let base = mem.base.as_ref();
        let index = mem.index.as_ref();

        // Parse displacement but defer relocation until after ModR/M+SIB bytes
        let (disp_val, has_symbol, pending_reloc) = match &mem.displacement {
            Displacement::None => (0i64, false, None),
            Displacement::Integer(v) => (*v, false, None),
            Displacement::Symbol(sym) => {
                (0i64, true, Some((sym.clone(), R_386_32, 0i64)))
            }
            Displacement::SymbolAddend(sym, addend) => {
                (0i64, true, Some((sym.clone(), R_386_32, *addend)))
            }
            Displacement::SymbolPlusOffset(sym, offset) => {
                (0i64, true, Some((sym.clone(), R_386_32, *offset)))
            }
            Displacement::SymbolMod(sym, modifier) => {
                let reloc_type = self.tls_reloc_type(modifier);
                (0i64, true, Some((sym.clone(), reloc_type, 0i64)))
            }
        };

        // No base register - need SIB with no-base encoding or direct displacement
        if base.is_none() && index.is_none() {
            // Direct memory reference - mod=00, rm=101 (disp32)
            self.bytes.push(self.modrm(0, reg_field, 5));
            // Emit relocation now, pointing at the displacement bytes
            if let Some((sym, reloc_type, addend)) = pending_reloc {
                self.add_relocation(&sym, reloc_type, addend);
            }
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
        } else if (-128..=127).contains(&disp_val) {
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
                // Emit relocation after ModR/M+SIB, before displacement
                if let Some((sym, reloc_type, addend)) = pending_reloc {
                    self.add_relocation(&sym, reloc_type, addend);
                }
                self.bytes.extend_from_slice(&(disp_val as i32).to_le_bytes());
            } else {
                self.bytes.push(self.modrm(mod_bits, reg_field, 4));
                self.bytes.push(self.sib(scale, idx_num, base_num));
                // Emit relocation after ModR/M+SIB, before displacement
                if let Some((sym, reloc_type, addend)) = pending_reloc {
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
            // Emit relocation after ModR/M, before displacement
            if let Some((sym, reloc_type, addend)) = pending_reloc {
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
        // Strip @PLT suffix from symbol names - the suffix only affects relocation type,
        // not the symbol name in the ELF symbol table.
        let (sym, rtype) = if let Some(base) = symbol.strip_suffix("@PLT") {
            let plt_type = if reloc_type == R_386_PC32 { R_386_PLT32 } else { reloc_type };
            (base, plt_type)
        } else {
            (symbol, reloc_type)
        };
        self.relocations.push(Relocation {
            offset: self.bytes.len() as u64,
            symbol: sym.to_string(),
            reloc_type: rtype,
            addend,
            diff_symbol: None,
        });
    }

    fn add_relocation_with_diff(&mut self, symbol: &str, reloc_type: u32, addend: i64, diff_sym: &str) {
        self.relocations.push(Relocation {
            offset: self.bytes.len() as u64,
            symbol: symbol.to_string(),
            reloc_type,
            addend,
            diff_symbol: Some(diff_sym.to_string()),
        });
    }

    // ---- Instruction-specific encoders ----

    fn encode_mov(&mut self, ops: &[Operand], size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("mov requires 2 operands, got {}", ops.len()));
        }

        // Check for control register moves
        if let (Operand::Register(r1), Operand::Register(r2)) = (&ops[0], &ops[1]) {
            if is_control_reg(&r1.name) || is_control_reg(&r2.name) {
                return self.encode_mov_cr(ops);
            }
            if is_segment_reg(&r1.name) || is_segment_reg(&r2.name) {
                return self.encode_mov_seg(ops);
            }
        }
        // Check for segment register moves involving memory
        if let (Operand::Register(r), Operand::Memory(_)) = (&ops[0], &ops[1]) {
            if is_segment_reg(&r.name) {
                return self.encode_mov_seg(ops);
            }
        }
        if let (Operand::Memory(_), Operand::Register(r)) = (&ops[0], &ops[1]) {
            if is_segment_reg(&r.name) {
                return self.encode_mov_seg(ops);
            }
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
            // Label as memory source: movl symbol, %reg (absolute address)
            (Operand::Label(label), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or_else(|| format!("bad register: {}", dst.name))?;
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0x8A } else { 0x8B });
                // mod=00, rm=101 for disp32 (no base)
                self.bytes.push(self.modrm(0, dst_num, 5));
                // Check if label is a numeric literal (absolute address)
                if let Ok(addr) = label.parse::<i64>() {
                    self.bytes.extend_from_slice(&(addr as i32).to_le_bytes());
                } else {
                    self.add_relocation(label, R_386_32, 0);
                    self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                }
                Ok(())
            }
            // Label as memory destination: movl %reg, symbol
            (Operand::Register(src), Operand::Label(label)) => {
                let src_num = reg_num(&src.name).ok_or_else(|| format!("bad register: {}", src.name))?;
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0x88 } else { 0x89 });
                self.bytes.push(self.modrm(0, src_num, 5));
                if let Ok(addr) = label.parse::<i64>() {
                    self.bytes.extend_from_slice(&(addr as i32).to_le_bytes());
                } else {
                    self.add_relocation(label, R_386_32, 0);
                    self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                }
                Ok(())
            }
            // movl $imm, symbol (immediate to memory at absolute address)
            (Operand::Immediate(imm), Operand::Label(label)) => {
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xC6 } else { 0xC7 });
                self.bytes.push(self.modrm(0, 0, 5));
                if let Ok(addr) = label.parse::<i64>() {
                    self.bytes.extend_from_slice(&(addr as i32).to_le_bytes());
                } else {
                    self.add_relocation(label, R_386_32, 0);
                    self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                }
                match imm {
                    ImmediateValue::Integer(val) => {
                        match size {
                            1 => self.bytes.push(*val as u8),
                            2 => self.bytes.extend_from_slice(&(*val as i16).to_le_bytes()),
                            4 => self.bytes.extend_from_slice(&(*val as i32).to_le_bytes()),
                            _ => unreachable!(),
                        }
                    }
                    _ => return Err("unsupported immediate for mov to label address".to_string()),
                }
                Ok(())
            }
            _ => Err("unsupported mov operand combination".to_string()),
        }
    }

    /// Handle unsuffixed `mov` from inline asm - infer size from operands
    fn encode_mov_infer_size(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("mov requires 2 operands, got {}", ops.len()));
        }
        // Infer size from register operands
        let size = match (&ops[0], &ops[1]) {
            (Operand::Register(r), _) => reg_size(&r.name),
            (_, Operand::Register(r)) => reg_size(&r.name),
            _ => 4, // default to 32-bit
        };
        self.encode_mov(ops, size)
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
            ImmediateValue::SymbolMod(_, _) | ImmediateValue::SymbolDiff(_, _) => {
                return Err("unsupported immediate type for mov".to_string());
            }
        }
        Ok(())
    }

    fn encode_mov_rr(&mut self, src: &Register, dst: &Register, size: u8) -> Result<(), String> {
        // Handle segment register moves
        if let Some(seg_num) = seg_reg_num(&dst.name) {
            // mov %r16, %sreg (8E /r)
            let src_num = reg_num(&src.name).ok_or_else(|| format!("bad register: {}", src.name))?;
            self.bytes.push(0x8E);
            self.bytes.push(self.modrm(3, seg_num, src_num));
            return Ok(());
        }
        if let Some(seg_num) = seg_reg_num(&src.name) {
            // mov %sreg, %r16 (8C /r)
            let dst_num = reg_num(&dst.name).ok_or_else(|| format!("bad register: {}", dst.name))?;
            self.bytes.push(0x8C);
            self.bytes.push(self.modrm(3, seg_num, dst_num));
            return Ok(());
        }

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
                if is_segment_reg(&reg.name) {
                    // Pop to segment register
                    match reg.name.as_str() {
                        "es" => { self.bytes.push(0x07); Ok(()) }
                        "ss" => { self.bytes.push(0x17); Ok(()) }
                        "ds" => { self.bytes.push(0x1F); Ok(()) }
                        "fs" => { self.bytes.extend_from_slice(&[0x0F, 0xA1]); Ok(()) }
                        "gs" => { self.bytes.extend_from_slice(&[0x0F, 0xA9]); Ok(()) }
                        _ => Err(format!("cannot pop to {}", reg.name)),
                    }
                } else {
                    let num = reg_num(&reg.name).ok_or("bad register")?;
                    self.bytes.push(0x58 + num);
                    Ok(())
                }
            }
            Operand::Memory(mem) => {
                // pop m32: 0x8F /0
                self.bytes.push(0x8F);
                self.encode_modrm_mem(0, mem)
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
                } else if (-128..=127).contains(&val) {
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
                let opcode_len = if dst_num == 0 {
                    self.bytes.push(0x05 + alu_op * 8);
                    1u32
                } else {
                    self.bytes.push(0x81);
                    self.bytes.push(self.modrm(3, alu_op, dst_num));
                    2u32
                };
                // _GLOBAL_OFFSET_TABLE_ requires R_386_GOTPC (PC-relative to GOT).
                // The implicit addend = opcode length so the PC correction works:
                // ebx (= return addr of thunk call) + (GOT + addend - P) = GOT
                if sym == "_GLOBAL_OFFSET_TABLE_" {
                    self.add_relocation(sym, R_386_GOTPC, 0);
                    self.bytes.extend_from_slice(&opcode_len.to_le_bytes());
                } else {
                    self.add_relocation(sym, R_386_32, 0);
                    self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                }
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
            (Operand::Immediate(ImmediateValue::SymbolMod(sym, modifier)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                let reloc_type = self.tls_reloc_type(modifier);
                if dst_num == 0 {
                    self.bytes.push(0x05 + alu_op * 8);
                } else {
                    self.bytes.push(0x81);
                    self.bytes.push(self.modrm(3, alu_op, dst_num));
                }
                self.add_relocation(sym, reloc_type, 0);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::SymbolMod(sym, modifier)), Operand::Memory(mem)) => {
                if size == 2 { self.bytes.push(0x66); }
                let reloc_type = self.tls_reloc_type(modifier);
                self.bytes.push(0x81);
                self.encode_modrm_mem(alu_op, mem)?;
                self.add_relocation(sym, reloc_type, 0);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Symbol(sym)), Operand::Memory(mem)) => {
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(0x81);
                self.encode_modrm_mem(alu_op, mem)?;
                self.add_relocation(sym, R_386_32, 0);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            // Symbol difference immediate: e.g. addl $_DYNAMIC-1b, (%esp)
            // Uses R_386_PC32 with diff_symbol so the ELF writer resolves A - B
            (Operand::Immediate(ImmediateValue::SymbolDiff(sym_a, sym_b)), Operand::Memory(mem)) => {
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(0x81);
                self.encode_modrm_mem(alu_op, mem)?;
                self.add_relocation_with_diff(sym_a, R_386_PC32, 0, sym_b);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::SymbolDiff(sym_a, sym_b)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                if dst_num == 0 {
                    self.bytes.push(0x05 + alu_op * 8);
                } else {
                    self.bytes.push(0x81);
                    self.bytes.push(self.modrm(3, alu_op, dst_num));
                }
                self.add_relocation_with_diff(sym_a, R_386_PC32, 0, sym_b);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            // Label as memory reference: addl %reg, symbol
            (Operand::Register(src), Operand::Label(label)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0x00 } else { 0x01 } + alu_op * 8);
                // Encode as disp32 (mod=00, rm=101)
                self.bytes.push(self.modrm(0, src_num, 5));
                self.add_relocation(label, R_386_32, 0);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            (Operand::Label(label), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0x02 } else { 0x03 } + alu_op * 8);
                self.bytes.push(self.modrm(0, dst_num, 5));
                self.add_relocation(label, R_386_32, 0);
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            // Immediate to label-as-memory: addl $1, global_counter
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Label(label)) => {
                let val = *val;
                if size == 2 { self.bytes.push(0x66); }

                if size == 1 {
                    self.bytes.push(0x80);
                } else if (-128..=127).contains(&val) {
                    self.bytes.push(0x83);
                } else {
                    self.bytes.push(0x81);
                }
                // mod=00, rm=101 for disp32 (no base)
                self.bytes.push(self.modrm(0, alu_op, 5));
                // Handle "symbol+offset" syntax
                if let Some(plus_pos) = label.find('+') {
                    let sym = &label[..plus_pos];
                    let off: i64 = label[plus_pos+1..].parse().unwrap_or(0);
                    self.add_relocation(sym, R_386_32, off);
                } else {
                    self.add_relocation(label, R_386_32, 0);
                }
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                if size == 1 || (-128..=127).contains(&val) {
                    self.bytes.push(val as u8);
                } else if size == 2 {
                    self.bytes.extend_from_slice(&(val as i16).to_le_bytes());
                } else {
                    self.bytes.extend_from_slice(&(val as i32).to_le_bytes());
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
                    // imul $imm, %reg  =>  imul $imm, %reg, %reg (dst = src * imm)
                    (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(dst)) => {
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
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
            // Label as memory reference: incl symbol or incl symbol+4
            Operand::Label(label) => {
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xFE } else { 0xFF });
                // Encode as disp32 (mod=00, rm=101)
                self.bytes.push(self.modrm(0, op_ext, 5));
                // Handle "symbol+offset" syntax
                if let Some(plus_pos) = label.find('+') {
                    let sym = &label[..plus_pos];
                    let off: i64 = label[plus_pos+1..].parse().unwrap_or(0);
                    self.add_relocation(sym, R_386_32, off);
                } else {
                    self.add_relocation(label, R_386_32, 0);
                }
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            _ => Err("unsupported inc/dec operand".to_string()),
        }
    }

    fn encode_shift(&mut self, ops: &[Operand], mnemonic: &str, shift_op: u8) -> Result<(), String> {
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(4);

        // Handle 1-operand form: shrl %eax means shift right by 1
        if ops.len() == 1 {
            match &ops[0] {
                Operand::Register(dst) => {
                    let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                    if size == 2 { self.bytes.push(0x66); }
                    self.bytes.push(if size == 1 { 0xD0 } else { 0xD1 });
                    self.bytes.push(self.modrm(3, shift_op, dst_num));
                    return Ok(());
                }
                Operand::Memory(mem) => {
                    if size == 2 { self.bytes.push(0x66); }
                    self.bytes.push(if size == 1 { 0xD0 } else { 0xD1 });
                    return self.encode_modrm_mem(shift_op, mem);
                }
                _ => return Err(format!("unsupported {} operand", mnemonic)),
            }
        }

        if ops.len() != 2 {
            return Err(format!("{} requires 1 or 2 operands", mnemonic));
        }

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
            "bsrl" | "bsr" => [0x0F, 0xBD],
            "bsfl" | "bsf" => [0x0F, 0xBC],
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
            "btl" | "bt" => (0xA3u8, 4u8),
            "btsl" | "bts" => (0xAB, 5),
            "btrl" | "btr" => (0xB3, 6),
            "btcl" | "btc" => (0xBB, 7),
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
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, opcode_rr]);
                self.encode_modrm_mem(src_num, mem)
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
            // bt $imm, label (treat label as absolute memory reference)
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Label(label)) => {
                self.bytes.extend_from_slice(&[0x0F, 0xBA]);
                // mod=00, rm=101 for disp32 (no base register)
                self.bytes.push(self.modrm(0, ext, 5));
                if let Some(plus_pos) = label.find('+') {
                    let sym = &label[..plus_pos];
                    let off: i64 = label[plus_pos+1..].parse().unwrap_or(0);
                    self.add_relocation(sym, R_386_32, off);
                } else {
                    self.add_relocation(label, R_386_32, 0);
                }
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                self.bytes.push(*val as u8);
                Ok(())
            }
            // bt %reg, label (treat label as absolute memory reference)
            (Operand::Register(src), Operand::Label(label)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, opcode_rr]);
                // mod=00, rm=101 for disp32 (no base register)
                self.bytes.push(self.modrm(0, src_num, 5));
                if let Some(plus_pos) = label.find('+') {
                    let sym = &label[..plus_pos];
                    let off: i64 = label[plus_pos+1..].parse().unwrap_or(0);
                    self.add_relocation(sym, R_386_32, off);
                } else {
                    self.add_relocation(label, R_386_32, 0);
                }
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
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
        // Strip size suffix if present, otherwise use as-is (unsuffixed = 32-bit default)
        let (cc_str, is_16bit) = if without_prefix.ends_with('w')
            && without_prefix != "w"
            && cc_from_mnemonic(&without_prefix[..without_prefix.len()-1]).is_ok()
        {
            (&without_prefix[..without_prefix.len()-1], true)
        } else if without_prefix.ends_with('l')
            && without_prefix != "l"
            && cc_from_mnemonic(&without_prefix[..without_prefix.len()-1]).is_ok()
        {
            (&without_prefix[..without_prefix.len()-1], false)
        } else {
            (without_prefix, false)
        };
        let cc = cc_from_mnemonic(cc_str)?;

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if is_16bit { self.bytes.push(0x66); }
                self.bytes.extend_from_slice(&[0x0F, 0x40 + cc]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if is_16bit { self.bytes.push(0x66); }
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
                // Strip @PLT suffix and use PLT32 relocation (same as call)
                self.bytes.push(0xE9);
                let (sym, reloc_type) = if label.ends_with("@PLT") {
                    (label.trim_end_matches("@PLT"), R_386_PLT32)
                } else {
                    (label.as_str(), R_386_PC32)
                };
                self.add_relocation(sym, reloc_type, -4);
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
                        self.emit_segment_prefix(mem);
                        self.bytes.push(0xFF);
                        self.encode_modrm_mem(4, mem)
                    }
                    _ => Err("unsupported indirect jmp target".to_string()),
                }
            }
            _ => Err("unsupported jmp operand".to_string()),
        }
    }

    /// Encode far jump (ljmpl/ljmp): direct or indirect
    fn encode_ljmp(&mut self, ops: &[Operand]) -> Result<(), String> {
        match ops.len() {
            // ljmpl *mem - indirect far jump through memory (FF /5)
            1 => {
                match &ops[0] {
                    Operand::Indirect(inner) => {
                        match inner.as_ref() {
                            Operand::Memory(mem) => {
                                self.emit_segment_prefix(mem);
                                self.bytes.push(0xFF);
                                self.encode_modrm_mem(5, mem)
                            }
                            Operand::Label(label) => {
                                // ljmpl *symbol - indirect far jump via label
                                self.bytes.push(0xFF);
                                self.bytes.push(self.modrm(0, 5, 5));
                                if let Some(plus_pos) = label.find('+') {
                                    let sym = &label[..plus_pos];
                                    let off: i64 = label[plus_pos+1..].parse().unwrap_or(0);
                                    self.add_relocation(sym, R_386_32, off);
                                } else {
                                    self.add_relocation(label, R_386_32, 0);
                                }
                                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                                Ok(())
                            }
                            _ => Err("ljmp indirect requires memory or label operand".to_string()),
                        }
                    }
                    Operand::Memory(mem) => {
                        // ljmp *mem (without explicit indirect prefix)
                        self.emit_segment_prefix(mem);
                        self.bytes.push(0xFF);
                        self.encode_modrm_mem(5, mem)
                    }
                    _ => Err("ljmp requires indirect memory or segment:offset operands".to_string()),
                }
            }
            // ljmpl $segment, $offset - direct far jump (opcode 0xEA)
            2 => {
                match (&ops[0], &ops[1]) {
                    (Operand::Immediate(ImmediateValue::Integer(seg)), Operand::Immediate(ImmediateValue::Integer(off))) => {
                        self.bytes.push(0xEA);
                        self.bytes.extend_from_slice(&(*off as u32).to_le_bytes());
                        self.bytes.extend_from_slice(&(*seg as u16).to_le_bytes());
                        Ok(())
                    }
                    (Operand::Immediate(ImmediateValue::Integer(seg)), Operand::Immediate(ImmediateValue::Symbol(sym))) => {
                        self.bytes.push(0xEA);
                        self.add_relocation(sym, R_386_32, 0);
                        self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                        self.bytes.extend_from_slice(&(*seg as u16).to_le_bytes());
                        Ok(())
                    }
                    _ => Err("ljmp requires $segment, $offset operands".to_string()),
                }
            }
            _ => Err("ljmp requires 1 or 2 operands".to_string()),
        }
    }

    fn encode_jcc(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("jcc requires 1 operand".to_string());
        }

        let cc = cc_from_mnemonic(&mnemonic[1..])?;

        match &ops[0] {
            Operand::Label(label) => {
                // Strip @PLT suffix and use PLT32 relocation (same as call)
                self.bytes.extend_from_slice(&[0x0F, 0x80 + cc]);
                let (sym, reloc_type) = if label.ends_with("@PLT") {
                    (label.trim_end_matches("@PLT"), R_386_PLT32)
                } else {
                    (label.as_str(), R_386_PC32)
                };
                self.add_relocation(sym, reloc_type, -4);
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
                        self.emit_segment_prefix(mem);
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

    /// Encode SSE memory-only instructions (ldmxcsr, stmxcsr).
    /// Format: 0F AE /ext mem
    fn encode_sse_mem_only(&mut self, ops: &[Operand], ext: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("SSE mem-only op requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.bytes.extend_from_slice(&[0x0F, 0xAE]);
                self.encode_modrm_mem(ext, mem)
            }
            _ => Err("SSE mem-only op requires memory operand".to_string()),
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
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(dst_num, mem)?;
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
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(dst_num, mem)
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
        if ops.is_empty() {
            // fxch with no operand defaults to st(1)
            self.bytes.extend_from_slice(&[0xD9, 0xC9]);
            return Ok(());
        }
        if ops.len() == 1 || ops.len() == 2 {
            // With 1 operand: fxch %st(i)
            // With 2 operands: fxch %st(i), %st (AT&T syntax)
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xD9, 0xC8 + n]);
                    Ok(())
                }
                _ => Err("fxch requires st register".to_string()),
            }
        } else {
            Err("fxch requires 0, 1 or 2 operands".to_string())
        }
    }

    // ---- Additional instruction encoders ----

    /// Encode fnstsw (store FPU status word).
    fn encode_fnstsw(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.is_empty() {
            // fnstsw with no operand defaults to %ax
            self.bytes.extend_from_slice(&[0xDF, 0xE0]);
            return Ok(());
        }
        if ops.len() != 1 {
            return Err("fnstsw requires 0 or 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) if reg.name == "ax" => {
                self.bytes.extend_from_slice(&[0xDF, 0xE0]);
                Ok(())
            }
            Operand::Memory(mem) => {
                self.bytes.push(0xDD);
                self.encode_modrm_mem(7, mem)
            }
            _ => Err("fnstsw requires %ax or memory operand".to_string()),
        }
    }

    /// Encode fucomi (unordered compare and set EFLAGS).
    fn encode_fucomi(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 2 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDB, 0xE8 + n]);
                    Ok(())
                }
                _ => Err("fucomi requires st register".to_string()),
            }
        } else if ops.is_empty() {
            self.bytes.extend_from_slice(&[0xDB, 0xE9]);
            Ok(())
        } else {
            Err("fucomi requires 0 or 2 operands".to_string())
        }
    }

    /// Encode fucomp (unordered compare and pop, sets FPU status word).
    fn encode_fucomp(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 1 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDD, 0xE8 + n]);
                    Ok(())
                }
                _ => Err("fucomp requires st register".to_string()),
            }
        } else if ops.len() == 2 {
            // AT&T syntax: fucomp %st(1), %st  — first operand is the source
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDD, 0xE8 + n]);
                    Ok(())
                }
                _ => Err("fucomp requires st register".to_string()),
            }
        } else if ops.is_empty() {
            // Default: fucomp %st(1)
            self.bytes.extend_from_slice(&[0xDD, 0xE9]);
            Ok(())
        } else {
            Err("fucomp requires 0, 1 or 2 operands".to_string())
        }
    }

    /// Encode fucom (unordered compare, sets FPU status word, no pop).
    fn encode_fucom(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 1 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDD, 0xE0 + n]);
                    Ok(())
                }
                _ => Err("fucom requires st register".to_string()),
            }
        } else if ops.len() == 2 {
            // AT&T syntax: fucom %st(1), %st — first operand is the source
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDD, 0xE0 + n]);
                    Ok(())
                }
                _ => Err("fucom requires st register".to_string()),
            }
        } else if ops.is_empty() {
            // Default: fucom %st(1)
            self.bytes.extend_from_slice(&[0xDD, 0xE1]);
            Ok(())
        } else {
            Err("fucom requires 0, 1 or 2 operands".to_string())
        }
    }

    /// Encode movq for MMX/SSE: 64-bit move between MMX/XMM registers and memory.
    fn encode_movq(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movq requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            // movq xmm -> xmm or mem -> xmm (load): F3 0F 7E
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) && is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0xF3, 0x0F, 0x7E]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0xF3, 0x0F, 0x7E]);
                self.encode_modrm_mem(dst_num, mem)
            }
            // movq xmm -> mem (store): 66 0F D6
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x66, 0x0F, 0xD6]);
                self.encode_modrm_mem(src_num, mem)
            }
            // MMX movq: mm -> mm, mem -> mm, mm -> mem
            (Operand::Register(src), Operand::Register(dst)) if is_mm(&src.name) || is_mm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if is_mm(&dst.name) {
                    // load: 0F 6F
                    self.bytes.extend_from_slice(&[0x0F, 0x6F]);
                    self.bytes.push(self.modrm(3, dst_num, src_num));
                } else {
                    // store: 0F 7F
                    self.bytes.extend_from_slice(&[0x0F, 0x7F]);
                    self.bytes.push(self.modrm(3, src_num, dst_num));
                }
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_mm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0x6F]);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) if is_mm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0x7F]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported movq operands".to_string()),
        }
    }

    /// Encode movnti: non-temporal store from GP register to memory.
    fn encode_movnti(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movnti requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0xC3]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("movnti requires register source, memory destination".to_string()),
        }
    }

    /// Encode SSE store-only instructions (xmm -> mem).
    fn encode_sse_store_only(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE store requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("SSE store requires xmm source and memory destination".to_string()),
        }
    }

    /// Encode pslldq/psrldq (byte shifts, immediate-only).
    fn encode_sse_byte_shift(&mut self, ops: &[Operand], ext: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("pslldq/psrldq requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x66, 0x0F, 0x73]);
                self.bytes.push(self.modrm(3, ext, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("pslldq/psrldq requires immediate and xmm register".to_string()),
        }
    }

    /// Encode cmpxchg8b (compare and exchange 8 bytes).
    fn encode_cmpxchg8b(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("cmpxchg8b requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.bytes.extend_from_slice(&[0x0F, 0xC7]);
                self.encode_modrm_mem(1, mem)
            }
            _ => Err("cmpxchg8b requires memory operand".to_string()),
        }
    }

    /// Encode pextrw (extract word from XMM).
    fn encode_pextrw(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("pextrw requires 3 operands".to_string());
        }
        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x66, 0x0F, 0xC5]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported pextrw operands".to_string()),
        }
    }

    /// Encode pinsrw (insert word into XMM).
    fn encode_pinsrw(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("pinsrw requires 3 operands".to_string());
        }
        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x66, 0x0F, 0xC4]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x66, 0x0F, 0xC4]);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported pinsrw operands".to_string()),
        }
    }

    /// Map TLS modifier string to relocation type.
    fn tls_reloc_type(&self, modifier: &str) -> u32 {
        match modifier {
            "NTPOFF" => R_386_TLS_LE_32,
            "TPOFF" => R_386_32S,
            "TLSGD" => R_386_TLS_GD,
            "TLSLDM" => R_386_TLS_LDM,
            "DTPOFF" => R_386_TLS_LDO_32,
            "GOT" => R_386_GOT32,
            "GOTOFF" => R_386_GOTOFF,
            "PLT" => R_386_PLT32,
            "GOTPC" => R_386_GOTPC,
            "GOTNTPOFF" | "INDNTPOFF" => R_386_TLS_IE,
            _ => R_386_32,
        }
    }

    /// Encode x87 register-register arithmetic (fadd/fmul/fsub/fdiv with st(i) operands).
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
                            // Note: fsub/fdiv swap in DC encoding
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

    /// Encode SSE4.1 insert (pinsrd, pinsrb): $imm8, r/m32, xmm
    fn encode_sse_insert(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("pinsrX requires 3 operands".to_string());
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
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported pinsrX operands".to_string()),
        }
    }

    /// Encode SSE4.1 extract (pextrd, pextrb): $imm8, xmm, r/m32
    fn encode_sse_extract(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("pextrX requires 3 operands".to_string());
        }
        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(src_num, mem)?;
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported pextrX operands".to_string()),
        }
    }

    /// Encode prefetch instructions (0F 18 /hint)
    fn encode_prefetch(&mut self, ops: &[Operand], hint: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("prefetch requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.bytes.extend_from_slice(&[0x0F, 0x18]);
                self.encode_modrm_mem(hint, mem)
            }
            _ => Err("prefetch requires memory operand".to_string()),
        }
    }

    /// Encode prefetchw (0F 0D /1)
    fn encode_prefetch_0f0d(&mut self, ops: &[Operand], hint: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("prefetchw requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.bytes.extend_from_slice(&[0x0F, 0x0D]);
                self.encode_modrm_mem(hint, mem)
            }
            _ => Err("prefetchw requires memory operand".to_string()),
        }
    }

    /// Encode OUT instruction: outb/outw/outl
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

        match (&ops[0], &ops[1]) {
            (Operand::Register(_src), Operand::Register(_dst)) => {
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xEE } else { 0xEF });
                Ok(())
            }
            (Operand::Register(_src), Operand::Immediate(ImmediateValue::Integer(val))) => {
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xE6 } else { 0xE7 });
                self.bytes.push(*val as u8);
                Ok(())
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    /// Encode IN instruction: inb/inw/inl
    fn encode_in(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        let size: u8 = match mnemonic {
            "inb" => 1,
            "inw" => 2,
            "inl" => 4,
            _ => return Err(format!("unknown in mnemonic: {}", mnemonic)),
        };

        if ops.is_empty() {
            if size == 2 { self.bytes.push(0x66); }
            self.bytes.push(if size == 1 { 0xEC } else { 0xED });
            return Ok(());
        }

        if ops.len() != 2 {
            return Err(format!("{} requires 0 or 2 operands", mnemonic));
        }

        match (&ops[0], &ops[1]) {
            (Operand::Register(_src), Operand::Register(_dst)) => {
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xEC } else { 0xED });
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(val)), Operand::Register(_dst)) => {
                if size == 2 { self.bytes.push(0x66); }
                self.bytes.push(if size == 1 { 0xE4 } else { 0xE5 });
                self.bytes.push(*val as u8);
                Ok(())
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    /// Encode INVLPG: 0F 01 /7 (memory operand)
    fn encode_invlpg(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("invlpg requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.bytes.extend_from_slice(&[0x0F, 0x01]);
                self.encode_modrm_mem(7, mem)
            }
            _ => Err("invlpg requires memory operand".to_string()),
        }
    }

    /// Encode VERW: 0F 00 /5
    fn encode_verw(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("verw requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
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

    /// Encode LSL (Load Segment Limit): 0F 03 /r
    fn encode_lsl(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("lsl requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let is_16 = matches!(src.name.as_str(), "ax"|"bx"|"cx"|"dx"|"si"|"di"|"sp"|"bp");
                if is_16 {
                    self.bytes.push(0x66);
                }
                self.bytes.extend_from_slice(&[0x0F, 0x03]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0x03]);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported lsl operands".to_string()),
        }
    }

    /// Encode SGDT/SIDT/LGDT/LIDT: 0F 01 /N (memory operand)
    fn encode_system_table(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 1 {
            return Err(format!("{} requires 1 operand", mnemonic));
        }
        // Strip optional 'l' suffix (e.g., "lgdtl" -> "lgdt")
        let base = mnemonic.strip_suffix('l').unwrap_or(mnemonic);
        let reg_ext = match base {
            "sgdt" => 0,
            "sidt" => 1,
            "lgdt" => 2,
            "lidt" => 3,
            _ => return Err(format!("unknown system table instruction: {}", mnemonic)),
        };
        match &ops[0] {
            Operand::Memory(mem) => {
                self.bytes.extend_from_slice(&[0x0F, 0x01]);
                self.encode_modrm_mem(reg_ext, mem)
            }
            // Label as absolute memory reference: lgdtl tr_gdt
            Operand::Label(label) => {
                self.bytes.extend_from_slice(&[0x0F, 0x01]);
                // mod=00, rm=101 for disp32 (no base register)
                self.bytes.push(self.modrm(0, reg_ext, 5));
                if let Some(plus_pos) = label.find('+') {
                    let sym = &label[..plus_pos];
                    let off: i64 = label[plus_pos+1..].parse().unwrap_or(0);
                    self.add_relocation(sym, R_386_32, off);
                } else {
                    self.add_relocation(label, R_386_32, 0);
                }
                self.bytes.extend_from_slice(&[0, 0, 0, 0]);
                Ok(())
            }
            _ => Err(format!("{} requires memory operand", mnemonic)),
        }
    }

    /// Encode MOV to/from control register: 0F 20 /r (read) or 0F 22 /r (write)
    fn encode_mov_cr(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("mov cr requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(cr), Operand::Register(gp)) if is_control_reg(&cr.name) => {
                let cr_num = control_reg_num(&cr.name).ok_or("bad control register")?;
                let gp_num = reg_num(&gp.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0x20]);
                self.bytes.push(self.modrm(3, cr_num, gp_num));
                Ok(())
            }
            (Operand::Register(gp), Operand::Register(cr)) if is_control_reg(&cr.name) => {
                let cr_num = control_reg_num(&cr.name).ok_or("bad control register")?;
                let gp_num = reg_num(&gp.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0x22]);
                self.bytes.push(self.modrm(3, cr_num, gp_num));
                Ok(())
            }
            _ => Err("unsupported mov cr operands".to_string()),
        }
    }

    /// Encode MOV to/from segment register
    fn encode_mov_seg(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("mov seg requires 2 operands".to_string());
        }

        let seg_num = |name: &str| -> Option<u8> {
            match name {
                "es" => Some(0),
                "cs" => Some(1),
                "ss" => Some(2),
                "ds" => Some(3),
                "fs" => Some(4),
                "gs" => Some(5),
                _ => None,
            }
        };

        match (&ops[0], &ops[1]) {
            // mov %sreg, %reg32
            (Operand::Register(src), Operand::Register(dst)) if is_segment_reg(&src.name) => {
                let sr = seg_num(&src.name).ok_or("bad segment register")?;
                let gp = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x8C);
                self.bytes.push(self.modrm(3, sr, gp));
                Ok(())
            }
            // mov %reg32, %sreg
            (Operand::Register(src), Operand::Register(dst)) if is_segment_reg(&dst.name) => {
                let gp = reg_num(&src.name).ok_or("bad register")?;
                let sr = seg_num(&dst.name).ok_or("bad segment register")?;
                self.bytes.push(0x8E);
                self.bytes.push(self.modrm(3, sr, gp));
                Ok(())
            }
            // mov %sreg, mem
            (Operand::Register(src), Operand::Memory(mem)) if is_segment_reg(&src.name) => {
                let sr = seg_num(&src.name).ok_or("bad segment register")?;
                self.bytes.push(0x8C);
                self.encode_modrm_mem(sr, mem)
            }
            // mov mem, %sreg
            (Operand::Memory(mem), Operand::Register(dst)) if is_segment_reg(&dst.name) => {
                let sr = seg_num(&dst.name).ok_or("bad segment register")?;
                self.bytes.push(0x8E);
                self.encode_modrm_mem(sr, mem)
            }
            _ => Err("unsupported mov seg operands".to_string()),
        }
    }

    /// Encode popw (16-bit pop)
    fn encode_pop16(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("popw requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                if is_segment_reg(&reg.name) {
                    // Segment register pops don't use 0x66 prefix
                    match reg.name.as_str() {
                        "es" => { self.bytes.push(0x07); Ok(()) }
                        "ss" => { self.bytes.push(0x17); Ok(()) }
                        "ds" => { self.bytes.push(0x1F); Ok(()) }
                        "fs" => { self.bytes.extend_from_slice(&[0x0F, 0xA1]); Ok(()) }
                        "gs" => { self.bytes.extend_from_slice(&[0x0F, 0xA9]); Ok(()) }
                        _ => Err(format!("cannot pop to {}", reg.name)),
                    }
                } else {
                    let num = reg_num(&reg.name).ok_or("bad register")?;
                    self.bytes.push(0x66);
                    self.bytes.push(0x58 + num);
                    Ok(())
                }
            }
            _ => Err("unsupported popw operand".to_string()),
        }
    }

    /// Encode 16-bit BSF/BSR: bsfw/bsrw
    fn encode_bsr_bsf_16(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }
        let opcode = match mnemonic {
            "bsrw" => [0x0F, 0xBD],
            "bsfw" => [0x0F, 0xBC],
            _ => return Err(format!("unknown bit scan: {}", mnemonic)),
        };
        self.bytes.push(0x66); // 16-bit operand size prefix
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
