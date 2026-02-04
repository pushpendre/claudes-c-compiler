//! AArch64 instruction encoder.
//!
//! Encodes AArch64 instructions into 32-bit machine code words.
//! This covers the subset of instructions emitted by our codegen.
//!
//! AArch64 instructions are always 4 bytes (32 bits), little-endian.
//! The encoding format varies by instruction class.

#![allow(dead_code)]

use super::parser::Operand;

/// Result of encoding an instruction.
#[derive(Debug, Clone)]
pub enum EncodeResult {
    /// Successfully encoded as a 4-byte instruction word
    Word(u32),
    /// Instruction needs a relocation to be applied later
    WordWithReloc {
        word: u32,
        reloc: Relocation,
    },
    /// Multiple encoded words (e.g., movz+movk sequence)
    Words(Vec<u32>),
    /// Skip this instruction (e.g., pseudo-instruction handled elsewhere)
    Skip,
}

/// Relocation types for AArch64 ELF
#[derive(Debug, Clone)]
pub enum RelocType {
    /// R_AARCH64_CALL26 - for BL instruction (26-bit PC-relative)
    Call26,
    /// R_AARCH64_JUMP26 - for B instruction (26-bit PC-relative)
    Jump26,
    /// R_AARCH64_ADR_PREL_PG_HI21 - for ADRP (page-relative, bits [32:12])
    AdrpPage21,
    /// R_AARCH64_ADD_ABS_LO12_NC - for ADD :lo12: (low 12 bits)
    AddAbsLo12,
    /// R_AARCH64_LDST8_ABS_LO12_NC
    Ldst8AbsLo12,
    /// R_AARCH64_LDST16_ABS_LO12_NC
    Ldst16AbsLo12,
    /// R_AARCH64_LDST32_ABS_LO12_NC
    Ldst32AbsLo12,
    /// R_AARCH64_LDST64_ABS_LO12_NC
    Ldst64AbsLo12,
    /// R_AARCH64_LDST128_ABS_LO12_NC
    Ldst128AbsLo12,
    /// R_AARCH64_ADR_GOT_PAGE21 - GOT-relative ADRP
    AdrGotPage21,
    /// R_AARCH64_LD64_GOT_LO12_NC - GOT entry LDR
    Ld64GotLo12,
    /// R_AARCH64_TLSLE_ADD_TPREL_HI12
    TlsLeAddTprelHi12,
    /// R_AARCH64_TLSLE_ADD_TPREL_LO12_NC
    TlsLeAddTprelLo12,
    /// R_AARCH64_CONDBR19 - conditional branch, 19-bit offset
    CondBr19,
    /// R_AARCH64_TSTBR14 - test-and-branch, 14-bit offset
    TstBr14,
    /// R_AARCH64_ABS64 - 64-bit absolute
    Abs64,
    /// R_AARCH64_ABS32 - 32-bit absolute
    Abs32,
    /// R_AARCH64_PREL32 - 32-bit PC-relative
    Prel32,
}

impl RelocType {
    /// Get the ELF relocation type number
    pub fn elf_type(&self) -> u32 {
        match self {
            RelocType::Abs64 => 257,           // R_AARCH64_ABS64
            RelocType::Abs32 => 258,           // R_AARCH64_ABS32
            RelocType::Prel32 => 261,          // R_AARCH64_PREL32
            RelocType::Call26 => 283,          // R_AARCH64_CALL26
            RelocType::Jump26 => 282,          // R_AARCH64_JUMP26
            RelocType::AdrpPage21 => 275,      // R_AARCH64_ADR_PREL_PG_HI21
            RelocType::AddAbsLo12 => 277,      // R_AARCH64_ADD_ABS_LO12_NC
            RelocType::Ldst8AbsLo12 => 278,    // R_AARCH64_LDST8_ABS_LO12_NC
            RelocType::Ldst16AbsLo12 => 284,   // R_AARCH64_LDST16_ABS_LO12_NC
            RelocType::Ldst32AbsLo12 => 285,   // R_AARCH64_LDST32_ABS_LO12_NC
            RelocType::Ldst64AbsLo12 => 286,   // R_AARCH64_LDST64_ABS_LO12_NC
            RelocType::Ldst128AbsLo12 => 299,  // R_AARCH64_LDST128_ABS_LO12_NC
            RelocType::AdrGotPage21 => 311,    // R_AARCH64_ADR_GOT_PAGE21
            RelocType::Ld64GotLo12 => 312,     // R_AARCH64_LD64_GOT_LO12_NC
            RelocType::TlsLeAddTprelHi12 => 549, // R_AARCH64_TLSLE_ADD_TPREL_HI12
            RelocType::TlsLeAddTprelLo12 => 550, // R_AARCH64_TLSLE_ADD_TPREL_LO12_NC
            RelocType::CondBr19 => 280,        // R_AARCH64_CONDBR19
            RelocType::TstBr14 => 279,         // R_AARCH64_TSTBR14
        }
    }
}

/// A relocation to be applied.
#[derive(Debug, Clone)]
pub struct Relocation {
    pub reloc_type: RelocType,
    pub symbol: String,
    pub addend: i64,
}

/// Parse a register name to its 5-bit encoding number (0-30, 31 for sp/zr).
pub fn parse_reg_num(name: &str) -> Option<u32> {
    let name = name.to_lowercase();
    match name.as_str() {
        "sp" | "wsp" => Some(31),
        "xzr" | "wzr" => Some(31),
        "lr" => Some(30),
        _ => {
            let prefix = name.chars().next()?;
            match prefix {
                'x' | 'w' | 'd' | 's' | 'q' | 'v' | 'h' | 'b' => {
                    let num: u32 = name[1..].parse().ok()?;
                    if num <= 31 { Some(num) } else { None }
                }
                _ => None,
            }
        }
    }
}

/// Check if a register name is a 64-bit (X) register or SP.
fn is_64bit_reg(name: &str) -> bool {
    let name = name.to_lowercase();
    name.starts_with('x') || name == "sp" || name == "xzr" || name == "lr"
}

/// Check if a register name is a 32-bit (W) register.
fn is_32bit_reg(name: &str) -> bool {
    let name = name.to_lowercase();
    name.starts_with('w') || name == "wsp" || name == "wzr"
}

/// Check if a register is a floating-point/SIMD register.
fn is_fp_reg(name: &str) -> bool {
    let c = name.chars().next().unwrap_or(' ').to_ascii_lowercase();
    matches!(c, 'd' | 's' | 'q' | 'v' | 'h' | 'b')
}

/// Encode a condition code string to 4-bit encoding.
fn encode_cond(cond: &str) -> Option<u32> {
    match cond.to_lowercase().as_str() {
        "eq" => Some(0),
        "ne" => Some(1),
        "cs" | "hs" => Some(2),
        "cc" | "lo" => Some(3),
        "mi" => Some(4),
        "pl" => Some(5),
        "vs" => Some(6),
        "vc" => Some(7),
        "hi" => Some(8),
        "ls" => Some(9),
        "ge" => Some(10),
        "lt" => Some(11),
        "gt" => Some(12),
        "le" => Some(13),
        "al" => Some(14),
        "nv" => Some(15),
        _ => None,
    }
}

/// Encode an AArch64 instruction from its mnemonic and parsed operands.
pub fn encode_instruction(mnemonic: &str, operands: &[Operand], raw_operands: &str) -> Result<EncodeResult, String> {
    let mn = mnemonic.to_lowercase();

    // Handle condition-code suffixed branches: b.eq, b.ne, b.lt, etc.
    if mn.starts_with("b.") {
        let cond = &mn[2..];
        return encode_cond_branch(cond, operands);
    }

    match mn.as_str() {
        // Data processing - register
        "mov" => encode_mov(operands),
        "movz" => encode_movz(operands),
        "movk" => encode_movk(operands),
        "movn" => encode_movn(operands),
        "add" => encode_add_sub(operands, false, false),
        "adds" => encode_add_sub(operands, false, true),
        "sub" => encode_add_sub(operands, true, false),
        "subs" => encode_add_sub(operands, true, true),
        "and" => encode_logical(operands, 0b00),
        "orr" => encode_logical(operands, 0b01),
        "eor" => encode_logical(operands, 0b10),
        "ands" => encode_logical(operands, 0b11),
        "orn" => encode_orn(operands),
        "bics" => encode_bics(operands),
        "mul" => encode_mul(operands),
        "madd" => encode_madd(operands),
        "msub" => encode_msub(operands),
        "udiv" => encode_div(operands, true),
        "sdiv" => encode_div(operands, false),
        "umulh" => encode_umulh(operands),
        "smulh" => encode_smulh(operands),
        "neg" => encode_neg(operands),
        "negs" => encode_negs(operands),
        "mvn" => encode_mvn(operands),
        "adc" => encode_adc(operands, false),
        "adcs" => encode_adc(operands, true),
        "sbc" => encode_sbc(operands, false),
        "sbcs" => encode_sbc(operands, true),

        // Shifts
        "lsl" => encode_shift(operands, 0b00),
        "lsr" => encode_shift(operands, 0b01),
        "asr" => encode_shift(operands, 0b10),
        "ror" => encode_shift(operands, 0b11),

        // Extensions
        "sxtw" => encode_sxtw(operands),
        "sxth" => encode_sxth(operands),
        "sxtb" => encode_sxtb(operands),
        "uxtw" => encode_uxtw(operands),
        "uxth" => encode_uxth(operands),
        "uxtb" => encode_uxtb(operands),

        // Compare
        "cmp" => encode_cmp(operands),
        "cmn" => encode_cmn(operands),
        "tst" => encode_tst(operands),
        "ccmp" => encode_ccmp(operands),

        // Conditional select
        "csel" => encode_csel(operands),
        "csinc" => encode_csinc(operands),
        "csinv" => encode_csinv(operands),
        "csneg" => encode_csneg(operands),
        "cset" => encode_cset(operands),
        "csetm" => encode_csetm(operands),

        // Branches
        "b" => encode_branch(operands),
        "bl" => encode_bl(operands),
        "br" => encode_br(operands),
        "blr" => encode_blr(operands),
        "ret" => encode_ret(operands),
        "cbz" => encode_cbz(operands, false),
        "cbnz" => encode_cbz(operands, true),
        "tbz" => encode_tbz(operands, false),
        "tbnz" => encode_tbz(operands, true),

        // Loads/stores - size determined from register width
        "ldr" => encode_ldr_str_auto(operands, true),
        "str" => encode_ldr_str_auto(operands, false),
        "ldrb" => encode_ldr_str(operands, true, 0b00, false), // byte load
        "strb" => encode_ldr_str(operands, false, 0b00, false),
        "ldrh" => encode_ldr_str(operands, true, 0b01, false), // halfword load
        "strh" => encode_ldr_str(operands, false, 0b01, false),
        "ldrw" | "ldrsw" => encode_ldrsw(operands),
        "ldrsb" => encode_ldrs(operands, 0b00),
        "ldrsh" => encode_ldrs(operands, 0b01),
        "ldp" => encode_ldp_stp(operands, true),
        "stp" => encode_ldp_stp(operands, false),
        "ldxr" => encode_ldxr_stxr(operands, true, None),
        "stxr" => encode_ldxr_stxr(operands, false, None),
        "ldxrb" => encode_ldxr_stxr(operands, true, Some(0b00)),
        "stxrb" => encode_ldxr_stxr(operands, false, Some(0b00)),
        "ldxrh" => encode_ldxr_stxr(operands, true, Some(0b01)),
        "stxrh" => encode_ldxr_stxr(operands, false, Some(0b01)),
        "ldaxr" => encode_ldaxr_stlxr(operands, true, None),
        "stlxr" => encode_ldaxr_stlxr(operands, false, None),
        "ldaxrb" => encode_ldaxr_stlxr(operands, true, Some(0b00)),
        "stlxrb" => encode_ldaxr_stlxr(operands, false, Some(0b00)),
        "ldaxrh" => encode_ldaxr_stlxr(operands, true, Some(0b01)),
        "stlxrh" => encode_ldaxr_stlxr(operands, false, Some(0b01)),
        "ldar" => encode_ldar_stlr(operands, true, None),
        "stlr" => encode_ldar_stlr(operands, false, None),
        "ldarb" => encode_ldar_stlr(operands, true, Some(0b00)),
        "stlrb" => encode_ldar_stlr(operands, false, Some(0b00)),
        "ldarh" => encode_ldar_stlr(operands, true, Some(0b01)),
        "stlrh" => encode_ldar_stlr(operands, false, Some(0b01)),

        // Address computation
        "adrp" => encode_adrp(operands),
        "adr" => encode_adr(operands),

        // Floating point
        "fmov" => encode_fmov(operands),
        "fadd" => encode_fp_arith(operands, 0b0010),
        "fsub" => encode_fp_arith(operands, 0b0011),
        "fmul" => encode_fp_arith(operands, 0b0000),
        "fdiv" => encode_fp_arith(operands, 0b0001),
        "fneg" => encode_fneg(operands),
        "fabs" => encode_fabs(operands),
        "fsqrt" => encode_fsqrt(operands),
        "fcmp" => encode_fcmp(operands),
        "fcvtzs" => encode_fcvt(operands, true),
        "fcvtzu" => encode_fcvt(operands, false),
        "ucvtf" => encode_ucvtf(operands),
        "scvtf" => encode_scvtf(operands),
        "fcvt" => encode_fcvt_precision(operands),

        // NEON/SIMD
        "cnt" => encode_cnt(operands),
        "uaddlv" => encode_uaddlv(operands),
        "cmeq" => encode_neon_three_same(operands, 0b01110, 0b10001, false),
        "cmtst" => encode_neon_three_same(operands, 0b01110, 0b10001, true), // U=0, opcode=10001, but size-variant
        "uqsub" => encode_neon_three_same(operands, 0b11110, 0b00101, false),
        "sqsub" => encode_neon_three_same(operands, 0b01110, 0b00101, false),
        "ushr" => encode_neon_ushr(operands),
        "sshr" => encode_neon_sshr(operands),
        "shl" => encode_neon_shl(operands),
        "sli" => encode_neon_sli(operands),
        "ext" => encode_neon_ext(operands),
        "addv" => encode_neon_addv(operands),
        "umov" => encode_neon_umov(operands),
        "dup" => encode_neon_dup(operands),
        "ins" => encode_neon_ins(operands),
        "not" => encode_neon_not(operands),
        "movi" => encode_neon_movi(operands),
        "bic" => encode_neon_bic(operands),
        "bsl" => encode_neon_bsl(operands),
        "rev64" => encode_neon_rev64(operands),
        "tbl" => encode_neon_tbl(operands),
        "ld1" => encode_neon_ld1(operands),
        "st1" => encode_neon_st1(operands),
        "uzp1" => encode_neon_zip_uzp(operands, 0b001, false),
        "uzp2" => encode_neon_zip_uzp(operands, 0b101, false),
        "zip1" => encode_neon_zip_uzp(operands, 0b011, false),
        "zip2" => encode_neon_zip_uzp(operands, 0b111, false),
        "eor3" => encode_neon_eor3(operands),
        "pmull" => encode_neon_pmull(operands, false),
        "pmull2" => encode_neon_pmull(operands, true),
        "aese" => encode_neon_aes(operands, 0b00100),
        "aesd" => encode_neon_aes(operands, 0b00101),
        "aesmc" => encode_neon_aes(operands, 0b00110),
        "aesimc" => encode_neon_aes(operands, 0b00111),

        // System
        "nop" => Ok(EncodeResult::Word(0xd503201f)),
        "yield" => Ok(EncodeResult::Word(0xd503203f)),
        "clrex" => Ok(EncodeResult::Word(0xd503305f)),
        "dc" => encode_dc(operands, raw_operands),
        "dmb" => encode_dmb(operands),
        "dsb" => encode_dsb(operands),
        "isb" => Ok(EncodeResult::Word(0xd5033fdf)),
        "mrs" => encode_mrs(operands),
        "msr" => encode_msr(operands),
        "svc" => encode_svc(operands),
        "brk" => encode_brk(operands),

        // Bit manipulation
        "clz" => encode_clz(operands),
        "cls" => encode_cls(operands),
        "rbit" => encode_rbit(operands),
        "rev" => encode_rev(operands),
        "rev16" => encode_rev16(operands),
        "rev32" => encode_rev32(operands),

        // CRC32
        "crc32b" | "crc32h" | "crc32w" | "crc32x"
        | "crc32cb" | "crc32ch" | "crc32cw" | "crc32cx" => encode_crc32(mnemonic, operands),

        // Prefetch
        "prfm" => encode_prfm(operands),

        _ => {
            // TODO: handle remaining instructions
            Err(format!("unsupported instruction: {} {}", mnemonic, raw_operands))
        }
    }
}

// ── Encoding helpers ──────────────────────────────────────────────────────

fn get_reg(operands: &[Operand], idx: usize) -> Result<(u32, bool), String> {
    match operands.get(idx) {
        Some(Operand::Reg(name)) => {
            let num = parse_reg_num(name)
                .ok_or_else(|| format!("invalid register: {}", name))?;
            let is_64 = is_64bit_reg(name);
            Ok((num, is_64))
        }
        other => Err(format!("expected register at operand {}, got {:?}", idx, other)),
    }
}

fn get_imm(operands: &[Operand], idx: usize) -> Result<i64, String> {
    match operands.get(idx) {
        Some(Operand::Imm(v)) => Ok(*v),
        other => Err(format!("expected immediate at operand {}, got {:?}", idx, other)),
    }
}

fn get_symbol(operands: &[Operand], idx: usize) -> Result<(String, i64), String> {
    match operands.get(idx) {
        Some(Operand::Symbol(s)) => Ok((s.clone(), 0)),
        Some(Operand::Label(s)) => Ok((s.clone(), 0)),
        Some(Operand::SymbolOffset(s, off)) => Ok((s.clone(), *off)),
        Some(Operand::Modifier { symbol, .. }) => Ok((symbol.clone(), 0)),
        Some(Operand::ModifierOffset { symbol, offset, .. }) => Ok((symbol.clone(), *offset)),
        other => Err(format!("expected symbol at operand {}, got {:?}", idx, other)),
    }
}

fn sf_bit(is_64: bool) -> u32 {
    if is_64 { 1 } else { 0 }
}

// ── MOV ──────────────────────────────────────────────────────────────────

fn encode_mov(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("mov requires 2 operands".to_string());
    }

    // NEON register-to-register move: mov v1.16b, v0.16b -> ORR v1.16b, v0.16b, v0.16b
    if let (Some(Operand::RegArrangement { reg: rd_name, arrangement: arr_d }),
            Some(Operand::RegArrangement { reg: rm_name, arrangement: _arr_m })) =
        (operands.get(0), operands.get(1))
    {
        let rd = parse_reg_num(rd_name).ok_or("invalid NEON rd")?;
        let rm = parse_reg_num(rm_name).ok_or("invalid NEON rm")?;
        let q: u32 = if arr_d == "16b" { 1 } else { 0 };
        // ORR Vd.T, Vm.T, Vm.T: 0 Q 0 01110 10 1 Rm 0 00111 Rn Rd
        let word = (q << 30) | (0b001110 << 24) | (0b10 << 22) | (1 << 21)
            | (rm << 16) | (0b000111 << 10) | (rm << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    // NEON lane insert: mov v0.d[1], x1 -> INS Vd.D[index], Xn
    if let (Some(Operand::RegLane { reg: vd_name, elem_size, index }),
            Some(Operand::Reg(rn_name))) =
        (operands.get(0), operands.get(1))
    {
        let vd = parse_reg_num(vd_name).ok_or("invalid NEON vd")?;
        let rn = parse_reg_num(rn_name).ok_or("invalid rn")?;
        // INS Vd.Ts[index], Rn
        // Encoding: 0 1 0 0 1110 000 imm5 0 0011 1 Rn Rd
        // imm5 encoding depends on element size and index
        let imm5 = match elem_size.as_str() {
            "b" => ((*index & 0xF) << 1) | 0b00001,
            "h" => ((*index & 0x7) << 2) | 0b00010,
            "s" => ((*index & 0x3) << 3) | 0b00100,
            "d" => ((*index & 0x1) << 4) | 0b01000,
            _ => return Err(format!("unsupported element size for ins: {}", elem_size)),
        };
        let word = (0b01001110000u32 << 21) | (imm5 << 16) | (0b000111 << 10) | (rn << 5) | vd;
        return Ok(EncodeResult::Word(word));
    }

    // NEON lane extract: mov x0, v0.d[1] -> UMOV Xd, Vn.D[index]
    if let (Some(Operand::Reg(rd_name)),
            Some(Operand::RegLane { reg: vn_name, elem_size, index })) =
        (operands.get(0), operands.get(1))
    {
        let rd = parse_reg_num(rd_name).ok_or("invalid rd")?;
        let vn = parse_reg_num(vn_name).ok_or("invalid NEON vn")?;
        // UMOV Rd, Vn.Ts[index]
        // Encoding: 0 Q 0 0 1110 000 imm5 0 0111 1 Rn Rd
        let (q, imm5) = match elem_size.as_str() {
            "b" => (0u32, ((*index & 0xF) << 1) | 0b00001),
            "h" => (0, ((*index & 0x7) << 2) | 0b00010),
            "s" => (0, ((*index & 0x3) << 3) | 0b00100),
            "d" => (1, ((*index & 0x1) << 4) | 0b01000),
            _ => return Err(format!("unsupported element size for umov: {}", elem_size)),
        };
        let word = (q << 30) | (0b001110000u32 << 21) | (imm5 << 16) | (0b001111 << 10) | (vn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    // NEON element-to-element move: mov v0.s[3], v1.s[0] -> INS Vd.Ts[i1], Vn.Ts[i2]
    if let (Some(Operand::RegLane { reg: vd_name, elem_size: es_d, index: idx_d }),
            Some(Operand::RegLane { reg: vn_name, elem_size: _es_n, index: idx_n })) =
        (operands.get(0), operands.get(1))
    {
        let vd = parse_reg_num(vd_name).ok_or("invalid NEON vd")?;
        let vn = parse_reg_num(vn_name).ok_or("invalid NEON vn")?;
        // INS Vd.Ts[i1], Vn.Ts[i2]
        // Encoding: 0 1 1 01110 000 imm5 0 imm4 1 Rn Rd
        let (imm5, imm4) = match es_d.as_str() {
            "b" => ((idx_d << 1) | 0b00001, *idx_n),
            "h" => ((idx_d << 2) | 0b00010, idx_n << 1),
            "s" => ((idx_d << 3) | 0b00100, idx_n << 2),
            "d" => ((idx_d << 4) | 0b01000, idx_n << 3),
            _ => return Err(format!("unsupported element size for ins: {}", es_d)),
        };
        let word = (0b01101110000u32 << 21) | (imm5 << 16)
            | (0 << 15) | (imm4 << 11) | (1 << 10) | (vn << 5) | vd;
        return Ok(EncodeResult::Word(word));
    }

    // mov Xd, #imm -> movz or movn
    if let Some(Operand::Imm(imm)) = operands.get(1) {
        let (rd, is_64) = get_reg(operands, 0)?;
        let imm = *imm;

        // Check if it can be a simple MOVZ
        if imm >= 0 && imm <= 0xFFFF {
            let sf = sf_bit(is_64);
            let word = (sf << 31) | (0b10100101 << 23) | ((imm as u32 & 0xFFFF) << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }

        // Negative: try MOVN
        if imm < 0 {
            let not_imm = !imm;
            if not_imm >= 0 && not_imm <= 0xFFFF {
                let sf = sf_bit(is_64);
                let word = (sf << 31) | (0b00100101 << 23) | ((not_imm as u32 & 0xFFFF) << 5) | rd;
                return Ok(EncodeResult::Word(word));
            }
        }

        // Need movz + movk sequence for large immediates
        return encode_mov_wide_imm(rd, is_64, imm as u64);
    }

    // mov Xd, Xm -> ORR Xd, XZR, Xm
    if let (Some(Operand::Reg(rd_name)), Some(Operand::Reg(rm_name))) = (operands.get(0), operands.get(1)) {
        let rd = parse_reg_num(rd_name).ok_or("invalid rd")?;
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;
        let is_64 = is_64bit_reg(rd_name);

        // Check for MOV to/from SP: uses ADD Xd, Xn, #0
        if rd_name.to_lowercase() == "sp" || rm_name.to_lowercase() == "sp" {
            let sf = sf_bit(is_64);
            // ADD Xd, Xn, #0: sf 0 0 10001 00 imm12=0 Rn Rd
            let word = (sf << 31) | (0b10001 << 24) | (0 << 22) | (0 << 10) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }

        let sf = sf_bit(is_64);
        // ORR Rd, XZR, Rm: sf 01 01010 00 0 Rm 000000 11111 Rd
        let word = (sf << 31) | (0b01 << 29) | (0b01010 << 24) | (0 << 21) | (rm << 16) | (0b11111 << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err(format!("unsupported mov operands: {:?}", operands))
}

fn encode_mov_wide_imm(rd: u32, is_64: bool, imm: u64) -> Result<EncodeResult, String> {
    let sf = sf_bit(is_64);
    let mut words = Vec::new();
    let max_hw = if is_64 { 4 } else { 2 };
    let mut first = true;

    for hw in 0..max_hw {
        let chunk = ((imm >> (hw * 16)) & 0xFFFF) as u32;
        if chunk != 0 || (hw == 0 && imm == 0) {
            if first {
                // MOVZ
                let word = (sf << 31) | (0b10100101 << 23) | (hw << 21) | (chunk << 5) | rd;
                words.push(word);
                first = false;
            } else {
                // MOVK
                let word = (sf << 31) | (0b11100101 << 23) | (hw << 21) | (chunk << 5) | rd;
                words.push(word);
            }
        }
    }

    if words.is_empty() {
        // imm is 0
        let word = (sf << 31) | (0b10100101 << 23) | rd;
        words.push(word);
    }

    if words.len() == 1 {
        Ok(EncodeResult::Word(words[0]))
    } else {
        Ok(EncodeResult::Words(words))
    }
}

fn encode_movz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let imm = get_imm(operands, 1)?;
    let sf = sf_bit(is_64);

    // Check for lsl #N shift
    let hw = if operands.len() > 2 {
        if let Some(Operand::Shift { kind, amount }) = operands.get(2) {
            if kind == "lsl" {
                *amount / 16
            } else {
                0
            }
        } else {
            0
        }
    } else {
        0
    };

    let word = (sf << 31) | (0b10100101 << 23) | (hw << 21) | (((imm as u32) & 0xFFFF) << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_movk(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let imm = get_imm(operands, 1)?;
    let sf = sf_bit(is_64);

    let hw = if operands.len() > 2 {
        if let Some(Operand::Shift { kind, amount }) = operands.get(2) {
            if kind == "lsl" {
                *amount / 16
            } else {
                0
            }
        } else {
            0
        }
    } else {
        0
    };

    let word = (sf << 31) | (0b11100101 << 23) | (hw << 21) | (((imm as u32) & 0xFFFF) << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_movn(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let imm = get_imm(operands, 1)?;
    let sf = sf_bit(is_64);

    let hw = if operands.len() > 2 {
        if let Some(Operand::Shift { kind, amount }) = operands.get(2) {
            if kind == "lsl" {
                *amount / 16
            } else {
                0
            }
        } else {
            0
        }
    } else {
        0
    };

    let word = (sf << 31) | (0b00100101 << 23) | (hw << 21) | (((imm as u32) & 0xFFFF) << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── ADD/SUB ──────────────────────────────────────────────────────────────

fn encode_add_sub(operands: &[Operand], is_sub: bool, set_flags: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err(format!("add/sub requires 3 operands, got {}", operands.len()));
    }

    // NEON vector form: ADD/SUB Vd.T, Vn.T, Vm.T
    if let Some(Operand::RegArrangement { .. }) = operands.get(0) {
        if !set_flags {
            return encode_neon_add_sub(operands, is_sub);
        }
    }

    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let op = if is_sub { 1u32 } else { 0u32 };
    let s_bit = if set_flags { 1u32 } else { 0u32 };

    // ADD Rd, Rn, #imm
    if let Some(Operand::Imm(imm)) = operands.get(2) {
        let imm12 = (*imm as u32) & 0xFFF;
        // Check for lsl #12 shift
        let sh = if operands.len() > 3 {
            if let Some(Operand::Shift { kind, amount }) = operands.get(3) {
                if kind == "lsl" && *amount == 12 { 1u32 } else { 0u32 }
            } else { 0u32 }
        } else { 0u32 };
        let word = (sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24) | (sh << 22) | (imm12 << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    // ADD Rd, Rn, :lo12:symbol
    if let Some(Operand::Modifier { kind, symbol }) = operands.get(2) {
        if kind == "lo12" {
            let word = (sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24) | (0 << 22) | (0 << 10) | (rn << 5) | rd;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::AddAbsLo12,
                    symbol: symbol.clone(),
                    addend: 0,
                },
            });
        }
        if kind == "tprel_lo12_nc" {
            let word = (sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24) | (0 << 22) | (0 << 10) | (rn << 5) | rd;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::TlsLeAddTprelLo12,
                    symbol: symbol.clone(),
                    addend: 0,
                },
            });
        }
    }
    if let Some(Operand::ModifierOffset { kind, symbol, offset }) = operands.get(2) {
        if kind == "lo12" {
            let word = (sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24) | (0 << 22) | (0 << 10) | (rn << 5) | rd;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::AddAbsLo12,
                    symbol: symbol.clone(),
                    addend: *offset,
                },
            });
        }
    }

    // ADD Rd, Rn, Rm
    if let Some(Operand::Reg(rm_name)) = operands.get(2) {
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;

        // Check for shifted register: add Xd, Xn, Xm, lsl #N
        let (shift_type, shift_amount) = if let Some(Operand::Shift { kind, amount }) = operands.get(3) {
            let st = match kind.as_str() {
                "lsl" => 0b00u32,
                "lsr" => 0b01,
                "asr" => 0b10,
                _ => 0b00,
            };
            (st, *amount)
        } else {
            (0, 0)
        };

        let word = (sf << 31) | (op << 30) | (s_bit << 29) | (0b01011 << 24) | (shift_type << 22)
            | (0 << 21) | (rm << 16) | ((shift_amount as u32 & 0x3F) << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err(format!("unsupported add/sub operands: {:?}", operands))
}

// ── Logical ──────────────────────────────────────────────────────────────

fn encode_logical(operands: &[Operand], opc: u32) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("logical op requires 3 operands".to_string());
    }

    // NEON vector form: ORR/AND/EOR Vd.T, Vn.T, Vm.T
    if let Some(Operand::RegArrangement { .. }) = operands.get(0) {
        return encode_neon_logical(operands, opc);
    }

    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);

    // AND/ORR/EOR Rd, Rn, #imm (bitmask immediate)
    if let Some(Operand::Imm(imm)) = operands.get(2) {
        if let Some((n, immr, imms)) = encode_bitmask_imm(*imm as u64, is_64) {
            let word = (sf << 31) | (opc << 29) | (0b100100 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }
        return Err(format!("cannot encode bitmask immediate: 0x{:x}", imm));
    }

    // AND/ORR/EOR Rd, Rn, Rm [, shift #amount]
    if let Some(Operand::Reg(rm_name)) = operands.get(2) {
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;

        let (shift_type, shift_amount) = if let Some(Operand::Shift { kind, amount }) = operands.get(3) {
            let st = match kind.as_str() {
                "lsl" => 0b00u32,
                "lsr" => 0b01,
                "asr" => 0b10,
                "ror" => 0b11,
                _ => 0b00,
            };
            (st, *amount)
        } else {
            (0, 0)
        };

        let word = (sf << 31) | (opc << 29) | (0b01010 << 24) | (shift_type << 22) | (0 << 21)
            | (rm << 16) | ((shift_amount as u32 & 0x3F) << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err("unsupported logical operands".to_string())
}

/// Encode a bitmask immediate for AArch64.
/// Returns (N, immr, imms) if the value is a valid bitmask immediate.
fn encode_bitmask_imm(val: u64, is_64: bool) -> Option<(u32, u32, u32)> {
    if val == 0 || (!is_64 && val == 0xFFFFFFFF) || (is_64 && val == u64::MAX) {
        return None; // Not a valid bitmask immediate
    }

    let width = if is_64 { 64 } else { 32 };
    let val = if !is_64 { val & 0xFFFFFFFF } else { val };

    // Try each possible element size: 2, 4, 8, 16, 32, 64
    for size in [2u32, 4, 8, 16, 32, 64] {
        if size > width {
            continue;
        }

        let mask = if size == 64 { u64::MAX } else { (1u64 << size) - 1 };
        let elem = val & mask;

        // Check that the pattern repeats
        let mut repeats = true;
        let mut pos = size;
        while pos < width {
            if ((val >> pos) & mask) != elem {
                repeats = false;
                break;
            }
            pos += size;
        }
        if !repeats {
            continue;
        }

        // Check that elem is a contiguous run of 1s (possibly rotated)
        let ones = elem.count_ones();
        if ones == 0 || ones == size {
            continue; // All zeros or all ones in element
        }

        // Find rotation: rotate elem right until the least significant bit is 1
        // and the run of 1s starts at bit 0
        let mut rotated = elem;
        let mut rotation = 0u32;
        // Rotate right until we find a 0->1 transition at bit 0
        for r in 0..size {
            let rot = ((elem >> r) | (elem << (size - r))) & mask;
            // Check if this is a contiguous run from bit 0
            let run = rot.trailing_ones();
            if run == ones {
                rotated = rot;
                rotation = r;
                break;
            }
        }

        // Now rotated has `ones` consecutive 1s starting at bit 0
        let _ = rotated;

        // Encode the fields
        let n = if size == 64 { 1u32 } else { 0u32 };
        let immr = rotation;
        let imms = match size {
            2 => 0b111100 | (ones - 1),
            4 => 0b111000 | (ones - 1),
            8 => 0b110000 | (ones - 1),
            16 => 0b100000 | (ones - 1),
            32 => 0b000000 | (ones - 1),
            64 => ones - 1,
            _ => unreachable!(),
        };

        return Some((n, immr, imms));
    }

    None
}

// ── MUL/DIV ──────────────────────────────────────────────────────────────

fn encode_mul(operands: &[Operand]) -> Result<EncodeResult, String> {
    // NEON vector form: MUL Vd.T, Vn.T, Vm.T
    if let Some(Operand::RegArrangement { .. }) = operands.get(0) {
        return encode_neon_mul(operands);
    }
    // MUL Rd, Rn, Rm is MADD Rd, Rn, Rm, XZR
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0b0011011000 << 21) | (rm << 16) | (0b11111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_madd(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let (ra, _) = get_reg(operands, 3)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0b0011011000 << 21) | (rm << 16) | (0 << 15) | (ra << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_msub(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let (ra, _) = get_reg(operands, 3)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0b0011011000 << 21) | (rm << 16) | (1 << 15) | (ra << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_div(operands: &[Operand], unsigned: bool) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    let o1 = if unsigned { 0u32 } else { 1u32 };
    // Data-processing (2 source): sf 0 S=0 11010110 Rm 00001 o1 Rn Rd
    let word = (sf << 31) | (0b0011010110 << 21) | (rm << 16)
        | (0b00001 << 11) | (o1 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_umulh(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    // UMULH: 1 00 11011 1 10 Rm 0 11111 Rn Rd
    let word = (1u32 << 31) | (0b0011011110 << 21) | (rm << 16) | (0b011111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_smulh(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    // SMULH: 1 00 11011 0 10 Rm 0 11111 Rn Rd
    let word = (1u32 << 31) | (0b0011011010 << 21) | (rm << 16) | (0b011111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_neg(operands: &[Operand]) -> Result<EncodeResult, String> {
    // NEG Rd, Rm -> SUB Rd, XZR, Rm
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rm, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0b01011 << 24) | (0 << 22) | (0 << 21)
        | (rm << 16) | (0 << 10) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_negs(operands: &[Operand]) -> Result<EncodeResult, String> {
    // NEGS Rd, Rm -> SUBS Rd, XZR, Rm
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rm, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (1 << 29) | (0b01011 << 24) | (0 << 22) | (0 << 21)
        | (rm << 16) | (0 << 10) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_mvn(operands: &[Operand]) -> Result<EncodeResult, String> {
    // MVN Rd, Rm -> ORN Rd, XZR, Rm
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rm, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0b01 << 29) | (0b01010 << 24) | (0 << 22) | (1 << 21)
        | (rm << 16) | (0 << 10) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_adc(operands: &[Operand], set_flags: bool) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    let s = if set_flags { 1u32 } else { 0 };
    let word = (sf << 31) | (0 << 30) | (s << 29) | (0b11010000 << 21) | (rm << 16) | (0 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_sbc(operands: &[Operand], set_flags: bool) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    let s = if set_flags { 1u32 } else { 0 };
    let word = (sf << 31) | (1 << 30) | (s << 29) | (0b11010000 << 21) | (rm << 16) | (0 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Shifts ───────────────────────────────────────────────────────────────

fn encode_shift(operands: &[Operand], shift_type: u32) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;

    // LSL/LSR/ASR Rd, Rn, #imm (immediate form -> UBFM/SBFM)
    if let Some(Operand::Imm(imm)) = operands.get(2) {
        let sf = sf_bit(is_64);
        let imm = *imm as u32;
        let width = if is_64 { 64 } else { 32 };
        let n = if is_64 { 1u32 } else { 0u32 };

        match shift_type {
            0b00 => {
                // LSL #imm -> UBFM Rd, Rn, #(-imm mod width), #(width-1-imm)
                let immr = (width - imm) % width;
                let imms = width - 1 - imm;
                let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
                return Ok(EncodeResult::Word(word));
            }
            0b01 => {
                // LSR #imm -> UBFM Rd, Rn, #imm, #(width-1)
                let immr = imm;
                let imms = width - 1;
                let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
                return Ok(EncodeResult::Word(word));
            }
            0b10 => {
                // ASR #imm -> SBFM Rd, Rn, #imm, #(width-1)
                let immr = imm;
                let imms = width - 1;
                let word = (sf << 31) | (0b00 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
                return Ok(EncodeResult::Word(word));
            }
            _ => {}
        }
    }

    // LSL/LSR/ASR Rd, Rn, Rm (register form)
    if let Some(Operand::Reg(rm_name)) = operands.get(2) {
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;
        let sf = sf_bit(is_64);
        // Data-processing (2 source): sf 0 S=0 11010110 Rm 0010 op2 Rn Rd
        let op2 = shift_type; // 00=LSL, 01=LSR, 10=ASR, 11=ROR
        let word = (sf << 31) | (0b0011010110 << 21) | (rm << 16) | (0b0010 << 12) | (op2 << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err("unsupported shift operands".to_string())
}

// ── Extensions ───────────────────────────────────────────────────────────

fn encode_sxtw(operands: &[Operand]) -> Result<EncodeResult, String> {
    // SXTW Xd, Wn -> SBFM Xd, Xn, #0, #31
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let word = (1u32 << 31) | (0b00 << 29) | (0b100110 << 23) | (1 << 22) | (0 << 16) | (31 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_sxth(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = (sf << 31) | (0b00 << 29) | (0b100110 << 23) | (n << 22) | (0 << 16) | (15 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_sxtb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = (sf << 31) | (0b00 << 29) | (0b100110 << 23) | (n << 22) | (0 << 16) | (7 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_uxtw(operands: &[Operand]) -> Result<EncodeResult, String> {
    // UXTW is MOV Wd, Wn (the upper 32 bits are zeroed)
    // Or: UBFM Xd, Xn, #0, #31
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    // Use 32-bit ORR (MOV alias)
    let word = (0b001010100 << 23) | (rn << 16) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_uxth(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (0 << 16) | (15 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_uxtb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (0 << 16) | (7 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Compare ──────────────────────────────────────────────────────────────

fn encode_cmp(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CMP Rn, op -> SUBS XZR, Rn, op
    let mut new_ops = vec![Operand::Reg("xzr".to_string())];
    new_ops.extend(operands.iter().cloned());
    // Determine if 32-bit or 64-bit from the first operand
    let is_32 = if let Some(Operand::Reg(r)) = operands.get(0) {
        is_32bit_reg(r)
    } else {
        false
    };
    if is_32 {
        new_ops[0] = Operand::Reg("wzr".to_string());
    }
    encode_add_sub(&new_ops, true, true)
}

fn encode_cmn(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CMN Rn, op -> ADDS XZR, Rn, op
    let mut new_ops = vec![Operand::Reg("xzr".to_string())];
    new_ops.extend(operands.iter().cloned());
    let is_32 = if let Some(Operand::Reg(r)) = operands.get(0) {
        is_32bit_reg(r)
    } else {
        false
    };
    if is_32 {
        new_ops[0] = Operand::Reg("wzr".to_string());
    }
    encode_add_sub(&new_ops, false, true)
}

fn encode_tst(operands: &[Operand]) -> Result<EncodeResult, String> {
    // TST Rn, op -> ANDS XZR, Rn, op
    let mut new_ops = vec![Operand::Reg("xzr".to_string())];
    new_ops.extend(operands.iter().cloned());
    let is_32 = if let Some(Operand::Reg(r)) = operands.get(0) {
        is_32bit_reg(r)
    } else {
        false
    };
    if is_32 {
        new_ops[0] = Operand::Reg("wzr".to_string());
    }
    encode_logical(&new_ops, 0b11)
}

fn encode_ccmp(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CCMP Rn, #imm5, #nzcv, cond
    let (rn, is_64) = get_reg(operands, 0)?;
    let sf = sf_bit(is_64);

    if let (Some(Operand::Imm(imm5)), Some(Operand::Imm(nzcv)), Some(Operand::Cond(cond))) =
        (operands.get(1), operands.get(2), operands.get(3))
    {
        let cond_val = encode_cond(cond).ok_or("invalid condition")?;
        let word = (sf << 31) | (1 << 30) | (1 << 29) | (0b11010010 << 21)
            | ((*imm5 as u32 & 0x1F) << 16) | (cond_val << 12) | (1 << 11) | (0 << 10) | (rn << 5) | (*nzcv as u32 & 0xF);
        return Ok(EncodeResult::Word(word));
    }

    // CCMP Rn, Rm, #nzcv, cond
    if let (Some(Operand::Reg(rm_name)), Some(Operand::Imm(nzcv)), Some(Operand::Cond(cond))) =
        (operands.get(1), operands.get(2), operands.get(3))
    {
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;
        let cond_val = encode_cond(cond).ok_or("invalid condition")?;
        let word = (sf << 31) | (1 << 30) | (1 << 29) | (0b11010010 << 21)
            | (rm << 16) | (cond_val << 12) | (0 << 11) | (0 << 10) | (rn << 5) | (*nzcv as u32 & 0xF);
        return Ok(EncodeResult::Word(word));
    }

    Err("unsupported ccmp operands".to_string())
}

// ── Conditional select ───────────────────────────────────────────────────

fn encode_csel(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let cond = match operands.get(3) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("csel requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0 << 30) | (0 << 29) | (0b11010100 << 21)
        | (rm << 16) | (cond << 12) | (0b00 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_csinc(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let cond = match operands.get(3) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("csinc requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0 << 30) | (0 << 29) | (0b11010100 << 21)
        | (rm << 16) | (cond << 12) | (0b01 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_csinv(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let cond = match operands.get(3) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("csinv requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0 << 29) | (0b11010100 << 21)
        | (rm << 16) | (cond << 12) | (0b00 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_csneg(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let cond = match operands.get(3) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("csneg requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0 << 29) | (0b11010100 << 21)
        | (rm << 16) | (cond << 12) | (0b01 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_cset(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CSET Rd, cond -> CSINC Rd, XZR, XZR, invert(cond)
    let (rd, is_64) = get_reg(operands, 0)?;
    let cond = match operands.get(1) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("cset requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let inv_cond = cond ^ 1; // invert least significant bit
    let word = (sf << 31) | (0 << 30) | (0 << 29) | (0b11010100 << 21)
        | (0b11111 << 16) | (inv_cond << 12) | (0b01 << 10) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_csetm(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CSETM Rd, cond -> CSINV Rd, XZR, XZR, invert(cond)
    let (rd, is_64) = get_reg(operands, 0)?;
    let cond = match operands.get(1) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("csetm requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let inv_cond = cond ^ 1;
    let word = (sf << 31) | (1 << 30) | (0 << 29) | (0b11010100 << 21)
        | (0b11111 << 16) | (inv_cond << 12) | (0b00 << 10) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Branches ─────────────────────────────────────────────────────────────

fn encode_branch(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (sym, addend) = get_symbol(operands, 0)?;
    // B: 000101 imm26 (filled by linker/assembler)
    Ok(EncodeResult::WordWithReloc {
        word: 0b000101 << 26,
        reloc: Relocation {
            reloc_type: RelocType::Jump26,
            symbol: sym,
            addend,
        },
    })
}

fn encode_bl(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (sym, addend) = get_symbol(operands, 0)?;
    // BL: 100101 imm26
    Ok(EncodeResult::WordWithReloc {
        word: 0b100101 << 26,
        reloc: Relocation {
            reloc_type: RelocType::Call26,
            symbol: sym,
            addend,
        },
    })
}

fn encode_cond_branch(cond: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    let cond_val = encode_cond(cond).ok_or_else(|| format!("unknown condition: {}", cond))?;
    let (sym, addend) = get_symbol(operands, 0)?;
    // B.cond: 01010100 imm19 0 cond
    let word = (0b01010100 << 24) | cond_val;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::CondBr19,
            symbol: sym,
            addend,
        },
    })
}

fn encode_br(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rn, _) = get_reg(operands, 0)?;
    // BR: 1101011 0000 11111 000000 Rn 00000
    let word = 0xd61f0000 | (rn << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_blr(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rn, _) = get_reg(operands, 0)?;
    // BLR: 1101011 0001 11111 000000 Rn 00000
    let word = 0xd63f0000 | (rn << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_ret(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rn = if operands.is_empty() {
        30 // default to x30 (LR)
    } else {
        get_reg(operands, 0)?.0
    };
    // RET: 1101011 0010 11111 000000 Rn 00000
    let word = 0xd65f0000 | (rn << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_cbz(operands: &[Operand], is_nz: bool) -> Result<EncodeResult, String> {
    let (rt, is_64) = get_reg(operands, 0)?;
    let (sym, addend) = get_symbol(operands, 1)?;
    let sf = sf_bit(is_64);
    let op = if is_nz { 1u32 } else { 0u32 };
    // CBZ/CBNZ: sf 011010 op imm19 Rt
    let word = (sf << 31) | (0b011010 << 25) | (op << 24) | rt;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::CondBr19,
            symbol: sym,
            addend,
        },
    })
}

fn encode_tbz(operands: &[Operand], is_nz: bool) -> Result<EncodeResult, String> {
    let (rt, _) = get_reg(operands, 0)?;
    let bit = get_imm(operands, 1)?;
    let (sym, addend) = get_symbol(operands, 2)?;
    let b5 = ((bit as u32) >> 5) & 1;
    let b40 = (bit as u32) & 0x1F;
    let op = if is_nz { 1u32 } else { 0u32 };
    // TBZ/TBNZ: b5 011011 op b40 imm14 Rt
    let word = (b5 << 31) | (0b011011 << 25) | (op << 24) | (b40 << 19) | rt;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::TstBr14,
            symbol: sym,
            addend,
        },
    })
}

// ── Loads/Stores ─────────────────────────────────────────────────────────

/// Auto-detect LDR/STR size from the first register operand.
fn encode_ldr_str_auto(operands: &[Operand], is_load: bool) -> Result<EncodeResult, String> {
    // Determine size from register: Wn -> 32-bit (size=10), Xn -> 64-bit (size=11)
    // FP: Sn -> 32-bit, Dn -> 64-bit, Qn -> 128-bit
    let reg_name = match operands.get(0) {
        Some(Operand::Reg(r)) => r.to_lowercase(),
        _ => return Err("ldr/str needs register operand".to_string()),
    };

    let size = if reg_name.starts_with('w') {
        0b10 // 32-bit
    } else if reg_name.starts_with('x') || reg_name == "sp" || reg_name == "xzr" || reg_name == "lr" {
        0b11 // 64-bit
    } else if reg_name.starts_with('s') {
        0b10 // 32-bit float
    } else if reg_name.starts_with('d') {
        0b11 // 64-bit float
    } else if reg_name.starts_with('q') {
        0b00 // 128-bit (special encoding with V=1)
    } else {
        0b11 // default 64-bit
    };

    encode_ldr_str(operands, is_load, size, false)
}

fn encode_ldr_str(operands: &[Operand], is_load: bool, size: u32, is_signed: bool) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("ldr/str requires at least 2 operands".to_string());
    }

    let (rt, _) = get_reg(operands, 0)?;
    let fp = is_fp_reg(operands.get(0).map(|o| match o { Operand::Reg(r) => r.as_str(), _ => "" }).unwrap_or(""));

    // Use the size parameter as-is (auto-detection happens in encode_ldr_str_auto)
    let actual_size = size;

    let v = if fp { 1u32 } else { 0u32 };

    match operands.get(1) {
        // [base, #offset]
        Some(Operand::Mem { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;

            // Unsigned offset encoding
            // Size determines the shift for offset alignment
            let shift = actual_size as u32;
            let opc = if is_load {
                if is_signed { 0b10 } else { 0b01 }
            } else {
                0b00
            };

            // Check if offset is aligned and fits in 12-bit unsigned field
            let abs_offset = *offset as u64;
            let align = 1u64 << shift;
            if *offset >= 0 && abs_offset % align == 0 {
                let imm12 = (abs_offset / align) as u32;
                if imm12 < 4096 {
                    // Unsigned offset form: size 111 V 01 opc imm12 Rn Rt
                    let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (0b01 << 24)
                        | (opc << 22) | (imm12 << 10) | (rn << 5) | rt;
                    return Ok(EncodeResult::Word(word));
                }
            }

            // Unscaled offset (LDUR/STUR form)
            let imm9 = (*offset as i32) & 0x1FF;
            let opc = if is_load {
                if is_signed { 0b10 } else { 0b01 }
            } else {
                0b00
            };
            let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (0b00 << 24) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b00 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        // [base, #offset]! (pre-index)
        Some(Operand::MemPreIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let opc = if is_load { 0b01 } else { 0b00 };
            let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (0b00 << 24) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b11 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        // [base], #offset (post-index)
        Some(Operand::MemPostIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let opc = if is_load { 0b01 } else { 0b00 };
            let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (0b00 << 24) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b01 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        // [base, Xm] register offset
        Some(Operand::MemRegOffset { base, index, extend: _, shift: _ }) => {
            // Check if index is a :lo12: modifier
            if index.starts_with(':') {
                // Parse modifier from the index string
                let rn = parse_reg_num(base).ok_or("invalid base reg")?;
                let mod_str = index.trim_start_matches(':');
                let (kind, sym) = if let Some(colon_pos) = mod_str.find(':') {
                    (&mod_str[..colon_pos], &mod_str[colon_pos + 1..])
                } else {
                    return Err(format!("malformed modifier in memory operand: {}", index));
                };

                let (symbol, addend) = if let Some(plus_pos) = sym.find('+') {
                    let s = &sym[..plus_pos];
                    let off: i64 = sym[plus_pos + 1..].parse().unwrap_or(0);
                    (s.to_string(), off)
                } else {
                    (sym.to_string(), 0i64)
                };

                let opc = if is_load { 0b01 } else { 0b00 };

                let reloc_type = match kind {
                    "lo12" => {
                        match actual_size {
                            0b00 => RelocType::Ldst8AbsLo12,
                            0b01 => RelocType::Ldst16AbsLo12,
                            0b10 => RelocType::Ldst32AbsLo12,
                            0b11 => RelocType::Ldst64AbsLo12,
                            _ => RelocType::Ldst64AbsLo12,
                        }
                    }
                    "got_lo12" => RelocType::Ld64GotLo12,
                    _ => return Err(format!("unsupported modifier in load/store: {}", kind)),
                };

                let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (opc << 22)
                    | (0 << 10) | (rn << 5) | rt;
                return Ok(EncodeResult::WordWithReloc {
                    word,
                    reloc: Relocation {
                        reloc_type,
                        symbol,
                        addend,
                    },
                });
            }

            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let rm = parse_reg_num(index).ok_or("invalid index reg")?;
            let opc = if is_load { 0b01 } else { 0b00 };
            // Register offset: size 111 V opc 1 Rm option S 10 Rn Rt
            let option = 0b011u32; // LSL
            let s = 0u32; // no shift
            let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (opc << 22)
                | (1 << 21) | (rm << 16) | (option << 13) | (s << 12) | (0b10 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        _ => {}
    }

    Err(format!("unsupported ldr/str operands: {:?}", operands))
}

fn encode_ldrsw(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("ldrsw requires 2 operands".to_string());
    }

    let (rt, _) = get_reg(operands, 0)?;

    match operands.get(1) {
        Some(Operand::Mem { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            // LDRSW: size=10 111 V=0 01 opc=10 -> unsigned offset
            // Actually: 10 111 0 01 10 imm12 Rn Rt
            let abs_offset = *offset as u64;
            if *offset >= 0 && abs_offset % 4 == 0 {
                let imm12 = (abs_offset / 4) as u32;
                if imm12 < 4096 {
                    let word = (0b10 << 30) | (0b111 << 27) | (0 << 26) | (0b01 << 24) | (0b10 << 22)
                        | (imm12 << 10) | (rn << 5) | rt;
                    return Ok(EncodeResult::Word(word));
                }
            }
            // Unscaled: LDURSW
            let imm9 = (*offset as i32) & 0x1FF;
            let word = (0b10 << 30) | (0b111 << 27) | (0 << 26) | (0b00 << 24) | (0b10 << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b00 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        Some(Operand::MemPostIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let word = (0b10 << 30) | (0b111 << 27) | (0 << 26) | (0b00 << 24) | (0b10 << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b01 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        Some(Operand::MemPreIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let word = (0b10 << 30) | (0b111 << 27) | (0 << 26) | (0b00 << 24) | (0b10 << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b11 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        _ => {}
    }

    Err(format!("unsupported ldrsw operands: {:?}", operands))
}

fn encode_ldrs(operands: &[Operand], size: u32) -> Result<EncodeResult, String> {
    // LDRSB/LDRSH: sign-extending byte/halfword loads
    if operands.len() < 2 {
        return Err("ldrsb/ldrsh requires 2 operands".to_string());
    }

    let (rt, is_64) = get_reg(operands, 0)?;
    let opc = if is_64 { 0b10 } else { 0b11 }; // 64-bit target: opc=10, 32-bit: opc=11

    match operands.get(1) {
        Some(Operand::Mem { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let shift = size;
            let abs_offset = *offset as u64;
            let align = 1u64 << shift;
            if *offset >= 0 && abs_offset % align == 0 {
                let imm12 = (abs_offset / align) as u32;
                if imm12 < 4096 {
                    let word = (size << 30) | (0b111 << 27) | (0 << 26) | (0b01 << 24) | (opc << 22)
                        | (imm12 << 10) | (rn << 5) | rt;
                    return Ok(EncodeResult::Word(word));
                }
            }
            // Unscaled
            let imm9 = (*offset as i32) & 0x1FF;
            let word = (size << 30) | (0b111 << 27) | (0 << 26) | (0b00 << 24) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b00 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }
        _ => {}
    }

    Err(format!("unsupported ldrsb/ldrsh operands: {:?}", operands))
}

fn encode_ldp_stp(operands: &[Operand], is_load: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("ldp/stp requires 3 operands".to_string());
    }

    let (rt1, is_64) = get_reg(operands, 0)?;
    let (rt2, _) = get_reg(operands, 1)?;
    let fp = is_fp_reg(match &operands[0] { Operand::Reg(r) => r.as_str(), _ => "" });

    let opc = if fp {
        let r = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
        if r.starts_with('s') { 0b00 }
        else if r.starts_with('d') { 0b01 }
        else if r.starts_with('q') { 0b10 }
        else { if is_64 { 0b10 } else { 0b00 } }
    } else {
        if is_64 { 0b10 } else { 0b00 }
    };

    let v = if fp { 1u32 } else { 0u32 };
    let l = if is_load { 1u32 } else { 0u32 };

    // Shift depends on register size
    let shift = if fp {
        let r = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
        if r.starts_with('s') { 2 }
        else if r.starts_with('d') { 3 }
        else if r.starts_with('q') { 4 }
        else { if is_64 { 3 } else { 2 } }
    } else {
        if is_64 { 3 } else { 2 }
    };

    match operands.get(2) {
        // STP rt1, rt2, [base, #offset]! (pre-index)
        Some(Operand::MemPreIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm7 = ((*offset >> shift) as i32) & 0x7F;
            let word = (opc << 30) | (0b101 << 27) | (v << 26) | (0b011 << 23) | (l << 22)
                | ((imm7 as u32 & 0x7F) << 15) | (rt2 << 10) | (rn << 5) | rt1;
            return Ok(EncodeResult::Word(word));
        }

        // LDP/STP rt1, rt2, [base], #offset (post-index)
        Some(Operand::MemPostIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm7 = ((*offset >> shift) as i32) & 0x7F;
            let word = (opc << 30) | (0b101 << 27) | (v << 26) | (0b001 << 23) | (l << 22)
                | ((imm7 as u32 & 0x7F) << 15) | (rt2 << 10) | (rn << 5) | rt1;
            return Ok(EncodeResult::Word(word));
        }

        // LDP/STP rt1, rt2, [base, #offset] (signed offset)
        Some(Operand::Mem { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm7 = ((*offset >> shift) as i32) & 0x7F;
            let word = (opc << 30) | (0b101 << 27) | (v << 26) | (0b010 << 23) | (l << 22)
                | ((imm7 as u32 & 0x7F) << 15) | (rt2 << 10) | (rn << 5) | rt1;
            return Ok(EncodeResult::Word(word));
        }

        _ => {}
    }

    Err(format!("unsupported ldp/stp operands: {:?}", operands))
}

// ── Exclusive loads/stores ───────────────────────────────────────────────

/// Encode LDXR/STXR and byte/halfword variants.
/// `forced_size`: None = auto-detect from register width, Some(0b00) = byte, Some(0b01) = halfword
fn encode_ldxr_stxr(operands: &[Operand], is_load: bool, forced_size: Option<u32>) -> Result<EncodeResult, String> {
    if is_load {
        let (rt, is_64) = get_reg(operands, 0)?;
        let rn = match operands.get(1) {
            Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("invalid base")?,
            _ => return Err("ldxr needs memory operand".to_string()),
        };
        let size = forced_size.unwrap_or(if is_64 { 0b11 } else { 0b10 });
        let word = (size << 30) | (0b001000010 << 21) | (0b11111 << 16) | (0 << 15)
            | (0b11111 << 10) | (rn << 5) | rt;
        Ok(EncodeResult::Word(word))
    } else {
        let (ws, _) = get_reg(operands, 0)?;
        let (rt, is_64) = get_reg(operands, 1)?;
        let rn = match operands.get(2) {
            Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("invalid base")?,
            _ => return Err("stxr needs memory operand".to_string()),
        };
        let size = forced_size.unwrap_or(if is_64 { 0b11 } else { 0b10 });
        let word = (size << 30) | (0b001000000 << 21) | (ws << 16) | (0 << 15)
            | (0b11111 << 10) | (rn << 5) | rt;
        Ok(EncodeResult::Word(word))
    }
}

/// Encode LDAXR/STLXR and byte/halfword variants.
fn encode_ldaxr_stlxr(operands: &[Operand], is_load: bool, forced_size: Option<u32>) -> Result<EncodeResult, String> {
    if is_load {
        let (rt, is_64) = get_reg(operands, 0)?;
        let rn = match operands.get(1) {
            Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("invalid base")?,
            _ => return Err("ldaxr needs memory operand".to_string()),
        };
        let size = forced_size.unwrap_or(if is_64 { 0b11 } else { 0b10 });
        let word = (size << 30) | (0b001000010 << 21) | (0b11111 << 16) | (1 << 15)
            | (0b11111 << 10) | (rn << 5) | rt;
        Ok(EncodeResult::Word(word))
    } else {
        let (ws, _) = get_reg(operands, 0)?;
        let (rt, is_64) = get_reg(operands, 1)?;
        let rn = match operands.get(2) {
            Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("invalid base")?,
            _ => return Err("stlxr needs memory operand".to_string()),
        };
        let size = forced_size.unwrap_or(if is_64 { 0b11 } else { 0b10 });
        let word = (size << 30) | (0b001000000 << 21) | (ws << 16) | (1 << 15)
            | (0b11111 << 10) | (rn << 5) | rt;
        Ok(EncodeResult::Word(word))
    }
}

/// Encode LDAR/STLR and byte/halfword variants.
fn encode_ldar_stlr(operands: &[Operand], is_load: bool, forced_size: Option<u32>) -> Result<EncodeResult, String> {
    let (rt, is_64) = get_reg(operands, 0)?;
    let rn = match operands.get(1) {
        Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("invalid base")?,
        _ => return Err("ldar/stlr needs memory operand".to_string()),
    };
    let size = forced_size.unwrap_or(if is_64 { 0b11 } else { 0b10 });
    let l = if is_load { 1u32 } else { 0 };
    // LDAR/STLR: size 001000 1 L 0 11111 1 11111 Rn Rt
    let word = (size << 30) | (0b001000 << 24) | (1 << 23) | (l << 22) | (0 << 21)
        | (0b11111 << 16) | (1 << 15) | (0b11111 << 10) | (rn << 5) | rt;
    Ok(EncodeResult::Word(word))
}

// ── Address computation ──────────────────────────────────────────────────

fn encode_adrp(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;

    let (sym, addend) = match operands.get(1) {
        Some(Operand::Symbol(s)) => (s.clone(), 0i64),
        Some(Operand::Modifier { kind, symbol }) if kind == "got" => {
            // adrp x0, :got:symbol
            let word = (1u32 << 31) | (0b10000 << 24) | rd;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::AdrGotPage21,
                    symbol: symbol.clone(),
                    addend: 0,
                },
            });
        }
        Some(Operand::SymbolOffset(s, off)) => (s.clone(), *off),
        Some(Operand::Label(s)) => (s.clone(), 0i64),
        _ => return Err(format!("adrp needs symbol operand, got {:?}", operands.get(1))),
    };

    // ADRP: 1 immlo[1:0] 10000 immhi[18:0] Rd
    let word = (1u32 << 31) | (0b10000 << 24) | rd;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::AdrpPage21,
            symbol: sym,
            addend,
        },
    })
}

fn encode_adr(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (sym, addend) = get_symbol(operands, 1)?;
    // ADR: 0 immlo[1:0] 10000 immhi[18:0] Rd
    let word = (0u32 << 31) | (0b10000 << 24) | rd;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::AdrpPage21, // TODO: should use ADR reloc
            symbol: sym,
            addend,
        },
    })
}

// ── Floating point ───────────────────────────────────────────────────────

fn encode_fmov(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("fmov requires 2 operands".to_string());
    }

    let (rd_name, rm_name) = match (&operands[0], &operands[1]) {
        (Operand::Reg(a), Operand::Reg(b)) => (a.clone(), b.clone()),
        (Operand::Reg(_a), Operand::Imm(_)) => {
            // TODO: implement fmov with float immediate encoding
            return Err("fmov with immediate operand not yet supported".to_string());
        }
        _ => return Err("fmov needs register operands".to_string()),
    };

    let rd = parse_reg_num(&rd_name).ok_or("invalid rd")?;
    let rm = parse_reg_num(&rm_name).ok_or("invalid rm")?;

    let rd_is_fp = is_fp_reg(&rd_name);
    let rm_is_fp = is_fp_reg(&rm_name);
    let rd_lower = rd_name.to_lowercase();
    let rm_lower = rm_name.to_lowercase();

    if rd_is_fp && rm_is_fp {
        // FMOV between FP registers
        let is_double = rd_lower.starts_with('d') || rm_lower.starts_with('d');
        let ftype = if is_double { 0b01 } else { 0b00 };
        // 0 00 11110 ftype 1 0000 00 10000 Rn Rd
        let word = (0b00011110 << 24) | (ftype << 22) | (0b100000 << 16) | (0b10000 << 10) | (rm << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    if rd_is_fp && !rm_is_fp {
        // FMOV from GP to FP: FMOV Dn, Xn or FMOV Sn, Wn
        let is_double = rd_lower.starts_with('d');
        if is_double {
            // FMOV Dd, Xn: 1 00 11110 01 1 00 111 000000 Rn Rd
            let word = (0b1001111001 << 22) | (0b100111 << 16) | (0 << 10) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        } else {
            // FMOV Sd, Wn: 0 00 11110 00 1 00 111 000000 Rn Rd
            let word = (0b0001111000 << 22) | (0b100111 << 16) | (0 << 10) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }
    }

    if !rd_is_fp && rm_is_fp {
        // FMOV from FP to GP: FMOV Xn, Dn or FMOV Wn, Sn
        let is_double = rm_lower.starts_with('d');
        if is_double {
            // FMOV Xd, Dn: 1 00 11110 01 1 00 110 000000 Rn Rd
            let word = (0b1001111001 << 22) | (0b100110 << 16) | (0 << 10) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        } else {
            // FMOV Wd, Sn: 0 00 11110 00 1 00 110 000000 Rn Rd
            let word = (0b0001111000 << 22) | (0b100110 << 16) | (0 << 10) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }
    }

    Err(format!("unsupported fmov operands: {} -> {}", rd_name, rm_name))
}

fn encode_fp_arith(operands: &[Operand], opcode: u32) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;

    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };

    // 0 00 11110 ftype 1 Rm opcode 10 Rn Rd
    let word = (0b00011110 << 24) | (ftype << 22) | (1 << 21) | (rm << 16) | (opcode << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_fneg(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };
    // FNEG: 0 00 11110 ftype 1 0000 10 10000 Rn Rd
    let word = (0b00011110 << 24) | (ftype << 22) | (0b100001 << 16) | (0b10000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_fabs(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };
    // FABS: 0 00 11110 ftype 1 0000 01 10000 Rn Rd
    let word = (0b00011110 << 24) | (ftype << 22) | (0b100000 << 16) | (0b110000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_fsqrt(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };
    // FSQRT: 0 00 11110 ftype 1 0000 11 10000 Rn Rd
    let word = (0b00011110 << 24) | (ftype << 22) | (0b100001 << 16) | (0b110000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_fcmp(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rn, _) = get_reg(operands, 0)?;
    let rn_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rn_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };

    // FCMP Dn, #0.0
    if operands.len() < 2 || matches!(operands.get(1), Some(Operand::Imm(0))) {
        let word = (0b00011110 << 24) | (ftype << 22) | (1 << 21) | (0 << 16) | (0b001000 << 10) | (rn << 5) | 0b01000;
        return Ok(EncodeResult::Word(word));
    }

    let (rm, _) = get_reg(operands, 1)?;
    // FCMP Dn, Dm: 0 00 11110 ftype 1 Rm 00 1000 Rn 00 000
    let word = (0b00011110 << 24) | (ftype << 22) | (1 << 21) | (rm << 16) | (0b001000 << 10) | (rn << 5) | 0;
    Ok(EncodeResult::Word(word))
}

fn encode_fcvt(operands: &[Operand], is_signed: bool) -> Result<EncodeResult, String> {
    // FCVTZS/FCVTZU: float-to-integer conversion (round toward zero)
    // Encoding: sf 00 11110 ftype 1 11 opcode 000000 Rn Rd
    // sf: 0=W dest, 1=X dest
    // ftype: 00=S source, 01=D source
    // rmode: 11 (toward zero)
    // opcode: 000=signed (FCVTZS), 001=unsigned (FCVTZU)
    if operands.len() < 2 {
        return Err("fcvtzs/fcvtzu requires 2 operands".to_string());
    }
    let (rd, rd_is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;

    let src_name = match &operands[1] {
        Operand::Reg(name) => name.to_lowercase(),
        _ => return Err("fcvtzs/fcvtzu: expected register source".to_string()),
    };
    let ftype: u32 = if src_name.starts_with('d') { 0b01 } else { 0b00 };
    let sf: u32 = if rd_is_64 { 1 } else { 0 };
    let opcode: u32 = if is_signed { 0b000 } else { 0b001 };

    let word = (sf << 31) | (0b00 << 29) | (0b11110 << 24) | (ftype << 22)
        | (1 << 21) | (0b11 << 19) | (opcode << 16) | (0b000000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_ucvtf(operands: &[Operand]) -> Result<EncodeResult, String> {
    encode_int_to_float(operands, false)
}

fn encode_scvtf(operands: &[Operand]) -> Result<EncodeResult, String> {
    encode_int_to_float(operands, true)
}

fn encode_int_to_float(operands: &[Operand], is_signed: bool) -> Result<EncodeResult, String> {
    // SCVTF/UCVTF: integer-to-float conversion
    // Encoding: sf 00 11110 ftype 1 00 opcode 000000 Rn Rd
    // sf: 0=W source, 1=X source
    // ftype: 00=S dest, 01=D dest
    // opcode: 010=signed (SCVTF), 011=unsigned (UCVTF)
    if operands.len() < 2 {
        return Err("scvtf/ucvtf requires 2 operands".to_string());
    }
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, rn_is_64) = get_reg(operands, 1)?;

    let dst_name = match &operands[0] {
        Operand::Reg(name) => name.to_lowercase(),
        _ => return Err("scvtf/ucvtf: expected register dest".to_string()),
    };
    let ftype: u32 = if dst_name.starts_with('d') { 0b01 } else { 0b00 };
    let sf: u32 = if rn_is_64 { 1 } else { 0 };
    let opcode: u32 = if is_signed { 0b010 } else { 0b011 };

    let word = (sf << 31) | (0b00 << 29) | (0b11110 << 24) | (ftype << 22)
        | (1 << 21) | (0b00 << 19) | (opcode << 16) | (0b000000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_fcvt_precision(operands: &[Operand]) -> Result<EncodeResult, String> {
    // FCVT: float precision conversion (e.g., FCVT Dd, Sn or FCVT Sd, Dn)
    // Encoding: 0 00 11110 ftype 1 0001 opc 10000 Rn Rd
    // ftype: source precision (00=S, 01=D, 11=H)
    // opc: dest precision (00=S, 01=D, 11=H)
    if operands.len() < 2 {
        return Err("fcvt requires 2 operands".to_string());
    }
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;

    let dst_name = match &operands[0] {
        Operand::Reg(name) => name.to_lowercase(),
        _ => return Err("fcvt: expected register dest".to_string()),
    };
    let src_name = match &operands[1] {
        Operand::Reg(name) => name.to_lowercase(),
        _ => return Err("fcvt: expected register source".to_string()),
    };

    let ftype: u32 = match src_name.chars().next() {
        Some('s') => 0b00,
        Some('d') => 0b01,
        Some('h') => 0b11,
        _ => return Err(format!("fcvt: unsupported source type: {}", src_name)),
    };
    let opc: u32 = match dst_name.chars().next() {
        Some('s') => 0b00,
        Some('d') => 0b01,
        Some('h') => 0b11,
        _ => return Err(format!("fcvt: unsupported dest type: {}", dst_name)),
    };

    let word = (0b00011110 << 24) | (ftype << 22) | (1 << 21) | (0b0001 << 17)
        | (opc << 15) | (0b10000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON/SIMD ────────────────────────────────────────────────────────────

/// Helper to extract register number from a RegArrangement operand
fn get_neon_reg(operands: &[Operand], idx: usize) -> Result<(u32, String), String> {
    match operands.get(idx) {
        Some(Operand::RegArrangement { reg, arrangement }) => {
            let num = parse_reg_num(reg)
                .ok_or_else(|| format!("invalid NEON register: {}", reg))?;
            Ok((num, arrangement.clone()))
        }
        Some(Operand::Reg(name)) => {
            let num = parse_reg_num(name)
                .ok_or_else(|| format!("invalid register: {}", name))?;
            Ok((num, String::new()))
        }
        other => Err(format!("expected NEON register at operand {}, got {:?}", idx, other)),
    }
}

fn encode_cnt(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CNT Vd.<T>, Vn.<T>
    // Encoding: 0 Q 00 1110 size 10 0000 0101 10 Rn Rd
    // Only valid for .8b (Q=0) and .16b (Q=1)
    if operands.len() < 2 {
        return Err("cnt requires 2 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _arr_n) = get_neon_reg(operands, 1)?;

    let q: u32 = if arr_d == "16b" { 1 } else { 0 }; // .8b -> Q=0, .16b -> Q=1

    // 0 Q 00 1110 00 10 0000 0101 10 Rn Rd
    let word = (q << 30) | (0b001110 << 24) | (0b00 << 22) | (0b100000 << 16)
        | (0b010110 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_uaddlv(operands: &[Operand]) -> Result<EncodeResult, String> {
    // UADDLV Vd, Vn.<T>
    // Encoding: 0 Q 1 01110 size 11 0000 0011 10 Rn Rd
    // For .8b: Q=0, size=00, result in Hd
    // For .16b: Q=1, size=00, result in Hd
    // For .4h: Q=0, size=01, result in Sd
    // For .8h: Q=1, size=01, result in Sd
    // For .4s: Q=1, size=10, result in Dd
    if operands.len() < 2 {
        return Err("uaddlv requires 2 operands".to_string());
    }
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, arr) = get_neon_reg(operands, 1)?;

    let (q, size): (u32, u32) = match arr.as_str() {
        "8b" => (0, 0b00),
        "16b" => (1, 0b00),
        "4h" => (0, 0b01),
        "8h" => (1, 0b01),
        "4s" => (1, 0b10),
        _ => return Err(format!("uaddlv: unsupported arrangement: {}", arr)),
    };

    // 0 Q 1 01110 size 11 0000 0011 10 Rn Rd
    let word = (q << 30) | (0b1 << 29) | (0b01110 << 24) | (size << 22)
        | (0b11 << 20) | (0b0000 << 16) | (0b001110 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── System instructions ──────────────────────────────────────────────────

fn encode_dmb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let option = match operands.get(0) {
        Some(Operand::Barrier(b)) => match b.as_str() {
            "sy" => 0b1111,
            "ish" => 0b1011,
            "ishld" => 0b1001,
            "ishst" => 0b1010,
            "osh" => 0b0011,
            "oshld" => 0b0001,
            "oshst" => 0b0010,
            "nsh" => 0b0111,
            "nshld" => 0b0101,
            "nshst" => 0b0110,
            "ld" => 0b1101,
            "st" => 0b1110,
            _ => 0b1111,
        },
        Some(Operand::Symbol(s)) => match s.to_lowercase().as_str() {
            "sy" => 0b1111,
            "ish" => 0b1011,
            "ishld" => 0b1001,
            "ishst" => 0b1010,
            _ => 0b1111,
        },
        _ => 0b1111,
    };
    // DMB: 0xD50330BF | (CRm << 8)
    let word = 0xd50330bf | (option << 8);
    Ok(EncodeResult::Word(word))
}

fn encode_dsb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let option = match operands.get(0) {
        Some(Operand::Barrier(b)) => match b.as_str() {
            "sy" => 0b1111u32,
            "ish" => 0b1011,
            "ishld" => 0b1001,
            "ishst" => 0b1010,
            _ => 0b1111,
        },
        Some(Operand::Symbol(s)) => match s.to_lowercase().as_str() {
            "sy" => 0b1111u32,
            "ish" => 0b1011,
            _ => 0b1111,
        },
        _ => 0b1111,
    };
    // DSB: 0xD503309F | (option << 8)
    let word = 0xd503309f | (option << 8);
    Ok(EncodeResult::Word(word))
}

fn encode_mrs(operands: &[Operand]) -> Result<EncodeResult, String> {
    // MRS Xt, system_reg
    let (rt, _) = get_reg(operands, 0)?;
    let sysreg = match operands.get(1) {
        Some(Operand::Symbol(s)) => s.to_lowercase(),
        _ => return Err("mrs needs system register name".to_string()),
    };

    let encoding = match sysreg.as_str() {
        "tpidr_el0" => 0xde82u32, // 11 011 1101 0000 010
        "cntfrq_el0" => 0xdf00,
        "cntvct_el0" => 0xdf02,
        "fpcr" => 0xda20,
        "fpsr" => 0xda21,
        "nzcv" => 0xda10,
        _ => return Err(format!("unsupported system register: {}", sysreg)),
    };

    let word = 0xd5300000 | (encoding << 5) | rt;
    Ok(EncodeResult::Word(word))
}

fn encode_msr(operands: &[Operand]) -> Result<EncodeResult, String> {
    // MSR system_reg, Xt
    let sysreg = match operands.get(0) {
        Some(Operand::Symbol(s)) => s.to_lowercase(),
        _ => return Err("msr needs system register name".to_string()),
    };
    let (rt, _) = get_reg(operands, 1)?;

    let encoding = match sysreg.as_str() {
        "tpidr_el0" => 0xde82u32,
        "fpcr" => 0xda20,
        "fpsr" => 0xda21,
        "nzcv" => 0xda10,
        _ => return Err(format!("unsupported system register: {}", sysreg)),
    };

    let word = 0xd5100000 | (encoding << 5) | rt;
    Ok(EncodeResult::Word(word))
}

fn encode_svc(operands: &[Operand]) -> Result<EncodeResult, String> {
    let imm = get_imm(operands, 0)?;
    let word = 0xd4000001 | ((imm as u32 & 0xFFFF) << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_brk(operands: &[Operand]) -> Result<EncodeResult, String> {
    let imm = get_imm(operands, 0)?;
    let word = 0xd4200000 | ((imm as u32 & 0xFFFF) << 5);
    Ok(EncodeResult::Word(word))
}

// ── Bit manipulation ─────────────────────────────────────────────────────

fn encode_clz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    // CLZ: sf 1 0 11010110 00000 00010 0 Rn Rd
    let word = (sf << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (0b000100 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_cls(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (0b000101 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rbit(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (0b000000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rev(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let opc = if is_64 { 0b000011 } else { 0b000010 };
    let word = (sf << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (opc << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rev16(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (0b000001 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rev32(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    // REV32 is 64-bit only: 1 1 0 11010110 00000 000010 Rn Rd
    let word = (1u32 << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (0b000010 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── CRC32 ────────────────────────────────────────────────────────────────

fn encode_crc32(mnemonic: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;

    let is_c = mnemonic.contains("crc32c");
    let c_bit = if is_c { 1u32 } else { 0 };

    let (sf, sz) = match mnemonic {
        "crc32b" | "crc32cb" => (0u32, 0b00u32),
        "crc32h" | "crc32ch" => (0, 0b01),
        "crc32w" | "crc32cw" => (0, 0b10),
        "crc32x" | "crc32cx" => (1, 0b11),
        _ => (0, 0b00),
    };

    // CRC32: sf 0 0 11010110 Rm 010 C sz Rn Rd
    let word = (sf << 31) | (0b0011010110 << 21) | (rm << 16) | (0b010 << 13)
        | (c_bit << 12) | (sz << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Prefetch ─────────────────────────────────────────────────────────────

/// Encode the PRFM (prefetch memory) instruction.
/// Format: PRFM <prfop>, [<Xn|SP>{, #<pimm>}]
/// Encoding: 1111 1001 10 imm12 Rn Rt
/// where Rt is the 5-bit prefetch operation type.
fn encode_prfm(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("prfm requires 2 operands".to_string());
    }

    // First operand: prefetch operation type (parsed as Symbol)
    let prfop = match &operands[0] {
        Operand::Symbol(s) => encode_prfop(s)?,
        Operand::Imm(v) => {
            if *v < 0 || *v > 31 {
                return Err(format!("prfm: immediate prefetch type out of range: {}", v));
            }
            *v as u32
        }
        _ => return Err(format!("prfm: expected prefetch operation name, got {:?}", operands[0])),
    };

    // Second operand: memory address [Xn{, #imm}]
    match &operands[1] {
        Operand::Mem { base, offset } => {
            let rn = parse_reg_num(base).ok_or_else(|| format!("prfm: invalid base register: {}", base))?;
            let imm = *offset;
            if imm < 0 || imm % 8 != 0 {
                return Err(format!("prfm: offset must be non-negative and 8-byte aligned, got {}", imm));
            }
            let imm12 = (imm / 8) as u32;
            if imm12 > 0xFFF {
                return Err(format!("prfm: offset too large: {}", imm));
            }
            // PRFM (imm): 1111 1001 10 imm12(12) Rn(5) Rt(5)
            let word = 0xF9800000 | (imm12 << 10) | (rn << 5) | prfop;
            Ok(EncodeResult::Word(word))
        }
        Operand::Symbol(_sym) => {
            // PRFM (literal) with symbol reference is not yet supported
            Err("prfm with symbol/label operand not yet supported".to_string())
        }
        _ => Err(format!("prfm: expected memory operand, got {:?}", operands[1])),
    }
}

/// Map prefetch operation name to its 5-bit encoding.
fn encode_prfop(name: &str) -> Result<u32, String> {
    match name.to_lowercase().as_str() {
        "pldl1keep" => Ok(0b00000),
        "pldl1strm" => Ok(0b00001),
        "pldl2keep" => Ok(0b00010),
        "pldl2strm" => Ok(0b00011),
        "pldl3keep" => Ok(0b00100),
        "pldl3strm" => Ok(0b00101),
        "plil1keep" => Ok(0b01000),
        "plil1strm" => Ok(0b01001),
        "plil2keep" => Ok(0b01010),
        "plil2strm" => Ok(0b01011),
        "plil3keep" => Ok(0b01100),
        "plil3strm" => Ok(0b01101),
        "pstl1keep" => Ok(0b10000),
        "pstl1strm" => Ok(0b10001),
        "pstl2keep" => Ok(0b10010),
        "pstl2strm" => Ok(0b10011),
        "pstl3keep" => Ok(0b10100),
        "pstl3strm" => Ok(0b10101),
        _ => Err(format!("prfm: unknown prefetch operation: {}", name)),
    }
}

// ── NEON three-same register operations ──────────────────────────────────

/// Get Q bit and size from arrangement specifier.
fn neon_arr_to_q_size(arr: &str) -> Result<(u32, u32), String> {
    match arr {
        "8b" => Ok((0, 0b00)),
        "16b" => Ok((1, 0b00)),
        "4h" => Ok((0, 0b01)),
        "8h" => Ok((1, 0b01)),
        "2s" => Ok((0, 0b10)),
        "4s" => Ok((1, 0b10)),
        "2d" => Ok((1, 0b11)),
        _ => Err(format!("unsupported NEON arrangement: {}", arr)),
    }
}

/// Encode NEON three-same-register instructions: CMEQ, UQSUB, SQSUB, etc.
/// `prefix_bits` and `opcode` encode the specific instruction.
fn encode_neon_three_same(operands: &[Operand], prefix: u32, opcode: u32, _u_bit: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("NEON three-same requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _arr_n) = get_neon_reg(operands, 1)?;
    let (rm, _arr_m) = get_neon_reg(operands, 2)?;

    let (q, size) = neon_arr_to_q_size(&arr_d)?;

    // Generic three-same encoding: 0 Q U prefix size 1 Rm opcode 1 Rn Rd
    // The exact bit layout depends on the instruction, but the caller passes
    // the top bits (prefix includes the U bit) and opcode.
    // Layout: 0 Q fffff size 1 Rm oooo 1 Rn Rd
    //         31 30 29-24   22 21 20-16 15-11 10  9-5  4-0
    let word = (q << 30) | (prefix << 24) | (size << 22) | (1 << 21)
        | (rm << 16) | (opcode << 11) | (1 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON logical operations: ORR/AND/EOR Vd.T, Vn.T, Vm.T
fn encode_neon_logical(operands: &[Operand], opc: u32) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _arr_n) = get_neon_reg(operands, 1)?;
    let (rm, _arr_m) = get_neon_reg(operands, 2)?;

    let q: u32 = if arr_d == "16b" { 1 } else { 0 };

    // NEON logical three-same:
    // ORR: 0 Q 0 01110 10 1 Rm 000111 Rn Rd  (opc=0b01 -> size=10)
    // AND: 0 Q 0 01110 00 1 Rm 000111 Rn Rd  (opc=0b00 -> size=00)
    // EOR: 0 Q 1 01110 00 1 Rm 000111 Rn Rd  (opc=0b10 -> size=00, U=1)
    // BIC: 0 Q 0 01110 01 1 Rm 000111 Rn Rd  (would be opc=0b01 with N=1... but not needed)
    let (u_bit, size_bits): (u32, u32) = match opc {
        0b00 => (0, 0b00),  // AND
        0b01 => (0, 0b10),  // ORR
        0b10 => (1, 0b00),  // EOR
        0b11 => (1, 0b00),  // ANDS - not valid for NEON, fall back
        _ => return Err("unsupported NEON logical opc".to_string()),
    };

    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size_bits << 22)
        | (1 << 21) | (rm << 16) | (0b000111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON MUL Vd.T, Vn.T, Vm.T
fn encode_neon_mul(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let (q, size) = neon_arr_to_q_size(&arr_d)?;

    // MUL (vector): 0 Q 0 01110 size 1 Rm 10011 1 Rn Rd
    let word = (q << 30) | (0b001110 << 24) | (size << 22) | (1 << 21)
        | (rm << 16) | (0b100111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON USHR Vd.T, Vn.T, #shift (unsigned shift right immediate)
fn encode_neon_shift_imm(operands: &[Operand], _is_unsigned: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("ushr requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)?;

    let (q, _size) = neon_arr_to_q_size(&arr_d)?;

    // USHR: 0 Q 1 011110 immh:immb 00000 1 Rn Rd
    // For .16b (bytes, size=8): immh = 0001, immb = 8-shift (3 bits)
    // For .8h (halfwords, size=16): immh = 001x
    // For .4s (words, size=32): immh = 01xx
    // For .2d (doublewords, size=64): immh = 1xxx
    // immh:immb = (element_size * 2 - shift)
    let (elem_bits, immh_immb) = match arr_d.as_str() {
        "8b" | "16b" => (8u32, (16 - shift as u32) & 0xF),   // immh:immb is 4 bits for 8-bit elems
        "4h" | "8h" => (16, (32 - shift as u32) & 0x1F),
        "2s" | "4s" => (32, (64 - shift as u32) & 0x3F),
        "2d" => (64, (128 - shift as u32) & 0x7F),
        _ => return Err(format!("unsupported USHR arrangement: {}", arr_d)),
    };
    let _ = elem_bits;

    // Full encoding: 0 Q 1 011110 immh:immb 000001 Rn Rd
    let word = (q << 30) | (1 << 29) | (0b011110 << 23) | (immh_immb << 16)
        | (0b000001 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON EXT Vd.T, Vn.T, Vm.T, #index
fn encode_neon_ext(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 4 {
        return Err("ext requires 4 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let index = get_imm(operands, 3)? as u32;

    let q: u32 = if arr_d == "16b" { 1 } else { 0 };

    // EXT Vd.T, Vn.T, Vm.T, #index
    // Encoding: 0 Q 10 1110 00 0 Rm 0 imm4 0 Rn Rd
    let word = (q << 30) | (0b101110 << 24) | (0b00 << 22) | (0 << 21)
        | (rm << 16) | (0 << 15) | ((index & 0xF) << 11) | (0 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON ADDV: add across vector lanes
fn encode_neon_addv(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("addv requires 2 operands".to_string());
    }
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;

    let (q, size) = neon_arr_to_q_size(&arr_n)?;

    // ADDV: 0 Q 0 01110 size 11000 11011 10 Rn Rd
    let word = (q << 30) | (0b001110 << 24) | (size << 22) | (0b11000 << 17)
        | (0b110111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON UMOV: move element to GP register
fn encode_neon_umov(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("umov requires 2 operands".to_string());
    }
    let (rd, is_64) = get_reg(operands, 0)?;

    // Second operand should be a RegLane (v0.b[0])
    match operands.get(1) {
        Some(Operand::RegLane { reg, elem_size, index }) => {
            let rn = parse_reg_num(reg).ok_or("invalid NEON register")?;
            let q = if is_64 { 1u32 } else { 0 };

            let imm5 = match elem_size.as_str() {
                "b" => ((*index & 0xF) << 1) | 0b00001,
                "h" => ((*index & 0x7) << 2) | 0b00010,
                "s" => ((*index & 0x3) << 3) | 0b00100,
                "d" => ((*index & 0x1) << 4) | 0b01000,
                _ => return Err(format!("unsupported umov element size: {}", elem_size)),
            };

            // UMOV Rd, Vn.Ts[index]: 0 Q 0 01110 000 imm5 0 0111 1 Rn Rd
            let word = (q << 30) | (0b001110000u32 << 21) | (imm5 << 16)
                | (0b001111 << 10) | (rn << 5) | rd;
            Ok(EncodeResult::Word(word))
        }
        _ => Err("umov: expected register lane operand".to_string()),
    }
}

/// Encode NEON DUP: broadcast GP register to all vector lanes
fn encode_neon_dup(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("dup requires 2 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;

    // DUP Vd.T, Rn (general form - broadcast GP reg to vector)
    if let Some(Operand::Reg(rn_name)) = operands.get(1) {
        let rn = parse_reg_num(rn_name).ok_or("invalid rn")?;
        let (q, _) = neon_arr_to_q_size(&arr_d)?;

        // imm5 encoding for element size:
        // .8b/.16b: imm5 = 00001
        // .4h/.8h:  imm5 = 00010
        // .2s/.4s:  imm5 = 00100
        // .2d:      imm5 = 01000
        let imm5 = match arr_d.as_str() {
            "8b" | "16b" => 0b00001u32,
            "4h" | "8h" => 0b00010,
            "2s" | "4s" => 0b00100,
            "2d" => 0b01000,
            _ => return Err(format!("unsupported dup arrangement: {}", arr_d)),
        };

        // DUP Vd.T, Rn: 0 Q 0 01110 000 imm5 0 0001 1 Rn Rd
        let word = (q << 30) | (0b001110000u32 << 21) | (imm5 << 16)
            | (0b000011 << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err("unsupported dup operands".to_string())
}

/// Encode NEON INS (insert element from GP register): INS Vd.Ts[index], Xn
fn encode_neon_ins(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("ins requires 2 operands".to_string());
    }
    // INS Vd.Ts[index], Xn
    // Encoding: 0 1 0 0 1110 000 imm5 0 0011 1 Rn Rd
    // This is the same as MOV Vd.Ts[index], Xn (alias)
    match (&operands[0], &operands[1]) {
        (Operand::RegLane { reg, elem_size, index }, Operand::Reg(rn_name)) => {
            let rd = parse_reg_num(reg).ok_or("invalid NEON register")?;
            let rn = parse_reg_num(rn_name).ok_or("invalid register")?;

            let imm5 = match elem_size.as_str() {
                "b" => ((*index & 0xF) << 1) | 0b00001,
                "h" => ((*index & 0x7) << 2) | 0b00010,
                "s" => ((*index & 0x3) << 3) | 0b00100,
                "d" => ((*index & 0x1) << 4) | 0b01000,
                _ => return Err(format!("unsupported ins element size: {}", elem_size)),
            };

            // INS Vd.Ts[i], Xn: 0 1 0 01110 000 imm5 0 0011 1 Rn Rd
            let word = (0b01001110000u32 << 21) | (imm5 << 16)
                | (0b000111 << 10) | (rn << 5) | rd;
            Ok(EncodeResult::Word(word))
        }
        _ => Err("ins: expected RegLane and Reg operands".to_string()),
    }
}

/// Encode NEON NOT (bitwise NOT): NOT Vd.T, Vn.T
fn encode_neon_not(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("not requires 2 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;

    let q: u32 = if arr_d == "16b" { 1 } else { 0 };

    // NOT Vd.T, Vn.T (alias of MVN): 0 Q 1 01110 00 10000 00101 10 Rn Rd
    let word = (q << 30) | (1 << 29) | (0b01110 << 24) | (0b00 << 22)
        | (0b10000 << 17) | (0b00101 << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON MOVI (move immediate to vector)
fn encode_neon_movi(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("movi requires 2 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let imm = get_imm(operands, 1)?;

    match arr_d.as_str() {
        "16b" | "8b" => {
            // MOVI Vd.16b, #imm8
            // Encoding: 0 Q 00 1111 00000 abc 1110 01 defgh Rd
            // where imm8 = abc:defgh
            let q: u32 = if arr_d == "16b" { 1 } else { 0 };
            let imm8 = imm as u32 & 0xFF;
            let abc = (imm8 >> 5) & 0x7;
            let defgh = imm8 & 0x1F;
            // 0 Q op 0 1111 0 a b c cmode(1110) o2(0) 1 defgh Rd
            let word = (q << 30) | (0b0011110 << 23) | ((abc >> 2) << 18) | (((abc >> 1) & 1) << 17)
                | ((abc & 1) << 16) | (0b1110 << 12) | (0b01 << 10) | (defgh << 5) | rd;
            Ok(EncodeResult::Word(word))
        }
        "2d" => {
            // MOVI Vd.2d, #imm
            // For #0: op=1, cmode=1110, o2=0
            // Full MOVI 64-bit: 0 1 1 0 1111 00000 abc 1110 01 defgh Rd
            // For zero: imm = 0, so abc=000, defgh=00000
            let imm8 = imm as u32 & 0xFF;
            // Each bit of imm8 expands to 8 bits in the 64-bit result
            let abc = (imm8 >> 5) & 0x7;
            let defgh = imm8 & 0x1F;
            let word = (0b01101111 << 24) | ((abc >> 2) << 18) | (((abc >> 1) & 1) << 17)
                | ((abc & 1) << 16) | (0b111001 << 10) | (defgh << 5) | rd;
            Ok(EncodeResult::Word(word))
        }
        "2s" | "4s" => {
            // MOVI Vd.2s/4s, #imm8 (32-bit element, no shift)
            // Encoding: 0 Q op(0) 0 1111 0 abc cmode(0000) o2(0) 1 defgh Rd
            let q: u32 = if arr_d == "4s" { 1 } else { 0 };
            let imm8 = imm as u32 & 0xFF;
            let abc = (imm8 >> 5) & 0x7;
            let defgh = imm8 & 0x1F;

            // Check for optional LSL shift operand
            let (cmode, shift_val) = if operands.len() > 2 {
                if let Some(Operand::Shift { kind, amount }) = operands.get(2) {
                    if kind == "lsl" {
                        match amount {
                            0 => (0b0000u32, 0),
                            8 => (0b0010, 8),
                            16 => (0b0100, 16),
                            24 => (0b0110, 24),
                            _ => return Err(format!("movi: unsupported shift amount: {}", amount)),
                        }
                    } else {
                        (0b0000, 0)
                    }
                } else {
                    (0b0000, 0)
                }
            } else {
                (0b0000, 0)
            };
            let _ = shift_val;

            let word = (q << 30) | (0b0011110 << 23) | ((abc >> 2) << 18) | (((abc >> 1) & 1) << 17)
                | ((abc & 1) << 16) | (cmode << 12) | (0b01 << 10) | (defgh << 5) | rd;
            Ok(EncodeResult::Word(word))
        }
        "4h" | "8h" => {
            // MOVI Vd.4h/8h, #imm8
            let q: u32 = if arr_d == "8h" { 1 } else { 0 };
            let imm8 = imm as u32 & 0xFF;
            let abc = (imm8 >> 5) & 0x7;
            let defgh = imm8 & 0x1F;
            // cmode=1000 for .4h/.8h with no shift
            let word = (q << 30) | (0b0011110 << 23) | ((abc >> 2) << 18) | (((abc >> 1) & 1) << 17)
                | ((abc & 1) << 16) | (0b1000 << 12) | (0b01 << 10) | (defgh << 5) | rd;
            Ok(EncodeResult::Word(word))
        }
        _ => Err(format!("movi: unsupported arrangement: {}", arr_d)),
    }
}

/// Encode NEON BIC (bitwise clear vector): BIC Vd.T, Vn.T, Vm.T
fn encode_neon_bic(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("bic requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;

    let q: u32 = if arr_d == "16b" { 1 } else { 0 };

    // BIC Vd.T, Vn.T, Vm.T: 0 Q 0 01110 01 1 Rm 000111 Rn Rd
    let word = (q << 30) | (0b001110 << 24) | (0b01 << 22) | (1 << 21)
        | (rm << 16) | (0b000111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON BSL (bitwise select): BSL Vd.T, Vn.T, Vm.T
fn encode_neon_bsl(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("bsl requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;

    let q: u32 = if arr_d == "16b" { 1 } else { 0 };

    // BSL Vd.T, Vn.T, Vm.T: 0 Q 1 01110 01 1 Rm 000111 Rn Rd
    let word = (q << 30) | (1 << 29) | (0b01110 << 24) | (0b01 << 22) | (1 << 21)
        | (rm << 16) | (0b000111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON REV64: reverse elements within 64-bit doublewords
fn encode_neon_rev64(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("rev64 requires 2 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;

    let (q, size) = neon_arr_to_q_size(&arr_d)?;

    // REV64 Vd.T, Vn.T: 0 Q 0 01110 size 10 0000 0000 10 Rn Rd
    let word = (q << 30) | (0b001110 << 24) | (size << 22)
        | (0b100000 << 16) | (0b000010 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON TBL: table vector lookup
fn encode_neon_tbl(operands: &[Operand]) -> Result<EncodeResult, String> {
    // TBL Vd.T, {Vn.T}, Vm.T  (single register table)
    // TBL Vd.T, {Vn.T, Vn+1.T}, Vm.T  (two register table)
    // etc.
    if operands.len() < 3 {
        return Err("tbl requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let q: u32 = if arr_d == "16b" { 1 } else { 0 };

    // The second operand is a register list
    let (rn, num_regs) = match &operands[1] {
        Operand::RegList(regs) => {
            let first_reg = match &regs[0] {
                Operand::RegArrangement { reg, .. } => parse_reg_num(reg).ok_or("invalid reg")?,
                Operand::Reg(name) => parse_reg_num(name).ok_or("invalid reg")?,
                _ => return Err("tbl: expected register in list".to_string()),
            };
            (first_reg, regs.len() as u32)
        }
        _ => return Err("tbl: expected register list as second operand".to_string()),
    };

    let (rm, _) = get_neon_reg(operands, 2)?;

    // len field: 1 reg -> 00, 2 -> 01, 3 -> 10, 4 -> 11
    let len = (num_regs - 1) & 0x3;

    // TBL: 0 Q 00 1110 000 Rm 0 len 0 00 Rn Rd
    let word = (q << 30) | (0b001110 << 24) | (0b000 << 21)
        | (rm << 16) | (0 << 15) | (len << 13) | (0b000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON LD1 (vector load, multiple structures)
fn encode_neon_ld1(operands: &[Operand]) -> Result<EncodeResult, String> {
    encode_neon_ld_st(operands, true)
}

/// Encode NEON ST1 (vector store, multiple structures)
fn encode_neon_st1(operands: &[Operand]) -> Result<EncodeResult, String> {
    encode_neon_ld_st(operands, false)
}

/// Common encoder for LD1/ST1 (multiple structures)
fn encode_neon_ld_st(operands: &[Operand], is_load: bool) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("ld1/st1 requires at least 2 operands".to_string());
    }

    // First operand: register list {Vt.T} or {Vt.T, Vt+1.T, ...}
    let (rt, arr, num_regs) = match &operands[0] {
        Operand::RegList(regs) => {
            let (first_reg, arrangement) = match &regs[0] {
                Operand::RegArrangement { reg, arrangement } => {
                    (parse_reg_num(reg).ok_or("invalid reg")?, arrangement.clone())
                }
                _ => return Err("ld1/st1: expected RegArrangement in list".to_string()),
            };
            (first_reg, arrangement, regs.len() as u32)
        }
        _ => return Err("ld1/st1: expected register list".to_string()),
    };

    let (q, size) = neon_arr_to_q_size(&arr)?;

    // Second operand: [Xn] memory base or [Xn], #imm (post-index, merged by parser)
    let (rn, post_index) = match &operands[1] {
        Operand::Mem { base, offset: 0 } => {
            let r = parse_reg_num(base).ok_or_else(|| format!("invalid base register: {}", base))?;
            (r, None)
        }
        Operand::MemPostIndex { base, offset } => {
            let r = parse_reg_num(base).ok_or_else(|| format!("invalid base register: {}", base))?;
            (r, Some(*offset))
        }
        _ => return Err("ld1/st1: expected [Xn] memory operand".to_string()),
    };

    // opcode field based on number of registers:
    // 1 reg: 0111, 2 reg: 1010, 3 reg: 0110, 4 reg: 0010
    let opcode = match num_regs {
        1 => 0b0111u32,
        2 => 0b1010,
        3 => 0b0110,
        4 => 0b0010,
        _ => return Err(format!("ld1/st1: unsupported register count: {}", num_regs)),
    };

    let l_bit = if is_load { 1u32 } else { 0u32 };

    // Handle post-index form from merged MemPostIndex
    if let Some(_imm) = post_index {
        // Post-index with immediate: use Rm=11111 (0x1F)
        let word = (q << 30) | (0b001100 << 24) | (1 << 23) | (l_bit << 22)
            | (0 << 21) | (0b11111 << 16) | (opcode << 12) | (size << 10) | (rn << 5) | rt;
        return Ok(EncodeResult::Word(word));
    }

    // Check for post-index form via separate operands: [Xn], Xm
    if operands.len() > 2 {
        match &operands[2] {
            Operand::Imm(_) => {
                // Post-index with immediate: use Rm=11111
                let word = (q << 30) | (0b001100 << 24) | (1 << 23) | (l_bit << 22)
                    | (0 << 21) | (0b11111 << 16) | (opcode << 12) | (size << 10) | (rn << 5) | rt;
                return Ok(EncodeResult::Word(word));
            }
            Operand::Reg(rm_name) => {
                let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;
                let word = (q << 30) | (0b001100 << 24) | (1 << 23) | (l_bit << 22)
                    | (0 << 21) | (rm << 16) | (opcode << 12) | (size << 10) | (rn << 5) | rt;
                return Ok(EncodeResult::Word(word));
            }
            _ => {}
        }
    }

    // No post-index: LD1/ST1 {Vt.T...}, [Xn]
    // 0 Q 001100 0 L 0 00000 opcode size Rn Rt
    let word = (q << 30) | (0b001100 << 24) | (0 << 23) | (l_bit << 22)
        | (0 << 21) | (0b00000 << 16) | (opcode << 12) | (size << 10) | (rn << 5) | rt;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON UZP1/UZP2/ZIP1/ZIP2
fn encode_neon_zip_uzp(operands: &[Operand], op_bits: u32, _is_zip: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("uzp/zip requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let (q, size) = neon_arr_to_q_size(&arr_d)?;

    // UZP1: 0 Q 0 01110 size 0 Rm 0 001 10 Rn Rd  (op_bits=001)
    // UZP2: 0 Q 0 01110 size 0 Rm 0 101 10 Rn Rd  (op_bits=101)
    // ZIP1: 0 Q 0 01110 size 0 Rm 0 011 10 Rn Rd  (op_bits=011)
    // ZIP2: 0 Q 0 01110 size 0 Rm 0 111 10 Rn Rd  (op_bits=111)
    let word = (q << 30) | (0b001110 << 24) | (size << 22)
        | (0 << 21) | (rm << 16) | (0 << 15) | (op_bits << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON EOR3 (three-way XOR, SHA3 extension): EOR3 Vd.16b, Vn.16b, Vm.16b, Vk.16b
fn encode_neon_eor3(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 4 {
        return Err("eor3 requires 4 operands".to_string());
    }
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let (rk, _) = get_neon_reg(operands, 3)?;

    // EOR3 Vd.16b, Vn.16b, Vm.16b, Vk.16b
    // Encoding: 11001110 000 Rm 0 Rk(4:0) 00 Rn Rd
    let word = (0b11001110u32 << 24) | (0b000 << 21) | (rm << 16)
        | (0 << 15) | (rk << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON PMULL/PMULL2 (polynomial multiply long)
fn encode_neon_pmull(operands: &[Operand], is_pmull2: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("pmull requires 3 operands".to_string());
    }
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;

    let q = if is_pmull2 { 1u32 } else { 0 };

    // PMULL  Vd.1q, Vn.1d, Vm.1d: 0 0 00 1110 11 1 Rm 11100 0 Rn Rd  (size=11)
    // PMULL2 Vd.1q, Vn.2d, Vm.2d: 0 1 00 1110 11 1 Rm 11100 0 Rn Rd
    let word = (q << 30) | (0b001110 << 24) | (0b11 << 22) | (1 << 21)
        | (rm << 16) | (0b11100 << 11) | (0 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON AES instructions (AESE, AESD, AESMC, AESIMC)
fn encode_neon_aes(operands: &[Operand], opcode: u32) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("aes instruction requires 2 operands".to_string());
    }
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;

    // AES instructions: 0100 1110 0010 1000 opcode 10 Rn Rd
    // AESE:  opcode = 00100 (0x4)
    // AESD:  opcode = 00101 (0x5)
    // AESMC: opcode = 00110 (0x6)
    // AESIMC:opcode = 00111 (0x7)
    let word = (0b01001110 << 24) | (0b0010100 << 17) | (opcode << 12)
        | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON ADD/SUB (vector integer): ADD/SUB Vd.T, Vn.T, Vm.T
fn encode_neon_add_sub(operands: &[Operand], is_sub: bool) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let (q, size) = neon_arr_to_q_size(&arr_d)?;
    let u = if is_sub { 1u32 } else { 0u32 };

    // ADD: 0 Q 0 01110 size 1 Rm 10000 1 Rn Rd
    // SUB: 0 Q 1 01110 size 1 Rm 10000 1 Rn Rd
    let word = (q << 30) | (u << 29) | (0b01110 << 24) | (size << 22) | (1 << 21)
        | (rm << 16) | (0b10000 << 11) | (1 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON USHR (unsigned shift right immediate)
fn encode_neon_ushr(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("ushr requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)? as u32;

    let (q, _) = neon_arr_to_q_size(&arr_d)?;

    // USHR Vd.T, Vn.T, #shift
    // 0 Q 1 0 11110 immh:immb 000001 Rn Rd
    let immh_immb = match arr_d.as_str() {
        "8b" | "16b" => (16 - shift) & 0xF,
        "4h" | "8h" => (32 - shift) & 0x1F,
        "2s" | "4s" => (64 - shift) & 0x3F,
        "2d" => (128 - shift) & 0x7F,
        _ => return Err(format!("unsupported ushr arrangement: {}", arr_d)),
    };

    let word = (q << 30) | (1 << 29) | (0b011110 << 23) | (immh_immb << 16)
        | (0b000001 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON SSHR (signed shift right immediate)
fn encode_neon_sshr(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("sshr requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)? as u32;

    let (q, _) = neon_arr_to_q_size(&arr_d)?;

    // SSHR Vd.T, Vn.T, #shift
    // 0 Q 0 0 11110 immh:immb 000001 Rn Rd  (U=0)
    let immh_immb = match arr_d.as_str() {
        "8b" | "16b" => (16 - shift) & 0xF,
        "4h" | "8h" => (32 - shift) & 0x1F,
        "2s" | "4s" => (64 - shift) & 0x3F,
        "2d" => (128 - shift) & 0x7F,
        _ => return Err(format!("unsupported sshr arrangement: {}", arr_d)),
    };

    let word = (q << 30) | (0 << 29) | (0b011110 << 23) | (immh_immb << 16)
        | (0b000001 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON SHL (shift left immediate)
fn encode_neon_shl(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("shl requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)? as u32;

    let (q, _) = neon_arr_to_q_size(&arr_d)?;

    // SHL Vd.T, Vn.T, #shift
    // 0 Q 0 0 11110 immh:immb 010101 Rn Rd
    // immh:immb = element_size + shift
    let immh_immb = match arr_d.as_str() {
        "8b" | "16b" => (8 + shift) & 0xF,
        "4h" | "8h" => (16 + shift) & 0x1F,
        "2s" | "4s" => (32 + shift) & 0x3F,
        "2d" => (64 + shift) & 0x7F,
        _ => return Err(format!("unsupported shl arrangement: {}", arr_d)),
    };

    let word = (q << 30) | (0 << 29) | (0b011110 << 23) | (immh_immb << 16)
        | (0b010101 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON SLI (shift left and insert)
fn encode_neon_sli(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("sli requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)? as u32;

    let (q, _) = neon_arr_to_q_size(&arr_d)?;

    // SLI Vd.T, Vn.T, #shift
    // 0 Q 1 0 11110 immh:immb 010101 Rn Rd  (U=1)
    let immh_immb = match arr_d.as_str() {
        "8b" | "16b" => (8 + shift) & 0xF,
        "4h" | "8h" => (16 + shift) & 0x1F,
        "2s" | "4s" => (32 + shift) & 0x3F,
        "2d" => (64 + shift) & 0x7F,
        _ => return Err(format!("unsupported sli arrangement: {}", arr_d)),
    };

    let word = (q << 30) | (1 << 29) | (0b011110 << 23) | (immh_immb << 16)
        | (0b010101 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode ORN (logical OR NOT): ORN Rd, Rn, Rm (scalar or vector)
fn encode_orn(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("orn requires 3 operands".to_string());
    }

    // NEON vector form: ORN Vd.T, Vn.T, Vm.T
    if let Some(Operand::RegArrangement { .. }) = operands.get(0) {
        let (rd, arr_d) = get_neon_reg(operands, 0)?;
        let (rn, _) = get_neon_reg(operands, 1)?;
        let (rm, _) = get_neon_reg(operands, 2)?;
        let q: u32 = if arr_d == "16b" { 1 } else { 0 };
        // ORN Vd.T, Vn.T, Vm.T: 0 Q 0 01110 11 1 Rm 000111 Rn Rd
        let word = (q << 30) | (0b001110 << 24) | (0b11 << 22) | (1 << 21)
            | (rm << 16) | (0b000111 << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);

    // ORN Rd, Rn, Rm: sf 01 01010 00 1 Rm 000000 Rn Rd
    let word = (sf << 31) | (0b01 << 29) | (0b01010 << 24) | (0b00 << 22) | (1 << 21)
        | (rm << 16) | (0b000000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode BICS (bitwise clear, setting flags): BICS Rd, Rn, Rm
fn encode_bics(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("bics requires 3 operands".to_string());
    }
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);

    // BICS Rd, Rn, Rm: sf 11 01010 00 1 Rm 000000 Rn Rd
    let word = (sf << 31) | (0b11 << 29) | (0b01010 << 24) | (0b00 << 22) | (1 << 21)
        | (rm << 16) | (0b000000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode DC (data cache) instruction: dc civac, Xt
fn encode_dc(operands: &[Operand], raw_operands: &str) -> Result<EncodeResult, String> {
    // Check for the operation type in the operands or raw string
    let op = match operands.get(0) {
        Some(Operand::Symbol(s)) => s.to_lowercase(),
        _ => raw_operands.to_lowercase(),
    };

    if op.contains("civac") {
        // Find the register operand (second operand)
        let rt = match operands.get(1) {
            Some(Operand::Reg(name)) => parse_reg_num(name).ok_or("invalid register for dc")?,
            _ => {
                // Fallback: last operand
                if let Some(Operand::Reg(name)) = operands.last() {
                    parse_reg_num(name).ok_or("invalid register for dc")?
                } else {
                    0
                }
            }
        };
        // DC CIVAC: sys #3, c7, c14, #1, Xt
        // Encoding: 1101 0101 0000 1 011 0111 1110 001 Rt
        let word = 0xd50b7e20 | rt;
        return Ok(EncodeResult::Word(word));
    }

    Err(format!("unsupported dc variant: {}", raw_operands))
}
