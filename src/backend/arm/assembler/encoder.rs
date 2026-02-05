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
    /// R_AARCH64_ADR_PREL_LO21 - for ADR (21-bit PC-relative)
    AdrPrelLo21,
    /// R_AARCH64_ABS64 - 64-bit absolute
    Abs64,
    /// R_AARCH64_ABS32 - 32-bit absolute
    Abs32,
    /// R_AARCH64_PREL32 - 32-bit PC-relative
    Prel32,
    /// R_AARCH64_LD_PREL_LO19 - LDR literal, 19-bit PC-relative
    Ldr19,
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
            RelocType::AdrPrelLo21 => 274,      // R_AARCH64_ADR_PREL_LO21
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
            RelocType::TlsLeAddTprelLo12 => 551, // R_AARCH64_TLSLE_ADD_TPREL_LO12_NC
            RelocType::CondBr19 => 280,        // R_AARCH64_CONDBR19
            RelocType::TstBr14 => 279,         // R_AARCH64_TSTBR14
            RelocType::Ldr19 => 273,             // R_AARCH64_LD_PREL_LO19
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
    if let Some(cond) = mn.strip_prefix("b.") {
        return encode_cond_branch(cond, operands);
    }

    // Handle condition-code branches without the dot: beq, bne, bge, blt, etc.
    // These are common aliases used in GNU assembler syntax.
    {
        let cond_aliases: &[(&str, &str)] = &[
            ("beq", "eq"), ("bne", "ne"), ("bcs", "cs"), ("bhs", "hs"),
            ("bcc", "cc"), ("blo", "lo"), ("bmi", "mi"), ("bpl", "pl"),
            ("bvs", "vs"), ("bvc", "vc"), ("bhi", "hi"), ("bls", "ls"),
            ("bge", "ge"), ("blt", "lt"), ("bgt", "gt"), ("ble", "le"),
            ("bal", "al"),
        ];
        for &(alias, cond) in cond_aliases {
            if mn == alias {
                return encode_cond_branch(cond, operands);
            }
        }
    }

    match mn.as_str() {
        // Data processing - register
        "mov" => encode_mov(operands),
        "movz" => encode_movz(operands),
        "movk" => encode_movk(operands),
        "movn" => encode_movn(operands),
        "add" => if is_neon_scalar_d_reg_op(operands) {
            encode_neon_scalar_three_same(operands, 0, 0b10000, 0b11)
        } else { encode_add_sub(operands, false, false) },
        "adds" => encode_add_sub(operands, false, true),
        "sub" => if is_neon_scalar_d_reg_op(operands) {
            encode_neon_scalar_three_same(operands, 1, 0b10000, 0b11)
        } else { encode_add_sub(operands, true, false) },
        "subs" => encode_add_sub(operands, true, true),
        "and" => encode_logical(operands, 0b00),
        "orr" => encode_logical(operands, 0b01),
        "eor" => encode_logical(operands, 0b10),
        "ands" => encode_logical(operands, 0b11),
        "orn" => encode_orn(operands),
        "eon" => encode_eon(operands),
        "bics" => encode_bics(operands),
        "mul" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem(operands, 0, 0b1000)
            } else {
                encode_neon_three_same(operands, 0, 0b10011)
            }
        } else { encode_mul(operands) },
        "madd" => encode_madd(operands),
        "msub" => encode_msub(operands),
        "smull" => {
            if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
                if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                    encode_neon_elem_long(operands, 0, 0b1010, false) // SMULL (by element)
                } else {
                    encode_neon_three_diff(operands, 0, 0b1100, false) // SMULL (vector)
                }
            } else {
                encode_smull(operands) // SMULL (scalar)
            }
        }
        "umull" => {
            if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
                if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                    encode_neon_elem_long(operands, 1, 0b1010, false) // UMULL (by element)
                } else {
                    encode_neon_three_diff(operands, 1, 0b1100, false) // UMULL (vector)
                }
            } else {
                encode_umull(operands) // UMULL (scalar)
            }
        }
        "smaddl" => encode_smaddl(operands),
        "umaddl" => encode_umaddl(operands),
        "mneg" => encode_mneg(operands),
        "udiv" => encode_div(operands, true),
        "sdiv" => encode_div(operands, false),
        "umulh" => encode_umulh(operands),
        "smulh" => encode_smulh(operands),
        "neg" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_two_misc(operands, 1, 0b01011)
        } else { encode_neg(operands) },
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
        "ldrb" => encode_ldr_str(operands, true, 0b00, false, false), // byte load
        "strb" => encode_ldr_str(operands, false, 0b00, false, false),
        "ldrh" => encode_ldr_str(operands, true, 0b01, false, false), // halfword load
        "strh" => encode_ldr_str(operands, false, 0b01, false, false),
        "ldrw" | "ldrsw" => encode_ldrsw(operands),
        "ldrsb" => encode_ldrs(operands, 0b00),
        "ldrsh" => encode_ldrs(operands, 0b01),
        "ldur" => encode_ldur_stur(operands, true, 0b00),
        "stur" => encode_ldur_stur(operands, false, 0b00),
        "ldtr" => encode_ldur_stur(operands, true, 0b10),
        "sttr" => encode_ldur_stur(operands, false, 0b10),
        "ldtrh" => encode_ldtr_sized(operands, true, 0b01),
        "sttrh" => encode_ldtr_sized(operands, false, 0b01),
        "ldtrb" => encode_ldtr_sized(operands, true, 0b00),
        "sttrb" => encode_ldtr_sized(operands, false, 0b00),
        "ldp" => encode_ldp_stp(operands, true),
        "stp" => encode_ldp_stp(operands, false),
        "ldnp" => encode_ldnp_stnp(operands, true),
        "stnp" => encode_ldnp_stnp(operands, false),
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

        // Floating point (scalar or vector based on operand type)
        "fmov" => encode_fmov(operands),
        "fadd" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_three_same(operands, 0, 0, 0b11010)
        } else { encode_fp_arith(operands, 0b0010) },
        "fsub" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_three_same(operands, 0, 1, 0b11010)
        } else { encode_fp_arith(operands, 0b0011) },
        "fmul" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_float_elem(operands, 1, 0b1001)
            } else {
                encode_neon_float_three_same(operands, 1, 0, 0b11011)
            }
        } else { encode_fp_arith(operands, 0b0000) },
        "fdiv" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_three_same(operands, 1, 0, 0b11111)
        } else { encode_fp_arith(operands, 0b0001) },
        "fmax" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_three_same(operands, 0, 0, 0b11110)
        } else { encode_fp_arith(operands, 0b0100) },
        "fmin" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_three_same(operands, 0, 1, 0b11110)
        } else { encode_fp_arith(operands, 0b0101) },
        "fmaxnm" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_three_same(operands, 0, 0, 0b11000)
        } else { encode_fp_arith(operands, 0b0110) },
        "fminnm" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_three_same(operands, 0, 1, 0b11000)
        } else { encode_fp_arith(operands, 0b0111) },
        "fneg" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 1, 0, 0b01111)
        } else { encode_fneg(operands) },
        "fabs" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 0, 1, 0b01111)
        } else { encode_fabs(operands) },
        "fsqrt" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 1, 1, 0b11111)
        } else { encode_fsqrt(operands) },
        "frintn" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 0, 0, 0b11000)
        } else { encode_fp_1src(operands, 0b001000) },
        "frintp" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 0, 1, 0b11000)
        } else { encode_fp_1src(operands, 0b001001) },
        "frintm" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 0, 0, 0b11001)
        } else { encode_fp_1src(operands, 0b001010) },
        "frintz" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 0, 1, 0b11001)
        } else { encode_fp_1src(operands, 0b001011) },
        "frinta" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 1, 0, 0b11000)
        } else { encode_fp_1src(operands, 0b001100) },
        "frintx" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 1, 0, 0b11001)
        } else { encode_fp_1src(operands, 0b001110) },
        "frinti" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 1, 1, 0b11001)
        } else { encode_fp_1src(operands, 0b001111) },
        "fmadd" => encode_fmadd_fmsub(operands, false),
        "fmsub" => encode_fmadd_fmsub(operands, true),
        "fnmadd" => encode_fnmadd_fnmsub(operands, false),
        "fnmsub" => encode_fnmadd_fnmsub(operands, true),
        "fcmp" => encode_fcmp(operands),
        "fcvtzs" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 0, 1, 0b11011)
        } else { encode_fcvt_rounding(operands, 0b11, 0b000) },
        "fcvtzu" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 1, 1, 0b11011)
        } else { encode_fcvt_rounding(operands, 0b11, 0b001) },
        "fcvtas" => encode_fcvt_rounding(operands, 0b00, 0b100),
        "fcvtau" => encode_fcvt_rounding(operands, 0b00, 0b101),
        "fcvtns" => encode_fcvt_rounding(operands, 0b00, 0b000),
        "fcvtnu" => encode_fcvt_rounding(operands, 0b00, 0b001),
        "fcvtms" => encode_fcvt_rounding(operands, 0b10, 0b000),
        "fcvtmu" => encode_fcvt_rounding(operands, 0b10, 0b001),
        "fcvtps" => encode_fcvt_rounding(operands, 0b01, 0b000),
        "fcvtpu" => encode_fcvt_rounding(operands, 0b01, 0b001),
        "ucvtf" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 1, 0, 0b11101)
        } else { encode_ucvtf(operands) },
        "scvtf" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_float_two_misc(operands, 0, 0, 0b11101)
        } else { encode_scvtf(operands) },
        "fcvt" => encode_fcvt_precision(operands),
        "fcvtl" => encode_neon_fcvtl(operands, false),
        "fcvtl2" => encode_neon_fcvtl(operands, true),
        "fcvtn" => encode_neon_fcvtn(operands, false),
        "fcvtn2" => encode_neon_fcvtn(operands, true),
        // NEON float three-same instructions (vector-only)
        "fmla" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_float_elem(operands, 0, 0b0001)
        } else {
            encode_neon_float_three_same(operands, 0, 0, 0b11001)
        },
        "fmls" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_float_elem(operands, 0, 0b0101)
        } else {
            encode_neon_float_three_same(operands, 0, 1, 0b11001)
        },
        "frecps" => encode_neon_float_three_same(operands, 0, 0, 0b11111),
        "frsqrts" => encode_neon_float_three_same(operands, 0, 1, 0b11111),
        "fcmeq" => if matches!(operands.get(2), Some(Operand::Imm(0))) {
            encode_neon_float_cmp_zero(operands, 0, 0, 0b01101)
        } else {
            encode_neon_float_three_same(operands, 0, 0, 0b11100)
        },
        "fcmge" => if matches!(operands.get(2), Some(Operand::Imm(0))) {
            encode_neon_float_cmp_zero(operands, 1, 0, 0b01100)
        } else {
            encode_neon_float_three_same(operands, 1, 0, 0b11100)
        },
        "fcmgt" => if matches!(operands.get(2), Some(Operand::Imm(0))) {
            encode_neon_float_cmp_zero(operands, 0, 1, 0b01100)
        } else {
            encode_neon_float_three_same(operands, 1, 1, 0b11100)
        },
        "fcmle" => encode_neon_float_cmp_zero(operands, 1, 0, 0b01101),
        "fcmlt" => encode_neon_float_cmp_zero(operands, 0, 1, 0b01101),
        "facge" => encode_neon_float_three_same(operands, 1, 0, 0b11101),
        "facgt" => encode_neon_float_three_same(operands, 1, 1, 0b11101),

        // NEON/SIMD
        "cnt" => encode_cnt(operands),
        "cmeq" => {
            // CMEQ has two forms:
            // - CMEQ Vd, Vn, Vm (three-same, U=1): compare registers
            // - CMEQ Vd, Vn, #0 (two-reg misc, U=0): compare to zero
            if matches!(operands.get(2), Some(Operand::Imm(0))) {
                encode_neon_cmp_zero(operands, 0, 0b01001)
            } else {
                encode_neon_three_same(operands, 1, 0b10001)
            }
        }
        "cmhi" => encode_neon_three_same(operands, 1, 0b00110),
        "cmhs" => encode_neon_three_same(operands, 1, 0b00111),
        "cmge" => if matches!(operands.get(2), Some(Operand::Imm(0))) {
            encode_neon_cmp_zero(operands, 1, 0b01000)   // CMGE #0
        } else {
            encode_neon_three_same(operands, 0, 0b00111)
        },
        "cmgt" => if matches!(operands.get(2), Some(Operand::Imm(0))) {
            encode_neon_cmp_zero(operands, 0, 0b01000)   // CMGT #0
        } else {
            encode_neon_three_same(operands, 0, 0b00110)
        },
        "cmtst" => encode_neon_three_same(operands, 0, 0b10001),
        "uqsub" => encode_neon_three_same(operands, 1, 0b00101),
        "sqsub" => encode_neon_three_same(operands, 0, 0b00101),
        "uhadd" => encode_neon_three_same(operands, 1, 0b00000),
        "shadd" => encode_neon_three_same(operands, 0, 0b00000),
        "urhadd" => encode_neon_three_same(operands, 1, 0b00010),
        "srhadd" => encode_neon_three_same(operands, 0, 0b00010),
        "uhsub" => encode_neon_three_same(operands, 1, 0b00100),
        "shsub" => encode_neon_three_same(operands, 0, 0b00100),
        "umax" => encode_neon_three_same(operands, 1, 0b01100),
        "smax" => encode_neon_three_same(operands, 0, 0b01100),
        "umin" => encode_neon_three_same(operands, 1, 0b01101),
        "smin" => encode_neon_three_same(operands, 0, 0b01101),
        "uabd" => encode_neon_three_same(operands, 1, 0b01110),
        "sabd" => encode_neon_three_same(operands, 0, 0b01110),
        "uaba" => encode_neon_three_same(operands, 1, 0b01111),
        "saba" => encode_neon_three_same(operands, 0, 0b01111),
        "uqadd" => encode_neon_three_same(operands, 1, 0b00001),
        "sqadd" => encode_neon_three_same(operands, 0, 0b00001),
        "sshl" => encode_neon_three_same(operands, 0, 0b01000),
        "ushl" => encode_neon_three_same(operands, 1, 0b01000),
        "sqshl" => if matches!(operands.get(2), Some(Operand::Imm(_))) {
            encode_neon_shift_left_imm(operands, 0, 0b01110)
        } else {
            encode_neon_three_same(operands, 0, 0b01001)
        },
        "uqshl" => if matches!(operands.get(2), Some(Operand::Imm(_))) {
            encode_neon_shift_left_imm(operands, 1, 0b01110)
        } else {
            encode_neon_three_same(operands, 1, 0b01001)
        },
        "srshl" => encode_neon_three_same(operands, 0, 0b01010),
        "urshl" => encode_neon_three_same(operands, 1, 0b01010),
        "sqrshl" => encode_neon_three_same(operands, 0, 0b01011),
        "uqrshl" => encode_neon_three_same(operands, 1, 0b01011),
        "addp" => if operands.len() == 2 && matches!(operands.first(), Some(Operand::Reg(r)) if r.starts_with('d') || r.starts_with('D')) {
            // Scalar ADDP: addp Dd, Vn.2d
            encode_neon_scalar_addp(operands)
        } else {
            encode_neon_three_same(operands, 0, 0b10111)
        },
        "uminp" => encode_neon_three_same(operands, 1, 0b10101),
        "umaxp" => encode_neon_three_same(operands, 1, 0b10100),
        "sminp" => encode_neon_three_same(operands, 0, 0b10101),
        "smaxp" => encode_neon_three_same(operands, 0, 0b10100),
        // NEON two-reg misc (integer)
        "abs" => encode_neon_two_misc(operands, 0, 0b01011),
        // neg dispatch moved to early scalar section
        "cls" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_two_misc(operands, 0, 0b00100)
        } else { encode_cls(operands) },
        "clz" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_two_misc(operands, 1, 0b00100)
        } else { encode_clz(operands) },
        "rev16" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_two_misc(operands, 0, 0b00001)
        } else { encode_rev16(operands) },
        "rev32" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_two_misc(operands, 1, 0b00000)
        } else { encode_rev32(operands) },
        "saddlp" => encode_neon_two_misc(operands, 0, 0b00010),
        "uaddlp" => encode_neon_two_misc(operands, 1, 0b00010),
        "sadalp" => encode_neon_two_misc(operands, 0, 0b00110),
        "uadalp" => encode_neon_two_misc(operands, 1, 0b00110),
        "sqxtun" => encode_neon_two_misc_narrow(operands, 1, 0b10010, false),
        "sqxtun2" => encode_neon_two_misc_narrow(operands, 1, 0b10010, true),
        "sqabs" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_two_misc(operands, 0, 0b00111)
        } else {
            encode_neon_scalar_two_misc(operands, 0, 0b00111)
        },
        "sqneg" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_two_misc(operands, 0, 0b01000)
        } else {
            encode_neon_scalar_two_misc(operands, 0, 0b01000)
        },
        // Compare to zero forms
        "cmlt" => encode_neon_cmp_zero(operands, 0, 0b01010),  // CMLT #0
        "cmle" => encode_neon_cmp_zero(operands, 1, 0b01001),  // CMLE #0
        // NEON shift right narrow
        "shrn" => encode_neon_shrn(operands, 0b100001, false),
        "shrn2" => encode_neon_shrn(operands, 0b100001, true),
        "rshrn" => encode_neon_shrn(operands, 0b100011, false),
        "rshrn2" => encode_neon_shrn(operands, 0b100011, true),
        // NEON shift right accumulate and rounding shift right
        "srshr" => encode_neon_shift_right(operands, 0, 0b001001),
        "urshr" => encode_neon_shift_right(operands, 1, 0b001001),
        "ssra" => encode_neon_shift_right(operands, 0, 0b000101),
        "usra" => encode_neon_shift_right(operands, 1, 0b000101),
        "srsra" => encode_neon_shift_right(operands, 0, 0b001101),
        "ursra" => encode_neon_shift_right(operands, 1, 0b001101),
        // NEON shift left long
        "ushll" => encode_neon_shll(operands, 1, false),
        "ushll2" => encode_neon_shll(operands, 1, true),
        "sshll" => encode_neon_shll(operands, 0, false),
        "sshll2" => encode_neon_shll(operands, 0, true),
        // NEON three-different extras
        "uabal" => encode_neon_three_diff(operands, 1, 0b0101, false),
        "uabal2" => encode_neon_three_diff(operands, 1, 0b0101, true),
        "sabal" => encode_neon_three_diff(operands, 0, 0b0101, false),
        "sabal2" => encode_neon_three_diff(operands, 0, 0b0101, true),
        "uabdl" => encode_neon_three_diff(operands, 1, 0b0111, false),
        "uabdl2" => encode_neon_three_diff(operands, 1, 0b0111, true),
        "sabdl" => encode_neon_three_diff(operands, 0, 0b0111, false),
        "sabdl2" => encode_neon_three_diff(operands, 0, 0b0111, true),
        // ADDHN/RADDHN/SUBHN/RSUBHN (narrowing high)
        "addhn" => encode_neon_three_diff_narrow(operands, 0, 0b0100, false),
        "addhn2" => encode_neon_three_diff_narrow(operands, 0, 0b0100, true),
        "raddhn" => encode_neon_three_diff_narrow(operands, 1, 0b0100, false),
        "raddhn2" => encode_neon_three_diff_narrow(operands, 1, 0b0100, true),
        "subhn" => encode_neon_three_diff_narrow(operands, 0, 0b0110, false),
        "subhn2" => encode_neon_three_diff_narrow(operands, 0, 0b0110, true),
        "rsubhn" => encode_neon_three_diff_narrow(operands, 1, 0b0110, false),
        "rsubhn2" => encode_neon_three_diff_narrow(operands, 1, 0b0110, true),
        // NEON sat shift right narrow
        "sqshrn" => if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
            encode_neon_qshrn(operands, 0, false, false)
        } else {
            encode_neon_scalar_qshrn(operands, 0, false)
        },
        "sqshrn2" => encode_neon_qshrn(operands, 0, false, true),
        "uqshrn" => encode_neon_qshrn(operands, 1, false, false),
        "uqshrn2" => encode_neon_qshrn(operands, 1, false, true),
        "sqrshrn" => encode_neon_qshrn(operands, 0, true, false),
        "sqrshrn2" => encode_neon_qshrn(operands, 0, true, true),
        "uqrshrn" => encode_neon_qshrn(operands, 1, true, false),
        "uqrshrn2" => encode_neon_qshrn(operands, 1, true, true),
        "sqrshrun" => encode_neon_sqshrun(operands, true, false),
        "sqrshrun2" => encode_neon_sqshrun(operands, true, true),
        // NEON permute: TRN1/TRN2
        "trn1" => encode_neon_zip_uzp(operands, 0b010, false),
        "trn2" => encode_neon_zip_uzp(operands, 0b110, false),
        // NEON replicate loads
        "ld2r" => encode_neon_ldnr(operands, 2),
        "ld3r" => encode_neon_ldnr(operands, 3),
        "ld4r" => encode_neon_ldnr(operands, 4),
        "ushr" => encode_neon_ushr(operands),
        "sshr" => encode_neon_sshr(operands),
        "shl" => encode_neon_shl(operands),
        "sli" => encode_neon_sli(operands),
        "sri" => encode_neon_sri(operands),
        "ext" => encode_neon_ext(operands),
        "addv" => encode_neon_addv(operands),
        "umaxv" => encode_neon_across(operands, 1, 0b01010),
        "uminv" => encode_neon_across(operands, 1, 0b11010),
        "smaxv" => encode_neon_across(operands, 0, 0b01010),
        "sminv" => encode_neon_across(operands, 0, 0b11010),
        "umov" => encode_neon_umov(operands),
        "dup" => encode_neon_dup(operands),
        "ins" => encode_neon_ins(operands),
        "not" => encode_neon_not(operands),
        "movi" => encode_neon_movi(operands),
        "bic" => encode_bic(operands),
        "bsl" => encode_neon_bsl(operands),
        "bit" => encode_neon_bitwise_insert(operands, 0b10),
        "bif" => encode_neon_bitwise_insert(operands, 0b11),
        "faddp" => encode_neon_faddp(operands),
        "saddlv" => encode_neon_across_long(operands, 0, 0b00011),
        "uaddlv" => encode_neon_across_long(operands, 1, 0b00011),
        "sqdmlal" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_elem_long(operands, 0, 0b0011, false)
        } else {
            encode_neon_three_diff(operands, 0, 0b1001, false)
        },
        "sqdmlal2" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_elem_long(operands, 0, 0b0011, true)
        } else {
            encode_neon_three_diff(operands, 0, 0b1001, true)
        },
        "sqdmlsl" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_elem_long(operands, 0, 0b0111, false)
        } else {
            encode_neon_three_diff(operands, 0, 0b1011, false)
        },
        "sqdmlsl2" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_elem_long(operands, 0, 0b0111, true)
        } else {
            encode_neon_three_diff(operands, 0, 0b1011, true)
        },
        "sqdmull" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_elem_long(operands, 0, 0b1011, false)
        } else {
            encode_neon_three_diff(operands, 0, 0b1101, false)
        },
        "sqdmull2" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_elem_long(operands, 0, 0b1011, true)
        } else {
            encode_neon_three_diff(operands, 0, 0b1101, true)
        },
        "sqdmulh" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_elem(operands, 0, 0b1100)
        } else {
            encode_neon_three_same(operands, 0, 0b10110)
        },
        "sqrdmulh" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_elem(operands, 0, 0b1101)
        } else {
            encode_neon_three_same(operands, 1, 0b10110)
        },
        "pmul" => encode_neon_pmul(operands),
        "mla" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_elem(operands, 0, 0b0000)
        } else { encode_neon_mla(operands) },
        "mls" => if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
            encode_neon_elem(operands, 0, 0b0100)
        } else { encode_neon_mls(operands) },
        "rev64" => encode_neon_rev64(operands),
        "tbl" => encode_neon_tbl(operands),
        "tbx" => encode_neon_tbx(operands),
        "ld1" => encode_neon_ld_st_dispatch(operands, true, 1),
        "ld1r" => encode_neon_ld1r(operands),
        "ld2" => encode_neon_ld_st_dispatch(operands, true, 2),
        "ld3" => encode_neon_ld_st_dispatch(operands, true, 3),
        "ld4" => encode_neon_ld_st_dispatch(operands, true, 4),
        "st1" => encode_neon_ld_st_dispatch(operands, false, 1),
        "st2" => encode_neon_ld_st_dispatch(operands, false, 2),
        "st3" => encode_neon_ld_st_dispatch(operands, false, 3),
        "st4" => encode_neon_ld_st_dispatch(operands, false, 4),
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

        // NEON three-different (widening/narrowing)
        "usubl" => encode_neon_three_diff(operands, 1, 0b0010, false),
        "usubl2" => encode_neon_three_diff(operands, 1, 0b0010, true),
        "ssubl" => encode_neon_three_diff(operands, 0, 0b0010, false),
        "ssubl2" => encode_neon_three_diff(operands, 0, 0b0010, true),
        "usubw" => encode_neon_three_diff(operands, 1, 0b0011, false),
        "usubw2" => encode_neon_three_diff(operands, 1, 0b0011, true),
        "ssubw" => encode_neon_three_diff(operands, 0, 0b0011, false),
        "ssubw2" => encode_neon_three_diff(operands, 0, 0b0011, true),
        "uaddl" => encode_neon_three_diff(operands, 1, 0b0000, false),
        "uaddl2" => encode_neon_three_diff(operands, 1, 0b0000, true),
        "saddl" => encode_neon_three_diff(operands, 0, 0b0000, false),
        "saddl2" => encode_neon_three_diff(operands, 0, 0b0000, true),
        "uaddw" => encode_neon_three_diff(operands, 1, 0b0001, false),
        "uaddw2" => encode_neon_three_diff(operands, 1, 0b0001, true),
        "saddw" => encode_neon_three_diff(operands, 0, 0b0001, false),
        "saddw2" => encode_neon_three_diff(operands, 0, 0b0001, true),
        "umlal" => {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem_long(operands, 1, 0b0010, false)
            } else {
                encode_neon_three_diff(operands, 1, 0b1000, false)
            }
        }
        "umlal2" => {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem_long(operands, 1, 0b0010, true)
            } else {
                encode_neon_three_diff(operands, 1, 0b1000, true)
            }
        }
        "smlal" => {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem_long(operands, 0, 0b0010, false)
            } else {
                encode_neon_three_diff(operands, 0, 0b1000, false)
            }
        }
        "smlal2" => {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem_long(operands, 0, 0b0010, true)
            } else {
                encode_neon_three_diff(operands, 0, 0b1000, true)
            }
        }
        "umlsl" => {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem_long(operands, 1, 0b0110, false)
            } else {
                encode_neon_three_diff(operands, 1, 0b1010, false)
            }
        }
        "umlsl2" => {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem_long(operands, 1, 0b0110, true)
            } else {
                encode_neon_three_diff(operands, 1, 0b1010, true)
            }
        }
        "smlsl" => {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem_long(operands, 0, 0b0110, false)
            } else {
                encode_neon_three_diff(operands, 0, 0b1010, false)
            }
        }
        "smlsl2" => {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem_long(operands, 0, 0b0110, true)
            } else {
                encode_neon_three_diff(operands, 0, 0b1010, true)
            }
        }
        "umull2" => {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem_long(operands, 1, 0b1010, true)
            } else {
                encode_neon_three_diff(operands, 1, 0b1100, true)
            }
        }
        "smull2" => {
            if matches!(operands.get(2), Some(Operand::RegLane { .. })) {
                encode_neon_elem_long(operands, 0, 0b1010, true)
            } else {
                encode_neon_three_diff(operands, 0, 0b1100, true)
            }
        }

        // NEON saturating shift right narrow
        "sqshrun" => encode_neon_sqshrun(operands, false, false),
        "sqshrun2" => encode_neon_sqshrun(operands, false, true),

        // NEON extend long (aliases for USHLL/SSHLL #0)
        "uxtl" => encode_neon_xtl(operands, 1, false),
        "uxtl2" => encode_neon_xtl(operands, 1, true),
        "sxtl" => encode_neon_xtl(operands, 0, false),
        "sxtl2" => encode_neon_xtl(operands, 0, true),

        // NEON two-register narrowing
        "uqxtn" => encode_neon_two_misc_narrow(operands, 1, 0b10100, false),
        "uqxtn2" => encode_neon_two_misc_narrow(operands, 1, 0b10100, true),
        "sqxtn" => encode_neon_two_misc_narrow(operands, 0, 0b10100, false),
        "sqxtn2" => encode_neon_two_misc_narrow(operands, 0, 0b10100, true),
        "xtn" => encode_neon_two_misc_narrow(operands, 0, 0b10010, false),
        "xtn2" => encode_neon_two_misc_narrow(operands, 0, 0b10010, true),

        // System
        "hint" => encode_hint(operands),
        "bti" => encode_bti(raw_operands),
        "nop" => Ok(EncodeResult::Word(0xd503201f)),
        "yield" => Ok(EncodeResult::Word(0xd503203f)),
        "wfe" => Ok(EncodeResult::Word(0xd503205f)),
        "wfi" => Ok(EncodeResult::Word(0xd503207f)),
        "sev" => Ok(EncodeResult::Word(0xd503209f)),
        "sevl" => Ok(EncodeResult::Word(0xd50320bf)),
        "eret" => Ok(EncodeResult::Word(0xd69f03e0)),
        "clrex" => Ok(EncodeResult::Word(0xd503305f)),
        "dc" => encode_dc(operands, raw_operands),
        "tlbi" => encode_tlbi(operands, raw_operands),
        "ic" => encode_ic(raw_operands),
        "dmb" => encode_dmb(operands),
        "dsb" => encode_dsb(operands),
        "isb" => Ok(EncodeResult::Word(0xd5033fdf)),
        "mrs" => encode_mrs(operands),
        "msr" => encode_msr(operands),
        "svc" => encode_svc(operands),
        "hvc" => encode_hvc(operands),
        "smc" => encode_smc(operands),
        "at" => encode_at(operands, raw_operands),
        "sys" => encode_sys(raw_operands),
        "brk" => encode_brk(operands),

        // Bitfield extract/insert
        "ubfx" => encode_ubfx(operands),
        "sbfx" => encode_sbfx(operands),
        "ubfm" => encode_ubfm(operands),
        "sbfm" => encode_sbfm(operands),
        "ubfiz" => encode_ubfiz(operands),
        "sbfiz" => encode_sbfiz(operands),
        "bfm" => encode_bfm(operands),
        "bfi" => encode_bfi(operands),
        "bfxil" => encode_bfxil(operands),
        "extr" => encode_extr(operands),

        // Additional conditional operations
        "cneg" => encode_cneg(operands),
        "cinc" => encode_cinc(operands),
        "cinv" => encode_cinv(operands),

        // Bit manipulation
        "rbit" => {
            // RBIT has both scalar and NEON forms
            if matches!(operands.first(), Some(Operand::RegArrangement { .. })) {
                encode_neon_rbit(operands)
            } else {
                encode_rbit(operands)
            }
        }
        "rev" => encode_rev(operands),

        // CRC32
        "crc32b" | "crc32h" | "crc32w" | "crc32x"
        | "crc32cb" | "crc32ch" | "crc32cw" | "crc32cx" => encode_crc32(mnemonic, operands),

        // Prefetch
        "prfm" => encode_prfm(operands),

        // LSE atomics
        "cas" | "casa" | "casal" | "casl" => encode_cas(mnemonic, operands),
        "swp" | "swpa" | "swpal" | "swpl" => encode_swp(mnemonic, operands),
        "ldadd" | "ldadda" | "ldaddal" | "ldaddl"
        | "ldclr" | "ldclra" | "ldclral" | "ldclrl"
        | "ldeor" | "ldeora" | "ldeoral" | "ldeorl"
        | "ldset" | "ldseta" | "ldsetal" | "ldsetl" => encode_ldop(mnemonic, operands),

        // NEON move-not-immediate
        "mvni" => encode_neon_mvni(operands),

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
        // The parser misclassifies symbol names that collide with register names,
        // condition codes, or barrier names. These are valid symbols in context.
        Some(Operand::Reg(name)) => Ok((name.clone(), 0)),
        Some(Operand::Cond(name)) => Ok((name.clone(), 0)),
        Some(Operand::Barrier(name)) => Ok((name.clone(), 0)),
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
        (operands.first(), operands.get(1))
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
        (operands.first(), operands.get(1))
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
        (operands.first(), operands.get(1))
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
        (operands.first(), operands.get(1))
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
        let word = ((0b01101110000u32 << 21) | (imm5 << 16)) | (imm4 << 11) | (1 << 10) | (vn << 5) | vd;
        return Ok(EncodeResult::Word(word));
    }

    // mov Xd, #imm -> movz or movn
    if let Some(Operand::Imm(imm)) = operands.get(1) {
        let (rd, is_64) = get_reg(operands, 0)?;
        let imm = *imm;

        // Check if it can be a simple MOVZ
        if (0..=0xFFFF).contains(&imm) {
            let sf = sf_bit(is_64);
            let word = (sf << 31) | (0b10100101 << 23) | ((imm as u32 & 0xFFFF) << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }

        // Negative: try MOVN
        if imm < 0 {
            let not_imm = !imm;
            if (0..=0xFFFF).contains(&not_imm) {
                let sf = sf_bit(is_64);
                let word = (sf << 31) | (0b00100101 << 23) | ((not_imm as u32 & 0xFFFF) << 5) | rd;
                return Ok(EncodeResult::Word(word));
            }
        }

        // Need movz + movk sequence for large immediates
        return encode_mov_wide_imm(rd, is_64, imm as u64);
    }

    // mov Xd, Xm -> ORR Xd, XZR, Xm
    if let (Some(Operand::Reg(rd_name)), Some(Operand::Reg(rm_name))) = (operands.first(), operands.get(1)) {
        let rd = parse_reg_num(rd_name).ok_or("invalid rd")?;
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;
        let is_64 = is_64bit_reg(rd_name);

        // Check for MOV to/from SP: uses ADD Xd, Xn, #0
        if rd_name.to_lowercase() == "sp" || rm_name.to_lowercase() == "sp" {
            let sf = sf_bit(is_64);
            // ADD Xd, Xn, #0: sf 0 0 10001 00 imm12=0 Rn Rd
            let word = ((sf << 31) | (0b10001 << 24)) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }

        let sf = sf_bit(is_64);
        // ORR Rd, XZR, Rm: sf 01 01010 00 0 Rm 000000 11111 Rd
        let word = ((sf << 31) | (0b01 << 29) | (0b01010 << 24)) | (rm << 16) | (0b11111 << 5) | rd;
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

/// Resolve `:abs_g0:`, `:abs_g1:`, etc. modifiers for movz/movk.
/// If the expression is a pure constant, returns Some((imm16, hw)) where
/// imm16 is the relevant 16-bit chunk and hw is the halfword selector.
/// If the expression contains a symbol reference, returns None (needs relocation).
fn resolve_abs_g_modifier(kind: &str, symbol: &str) -> Result<Option<(u32, u32)>, String> {
    let shift = match kind {
        "abs_g0" | "abs_g0_nc" => 0,
        "abs_g1" | "abs_g1_nc" => 16,
        "abs_g2" | "abs_g2_nc" => 32,
        "abs_g3" => 48,
        _ => return Ok(None), // Not an abs_g modifier
    };
    let hw = shift / 16;
    // Try to evaluate the expression as a constant
    if let Ok(val) = crate::backend::asm_expr::parse_integer_expr(symbol) {
        let imm16 = ((val as u64) >> shift) as u32 & 0xFFFF;
        Ok(Some((imm16, hw)))
    } else {
        Ok(None) // Contains symbol reference - needs relocation
    }
}

fn encode_movz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let sf = sf_bit(is_64);

    // Handle :abs_g*: modifiers
    if let Some(Operand::Modifier { kind, symbol }) = operands.get(1) {
        if let Some((imm16, hw)) = resolve_abs_g_modifier(kind, symbol)? {
            let word = (sf << 31) | (0b10100101 << 23) | (hw << 21) | ((imm16 & 0xFFFF) << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }
    }

    let imm = get_imm(operands, 1)?;

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
    let sf = sf_bit(is_64);

    // Handle :abs_g*: modifiers
    if let Some(Operand::Modifier { kind, symbol }) = operands.get(1) {
        if let Some((imm16, hw)) = resolve_abs_g_modifier(kind, symbol)? {
            let word = (sf << 31) | (0b11100101 << 23) | (hw << 21) | ((imm16 & 0xFFFF) << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }
    }

    let imm = get_imm(operands, 1)?;

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
    if let Some(Operand::RegArrangement { .. }) = operands.first() {
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
        let imm_signed = *imm;
        // Handle negative immediates: add #-N -> sub #N and vice versa
        let (imm_val, actual_op) = if imm_signed < 0 {
            ((-imm_signed) as u64, if is_sub { 0u32 } else { 1u32 })
        } else {
            (imm_signed as u64, op)
        };
        // Check for explicit lsl #12 shift
        let explicit_shift = if operands.len() > 3 {
            if let Some(Operand::Shift { kind, amount }) = operands.get(3) {
                kind == "lsl" && *amount == 12
            } else { false }
        } else { false };

        let (imm12, sh) = if explicit_shift {
            // Explicit lsl #12: use the immediate as-is (must fit in 12 bits)
            ((imm_val as u32) & 0xFFF, 1u32)
        } else if imm_val <= 0xFFF {
            // Fits in 12 bits unshifted
            (imm_val as u32, 0u32)
        } else if (imm_val & 0xFFF) == 0 && (imm_val >> 12) <= 0xFFF {
            // Low 12 bits are zero and shifted value fits: auto-shift
            // e.g., #4096 -> #1, lsl #12
            ((imm_val >> 12) as u32, 1u32)
        } else {
            return Err(format!("immediate {} does not fit in add/sub imm12 encoding", imm_val));
        };

        let word = (sf << 31) | (actual_op << 30) | (s_bit << 29) | (0b10001 << 24) | (sh << 22) | (imm12 << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    // ADD Rd, Rn, :lo12:symbol
    if let Some(Operand::Modifier { kind, symbol }) = operands.get(2) {
        if kind == "lo12" {
            let word = ((sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24)) | (rn << 5) | rd;
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
            let word = ((sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24)) | (rn << 5) | rd;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::TlsLeAddTprelLo12,
                    symbol: symbol.clone(),
                    addend: 0,
                },
            });
        }
        if kind == "tprel_hi12" {
            let word = ((sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24) | (1 << 22)) | (rn << 5) | rd;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::TlsLeAddTprelHi12,
                    symbol: symbol.clone(),
                    addend: 0,
                },
            });
        }
    }
    if let Some(Operand::ModifierOffset { kind, symbol, offset }) = operands.get(2) {
        if kind == "lo12" {
            let word = ((sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24)) | (rn << 5) | rd;
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

        // Check for extended register: add Xd, Xn, Wm, sxtw [#N]
        if let Some(Operand::Extend { kind, amount }) = operands.get(3) {
            let option = match kind.as_str() {
                "uxtb" => 0b000u32,
                "uxth" => 0b001,
                "uxtw" => 0b010,
                "uxtx" => 0b011,
                "sxtb" => 0b100,
                "sxth" => 0b101,
                "sxtw" => 0b110,
                "sxtx" => 0b111,
                _ => 0b011, // default UXTX/LSL
            };
            let imm3 = *amount & 0x7;
            // Extended register form: sf op S 01011 00 1 Rm option imm3 Rn Rd
            let word = ((sf << 31) | (op << 30) | (s_bit << 29) | (0b01011 << 24)) | (1 << 21) | (rm << 16) | (option << 13) | (imm3 << 10) | (rn << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }

        // When Rn or Rd is SP (register 31), the shifted register form encodes
        // register 31 as XZR, not SP. We must use the extended register form
        // with UXTX (option=0b011) to get SP semantics.
        let rn_is_sp = matches!(&operands[1], Operand::Reg(name) if {
            let n = name.to_lowercase(); n == "sp" || n == "wsp"
        });
        let rd_is_sp = matches!(&operands[0], Operand::Reg(name) if {
            let n = name.to_lowercase(); n == "sp" || n == "wsp"
        });

        if (rn_is_sp || rd_is_sp) && operands.len() <= 3 {
            // Extended register form with UXTX #0: sf op S 01011 00 1 Rm 011 000 Rn Rd
            let option = if is_64 { 0b011u32 } else { 0b010u32 }; // UXTX for 64-bit, UXTW for 32-bit
            let word = (((sf << 31) | (op << 30) | (s_bit << 29) | (0b01011 << 24)) | (1 << 21) | (rm << 16) | (option << 13)) | (rn << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }

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

        let word = ((sf << 31) | (op << 30) | (s_bit << 29) | (0b01011 << 24) | (shift_type << 22)) | (rm << 16) | ((shift_amount & 0x3F) << 10) | (rn << 5) | rd;
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
    if let Some(Operand::RegArrangement { .. }) = operands.first() {
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

        let word = ((sf << 31) | (opc << 29) | (0b01010 << 24) | (shift_type << 22))
            | (rm << 16) | ((shift_amount & 0x3F) << 10) | (rn << 5) | rd;
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
        // and the run of 1s starts at bit 0.
        // The `r` we find is the right-rotation from actual -> base.
        // immr is the right-rotation from base -> actual = size - r (mod size).
        let mut found_rotation = false;
        let mut rotation = 0u32;
        for r in 0..size {
            let rot = ((elem >> r) | (elem << (size - r))) & mask;
            // Check if this is a contiguous run from bit 0
            let run = rot.trailing_ones();
            if run == ones {
                // r rotates actual -> base, so immr = size - r (mod size) rotates base -> actual
                rotation = if r == 0 { 0 } else { size - r };
                found_rotation = true;
                break;
            }
        }
        if !found_rotation {
            continue;
        }

        // Encode the fields
        let n = if size == 64 { 1u32 } else { 0u32 };
        let immr = rotation;
        let imms = match size {
            2 => 0b111100 | (ones - 1),
            4 => 0b111000 | (ones - 1),
            8 => 0b110000 | (ones - 1),
            16 => 0b100000 | (ones - 1),
            32 => ones - 1,
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
    if let Some(Operand::RegArrangement { .. }) = operands.first() {
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
    let word = ((sf << 31) | (0b0011011000 << 21) | (rm << 16)) | (ra << 10) | (rn << 5) | rd;
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

/// Encode SMULL Xd, Wn, Wm -> SMADDL Xd, Wn, Wm, XZR
fn encode_smull(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    // SMADDL: 1 00 11011 001 Rm 0 11111 Rn Rd (Ra=XZR makes it SMULL)
    let word = (1u32 << 31) | (0b0011011001 << 21) | (rm << 16)
        | (0b011111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode UMULL Xd, Wn, Wm -> UMADDL Xd, Wn, Wm, XZR
fn encode_umull(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    // UMADDL: 1 00 11011 101 Rm 0 11111 Rn Rd (Ra=XZR makes it UMULL)
    let word = (1u32 << 31) | (0b0011011101 << 21) | (rm << 16)
        | (0b011111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode SMADDL Xd, Wn, Wm, Xa (signed multiply-add long)
fn encode_smaddl(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let (ra, _) = get_reg(operands, 3)?;
    // SMADDL: 1 00 11011 001 Rm 0 Ra Rn Rd
    let word = (1u32 << 31) | (0b0011011001 << 21) | (rm << 16)
        | (ra << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode UMADDL Xd, Wn, Wm, Xa (unsigned multiply-add long)
fn encode_umaddl(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let (ra, _) = get_reg(operands, 3)?;
    // UMADDL: 1 00 11011 101 Rm 0 Ra Rn Rd
    let word = (1u32 << 31) | (0b0011011101 << 21) | (rm << 16)
        | (ra << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode MNEG Xd, Xn, Xm -> MSUB Xd, Xn, Xm, XZR
fn encode_mneg(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    // MSUB with Ra=XZR: sf 00 11011 000 Rm 1 11111 Rn Rd
    let word = (sf << 31) | (0b0011011000 << 21) | (rm << 16)
        | (1 << 15) | (0b11111 << 10) | (rn << 5) | rd;
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
    let word = (((sf << 31) | (1 << 30) | (0b01011 << 24))
        | (rm << 16)) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_negs(operands: &[Operand]) -> Result<EncodeResult, String> {
    // NEGS Rd, Rm -> SUBS Rd, XZR, Rm
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rm, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (((sf << 31) | (1 << 30) | (1 << 29) | (0b01011 << 24))
        | (rm << 16)) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_mvn(operands: &[Operand]) -> Result<EncodeResult, String> {
    // NEON vector form: MVN Vd.T, Vn.T (alias of NOT)
    if let Some(Operand::RegArrangement { .. }) = operands.first() {
        return encode_neon_not(operands);
    }
    // MVN Rd, Rm -> ORN Rd, XZR, Rm
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rm, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (((sf << 31) | (0b01 << 29) | (0b01010 << 24)) | (1 << 21)
        | (rm << 16)) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_adc(operands: &[Operand], set_flags: bool) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    let s = if set_flags { 1u32 } else { 0 };
    let word = ((sf << 31) | (s << 29) | (0b11010000 << 21) | (rm << 16)) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_sbc(operands: &[Operand], set_flags: bool) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    let s = if set_flags { 1u32 } else { 0 };
    let word = ((sf << 31) | (1 << 30) | (s << 29) | (0b11010000 << 21) | (rm << 16)) | (rn << 5) | rd;
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
                let word = (sf << 31) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
                return Ok(EncodeResult::Word(word));
            }
            0b11 => {
                // ROR #imm -> EXTR Rd, Rn, Rn, #imm
                // EXTR: sf 0 0 100111 N 0 Rm imms Rn Rd
                let word = (sf << 31) | (0b00100111 << 23) | (n << 22) | (rn << 16)
                    | (imm << 10) | (rn << 5) | rd;
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
    let word = ((1u32 << 31) | (0b100110 << 23) | (1 << 22)) | (31 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_sxth(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = ((sf << 31) | (0b100110 << 23) | (n << 22)) | (15 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_sxtb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = ((sf << 31) | (0b100110 << 23) | (n << 22)) | (7 << 10) | (rn << 5) | rd;
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
    let word = ((sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22)) | (15 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_uxtb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = ((sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22)) | (7 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Compare ──────────────────────────────────────────────────────────────

fn encode_cmp(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CMP Rn, op -> SUBS XZR, Rn, op
    let mut new_ops = vec![Operand::Reg("xzr".to_string())];
    new_ops.extend(operands.iter().cloned());
    // Determine if 32-bit or 64-bit from the first operand
    let is_32 = if let Some(Operand::Reg(r)) = operands.first() {
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
    let is_32 = if let Some(Operand::Reg(r)) = operands.first() {
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
    let is_32 = if let Some(Operand::Reg(r)) = operands.first() {
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
        let word = ((sf << 31) | (1 << 30) | (1 << 29) | (0b11010010 << 21)
            | ((*imm5 as u32 & 0x1F) << 16) | (cond_val << 12) | (1 << 11)) | (rn << 5) | (*nzcv as u32 & 0xF);
        return Ok(EncodeResult::Word(word));
    }

    // CCMP Rn, Rm, #nzcv, cond
    if let (Some(Operand::Reg(rm_name)), Some(Operand::Imm(nzcv)), Some(Operand::Cond(cond))) =
        (operands.get(1), operands.get(2), operands.get(3))
    {
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;
        let cond_val = encode_cond(cond).ok_or("invalid condition")?;
        let word = ((sf << 31) | (1 << 30) | (1 << 29) | (0b11010010 << 21)
            | (rm << 16) | (cond_val << 12)) | (rn << 5) | (*nzcv as u32 & 0xF);
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
    let word = ((sf << 31) | (0b11010100 << 21)
        | (rm << 16) | (cond << 12)) | (rn << 5) | rd;
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
    let word = (sf << 31) | (0b11010100 << 21)
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
    let word = (((sf << 31) | (1 << 30)) | (0b11010100 << 21)
        | (rm << 16) | (cond << 12)) | (rn << 5) | rd;
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
    let word = ((sf << 31) | (1 << 30)) | (0b11010100 << 21)
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
    let word = (sf << 31) | (0b11010100 << 21)
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
    let word = (((sf << 31) | (1 << 30)) | (0b11010100 << 21)
        | (0b11111 << 16) | (inv_cond << 12)) | (0b11111 << 5) | rd;
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
    let reg_name = match operands.first() {
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
        0b00 // 128-bit: size=00 with opc adjustment in encode_ldr_str
    } else {
        0b11 // default 64-bit
    };

    let is_128bit = reg_name.starts_with('q');
    encode_ldr_str(operands, is_load, size, false, is_128bit)
}

fn encode_ldr_str(operands: &[Operand], is_load: bool, size: u32, is_signed: bool, is_128bit: bool) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("ldr/str requires at least 2 operands".to_string());
    }

    let (rt, _) = get_reg(operands, 0)?;
    let fp = is_fp_reg(operands.first().map(|o| match o { Operand::Reg(r) => r.as_str(), _ => "" }).unwrap_or(""));

    // Use the size parameter as-is (auto-detection happens in encode_ldr_str_auto)
    let actual_size = size;

    let v = if fp { 1u32 } else { 0u32 };

    match operands.get(1) {
        // [base, #offset]
        Some(Operand::Mem { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;

            // Unsigned offset encoding
            // Size determines the shift for offset alignment
            // For 128-bit Q registers: shift=4, opc=11 (load) or 10 (store)
            let shift = if is_128bit { 4 } else { actual_size };
            let opc = if is_128bit {
                if is_load { 0b11 } else { 0b10 }
            } else if is_load {
                if is_signed { 0b10 } else { 0b01 }
            } else {
                0b00
            };

            // Check if offset is aligned and fits in 12-bit unsigned field
            let abs_offset = *offset as u64;
            let align = 1u64 << shift;
            if *offset >= 0 && abs_offset.is_multiple_of(align) {
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
            let opc = if is_128bit {
                if is_load { 0b11 } else { 0b10 }
            } else if is_load {
                if is_signed { 0b10 } else { 0b01 }
            } else {
                0b00
            };
            let word = (((actual_size << 30) | (0b111 << 27) | (v << 26)) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12)) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        // [base, #offset]! (pre-index)
        Some(Operand::MemPreIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let opc = if is_128bit {
                if is_load { 0b11 } else { 0b10 }
            } else if is_load { 0b01 } else { 0b00 };
            let word = ((actual_size << 30) | (0b111 << 27) | (v << 26)) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b11 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        // [base], #offset (post-index)
        Some(Operand::MemPostIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let opc = if is_128bit {
                if is_load { 0b11 } else { 0b10 }
            } else if is_load { 0b01 } else { 0b00 };
            let word = ((actual_size << 30) | (0b111 << 27) | (v << 26)) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b01 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        // [base, Xm] register offset
        Some(Operand::MemRegOffset { base, index, extend, shift }) => {
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

                let opc = if is_128bit {
                    if is_load { 0b11 } else { 0b10 }
                } else if is_load { 0b01 } else { 0b00 };

                let reloc_type = match kind {
                    "lo12" => {
                        if is_128bit {
                            RelocType::Ldst128AbsLo12
                        } else {
                            match actual_size {
                                0b00 => RelocType::Ldst8AbsLo12,
                                0b01 => RelocType::Ldst16AbsLo12,
                                0b10 => RelocType::Ldst32AbsLo12,
                                0b11 => RelocType::Ldst64AbsLo12,
                                _ => RelocType::Ldst64AbsLo12,
                            }
                        }
                    }
                    "got_lo12" => RelocType::Ld64GotLo12,
                    _ => return Err(format!("unsupported modifier in load/store: {}", kind)),
                };

                let word = ((actual_size << 30) | (0b111 << 27) | (v << 26) | (opc << 22)) | (rn << 5) | rt;
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
            let opc = if is_128bit {
                if is_load { 0b11 } else { 0b10 }
            } else if is_load { 0b01 } else { 0b00 };
            // Register offset: size 111 V opc 1 Rm option S 10 Rn Rt
            // Determine option and S from extend/shift specifiers
            let is_w_index = index.starts_with('w') || index.starts_with('W');
            let shift_amount: u8 = match shift { Some(s) => *s, None => 0 };
            let (option, s_bit) = match extend.as_deref() {
                Some("lsl") => {
                    // LSL with shift: S=1 if shift amount > 0
                    let s_val = if shift_amount > 0 { 1u32 } else { 0u32 };
                    (0b011u32, s_val)
                }
                Some("sxtw") => {
                    let s_val = if shift_amount > 0 { 1u32 } else { 0u32 };
                    (0b110u32, s_val)
                }
                Some("sxtx") => {
                    let s_val = if shift_amount > 0 { 1u32 } else { 0u32 };
                    (0b111u32, s_val)
                }
                Some("uxtw") => {
                    let s_val = if shift_amount > 0 { 1u32 } else { 0u32 };
                    (0b010u32, s_val)
                }
                Some("uxtx") => {
                    let s_val = if shift_amount > 0 { 1u32 } else { 0u32 };
                    (0b011u32, s_val)
                }
                None => {
                    // Default: if W register index, use UXTW; if X register, use LSL
                    if is_w_index {
                        (0b010u32, 0u32) // UXTW, no shift
                    } else {
                        (0b011u32, 0u32) // LSL, no shift
                    }
                }
                _ => (0b011u32, 0u32), // default LSL
            };
            let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (opc << 22)
                | (1 << 21) | (rm << 16) | (option << 13) | (s_bit << 12) | (0b10 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        // LDR (literal): ldr Rt, label — PC-relative load
        Some(Operand::Symbol(sym)) if is_load => {
            // opc V 011 00 imm19 Rt
            // opc encodes size: 00=32/s, 01=64/d, 10=128/q for FP; 00=w, 01=x for GP
            let opc = if is_128bit { 0b10u32 } else { actual_size & 0b11 };
            let word = (opc << 30) | (v << 26) | (0b011 << 27) | rt;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::Ldr19,
                    symbol: sym.clone(),
                    addend: 0,
                },
            });
        }

        _ => {}
    }

    Err(format!("unsupported ldr/str operands: {:?}", operands))
}

/// Encode LDUR/STUR (unscaled immediate offset load/store)
/// Format: size 111 V 00 opc 0 imm9 00 Rn Rt
fn encode_ldur_stur(operands: &[Operand], is_load: bool, op2_bits: u32) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("ldur/stur requires 2 operands".to_string());
    }
    let (rt, _) = get_reg(operands, 0)?;
    let reg_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let fp = is_fp_reg(&reg_name);
    let v = if fp { 1u32 } else { 0u32 };

    let (size, opc) = if fp {
        if reg_name.starts_with('q') {
            (0b00u32, if is_load { 0b11u32 } else { 0b10 })
        } else if reg_name.starts_with('d') {
            (0b11, if is_load { 0b01 } else { 0b00 })
        } else if reg_name.starts_with('s') {
            (0b10, if is_load { 0b01 } else { 0b00 })
        } else if reg_name.starts_with('h') {
            (0b01, if is_load { 0b01 } else { 0b00 })
        } else if reg_name.starts_with('b') {
            (0b00, if is_load { 0b01 } else { 0b00 })
        } else {
            (0b11, if is_load { 0b01 } else { 0b00 })
        }
    } else {
        let is_64 = reg_name.starts_with('x');
        let sz = if is_64 { 0b11u32 } else { 0b10 };
        (sz, if is_load { 0b01u32 } else { 0b00 })
    };

    let (rn, imm9) = match &operands[1] {
        Operand::Mem { base, offset } => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            (rn, *offset as i32)
        }
        _ => return Err(format!("ldur/stur: expected memory operand, got {:?}", operands[1])),
    };

    let imm9_enc = (imm9 as u32) & 0x1FF;
    let word = (size << 30) | (0b111 << 27) | (v << 26) | (opc << 22)
        | (imm9_enc << 12) | (op2_bits << 10) | (rn << 5) | rt;
    Ok(EncodeResult::Word(word))
}

/// Encode LDTR/STTR with explicit size (for ldtrh, ldtrb, etc.)
fn encode_ldtr_sized(operands: &[Operand], is_load: bool, size: u32) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("ldtr/sttr requires 2 operands".to_string());
    }
    let (rt, _) = get_reg(operands, 0)?;
    let opc = if is_load { 0b01u32 } else { 0b00 };
    let (rn, imm9) = match &operands[1] {
        Operand::Mem { base, offset } => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            (rn, *offset as i32)
        }
        _ => return Err("ldtr/sttr: expected memory operand".to_string()),
    };
    let imm9_enc = (imm9 as u32) & 0x1FF;
    let word = (size << 30) | (0b111 << 27) | (opc << 22)
        | (imm9_enc << 12) | (0b10 << 10) | (rn << 5) | rt;
    Ok(EncodeResult::Word(word))
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
            if *offset >= 0 && abs_offset.is_multiple_of(4) {
                let imm12 = (abs_offset / 4) as u32;
                if imm12 < 4096 {
                    let word = ((0b10 << 30) | (0b111 << 27)) | (0b01 << 24) | (0b10 << 22)
                        | (imm12 << 10) | (rn << 5) | rt;
                    return Ok(EncodeResult::Word(word));
                }
            }
            // Unscaled: LDURSW
            let imm9 = (*offset as i32) & 0x1FF;
            let word = (((0b10 << 30) | (0b111 << 27)) | (0b10 << 22)
                | ((imm9 as u32 & 0x1FF) << 12)) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        Some(Operand::MemPostIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let word = ((0b10 << 30) | (0b111 << 27)) | (0b10 << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b01 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        Some(Operand::MemPreIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let word = ((0b10 << 30) | (0b111 << 27)) | (0b10 << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b11 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        Some(Operand::MemRegOffset { base, index, extend, shift }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let rm = parse_reg_num(index).ok_or("invalid index reg")?;
            let (option, s_bit) = match (extend.as_deref(), shift) {
                (Some("lsl"), Some(2)) => (0b011u32, 1u32),
                (Some("lsl"), Some(0)) | (Some("lsl"), None) => (0b011, 0),
                (None, None) | (None, Some(0)) => (0b011, 0),
                (Some("sxtw"), Some(2)) => (0b110, 1),
                (Some("sxtw"), Some(0)) | (Some("sxtw"), None) => (0b110, 0),
                (Some("uxtw"), Some(2)) => (0b010, 1),
                (Some("uxtw"), Some(0)) | (Some("uxtw"), None) => (0b010, 0),
                (Some("sxtx"), Some(2)) => (0b111, 1),
                (Some("sxtx"), Some(0)) | (Some("sxtx"), None) => (0b111, 0),
                _ => return Err(format!("unsupported ldrsw extend/shift: {:?}/{:?}", extend, shift)),
            };
            // LDRSW reg: 10 111 0 00 10 1 Rm option S 10 Rn Rt
            let word = (0b10 << 30) | (0b111 << 27) | (0b00 << 24) | (0b10 << 22) | (1 << 21)
                | (rm << 16) | (option << 13) | (s_bit << 12) | (0b10 << 10) | (rn << 5) | rt;
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

    if let Some(Operand::Mem { base, offset }) = operands.get(1) {
        let rn = parse_reg_num(base).ok_or("invalid base reg")?;
        let shift = size;
        let abs_offset = *offset as u64;
        let align = 1u64 << shift;
        if *offset >= 0 && abs_offset.is_multiple_of(align) {
            let imm12 = (abs_offset / align) as u32;
            if imm12 < 4096 {
                let word = ((size << 30) | (0b111 << 27)) | (0b01 << 24) | (opc << 22)
                    | (imm12 << 10) | (rn << 5) | rt;
                return Ok(EncodeResult::Word(word));
            }
        }
        // Unscaled
        let imm9 = (*offset as i32) & 0x1FF;
        let word = (((size << 30) | (0b111 << 27)) | (opc << 22)
            | ((imm9 as u32 & 0x1FF) << 12)) | (rn << 5) | rt;
        return Ok(EncodeResult::Word(word));
    }

    // Post-index: ldrsb/ldrsh Rt, [Xn], #imm
    if let Some(Operand::MemPostIndex { base, offset }) = operands.get(1) {
        let rn = parse_reg_num(base).ok_or("invalid base reg")?;
        let imm9 = (*offset as i32) & 0x1FF;
        let word = (size << 30) | (0b111 << 27) | (opc << 22)
            | ((imm9 as u32 & 0x1FF) << 12) | (0b01 << 10) | (rn << 5) | rt;
        return Ok(EncodeResult::Word(word));
    }

    // Pre-index: ldrsb/ldrsh Rt, [Xn, #imm]!
    if let Some(Operand::MemPreIndex { base, offset }) = operands.get(1) {
        let rn = parse_reg_num(base).ok_or("invalid base reg")?;
        let imm9 = (*offset as i32) & 0x1FF;
        let word = (size << 30) | (0b111 << 27) | (opc << 22)
            | ((imm9 as u32 & 0x1FF) << 12) | (0b11 << 10) | (rn << 5) | rt;
        return Ok(EncodeResult::Word(word));
    }

    // Register offset: ldrsb/ldrsh Rt, [Xn, Xm{, extend {#amount}}]
    if let Some(Operand::MemRegOffset { base, index, extend, shift }) = operands.get(1) {
        let rn = parse_reg_num(base).ok_or("invalid base reg")?;
        let rm = parse_reg_num(index).ok_or("invalid index reg")?;
        let is_w_index = index.starts_with('w') || index.starts_with('W');
        let shift_amount: u8 = match shift { Some(s) => *s, None => 0 };
        let (option, s_bit) = match extend.as_deref() {
            Some("lsl") => (0b011u32, if shift_amount > 0 { 1u32 } else { 0 }),
            Some("sxtw") => (0b110u32, if shift_amount > 0 { 1u32 } else { 0 }),
            Some("sxtx") => (0b111u32, if shift_amount > 0 { 1u32 } else { 0 }),
            Some("uxtw") => (0b010u32, if shift_amount > 0 { 1u32 } else { 0 }),
            Some("uxtx") => (0b011u32, if shift_amount > 0 { 1u32 } else { 0 }),
            None => if is_w_index { (0b010u32, 0u32) } else { (0b011u32, 0u32) },
            _ => (0b011u32, 0u32),
        };
        let word = (size << 30) | (0b111 << 27) | (opc << 22) | (1 << 21)
            | (rm << 16) | (option << 13) | (s_bit << 12) | (0b10 << 10) | (rn << 5) | rt;
        return Ok(EncodeResult::Word(word));
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
        else if r.starts_with('q') || is_64 { 0b10 }
        else { 0b00 }
    } else if is_64 { 0b10 } else { 0b00 };

    let v = if fp { 1u32 } else { 0u32 };
    let l = if is_load { 1u32 } else { 0u32 };

    // Shift depends on register size
    let shift = if fp {
        let r = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
        if r.starts_with('s') { 2 }
        else if r.starts_with('d') { 3 }
        else if r.starts_with('q') { 4 }
        else if is_64 { 3 } else { 2 }
    } else if is_64 { 3 } else { 2 };

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

/// Encode LDNP/STNP (load/store pair non-temporal)
/// Encoding: opc 101 V 000 L imm7 Rt2 Rn Rt
/// TODO: Only handles integer registers (V=0). FP/SIMD register support needed for V=1.
fn encode_ldnp_stnp(operands: &[Operand], is_load: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("ldnp/stnp requires 3 operands".to_string());
    }

    let (rt1, is_64) = get_reg(operands, 0)?;
    let (rt2, _) = get_reg(operands, 1)?;

    let opc: u32 = if is_64 { 0b10 } else { 0b00 };
    let l: u32 = if is_load { 1 } else { 0 };
    let shift = if is_64 { 3 } else { 2 }; // scale factor: 8 for 64-bit, 4 for 32-bit

    match operands.get(2) {
        Some(Operand::Mem { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm7 = ((*offset >> shift) as i32) & 0x7F;
            // LDNP/STNP: opc 101 V=0 000 L imm7 Rt2 Rn Rt
            let word = (opc << 30) | (0b101 << 27) | (l << 22)
                | ((imm7 as u32 & 0x7F) << 15) | (rt2 << 10) | (rn << 5) | rt1;
            Ok(EncodeResult::Word(word))
        }
        _ => Err(format!("unsupported ldnp/stnp operands: {:?}", operands)),
    }
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
        let word = ((size << 30) | (0b001000010 << 21) | (0b11111 << 16))
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
        let word = ((size << 30) | (0b001000000 << 21) | (ws << 16))
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
    let word = ((size << 30) | (0b001000 << 24) | (1 << 23) | (l << 22))
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
        // Parser misclassifies symbol names that collide with register names (s1, v0, d1, etc.),
        // condition codes (cc, lt, le), or barrier names (st, ld).
        // ADRP never takes these as actual operand types, so treat them as symbols.
        Some(Operand::Reg(name)) => (name.clone(), 0i64),
        Some(Operand::Cond(name)) => (name.clone(), 0i64),
        Some(Operand::Barrier(name)) => (name.clone(), 0i64),
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

    // Check for immediate offset form: adr Rd, #imm
    // TODO: validate 21-bit signed immediate range
    if let Some(Operand::Imm(imm)) = operands.get(1) {
        let imm = *imm;
        // ADR: 0 immlo[1:0] 10000 immhi[18:0] Rd
        let immlo = ((imm as u32) & 3) << 29;
        let immhi = (((imm as u32) >> 2) & 0x7FFFF) << 5;
        let word = immlo | (0b10000 << 24) | immhi | rd;
        return Ok(EncodeResult::Word(word));
    }

    let (sym, addend) = get_symbol(operands, 1)?;
    // ADR: 0 immlo[1:0] 10000 immhi[18:0] Rd
    let word = (0b10000 << 24) | rd;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::AdrPrelLo21,
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
            let word = ((0b1001111001 << 22) | (0b100111 << 16)) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        } else {
            // FMOV Sd, Wn: 0 00 11110 00 1 00 111 000000 Rn Rd
            let word = ((0b0001111000 << 22) | (0b100111 << 16)) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }
    }

    if !rd_is_fp && rm_is_fp {
        // FMOV from FP to GP: FMOV Xn, Dn or FMOV Wn, Sn
        let is_double = rm_lower.starts_with('d');
        if is_double {
            // FMOV Xd, Dn: 1 00 11110 01 1 00 110 000000 Rn Rd
            let word = ((0b1001111001 << 22) | (0b100110 << 16)) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        } else {
            // FMOV Wd, Sn: 0 00 11110 00 1 00 110 000000 Rn Rd
            let word = ((0b0001111000 << 22) | (0b100110 << 16)) | (rm << 5) | rd;
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

/// Encode FP 1-source ops: FRINTN/P/M/Z/A/X/I
/// Format: 0 00 11110 ftype 1 opcode 10000 Rn Rd
fn encode_fp_1src(operands: &[Operand], opcode: u32) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01u32 } else { 0b00 };
    let word = (0b00011110u32 << 24) | (ftype << 22) | (1 << 21)
        | (opcode << 15) | (0b10000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode FMADD/FMSUB: Rd = Ra +/- (Rn * Rm)
/// Format: 0 00 11111 ftype 0 Rm o1 Ra Rn Rd
fn encode_fmadd_fmsub(operands: &[Operand], is_sub: bool) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let (ra, _) = get_reg(operands, 3)?;
    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01u32 } else { 0b00 };
    let o1 = if is_sub { 1u32 } else { 0 };
    let word = (0b00011111u32 << 24) | (ftype << 22) | (rm << 16)
        | (o1 << 15) | (ra << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode FNMADD/FNMSUB: Rd = -Ra +/- (Rn * Rm)
/// Format: 0 00 11111 ftype 1 Rm o1 Ra Rn Rd
fn encode_fnmadd_fnmsub(operands: &[Operand], is_sub: bool) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let (ra, _) = get_reg(operands, 3)?;
    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01u32 } else { 0b00 };
    let o1 = if is_sub { 1u32 } else { 0 };
    let word = (0b00011111u32 << 24) | (ftype << 22) | (1 << 21) | (rm << 16)
        | (o1 << 15) | (ra << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_fcmp(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rn, _) = get_reg(operands, 0)?;
    let rn_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rn_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };

    // FCMP Dn, #0.0
    if operands.len() < 2 || matches!(operands.get(1), Some(Operand::Imm(0))) {
        let word = ((0b00011110 << 24) | (ftype << 22) | (1 << 21)) | (0b001000 << 10) | (rn << 5) | 0b01000;
        return Ok(EncodeResult::Word(word));
    }

    let (rm, _) = get_reg(operands, 1)?;
    // FCMP Dn, Dm: 0 00 11110 ftype 1 Rm 00 1000 Rn 00 000
    let word = (0b00011110 << 24) | (ftype << 22) | (1 << 21) | (rm << 16) | (0b001000 << 10) | (rn << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_fcvt_rounding(operands: &[Operand], rmode: u32, opcode: u32) -> Result<EncodeResult, String> {
    // Float-to-integer conversion with specified rounding mode
    // Encoding: sf 00 11110 ftype 1 rmode opcode 000000 Rn Rd
    // sf: 0=W dest, 1=X dest
    // ftype: 00=S source, 01=D source
    // rmode+opcode: determines rounding mode and signedness
    if operands.len() < 2 {
        return Err("fcvt* requires 2 operands".to_string());
    }
    let (rd, rd_is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;

    let src_name = match &operands[1] {
        Operand::Reg(name) => name.to_lowercase(),
        _ => return Err("fcvt*: expected register source".to_string()),
    };
    let ftype: u32 = if src_name.starts_with('d') { 0b01 } else { 0b00 };
    let sf: u32 = if rd_is_64 { 1 } else { 0 };

    let word = ((sf << 31) | (0b11110 << 24) | (ftype << 22)
        | (1 << 21) | (rmode << 19) | (opcode << 16)) | (rn << 5) | rd;
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

    let word = (((sf << 31) | (0b11110 << 24) | (ftype << 22)
        | (1 << 21)) | (opcode << 16)) | (rn << 5) | rd;
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
    let word = ((q << 30) | (0b001110 << 24)) | (0b100000 << 16)
        | (0b010110 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── System instructions ──────────────────────────────────────────────────

fn encode_dmb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let option = match operands.first() {
        Some(Operand::Barrier(b)) => match b.to_lowercase().as_str() {
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
    let option = match operands.first() {
        Some(Operand::Barrier(b)) => match b.to_lowercase().as_str() {
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
        "sp_el0" => 0xc208u32,
        "tpidr_el0" => 0xde82,
        "tpidr_el1" => 0xc684,
        "tpidr_el2" => 0xe682,
        "tpidrro_el0" => 0xde83,
        "tcr_el1" => 0xc102,
        "ttbr0_el1" => 0xc100,
        "sctlr_el1" => 0xc080,
        "mdscr_el1" => 0x8012,
        "id_aa64mmfr0_el1" => 0xc038,
        "id_aa64mmfr1_el1" => 0xc039,
        "cpacr_el1" => 0xc082,
        "par_el1" => 0xc3a0,
        "osdlr_el1" => 0x809c,
        "currentel" => 0xc212,
        "elr_el1" => 0xc201,
        "spsr_el1" => 0xc200,
        "esr_el1" => 0xc290,
        "far_el1" => 0xc300,
        "vbar_el1" => 0xc600,
        "mpidr_el1" => 0xc005,
        "contextidr_el1" => 0xc681,
        "mair_el1" => 0xc510,
        "isr_el1" => 0xc608,
        "oslsr_el1" => 0x808c,
        "midr_el1" => 0xc000,
        "revidr_el1" => 0xc006,
        "id_aa64pfr0_el1" => 0xc020,
        "id_aa64pfr1_el1" => 0xc021,
        "id_aa64isar0_el1" => 0xc030,
        "id_aa64isar1_el1" => 0xc031,
        "id_aa64isar2_el1" => 0xc032,
        "amair_el1" => 0xc518,
        "hcr_el2" => 0xe088,
        "cptr_el2" => 0xe08a,
        "hstr_el2" => 0xe08b,
        "hacr_el2" => 0xe08f,
        "vpidr_el2" => 0xe000,
        "vmpidr_el2" => 0xe005,
        "actlr_el2" => 0xe081,
        "elr_el2" => 0xe201,
        "esr_el2" => 0xe290,
        "afsr0_el2" => 0xe288,
        "afsr1_el2" => 0xe289,
        "far_el2" => 0xe300,
        "hpfar_el2" => 0xe304,
        "spsr_el2" => 0xe200,
        "sctlr_el2" => 0xe080,
        "mdcr_el2" => 0xe089,
        "tcr_el2" => 0xe102,
        "ttbr0_el2" => 0xe100,
        "vttbr_el2" => 0xe108,
        "vtcr_el2" => 0xe10a,
        "vbar_el2" => 0xe600,
        "mair_el2" => 0xe510,
        "amair_el2" => 0xe518,
        "sp_el1" => 0xe208,
        "pmuserenr_el0" => 0xdcf0,
        "cntfrq_el0" => 0xdf00,
        "cntpct_el0" => 0xdf01,
        "cntv_ctl_el0" => 0xdf19,
        "cntp_ctl_el0" => 0xdf11,
        "cntv_cval_el0" => 0xdf1c,
        "cntp_cval_el0" => 0xdf12,
        "ctr_el0" => 0xd801,
        "ttbr1_el1" => 0xc101,
        "cntkctl_el1" => 0xc708,
        "id_aa64dfr0_el1" => 0xc028,
        "oslar_el1" => 0x8084,
        "cntvct_el0" => 0xdf02,
        "clidr_el1" => 0xc801,
        "ccsidr_el1" => 0xc800,
        "csselr_el1" => 0xd000,
        "id_aa64mmfr2_el1" => 0xc03a,
        "id_aa64dfr1_el1" => 0xc029,
        "actlr_el1" => 0xc081,
        "afsr0_el1" => 0xc288,
        "afsr1_el1" => 0xc289,
        "id_pfr0_el1" => 0xc008,
        "id_pfr1_el1" => 0xc009,
        "cnthctl_el2" => 0xe708,
        "cntvoff_el2" => 0xe703,
        "sp_el2" => 0xf208,
        "pmcr_el0" => 0xdce0,
        "pmcntenset_el0" => 0xdce1,
        "pmovsclr_el0" => 0xdce3,
        "pmselr_el0" => 0xdce5,
        "pmccntr_el0" => 0xdce8,
        "pmxevtyper_el0" => 0xdce9,
        "pmxevcntr_el0" => 0xdcea,
        "dczid_el0" => 0xd807,
        "daif" => 0xda11,
        "fpcr" => 0xda20,
        "fpsr" => 0xda21,
        "nzcv" => 0xda10,
        _ => parse_generic_sysreg(&sysreg)?,
    };

    let word = 0xd5300000 | (encoding << 5) | rt;
    Ok(EncodeResult::Word(word))
}

/// Parse generic system register name like `s3_0_c1_c0_1` into encoding bits.
fn parse_generic_sysreg(name: &str) -> Result<u32, String> {
    // Format: s<op0>_<op1>_c<CRn>_c<CRm>_<op2>
    let parts: Vec<&str> = name.split('_').collect();
    if parts.len() == 5 && parts[0].starts_with('s') && parts[2].starts_with('c') && parts[3].starts_with('c') {
        let op0: u32 = parts[0][1..].parse().map_err(|_| format!("unsupported system register: {}", name))?;
        let op1: u32 = parts[1].parse().map_err(|_| format!("unsupported system register: {}", name))?;
        let crn: u32 = parts[2][1..].parse().map_err(|_| format!("unsupported system register: {}", name))?;
        let crm: u32 = parts[3][1..].parse().map_err(|_| format!("unsupported system register: {}", name))?;
        let op2: u32 = parts[4].parse().map_err(|_| format!("unsupported system register: {}", name))?;
        // Encoding: op0[1:0] op1[2:0] CRn[3:0] CRm[3:0] op2[2:0]
        let enc = ((op0 & 3) << 14) | ((op1 & 7) << 11) | ((crn & 0xF) << 7) | ((crm & 0xF) << 3) | (op2 & 7);
        Ok(enc)
    } else {
        Err(format!("unsupported system register: {}", name))
    }
}

fn encode_msr(operands: &[Operand]) -> Result<EncodeResult, String> {
    let sysreg = match operands.first() {
        Some(Operand::Symbol(s)) => s.to_lowercase(),
        _ => return Err("msr needs system register name".to_string()),
    };

    // MSR (immediate): msr <pstatefield>, #imm
    // Encoding: 1101_0101_0000_0 op1[18:16] 0100 CRm[11:8] 101 op2[7:5] 11111
    // daifset: op1=011, op2=110; daifclr: op1=011, op2=111
    match sysreg.as_str() {
        "daifset" => {
            let imm = get_imm(operands, 1)? as u32 & 0xF;
            let word = 0xd5034000 | (imm << 8) | (0b110 << 5) | 0x1F;
            return Ok(EncodeResult::Word(word));
        }
        "daifclr" => {
            let imm = get_imm(operands, 1)? as u32 & 0xF;
            let word = 0xd5034000 | (imm << 8) | (0b111 << 5) | 0x1F;
            return Ok(EncodeResult::Word(word));
        }
        _ => {}
    }

    // MSR (register): msr sysreg, Xt
    let (rt, _) = get_reg(operands, 1)?;

    let encoding = match sysreg.as_str() {
        "sp_el0" => 0xc208u32,
        "tpidr_el0" => 0xde82,
        "tpidr_el1" => 0xc684,
        "tpidr_el2" => 0xe682,
        "tpidrro_el0" => 0xde83,
        "tcr_el1" => 0xc102,
        "ttbr0_el1" => 0xc100,
        "sctlr_el1" => 0xc080,
        "mdscr_el1" => 0x8012,
        "cpacr_el1" => 0xc082,
        "osdlr_el1" => 0x809c,
        "oslar_el1" => 0x8084,
        "oslsr_el1" => 0x808c,
        "elr_el1" => 0xc201,
        "spsr_el1" => 0xc200,
        "esr_el1" => 0xc290,
        "far_el1" => 0xc300,
        "vbar_el1" => 0xc600,
        "contextidr_el1" => 0xc681,
        "mair_el1" => 0xc510,
        "amair_el1" => 0xc518,
        "hcr_el2" => 0xe088,
        "cptr_el2" => 0xe08a,
        "hstr_el2" => 0xe08b,
        "elr_el2" => 0xe201,
        "esr_el2" => 0xe290,
        "far_el2" => 0xe300,
        "spsr_el2" => 0xe200,
        "sctlr_el2" => 0xe080,
        "mdcr_el2" => 0xe089,
        "tcr_el2" => 0xe102,
        "ttbr0_el2" => 0xe100,
        "vttbr_el2" => 0xe108,
        "vtcr_el2" => 0xe10a,
        "vbar_el2" => 0xe600,
        "mair_el2" => 0xe510,
        "sp_el1" => 0xe208,
        "csselr_el1" => 0xd000,
        "actlr_el1" => 0xc081,
        "cnthctl_el2" => 0xe708,
        "cntvoff_el2" => 0xe703,
        "sp_el2" => 0xf208,
        "vpidr_el2" => 0xe000,
        "vmpidr_el2" => 0xe005,
        "hacr_el2" => 0xe08f,
        "actlr_el2" => 0xe081,
        "afsr0_el2" => 0xe288,
        "afsr1_el2" => 0xe289,
        "amair_el2" => 0xe518,
        "hpfar_el2" => 0xe304,
        "pmcr_el0" => 0xdce0,
        "pmcntenset_el0" => 0xdce1,
        "pmovsclr_el0" => 0xdce3,
        "pmselr_el0" => 0xdce5,
        "pmccntr_el0" => 0xdce8,
        "pmxevtyper_el0" => 0xdce9,
        "pmxevcntr_el0" => 0xdcea,
        "pmuserenr_el0" => 0xdcf0,
        "cntv_ctl_el0" => 0xdf19,
        "cntp_ctl_el0" => 0xdf11,
        "cntp_cval_el0" => 0xdf12,
        "cntv_cval_el0" => 0xdf1c,
        "ttbr1_el1" => 0xc101,
        "cntkctl_el1" => 0xc708,
        "daif" => 0xda11,
        "fpcr" => 0xda20,
        "fpsr" => 0xda21,
        "nzcv" => 0xda10,
        _ => parse_generic_sysreg(&sysreg)?,
    };

    let word = 0xd5100000 | (encoding << 5) | rt;
    Ok(EncodeResult::Word(word))
}

fn encode_svc(operands: &[Operand]) -> Result<EncodeResult, String> {
    let imm = get_imm(operands, 0)?;
    let word = 0xd4000001 | ((imm as u32 & 0xFFFF) << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_hvc(operands: &[Operand]) -> Result<EncodeResult, String> {
    let imm = get_imm(operands, 0)?;
    let word = 0xd4000002 | ((imm as u32 & 0xFFFF) << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_ic(raw_operands: &str) -> Result<EncodeResult, String> {
    let parts: Vec<&str> = raw_operands.splitn(2, ',').collect();
    let op_name = parts[0].trim().to_lowercase();
    let rt = if parts.len() > 1 {
        let reg_str = parts[1].trim();
        parse_reg_num(reg_str).ok_or_else(|| format!("ic: invalid register '{}'", reg_str))?
    } else {
        31 // xzr
    };
    let base = match op_name.as_str() {
        "ialluis" => 0xd508711fu32,
        "iallu"   => 0xd508751f,
        "ivau"    => 0xd50b7520,
        _ => return Err(format!("unsupported ic operation: {}", op_name)),
    };
    let word = (base & !0x1F) | rt;
    Ok(EncodeResult::Word(word))
}

fn encode_smc(operands: &[Operand]) -> Result<EncodeResult, String> {
    let imm = get_imm(operands, 0)?;
    let word = 0xd4000003 | ((imm as u32 & 0xFFFF) << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_at(operands: &[Operand], raw_operands: &str) -> Result<EncodeResult, String> {
    let parts: Vec<&str> = raw_operands.splitn(2, ',').collect();
    let op_name = parts[0].trim().to_lowercase();
    let rt = if parts.len() > 1 {
        let reg_str = parts[1].trim();
        parse_reg_num(reg_str).ok_or_else(|| format!("at: invalid register '{}'", reg_str))?
    } else {
        31
    };
    // AT encoding: SYS instruction. Base words from GCC:
    let base = match op_name.as_str() {
        "s1e1r" => 0xd5087800u32,
        "s1e1w" => 0xd5087820,
        "s1e0r" => 0xd5087840,
        "s1e0w" => 0xd5087860,
        _ => return Err(format!("unsupported at operation: {}", op_name)),
    };
    let word = (base & !0x1F) | rt;
    Ok(EncodeResult::Word(word))
}

/// Encode `sys #op1, Cn, Cm, #op2, Xt` instruction.
fn encode_sys(raw_operands: &str) -> Result<EncodeResult, String> {
    let parts: Vec<&str> = raw_operands.split(',').map(|s| s.trim()).collect();
    if parts.len() < 4 {
        return Err(format!("sys needs at least 4 operands, got: {}", raw_operands));
    }
    let op1: u32 = parts[0].trim_start_matches('#').trim().parse()
        .map_err(|_| format!("sys: invalid op1: {}", parts[0]))?;
    let crn: u32 = parts[1].trim().to_lowercase().trim_start_matches('c').parse()
        .map_err(|_| format!("sys: invalid CRn: {}", parts[1]))?;
    let crm: u32 = parts[2].trim().to_lowercase().trim_start_matches('c').parse()
        .map_err(|_| format!("sys: invalid CRm: {}", parts[2]))?;
    let op2: u32 = parts[3].trim_start_matches('#').trim().parse()
        .map_err(|_| format!("sys: invalid op2: {}", parts[3]))?;
    let rt = if parts.len() >= 5 {
        let reg = parts[4].trim().to_lowercase();
        parse_reg_num(&reg).ok_or_else(|| format!("sys: invalid register: {}", parts[4]))?
    } else {
        31 // xzr if no register specified
    };
    let word = 0xd5080000 | ((op1 & 7) << 16) | ((crn & 0xF) << 12) | ((crm & 0xF) << 8) | ((op2 & 7) << 5) | rt;
    Ok(EncodeResult::Word(word))
}

fn encode_brk(operands: &[Operand]) -> Result<EncodeResult, String> {
    let imm = get_imm(operands, 0)?;
    let word = 0xd4200000 | ((imm as u32 & 0xFFFF) << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_tlbi(operands: &[Operand], raw_operands: &str) -> Result<EncodeResult, String> {
    let parts: Vec<&str> = raw_operands.splitn(2, ',').collect();
    let op_name = parts[0].trim().to_lowercase();
    let rt = if parts.len() > 1 {
        let reg_str = parts[1].trim();
        parse_reg_num(reg_str).ok_or_else(|| format!("tlbi: invalid register '{}'", reg_str))?
    } else {
        31 // xzr
    };
    // TLBI encoding: SYS instruction with fixed fields
    // Full word from GCC objdump for known ops:
    let base = match op_name.as_str() {
        "vmalle1is" => 0xd508831fu32,
        "vmalle1"   => 0xd508871f,
        "alle1is"   => 0xd50c839f,
        "alle1"     => 0xd50c879f,
        "alle2is"   => 0xd50c831f,
        "vale1is"   => 0xd50883a0,
        "vale1"     => 0xd50887a0,
        "vale2is"   => 0xd50c83a0,
        "vaae1is"   => 0xd5088360,
        "vaae1"     => 0xd5088760,
        "vaale1is"  => 0xd50883e0,
        "vaale1"    => 0xd50887e0,
        "vae1is"    => 0xd5088320,
        "vae1"      => 0xd5088720,
        "aside1is"  => 0xd5088340,
        "aside1"    => 0xd5088740,
        "vmalls12e1is" => 0xd50c83df,
        "vmalls12e1"   => 0xd50c87df,
        _ => return Err(format!("unsupported tlbi operation: {}", op_name)),
    };
    // Replace Rt field (bits 4:0)
    let word = (base & !0x1F) | rt;
    Ok(EncodeResult::Word(word))
}

/// Encode HINT #imm (system hint instruction)
fn encode_bti(raw_operands: &str) -> Result<EncodeResult, String> {
    let target = raw_operands.trim().to_lowercase();
    let word = match target.as_str() {
        "" => 0xd503241f,    // bti (no target)
        "c" => 0xd503245f,   // bti c
        "j" => 0xd503249f,   // bti j
        "jc" => 0xd50324df,  // bti jc
        _ => return Err(format!("unsupported bti target: {}", target)),
    };
    Ok(EncodeResult::Word(word))
}

fn encode_hint(operands: &[Operand]) -> Result<EncodeResult, String> {
    let imm = get_imm(operands, 0)?;
    // HINT: 11010101 00000011 0010 CRm op2 11111
    // CRm = imm >> 3, op2 = imm & 7
    let crm = ((imm as u32) >> 3) & 0xF;
    let op2 = (imm as u32) & 0x7;
    let word = 0xd503201f | (crm << 8) | (op2 << 5);
    Ok(EncodeResult::Word(word))
}

// ── Bitfield extract/insert ──────────────────────────────────────────────

/// Encode UBFX Rd, Rn, #lsb, #width -> UBFM Rd, Rn, #lsb, #(lsb+width-1)
fn encode_ubfx(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let lsb = get_imm(operands, 2)? as u32;
    let width = get_imm(operands, 3)? as u32;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0u32 };
    let immr = lsb;
    let imms = lsb + width - 1;
    // UBFM: sf 10 100110 N immr imms Rn Rd
    let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode SBFX Rd, Rn, #lsb, #width -> SBFM Rd, Rn, #lsb, #(lsb+width-1)
fn encode_sbfx(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let lsb = get_imm(operands, 2)? as u32;
    let width = get_imm(operands, 3)? as u32;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0u32 };
    let immr = lsb;
    let imms = lsb + width - 1;
    // SBFM: sf 00 100110 N immr imms Rn Rd
    let word = (sf << 31) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode UBFM Rd, Rn, #immr, #imms (raw form)
fn encode_ubfm(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let immr = get_imm(operands, 2)? as u32;
    let imms = get_imm(operands, 3)? as u32;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0u32 };
    let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode SBFM Rd, Rn, #immr, #imms (raw form)
fn encode_sbfm(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let immr = get_imm(operands, 2)? as u32;
    let imms = get_imm(operands, 3)? as u32;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0u32 };
    let word = (sf << 31) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode SBFIZ Rd, Rn, #lsb, #width — alias for SBFM Rd, Rn, #(-lsb MOD regsize), #(width-1)
fn encode_sbfiz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let lsb = get_imm(operands, 2)? as u32;
    let width = get_imm(operands, 3)? as u32;
    let regsize = if is_64 { 64u32 } else { 32 };
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0u32 };
    let immr = (regsize.wrapping_sub(lsb)) & (regsize - 1);
    let imms = width - 1;
    let word = (sf << 31) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode UBFIZ Rd, Rn, #lsb, #width — alias for UBFM Rd, Rn, #(-lsb MOD regsize), #(width-1)
fn encode_ubfiz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let lsb = get_imm(operands, 2)? as u32;
    let width = get_imm(operands, 3)? as u32;
    let regsize = if is_64 { 64u32 } else { 32 };
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0u32 };
    let immr = (regsize.wrapping_sub(lsb)) & (regsize - 1);
    let imms = width - 1;
    let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode BFM Rd, Rn, #immr, #imms (bitfield move)
fn encode_bfm(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let immr = get_imm(operands, 2)? as u32;
    let imms = get_imm(operands, 3)? as u32;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0u32 };
    // BFM: sf 01 100110 N immr imms Rn Rd
    let word = (sf << 31) | (0b01 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode BFI Rd, Rn, #lsb, #width -> BFM Rd, Rn, #(-lsb mod width_reg), #(width-1)
fn encode_bfi(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let lsb = get_imm(operands, 2)? as u32;
    let width = get_imm(operands, 3)? as u32;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0u32 };
    let reg_width = if is_64 { 64u32 } else { 32u32 };
    let immr = (reg_width - lsb) % reg_width;
    let imms = width - 1;
    let word = (sf << 31) | (0b01 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode BFXIL Rd, Rn, #lsb, #width -> BFM Rd, Rn, #lsb, #(lsb+width-1)
fn encode_bfxil(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let lsb = get_imm(operands, 2)? as u32;
    let width = get_imm(operands, 3)? as u32;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0u32 };
    let immr = lsb;
    let imms = lsb + width - 1;
    let word = (sf << 31) | (0b01 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode EXTR Rd, Rn, Rm, #lsb
fn encode_extr(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let lsb = get_imm(operands, 3)? as u32;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0u32 };
    // EXTR: sf 0 0 100111 N 0 Rm imms Rn Rd
    let word = (sf << 31) | (0b00100111 << 23) | (n << 22) | (rm << 16)
        | (lsb << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Additional conditional operations ────────────────────────────────────

/// Encode CNEG Rd, Rn, cond -> CSNEG Rd, Rn, Rn, invert(cond)
fn encode_cneg(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let cond = match operands.get(2) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or_else(|| format!("unknown condition: {}", c))?,
        _ => return Err("cneg: expected condition code as third operand".to_string()),
    };
    let sf = sf_bit(is_64);
    // Invert the condition (flip bit 0)
    let inv_cond = cond ^ 1;
    // CSNEG: sf 1 0 11010100 Rm cond 0 1 Rn Rd (with Rm = Rn)
    let word = (sf << 31) | (1 << 30) | (0b011010100 << 21) | (rn << 16)
        | (inv_cond << 12) | (0b01 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode CINC Rd, Rn, cond -> CSINC Rd, Rn, Rn, invert(cond)
fn encode_cinc(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let cond = match operands.get(2) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or_else(|| format!("unknown condition: {}", c))?,
        _ => return Err("cinc: expected condition code as third operand".to_string()),
    };
    let sf = sf_bit(is_64);
    let inv_cond = cond ^ 1;
    // CSINC: sf 0 0 11010100 Rm cond 0 1 Rn Rd (with Rm = Rn)
    let word = (sf << 31) | (0b011010100 << 21) | (rn << 16)
        | (inv_cond << 12) | (0b01 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode CINV Rd, Rn, cond -> CSINV Rd, Rn, Rn, invert(cond)
fn encode_cinv(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let cond = match operands.get(2) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or_else(|| format!("unknown condition: {}", c))?,
        _ => return Err("cinv: expected condition code as third operand".to_string()),
    };
    let sf = sf_bit(is_64);
    let inv_cond = cond ^ 1;
    // CSINV: sf 1 0 11010100 Rm cond 0 0 Rn Rd (with Rm = Rn)
    let word = (sf << 31) | (1 << 30) | (0b011010100 << 21) | (rn << 16)
        | (inv_cond << 12) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Bit manipulation ─────────────────────────────────────────────────────

fn encode_clz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    // CLZ: sf 1 0 11010110 00000 00010 0 Rn Rd
    let word = ((sf << 31) | (1 << 30) | (0b011010110 << 21))
        | (0b000100 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_cls(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = ((sf << 31) | (1 << 30) | (0b011010110 << 21))
        | (0b000101 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rbit(operands: &[Operand]) -> Result<EncodeResult, String> {
    // NEON vector form: RBIT Vd.T, Vn.T (reverse bits in each byte)
    if let Some(Operand::RegArrangement { .. }) = operands.first() {
        let (rd, arr_d) = get_neon_reg(operands, 0)?;
        let (rn, _) = get_neon_reg(operands, 1)?;
        let q: u32 = if arr_d == "16b" { 1 } else { 0 };
        // RBIT (vector): 0 Q 1 01110 01 10000 00101 10 Rn Rd
        let word = (q << 30) | (1 << 29) | (0b01110 << 24) | (0b01 << 22)
            | (0b10000 << 17) | (0b00101 << 12) | (0b10 << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }
    // Scalar form: RBIT Rd, Rn
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = ((sf << 31) | (1 << 30) | (0b011010110 << 21)) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rev(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let opc = if is_64 { 0b000011 } else { 0b000010 };
    let word = ((sf << 31) | (1 << 30) | (0b011010110 << 21))
        | (opc << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rev16(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = ((sf << 31) | (1 << 30) | (0b011010110 << 21))
        | (0b000001 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rev32(operands: &[Operand]) -> Result<EncodeResult, String> {
    // Check for NEON vector form: REV32 Vd.T, Vn.T
    if let Some(Operand::RegArrangement { .. }) = operands.first() {
        let (rd, arr_d) = get_neon_reg(operands, 0)?;
        let (rn, _) = get_neon_reg(operands, 1)?;
        let (q, size) = neon_arr_to_q_size(&arr_d)?;
        // REV32 Vd.T, Vn.T: 0 Q 1 01110 size 10 0000 0000 10 Rn Rd
        let word = (q << 30) | (1 << 29) | (0b01110 << 24) | (size << 22)
            | (0b100000 << 16) | (0b000010 << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    // REV32 is 64-bit only: 1 1 0 11010110 00000 000010 Rn Rd
    let word = ((1u32 << 31) | (1 << 30) | (0b011010110 << 21))
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
        Operand::MemRegOffset { base, index, extend, shift } => {
            // PRFM (register): 11 111 0 00 10 1 Rm option S 10 Rn Rt
            let rn = parse_reg_num(base).ok_or_else(|| format!("prfm: invalid base register: {}", base))?;
            let rm = parse_reg_num(index).ok_or_else(|| format!("prfm: invalid index register: {}", index))?;
            let is_w_index = index.starts_with('w') || index.starts_with('W');
            let shift_amount: u8 = match shift { Some(s) => *s, None => 0 };
            let (option, s_bit) = match extend.as_deref() {
                Some("lsl") => (0b011u32, if shift_amount > 0 { 1u32 } else { 0 }),
                Some("sxtw") => (0b110u32, if shift_amount > 0 { 1u32 } else { 0 }),
                Some("sxtx") => (0b111u32, if shift_amount > 0 { 1u32 } else { 0 }),
                Some("uxtw") => (0b010u32, if shift_amount > 0 { 1u32 } else { 0 }),
                None => if is_w_index { (0b010u32, 0u32) } else { (0b011u32, 0u32) },
                _ => (0b011u32, 0u32),
            };
            let word = (0b11 << 30) | (0b111 << 27) | (0b00 << 25) | (0b10 << 23) | (1 << 21)
                | (rm << 16) | (option << 13) | (s_bit << 12) | (0b10 << 10) | (rn << 5) | prfop;
            Ok(EncodeResult::Word(word))
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
        "1d" => Ok((0, 0b11)),
        "2d" => Ok((1, 0b11)),
        _ => Err(format!("unsupported NEON arrangement: {}", arr)),
    }
}

/// Encode NEON three-same-register instructions: CMEQ, UQSUB, SQSUB, CMHI, etc.
///
/// Layout: 0 Q U 01110 size 1 Rm opcode 1 Rn Rd
///         31 30 29 28-24 23-22 21 20-16 15-11 10 9-5 4-0
///
/// `u_bit`: U field (bit 29) - 0 for signed, 1 for unsigned
/// `opcode`: instruction opcode (bits 15-11)
fn encode_neon_three_same(operands: &[Operand], u_bit: u32, opcode: u32) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("NEON three-same requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _arr_n) = get_neon_reg(operands, 1)?;
    let (rm, _arr_m) = get_neon_reg(operands, 2)?;

    let (q, size) = neon_arr_to_q_size(&arr_d)?;

    // 0 Q U 01110 size 1 Rm opcode 1 Rn Rd
    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size << 22) | (1 << 21)
        | (rm << 16) | (opcode << 11) | (1 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON three-different instructions: USUBL, SSUBL, UADDL, SADDL, etc.
///
/// These instructions have wider destination than source operands.
/// Format: 0 Q U 01110 size 1 Rm opcode 00 Rn Rd
///
/// `u_bit`: 0 for signed, 1 for unsigned
/// `opcode`: 4-bit opcode (bits 15-12)
/// `is_high`: true for the "2" variant (upper half, Q=1)
fn encode_neon_three_diff(operands: &[Operand], u_bit: u32, opcode: u32, is_high: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("NEON three-different requires 3 operands".to_string());
    }
    let (rd, _arr_d) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;
    let (rm, _arr_m) = get_neon_reg(operands, 2)?;

    // Size is determined from the source (narrow) arrangement
    let (q, size) = match arr_n.as_str() {
        "8b" => (0u32, 0b00u32),   // base
        "16b" => (1, 0b00),         // "2" variant
        "4h" => (0, 0b01),
        "8h" => (1, 0b01),
        "2s" => (0, 0b10),
        "4s" => (1, 0b10),
        _ => return Err(format!("unsupported source arrangement for three-diff: {}", arr_n)),
    };

    // For the "2" variant, override Q
    let q = if is_high { 1 } else { q };

    // 0 Q U 01110 size 1 Rm opcode 00 Rn Rd
    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size << 22)
        | (1 << 21) | (rm << 16) | (opcode << 12) | (0b00 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON SQSHRUN/SQSHRUN2: Signed saturating shift right unsigned narrow
/// Format: 0 Q 1 011110 immh immb 100011 Rn Rd
fn encode_neon_sqshrun(operands: &[Operand], is_rounding: bool, is_high: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("sqshrun requires 3 operands".to_string());
    }
    let (rd, _arr_d) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;
    let shift = match &operands[2] {
        Operand::Imm(v) => *v as u32,
        _ => return Err("sqshrun: expected immediate shift".to_string()),
    };

    // immh:immb encode element size and shift amount
    // For source .4s (dest .4h or .8h): immh=001x, shift_amount = 32 - (immh:immb)
    // For source .8h (dest .8b or .16b): immh=0001, shift_amount = 16 - (immh:immb)
    // For source .2d (dest .2s or .4s): immh=01xx, shift_amount = 64 - (immh:immb)
    let (element_bits, immh_base) = match arr_n.as_str() {
        "8h" => (16u32, 0b0001u32),
        "4s" => (32, 0b0010),
        "2d" => (64, 0b0100),
        _ => return Err(format!("sqshrun: unsupported source arrangement: {}", arr_n)),
    };

    if shift == 0 || shift > element_bits {
        return Err(format!("sqshrun: shift {} out of range for {}-bit elements", shift, element_bits));
    }

    let immhb = (element_bits - shift) & 0x7F; // immh:immb combined
    let immh = (immhb >> 3) | immh_base;
    let immb = immhb & 0x7;

    let q = if is_high { 1u32 } else { 0 };

    // 0 Q 1 011110 immh immb opcode 1 Rn Rd
    // SQSHRUN: opcode = 100001, SQRSHRUN: opcode = 100011
    let opcode_bits: u32 = if is_rounding { 0b100011 } else { 0b100001 };
    let word = (q << 30) | (1 << 29) | (0b011110 << 23) | (immh << 19) | (immb << 16)
        | (opcode_bits << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON UXTL/SXTL (unsigned/signed extend long).
/// These are aliases for USHLL/SSHLL with shift #0.
///
/// Format: 0 Q U 011110 immh immb 10100 1 Rn Rd
fn encode_neon_xtl(operands: &[Operand], u_bit: u32, is_high: bool) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("NEON uxtl/sxtl requires 2 operands".to_string());
    }
    let (rd, _arr_d) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;

    // immh encodes the source element size, immb=0 (shift=0)
    let immh = match arr_n.as_str() {
        "8b" | "16b" => 0b0001u32,
        "4h" | "8h" => 0b0010,
        "2s" | "4s" => 0b0100,
        _ => return Err(format!("uxtl/sxtl: unsupported source arrangement: {}", arr_n)),
    };

    let q = if is_high { 1u32 } else { 0 };

    // 0 Q U 011110 immh immb 10100 1 Rn Rd
    let word = (q << 30) | (u_bit << 29) | (0b011110 << 23) | (immh << 19)
        | (0b101001 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON compare-to-zero: CMEQ Vd, Vn, #0, CMGE Vd, Vn, #0, etc.
///
/// Format: 0 Q U 01110 size 10000 opcode 10 Rn Rd
fn encode_neon_cmp_zero(operands: &[Operand], u_bit: u32, opcode: u32) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("NEON compare-zero requires at least 2 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (q, size) = neon_arr_to_q_size(&arr_d)?;

    // 0 Q U 01110 size 10000 opcode 10 Rn Rd
    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size << 22)
        | (0b10000 << 17) | (opcode << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON two-register miscellaneous narrowing: UQXTN, SQXTN, XTN
///
/// Format: 0 Q U 01110 size 10000 opcode 10 Rn Rd
fn encode_neon_two_misc_narrow(operands: &[Operand], u_bit: u32, opcode: u32, is_high: bool) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("NEON two-reg narrow requires 2 operands".to_string());
    }
    let (rd, _arr_d) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;

    // Size from source (wider) arrangement
    let size = match arr_n.as_str() {
        "8h" => 0b00u32,
        "4s" => 0b01,
        "2d" => 0b10,
        _ => return Err(format!("unsupported source arrangement for narrow: {}", arr_n)),
    };

    let q = if is_high { 1u32 } else { 0 };

    // 0 Q U 01110 size 10000 opcode 10 Rn Rd
    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size << 22)
        | (0b10000 << 17) | (opcode << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON vector-by-element long instructions: SMULL/UMULL/SMLAL/UMLAL/SMLSL/UMLSL (elem)
///
/// Format: 0 Q U 01111 size L M Rm opcode H 0 Rn Rd
///
/// These are the widening multiply-by-element forms where the third operand
/// is a register lane (e.g., v0.h[2]).
fn encode_neon_elem_long(operands: &[Operand], u_bit: u32, opcode: u32, is_high: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("NEON elem-long requires 3 operands".to_string());
    }
    let (rd, _arr_d) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;

    // Third operand is RegLane: v0.h[2]
    let (rm, index) = match &operands[2] {
        Operand::RegLane { reg, elem_size: _, index } => {
            let rm = parse_reg_num(reg).ok_or("invalid NEON register")?;
            (rm, *index)
        }
        _ => return Err(format!("expected register lane operand, got {:?}", operands[2])),
    };

    // Determine size and Q from source arrangement
    let (q, size) = match arr_n.as_str() {
        "4h" => (0u32, 0b01u32),
        "8h" => (1, 0b01),
        "2s" => (0, 0b10),
        "4s" => (1, 0b10),
        _ => return Err(format!("unsupported source arrangement for elem-long: {}", arr_n)),
    };
    let q = if is_high { 1 } else { q };

    // Encode index into H:L:M bits depending on element size
    let (h, l, m) = match size {
        0b01 => {
            // Half-word: index = H:L:M (3 bits), Rm limited to v0-v15
            if index > 7 {
                return Err(format!("element index {} out of range for .h", index));
            }
            let h = (index >> 2) & 1;
            let l = (index >> 1) & 1;
            let m = index & 1;
            (h, l, m)
        }
        0b10 => {
            // Word: index = H:L (2 bits), M=Rm[4]
            if index > 3 {
                return Err(format!("element index {} out of range for .s", index));
            }
            let h = (index >> 1) & 1;
            let l = index & 1;
            let m = (rm >> 4) & 1; // M bit from Rm[4]
            (h, l, m)
        }
        _ => return Err("unsupported element size for by-element".to_string()),
    };

    // Limit Rm for half-word indexing (only v0-v15)
    let rm_enc = if size == 0b01 { rm & 0xF } else { rm & 0x1F };

    // 0 Q U 01111 size L M Rm opcode H 0 Rn Rd
    let word = (q << 30) | (u_bit << 29) | (0b01111 << 24) | (size << 22)
        | (l << 21) | (m << 20) | (rm_enc << 16) | (opcode << 12)
        | (h << 11) | (0 << 10) | (rn << 5) | rd;
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

/// Encode NEON PMUL Vd.T, Vn.T, Vm.T (polynomial multiply, bytes only)
fn encode_neon_pmul(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let q: u32 = if arr_d == "16b" { 1 } else { 0 };
    // PMUL: 0 Q 1 01110 00 1 Rm 10011 1 Rn Rd (size=00 for bytes, U=1)
    // PMUL encoding: size=00 (bytes) is implicit (zero bits at [23:22])
    let word = (q << 30) | (1 << 29) | (0b01110 << 24) | (1 << 21)
        | (rm << 16) | (0b100111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON MLA Vd.T, Vn.T, Vm.T (multiply-accumulate)
fn encode_neon_mla(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let (q, size) = neon_arr_to_q_size(&arr_d)?;
    // MLA: 0 Q 0 01110 size 1 Rm 10010 1 Rn Rd
    let word = (q << 30) | (0b001110 << 24) | (size << 22) | (1 << 21)
        | (rm << 16) | (0b100101 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON MLS Vd.T, Vn.T, Vm.T (multiply-subtract)
fn encode_neon_mls(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let (q, size) = neon_arr_to_q_size(&arr_d)?;
    // MLS: 0 Q 1 01110 size 1 Rm 10010 1 Rn Rd (U=1)
    let word = (q << 30) | (1 << 29) | (0b01110 << 24) | (size << 22) | (1 << 21)
        | (rm << 16) | (0b100101 << 10) | (rn << 5) | rd;
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
    let word = ((((q << 30) | (0b101110 << 24))
        | (rm << 16)) | ((index & 0xF) << 11)) | (rn << 5) | rd;
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

/// Encode NEON across-vector instructions: UMAXV, UMINV, SMAXV, SMINV
///
/// Format: 0 Q U 01110 size 11000 opcode 10 Rn Rd
///
/// `u_bit`: 0 for signed, 1 for unsigned
/// `opcode`: 5-bit opcode (bits 16-12)
fn encode_neon_across(operands: &[Operand], u_bit: u32, opcode: u32) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("NEON across-vector requires 2 operands".to_string());
    }
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;

    let (q, size) = neon_arr_to_q_size(&arr_n)?;

    // 0 Q U 01110 size 11000 opcode 10 Rn Rd
    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size << 22) | (0b11000 << 17)
        | (opcode << 12) | (0b10 << 10) | (rn << 5) | rd;
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

    // DUP Vd.T, Vn.Ts[index] (broadcast element to all lanes)
    if let Some(Operand::RegLane { reg, elem_size, index }) = operands.get(1) {
        let rn = parse_reg_num(reg).ok_or("invalid NEON register")?;
        let (q, _) = neon_arr_to_q_size(&arr_d)?;

        // imm5 encodes both element size and index:
        // .b[i]: imm5 = (i << 1) | 0b00001
        // .h[i]: imm5 = (i << 2) | 0b00010
        // .s[i]: imm5 = (i << 3) | 0b00100
        // .d[i]: imm5 = (i << 4) | 0b01000
        let imm5 = match elem_size.as_str() {
            "b" => ((*index & 0xF) << 1) | 0b00001,
            "h" => ((*index & 0x7) << 2) | 0b00010,
            "s" => ((*index & 0x3) << 3) | 0b00100,
            "d" => ((*index & 0x1) << 4) | 0b01000,
            _ => return Err(format!("unsupported dup element size: {}", elem_size)),
        };

        // DUP Vd.T, Vn.Ts[i]: 0 Q 0 01110 000 imm5 0 0000 1 Rn Rd
        let word = (q << 30) | (0b001110000u32 << 21) | (imm5 << 16)
            | (0b000001 << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err("unsupported dup operands".to_string())
}

/// Encode NEON INS (insert element from GP register): INS Vd.Ts[index], Xn
fn encode_neon_ins(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("ins requires 2 operands".to_string());
    }
    match (&operands[0], &operands[1]) {
        // INS Vd.Ts[dst_idx], Xn (general register to element)
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
        // INS Vd.Ts[dst_idx], Vn.Ts[src_idx] (element to element)
        (Operand::RegLane { reg: rd_name, elem_size: dst_size, index: dst_idx },
         Operand::RegLane { reg: rn_name, elem_size: _src_size, index: src_idx }) => {
            let rd = parse_reg_num(rd_name).ok_or("invalid NEON rd")?;
            let rn = parse_reg_num(rn_name).ok_or("invalid NEON rn")?;

            let (imm5, imm4) = match dst_size.as_str() {
                "b" => (
                    ((*dst_idx & 0xF) << 1) | 0b00001,
                    *src_idx & 0xF,
                ),
                "h" => (
                    ((*dst_idx & 0x7) << 2) | 0b00010,
                    (*src_idx & 0x7) << 1,
                ),
                "s" => (
                    ((*dst_idx & 0x3) << 3) | 0b00100,
                    (*src_idx & 0x3) << 2,
                ),
                "d" => (
                    ((*dst_idx & 0x1) << 4) | 0b01000,
                    (*src_idx & 0x1) << 3,
                ),
                _ => return Err(format!("unsupported ins element size: {}", dst_size)),
            };

            // INS Vd.Ts[dst], Vn.Ts[src]: 0 1 1 01110 000 imm5 0 imm4 1 Rn Rd
            let word = (0b01101110000u32 << 21) | (imm5 << 16)
                | (imm4 << 11) | (1 << 10) | (rn << 5) | rd;
            Ok(EncodeResult::Word(word))
        }
        _ => Err("ins: expected (RegLane, Reg) or (RegLane, RegLane) operands".to_string()),
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
    let word = ((q << 30) | (1 << 29) | (0b01110 << 24))
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
            // The 64-bit immediate is encoded as 8 bits, where each bit expands
            // to 8 bits (0x00 or 0xFF) in the result.
            // Convert the 64-bit value to the 8-bit encoding.
            let imm64 = imm as u64;
            let mut imm8 = 0u32;
            for i in 0..8 {
                let byte_val = (imm64 >> (i * 8)) & 0xFF;
                if byte_val == 0xFF {
                    imm8 |= 1 << i;
                } else if byte_val != 0 {
                    return Err(format!("movi .2d: each byte of immediate must be 0x00 or 0xFF, got 0x{:02x} at byte {}", byte_val, i));
                }
            }
            let abc = (imm8 >> 5) & 0x7;
            let defgh = imm8 & 0x1F;
            // MOVI Vd.2d, #imm: 0 1 1 0 1111 00 abc 1110 01 defgh Rd  (op=1, Q=1)
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

/// Encode BIC instruction - disambiguates between scalar and NEON forms.
/// Scalar register: BIC Xd, Xn, Xm [, shift #amount] -> AND NOT (opc=00, N=1)
/// Scalar immediate: BIC Xd, Xn, #imm -> AND Xd, Xn, #~imm
/// NEON vector: BIC Vd.T, Vn.T, Vm.T
fn encode_bic(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("bic requires 3 operands".to_string());
    }

    // NEON vector form: BIC Vd.T, Vn.T, Vm.T
    if let Some(Operand::RegArrangement { .. }) = operands.first() {
        return encode_neon_bic(operands);
    }

    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);

    // BIC Xd, Xn, #imm -> AND Xd, Xn, #~imm (bitmask immediate, inverted)
    if let Some(Operand::Imm(imm)) = operands.get(2) {
        let inverted = if is_64 {
            !(*imm as u64)
        } else {
            (!(*imm as u32)) as u64
        };
        if let Some((n, immr, imms)) = encode_bitmask_imm(inverted, is_64) {
            // AND Rd, Rn, #~imm: sf 00 100100 N immr imms Rn Rd
            // AND Rd, Rn, #~imm encoding: sf=bit31, opc=00 (bits29:30), 100100 (bits23:28), N, immr, imms, Rn, Rd
            let word = (sf << 31) | (0b100100 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }
        return Err(format!("cannot encode bitmask immediate for bic: 0x{:x} (inverted: 0x{:x})", imm, inverted));
    }

    // BIC Xd, Xn, Xm [, shift #amount]: sf 00 01010 shift 1 Rm imm6 Rn Rd (N=1)
    if let Some(Operand::Reg(rm_name)) = operands.get(2) {
        let rm = parse_reg_num(rm_name).ok_or("invalid rm register for bic")?;

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

        // BIC is AND with N=1 (bit 21): sf opc=00(bits29:30) 01010 shift 1 Rm imm6 Rn Rd
        let word = (sf << 31) | (0b01010 << 24) | (shift_type << 22) | (1 << 21)
            | (rm << 16) | ((shift_amount & 0x3F) << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err("unsupported bic operands".to_string())
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
    let word = ((((q << 30) | (0b001110 << 24))
        | (rm << 16)) | (len << 13)) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON TBX: table vector lookup with insert (preserves out-of-range lanes)
fn encode_neon_tbx(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("tbx requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let q: u32 = if arr_d == "16b" { 1 } else { 0 };

    let (rn, num_regs) = match &operands[1] {
        Operand::RegList(regs) => {
            let first_reg = match &regs[0] {
                Operand::RegArrangement { reg, .. } => parse_reg_num(reg).ok_or("invalid reg")?,
                Operand::Reg(name) => parse_reg_num(name).ok_or("invalid reg")?,
                _ => return Err("tbx: expected register in list".to_string()),
            };
            (first_reg, regs.len() as u32)
        }
        _ => return Err("tbx: expected register list as second operand".to_string()),
    };

    let (rm, _) = get_neon_reg(operands, 2)?;
    let len = (num_regs - 1) & 0x3;

    // TBX: 0 Q 00 1110 000 Rm 0 len 1 00 Rn Rd (op=1 for TBX vs op=0 for TBL)
    let word = (q << 30) | (0b001110 << 24) | (rm << 16) | (len << 13)
        | (1 << 12) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode NEON LD1R: load single structure and replicate to all lanes
fn encode_neon_ld1r(operands: &[Operand]) -> Result<EncodeResult, String> {
    // LD1R {Vt.T}, [Xn]
    if operands.len() < 2 {
        return Err("ld1r requires 2 operands".to_string());
    }

    let (rt, arr) = match &operands[0] {
        Operand::RegList(regs) => {
            if regs.len() != 1 {
                return Err("ld1r expects exactly one register in list".to_string());
            }
            match &regs[0] {
                Operand::RegArrangement { reg, arrangement } => {
                    let num = parse_reg_num(reg).ok_or("invalid reg")?;
                    (num, arrangement.clone())
                }
                _ => return Err("ld1r: expected register with arrangement".to_string()),
            }
        }
        _ => return Err("ld1r: expected register list as first operand".to_string()),
    };

    let (q, size) = match arr.as_str() {
        "8b"  => (0u32, 0b00u32),
        "16b" => (1, 0b00),
        "4h"  => (0, 0b01),
        "8h"  => (1, 0b01),
        "2s"  => (0, 0b10),
        "4s"  => (1, 0b10),
        "1d"  => (0, 0b11),
        "2d"  => (1, 0b11),
        _ => return Err(format!("ld1r: unsupported arrangement: {}", arr)),
    };

    match &operands[1] {
        Operand::Mem { base, offset: 0 } => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            // LD1R: 0 Q 0 01101 0 1 0 00000 110 0 size Rn Rt (no post-index)
            let word = (q << 30) | (0b001101 << 24) | (1 << 22) | (0b110 << 13)
                | (size << 10) | (rn << 5) | rt;
            Ok(EncodeResult::Word(word))
        }
        Operand::MemPostIndex { base, offset } => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            // LD1R post-index (immediate): 0 Q 0 01101 1 1 0 11111 110 0 size Rn Rt
            // Rm=11111 means post-index by element size
            let _ = offset; // offset must match element size, not encoded separately
            let word = (q << 30) | (0b001101 << 24) | (1 << 23) | (1 << 22)
                | (0b11111 << 16) | (0b110 << 13) | (size << 10) | (rn << 5) | rt;
            Ok(EncodeResult::Word(word))
        }
        _ => Err("ld1r: expected [Xn] or [Xn], #imm memory operand".to_string()),
    }
}

/// Encode NEON LD1 (vector load, multiple structures)
/// Dispatch LD/ST1-4: choose between "multiple structures" and "single structure (element)" encoding.
fn encode_neon_ld_st_dispatch(operands: &[Operand], is_load: bool, num_structs: u32) -> Result<EncodeResult, String> {
    // If the first operand is a RegListIndexed, use single-element encoding
    if let Some(Operand::RegListIndexed { .. }) = operands.first() {
        return encode_neon_ld_st_single(operands, is_load, num_structs);
    }
    // Multiple-structures encoding for ld1-4/st1-4
    encode_neon_ld_st_multi(operands, is_load, num_structs)
}

/// Encode NEON LD/ST single structure (element):
/// st1 {v0.s}[0], [x3]
/// st2 {v0.s, v1.s}[0], [x3]
/// st4 {v0.s, v1.s, v2.s, v3.s}[0], [x3]
/// ld2 {v0.s, v1.s}[0], [x3]
// TODO: add post-index form [Xn], #imm
fn encode_neon_ld_st_single(operands: &[Operand], is_load: bool, num_structs: u32) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err(format!("ld/st{} single element requires at least 2 operands", num_structs));
    }

    let (regs, index) = match &operands[0] {
        Operand::RegListIndexed { regs, index } => (regs, *index),
        _ => return Err("expected register list with index".to_string()),
    };

    if regs.len() as u32 != num_structs {
        return Err(format!("expected {} registers in list, got {}", num_structs, regs.len()));
    }
    // TODO: validate that registers in the list are consecutive (ARM ISA requirement)

    // Get element size and first register from the list
    let (rt, elem_size) = match &regs[0] {
        Operand::RegArrangement { reg, arrangement } => {
            (parse_reg_num(reg).ok_or("invalid register in list")?, arrangement.clone())
        }
        _ => return Err("expected register with arrangement in list".to_string()),
    };

    // Get base register and check for post-index
    let (rn, post_index) = match &operands[1] {
        Operand::Mem { base, offset: 0 } => {
            let rn = parse_reg_num(base).ok_or_else(|| format!("invalid base register: {}", base))?;
            // Check for post-index immediate: operands[2] is the post-index offset
            let pi = if operands.len() > 2 {
                match &operands[2] {
                    Operand::Imm(off) => Some(*off),
                    _ => None,
                }
            } else {
                None
            };
            (rn, pi)
        }
        Operand::MemPostIndex { base, offset } => {
            let rn = parse_reg_num(base).ok_or_else(|| format!("invalid base register: {}", base))?;
            (rn, Some(*offset))
        }
        _ => return Err("expected [Xn] memory operand".to_string()),
    };

    let l_bit = if is_load { 1u32 } else { 0u32 };

    // R bit: 0 for 1,3 registers; 1 for 2,4 registers
    let r_bit = match num_structs {
        1 | 3 => 0u32,
        2 | 4 => 1u32,
        _ => return Err(format!("unsupported struct count: {}", num_structs)),
    };

    // Compute opcode, S, Q, size based on element size and index
    let (opcode, s_bit, q_bit, size_field) = match elem_size.as_str() {
        "b" => {
            // opcode = 000 (1,2 regs) or 001 (3,4 regs)
            let base_opc = if num_structs <= 2 { 0b000u32 } else { 0b001u32 };
            // index bits: Q:S:size[1]:size[0] = 4 bits for 0-15
            let q = (index >> 3) & 1;
            let s = (index >> 2) & 1;
            let sz = index & 3;
            (base_opc, s, q, sz)
        }
        "h" => {
            let base_opc = if num_structs <= 2 { 0b010u32 } else { 0b011u32 };
            // index bits: Q:S:size[1] = 3 bits for 0-7, size[0]=0
            let q = (index >> 2) & 1;
            let s = (index >> 1) & 1;
            let sz = (index & 1) << 1;
            (base_opc, s, q, sz)
        }
        "s" => {
            let base_opc = if num_structs <= 2 { 0b100u32 } else { 0b101u32 };
            // index bits: Q:S = 2 bits for 0-3, size=00
            let q = (index >> 1) & 1;
            let s = index & 1;
            (base_opc, s, q, 0b00u32)
        }
        "d" => {
            let base_opc = if num_structs <= 2 { 0b100u32 } else { 0b101u32 };
            // index bits: Q = 1 bit for 0-1, S=0, size=01
            let q = index & 1;
            (base_opc, 0u32, q, 0b01u32)
        }
        _ => return Err(format!("unsupported element size for ld/st single: {}", elem_size)),
    };

    if let Some(_offset) = post_index {
        // Post-index form: Q 0011011 L R 11111 opcode S size Rn Rt
        // (Rm=11111 means immediate post-index, the amount is implicit from element size)
        let word = (q_bit << 30) | (0b0011011 << 23) | (l_bit << 22) | (r_bit << 21)
            | (0b11111 << 16) | (opcode << 13) | (s_bit << 12) | (size_field << 10) | (rn << 5) | rt;
        Ok(EncodeResult::Word(word))
    } else {
        // No post-index: Q 0011010 L R 00000 opcode S size Rn Rt
        let word = (q_bit << 30) | (0b0011010 << 23) | (l_bit << 22) | (r_bit << 21)
            | (opcode << 13) | (s_bit << 12) | (size_field << 10) | (rn << 5) | rt;
        Ok(EncodeResult::Word(word))
    }
}

/// Common encoder for LD1/ST1 (multiple structures)
fn encode_neon_ld_st_multi(operands: &[Operand], is_load: bool, num_structs: u32) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err(format!("ld{}/st{} requires at least 2 operands", num_structs, num_structs));
    }

    // First operand: register list {Vt.T} or {Vt.T, Vt+1.T, ...}
    let (rt, arr, num_regs) = match &operands[0] {
        Operand::RegList(regs) => {
            let (first_reg, arrangement) = match &regs[0] {
                Operand::RegArrangement { reg, arrangement } => {
                    (parse_reg_num(reg).ok_or("invalid reg")?, arrangement.clone())
                }
                _ => return Err(format!("ld{}/st{}: expected RegArrangement in list", num_structs, num_structs)),
            };
            (first_reg, arrangement, regs.len() as u32)
        }
        _ => return Err(format!("ld{}/st{}: expected register list", num_structs, num_structs)),
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
        _ => return Err(format!("ld{}/st{}: expected [Xn] memory operand", num_structs, num_structs)),
    };

    // opcode field based on structure count and number of registers:
    // LD1/ST1: 1 reg=0111, 2 reg=1010, 3 reg=0110, 4 reg=0010
    // LD2/ST2: 2 reg=1000
    // LD3/ST3: 3 reg=0100
    // LD4/ST4: 4 reg=0000
    let opcode = match num_structs {
        1 => match num_regs {
            1 => 0b0111u32,
            2 => 0b1010,
            3 => 0b0110,
            4 => 0b0010,
            _ => return Err(format!("ld1/st1: unsupported register count: {}", num_regs)),
        },
        2 => 0b1000u32,
        3 => 0b0100,
        4 => 0b0000,
        _ => return Err(format!("unsupported structure count: {}", num_structs)),
    };

    let l_bit = if is_load { 1u32 } else { 0u32 };

    // Handle post-index form from merged MemPostIndex
    if let Some(_imm) = post_index {
        // Post-index with immediate: use Rm=11111 (0x1F)
        let word = ((q << 30) | (0b001100 << 24) | (1 << 23) | (l_bit << 22)) | (0b11111 << 16) | (opcode << 12) | (size << 10) | (rn << 5) | rt;
        return Ok(EncodeResult::Word(word));
    }

    // Check for post-index form via separate operands: [Xn], Xm
    if operands.len() > 2 {
        match &operands[2] {
            Operand::Imm(_) => {
                // Post-index with immediate: use Rm=11111
                let word = ((q << 30) | (0b001100 << 24) | (1 << 23) | (l_bit << 22)) | (0b11111 << 16) | (opcode << 12) | (size << 10) | (rn << 5) | rt;
                return Ok(EncodeResult::Word(word));
            }
            Operand::Reg(rm_name) => {
                let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;
                let word = ((q << 30) | (0b001100 << 24) | (1 << 23) | (l_bit << 22)) | (rm << 16) | (opcode << 12) | (size << 10) | (rn << 5) | rt;
                return Ok(EncodeResult::Word(word));
            }
            _ => {}
        }
    }

    // No post-index: LD1/ST1 {Vt.T...}, [Xn]
    // 0 Q 001100 0 L 0 00000 opcode size Rn Rt
    let word = (((q << 30) | (0b001100 << 24)) | (l_bit << 22)) | (opcode << 12) | (size << 10) | (rn << 5) | rt;
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
    let word = (((q << 30) | (0b001110 << 24) | (size << 22)) | (rm << 16)) | (op_bits << 12) | (0b10 << 10) | (rn << 5) | rd;
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
    let word = ((0b11001110u32 << 24) | (rm << 16)) | (rk << 10) | (rn << 5) | rd;
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
    let word = ((q << 30) | (0b001110 << 24) | (0b11 << 22) | (1 << 21)
        | (rm << 16) | (0b11100 << 11)) | (rn << 5) | rd;
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

    let word = (q << 30) | (0b011110 << 23) | (immh_immb << 16)
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

    let word = (q << 30) | (0b011110 << 23) | (immh_immb << 16)
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

/// Encode SRI (Shift Right and Insert) immediate.
/// SRI Vd.T, Vn.T, #shift: 0 Q 1 0 11110 immh:immb 010001 Rn Rd  (U=1)
fn encode_neon_sri(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("sri requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)? as u32;

    let (q, _) = neon_arr_to_q_size(&arr_d)?;

    // immh:immb = (2*esize - shift) for right shift
    let immh_immb = match arr_d.as_str() {
        "8b" | "16b" => (16 - shift) & 0xF,
        "4h" | "8h" => (32 - shift) & 0x1F,
        "2s" | "4s" => (64 - shift) & 0x3F,
        "2d" => (128 - shift) & 0x7F,
        _ => return Err(format!("unsupported sri arrangement: {}", arr_d)),
    };

    let word = (q << 30) | (1 << 29) | (0b011110 << 23) | (immh_immb << 16)
        | (0b010001 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode ORN (logical OR NOT): ORN Rd, Rn, Rm (scalar or vector)
fn encode_orn(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("orn requires 3 operands".to_string());
    }

    // NEON vector form: ORN Vd.T, Vn.T, Vm.T
    if let Some(Operand::RegArrangement { .. }) = operands.first() {
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

    // ORN Rd, Rn, Rm [, shift #amount]: sf 01 01010 shift 1 Rm imm6 Rn Rd
    let word = (sf << 31) | (0b01 << 29) | (0b01010 << 24) | (shift_type << 22) | (1 << 21)
        | (rm << 16) | ((shift_amount & 0x3F) << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode EON (exclusive OR NOT): EON Rd, Rn, Rm [, shift #amount]
fn encode_eon(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("eon requires 3 operands".to_string());
    }
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);

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

    // EON Rd, Rn, Rm [, shift #amount]: sf 10 01010 shift 1 Rm imm6 Rn Rd (opc=10, N=1)
    let word = (sf << 31) | (0b10 << 29) | (0b01010 << 24) | (shift_type << 22) | (1 << 21)
        | (rm << 16) | ((shift_amount & 0x3F) << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode BICS (bitwise clear, setting flags): BICS Rd, Rn, Rm [, shift #amount]
fn encode_bics(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("bics requires 3 operands".to_string());
    }
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);

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

    // BICS Rd, Rn, Rm [, shift #amount]: sf 11 01010 shift 1 Rm imm6 Rn Rd
    let word = (sf << 31) | (0b11 << 29) | (0b01010 << 24) | (shift_type << 22) | (1 << 21)
        | (rm << 16) | ((shift_amount & 0x3F) << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// Encode DC (data cache) instruction: dc civac, Xt
fn encode_dc(operands: &[Operand], raw_operands: &str) -> Result<EncodeResult, String> {
    // Check for the operation type in the operands or raw string
    let op = match operands.first() {
        Some(Operand::Symbol(s)) => s.to_lowercase(),
        _ => raw_operands.to_lowercase(),
    };

    // Find the register operand (second operand or last operand)
    let rt = match operands.get(1) {
        Some(Operand::Reg(name)) => parse_reg_num(name).ok_or("invalid register for dc")?,
        _ => {
            if let Some(Operand::Reg(name)) = operands.last() {
                parse_reg_num(name).ok_or("invalid register for dc")?
            } else {
                0
            }
        }
    };

    if op.contains("civac") {
        // DC CIVAC: sys #3, c7, c14, #1, Xt
        let word = 0xd50b7e20 | rt;
        return Ok(EncodeResult::Word(word));
    }
    if op.contains("cvac") {
        // DC CVAC: sys #3, c7, c10, #1, Xt
        let word = 0xd50b7a20 | rt;
        return Ok(EncodeResult::Word(word));
    }
    if op.contains("cvap") {
        // DC CVAP: sys #3, c7, c12, #1, Xt
        let word = 0xd50b7c20 | rt;
        return Ok(EncodeResult::Word(word));
    }
    if op.contains("cvau") {
        let word = 0xd50b7b20 | rt;
        return Ok(EncodeResult::Word(word));
    }
    if op.contains("ivac") {
        let word = 0xd5087620 | rt;
        return Ok(EncodeResult::Word(word));
    }
    if op.contains("zva") {
        // DC ZVA: sys #3, c7, c4, #1, Xt
        let word = 0xd50b7420 | rt;
        return Ok(EncodeResult::Word(word));
    }

    Err(format!("unsupported dc variant: {}", raw_operands))
}

// ── LSE Atomics ──────────────────────────────────────────────────────────

/// Encode CAS/CASA/CASAL/CASL (Compare and Swap).
/// CAS Xs, Xt, [Xn]: size 001000 1 L=0 1 Rs o0 11111 Rn Rt
/// TODO: Only 32-bit (W) and 64-bit (X) sizes supported; casb/cash need size=00/01.
fn encode_cas(mnemonic: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err(format!("{} requires 3 operands", mnemonic));
    }
    let (rs, is_64) = get_reg(operands, 0)?;
    let (rt, _) = get_reg(operands, 1)?;
    let rn = match operands.get(2) {
        Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("cas: invalid base")?,
        _ => return Err("cas requires memory operand [Xn]".to_string()),
    };
    let size = if is_64 { 0b11u32 } else { 0b10u32 };
    let mn = mnemonic.to_lowercase();
    let suffix = mn.strip_prefix("cas").unwrap_or("");
    // L bit (acquire): set for casa, casal
    let l = if suffix.contains('a') { 1u32 } else { 0u32 };
    // o0 bit (release): set for casl, casal
    let o0 = if suffix.contains('l') { 1u32 } else { 0u32 };
    // size 001000 1 L 1 Rs o0 11111 Rn Rt
    let word = (size << 30) | (0b001000 << 24) | (1 << 23) | (l << 22) | (1 << 21)
        | (rs << 16) | (o0 << 15) | (0b11111 << 10) | (rn << 5) | rt;
    Ok(EncodeResult::Word(word))
}

/// Encode SWP/SWPA/SWPAL/SWPL (Swap).
/// SWP Xs, Xt, [Xn]: size 111000 AR 1 Rs 1 000 00 Rn Rt
fn encode_swp(mnemonic: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err(format!("{} requires 3 operands", mnemonic));
    }
    let (rs, is_64) = get_reg(operands, 0)?;
    let (rt, _) = get_reg(operands, 1)?;
    let rn = match operands.get(2) {
        Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("swp: invalid base")?,
        _ => return Err("swp requires memory operand [Xn]".to_string()),
    };
    let size = if is_64 { 0b11u32 } else { 0b10u32 };
    let mn = mnemonic.to_lowercase();
    let suffix = mn.strip_prefix("swp").unwrap_or("");
    let a = if suffix.contains('a') { 1u32 } else { 0u32 };
    let r = if suffix.contains('l') { 1u32 } else { 0u32 };
    // size 111000 A R 1 Rs 1 000 00 Rn Rt
    let word = (size << 30) | (0b111000 << 24) | (a << 23) | (r << 22) | (1 << 21)
        | (rs << 16) | (1 << 15) | (rn << 5) | rt;
    Ok(EncodeResult::Word(word))
}

/// Encode LDADD/LDCLR/LDEOR/LDSET and their acquire/release variants (LSE atomics).
/// LDADD Rs, Rt, [Xn]: size 111000 A R 1 Rs 0 opc 00 Rn Rt
/// opc: LDADD=000, LDCLR=001, LDEOR=010, LDSET=011
fn encode_ldop(mnemonic: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err(format!("{} requires 3 operands", mnemonic));
    }
    let (rs, is_64) = get_reg(operands, 0)?;
    let (rt, _) = get_reg(operands, 1)?;
    let rn = match operands.get(2) {
        Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("ldop: invalid base")?,
        _ => return Err(format!("{} requires memory operand [Xn]", mnemonic)),
    };
    let size = if is_64 { 0b11u32 } else { 0b10u32 };
    let mn = mnemonic.to_lowercase();
    // Determine base op and suffix
    let (base, suffix) = if let Some(s) = mn.strip_prefix("ldadd") {
        (0b000u32, s)
    } else if let Some(s) = mn.strip_prefix("ldclr") {
        (0b001u32, s)
    } else if let Some(s) = mn.strip_prefix("ldeor") {
        (0b010u32, s)
    } else if let Some(s) = mn.strip_prefix("ldset") {
        (0b011u32, s)
    } else {
        return Err(format!("unknown ld atomic op: {}", mnemonic));
    };
    let a = if suffix.contains('a') { 1u32 } else { 0u32 };
    let r = if suffix.contains('l') { 1u32 } else { 0u32 };
    // size 111000 A R 1 Rs 0 opc 00 Rn Rt
    let word = (size << 30) | (0b111000 << 24) | (a << 23) | (r << 22) | (1 << 21)
        | (rs << 16) | (base << 12) | (rn << 5) | rt;
    Ok(EncodeResult::Word(word))
}

// ── NEON RBIT (vector bit reverse) ───────────────────────────────────────

/// Encode NEON RBIT Vd.T, Vn.T (per-byte bit reversal in each element).
fn encode_neon_rbit(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("neon rbit requires 2 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;

    // Only .8b and .16b arrangements are valid for NEON RBIT
    if arr_d != "8b" && arr_d != "16b" {
        return Err(format!("neon rbit: unsupported arrangement .{}, expected .8b or .16b", arr_d));
    }
    let q: u32 = if arr_d == "16b" { 1 } else { 0 };
    // RBIT Vd.T, Vn.T: 0 Q 1 01110 01 10000 00101 10 Rn Rd
    let word = (q << 30) | (1 << 29) | (0b01110 << 24) | (0b01 << 22)
        | (0b10000 << 17) | (0b00101 << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON MVNI (move NOT immediate) ───────────────────────────────────────

/// Encode NEON MVNI Vd.T, #imm (move bitwise NOT immediate to vector).
fn encode_neon_mvni(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("mvni requires 2 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let imm = get_imm(operands, 1)?;
    let imm8 = imm as u32 & 0xFF;

    // Extract abc:defgh for encoding
    let abc = (imm8 >> 5) & 0x7;
    let defgh = imm8 & 0x1f;

    match arr_d.as_str() {
        "2s" | "4s" => {
            let q: u32 = if arr_d == "4s" { 1 } else { 0 };
            // Check for optional shift
            let cmode = if let Some(Operand::Shift { kind, amount }) = operands.get(2) {
                if kind.to_lowercase() == "lsl" {
                    match *amount {
                        0 => 0b0000u32,
                        8 => 0b0010,
                        16 => 0b0100,
                        24 => 0b0110,
                        _ => return Err(format!("mvni: unsupported shift amount: {}", amount)),
                    }
                } else if kind.to_lowercase() == "msl" {
                    match *amount {
                        8 => 0b1100u32,
                        16 => 0b1101,
                        _ => return Err(format!("mvni: unsupported MSL shift: {}", amount)),
                    }
                } else {
                    0b0000
                }
            } else {
                0b0000
            };
            // MVNI: 0 Q 1 0 1111 00 abc cmode 01 defgh Rd  (op=1)
            let word = (q << 30) | (1 << 29) | (0b0111100 << 22)
                | (abc << 16) | (cmode << 12) | (0b01 << 10) | (defgh << 5) | rd;
            Ok(EncodeResult::Word(word))
        }
        "4h" | "8h" => {
            let q: u32 = if arr_d == "8h" { 1 } else { 0 };
            // MVNI 16-bit: cmode=1000, op=1
            let word = (q << 30) | (1 << 29) | (0b0111100 << 22)
                | (abc << 16) | (0b1000 << 12) | (0b01 << 10) | (defgh << 5) | rd;
            Ok(EncodeResult::Word(word))
        }
        _ => Err(format!("mvni: unsupported arrangement: {}", arr_d)),
    }
}

// ── NEON float three-same ────────────────────────────────────────────────
/// Encode NEON float three-same: FADD, FSUB, FMUL, FDIV, FMLA, FMLS, etc.
/// Format: 0 Q U 01110 size 1 Rm opcode 1 Rn Rd
/// size[1]=size_hi (0 or 1), size[0]=sz (0=single, 1=double)
fn encode_neon_float_three_same(operands: &[Operand], u_bit: u32, size_hi: u32, opcode: u32) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let (q, sz) = match arr_d.as_str() {
        "2s" => (0u32, 0u32), "4s" => (1, 0), "2d" => (1, 1),
        _ => return Err(format!("float three-same: unsupported arrangement: {}", arr_d)),
    };
    let size = (size_hi << 1) | sz;
    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size << 22) | (1 << 21)
        | (rm << 16) | (opcode << 11) | (1 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON two-register misc (integer) ─────────────────────────────────────
/// Encode NEON two-reg misc: ABS, NEG, CLS, CLZ, etc.
/// Format: 0 Q U 01110 size 10000 opcode 10 Rn Rd
fn encode_neon_two_misc(operands: &[Operand], u_bit: u32, opcode: u32) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (q, size) = neon_arr_to_q_size(&arr_d)?;
    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size << 22)
        | (0b10000 << 17) | (opcode << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON float two-register misc ─────────────────────────────────────────
/// Encode NEON float two-reg misc: UCVTF, SCVTF, FCVTZS, FCVTZU, FNEG, FABS, etc. (vector)
/// Format: 0 Q U 01110 size 10000 opcode 10 Rn Rd
/// size[1]=size_hi, size[0]=sz (0=single, 1=double)
fn encode_neon_float_two_misc(operands: &[Operand], u_bit: u32, size_hi: u32, opcode: u32) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (q, sz) = match arr_d.as_str() {
        "2s" => (0u32, 0u32), "4s" => (1, 0), "2d" => (1, 1),
        _ => return Err(format!("float two-misc: unsupported arrangement: {}", arr_d)),
    };
    let size = (size_hi << 1) | sz;
    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size << 22)
        | (0b10000 << 17) | (opcode << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON shift right narrow (SHRN/RSHRN) ─────────────────────────────────
/// Format: 0 Q 0 01111 0 immh immb opcode 1 Rn Rd
/// SHRN opcode=10000, RSHRN opcode=10001
fn encode_neon_shrn(operands: &[Operand], opcode: u32, is_high: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 { return Err("shrn/rshrn requires 3 operands".to_string()); }
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)? as u32;
    let element_bits = match arr_n.as_str() { "8h" => 16u32, "4s" => 32, "2d" => 64,
        _ => return Err(format!("shrn: unsupported source: {}", arr_n)), };
    let half_bits = element_bits / 2;
    if shift == 0 || shift > half_bits { return Err(format!("shrn: shift {} out of range", shift)); }
    let immhb = element_bits - shift;
    let q = if is_high { 1u32 } else { 0 };
    let word = (q << 30) | (0b011110 << 23) | ((immhb >> 3) << 19) | ((immhb & 7) << 16)
        | (opcode << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON shift right accumulate (SSRA/USRA/SRSHR/URSHR) ─────────────────
/// Format: 0 Q U 01111 0 immh immb opcode 1 Rn Rd
fn encode_neon_shift_right(operands: &[Operand], u_bit: u32, opcode: u32) -> Result<EncodeResult, String> {
    if operands.len() < 3 { return Err("shift-right requires 3 operands".to_string()); }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)? as u32;
    let (q, _) = neon_arr_to_q_size(&arr_d)?;
    let element_bits: u32 = match arr_d.as_str() {
        "8b" | "16b" => 8, "4h" | "8h" => 16, "2s" | "4s" => 32, "2d" => 64,
        _ => return Err(format!("shift-right: unsupported: {}", arr_d)), };
    if shift == 0 || shift > element_bits { return Err(format!("shift {} out of range", shift)); }
    let immhb = (element_bits * 2) - shift;
    let word = (q << 30) | (u_bit << 29) | (0b011110 << 23) | ((immhb >> 3) << 19) | ((immhb & 7) << 16)
        | (opcode << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON SSHLL/USHLL (shift left long) ───────────────────────────────────
/// Format: 0 Q U 011110 immh immb 10100 1 Rn Rd
fn encode_neon_shll(operands: &[Operand], u_bit: u32, is_high: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 { return Err("sshll/ushll requires 3 operands".to_string()); }
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)? as u32;
    let base_val = match arr_n.as_str() {
        "8b" | "16b" => 8u32, "4h" | "8h" => 16, "2s" | "4s" => 32,
        _ => return Err(format!("sshll/ushll: unsupported source: {}", arr_n)), };
    let immhb = base_val + shift;
    let q = if is_high { 1u32 } else { 0 };
    let word = (q << 30) | (u_bit << 29) | (0b011110 << 23) | ((immhb >> 3) << 19) | ((immhb & 7) << 16)
        | (0b101001 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON pairwise add (UADDLP/SADDLP/UADALP/SADALP) ────────────────────

// ── NEON three-different extras: UABAL/SABAL/ADDHN/RADDHN/SUBHN/RSUBHN ──
// Already have encode_neon_three_diff which handles these opcodes.

// ── NEON SQXTUN ──────────────────────────────────────────────────────────
// Two-reg misc with U=1, opcode=10010. Reuse encode_neon_two_misc_narrow.

// ── NEON shift right narrow saturating (SQSHRN/UQSHRN/SQRSHRN/UQRSHRN) ─
fn encode_neon_qshrn(operands: &[Operand], u_bit: u32, is_rounding: bool, is_high: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 { return Err("qshrn requires 3 operands".to_string()); }
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)? as u32;
    let element_bits = match arr_n.as_str() { "8h" => 16u32, "4s" => 32, "2d" => 64,
        _ => return Err(format!("qshrn: unsupported source: {}", arr_n)), };
    if shift == 0 || shift > element_bits { return Err(format!("qshrn: shift {} out of range for {}-bit elements", shift, element_bits)); }
    let immhb = element_bits - shift;
    let q = if is_high { 1u32 } else { 0 };
    let opcode_bits: u32 = if is_rounding { 0b100111 } else { 0b100101 };
    let word = (q << 30) | (u_bit << 29) | (0b011110 << 23) | ((immhb >> 3) << 19) | ((immhb & 7) << 16)
        | (opcode_bits << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON ADDHN/RADDHN/SUBHN/RSUBHN ──────────────────────────────────────
/// Three-different narrowing high: Format: 0 Q U 01110 size 1 Rm opcode 00 Rn Rd
fn encode_neon_three_diff_narrow(operands: &[Operand], u_bit: u32, opcode: u32, is_high: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 { return Err("addhn/subhn requires 3 operands".to_string()); }
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let size = match arr_n.as_str() { "8h" => 0b00u32, "4s" => 0b01, "2d" => 0b10,
        _ => return Err(format!("addhn: unsupported source: {}", arr_n)), };
    let q = if is_high { 1u32 } else { 0 };
    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size << 22) | (1 << 21)
        | (rm << 16) | (opcode << 12) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON LD2R/LD3R/LD4R ──────────────────────────────────────────────────
fn encode_neon_ldnr(operands: &[Operand], num_structs: u32) -> Result<EncodeResult, String> {
    if operands.len() < 2 { return Err(format!("ld{}r requires 2 operands", num_structs)); }
    let (rt, arr, num_regs) = match &operands[0] {
        Operand::RegList(regs) => {
            let (first_reg, arrangement) = match &regs[0] {
                Operand::RegArrangement { reg, arrangement } =>
                    (parse_reg_num(reg).ok_or("invalid reg")?, arrangement.clone()),
                _ => return Err("expected RegArrangement in list".to_string()),
            };
            (first_reg, arrangement, regs.len() as u32)
        }
        _ => return Err("expected register list".to_string()),
    };
    if num_regs != num_structs { return Err(format!("ld{}r: expected {} regs, got {}", num_structs, num_structs, num_regs)); }
    let (q, size) = match arr.as_str() {
        "8b" => (0u32, 0b00u32), "16b" => (1, 0b00),
        "4h" => (0, 0b01), "8h" => (1, 0b01),
        "2s" => (0, 0b10), "4s" => (1, 0b10),
        "1d" => (0, 0b11), "2d" => (1, 0b11),
        _ => return Err(format!("ld{}r: unsupported arrangement: {}", num_structs, arr)),
    };
    // opcode: ld1r=110, ld2r=110(S=1), ld3r=111, ld4r=111(S=1)
    let (opcode, s_bit) = match num_structs {
        1 => (0b110u32, 0u32),
        2 => (0b110, 1),
        3 => (0b111, 0),
        4 => (0b111, 1),
        _ => return Err(format!("unsupported: ld{}r", num_structs)),
    };
    let base = match &operands[1] {
        Operand::Mem { base, .. } => parse_reg_num(base).ok_or("invalid base")?,
        Operand::MemPostIndex { base, .. } => parse_reg_num(base).ok_or("invalid base")?,
        _ => return Err("expected memory operand".to_string()),
    };
    // check for post-index
    let rm = match &operands[1] {
        Operand::MemPostIndex { .. } => 0b11111u32, // immediate post-index
        _ => 0u32,
    };
    let has_post = rm != 0;
    let word = (q << 30) | (0b001101 << 24) | (if has_post { 1u32 } else { 0 } << 23)
        | (1 << 22) | (if has_post { rm } else { 0 } << 16) | (opcode << 13) | (s_bit << 12) | (size << 10) | (base << 5) | rt;
    Ok(EncodeResult::Word(word))
}

// ── NEON float compare-to-zero ───────────────────────────────────────────
/// FCMEQ/FCMLE/FCMLT/FCMGE/FCMGT to zero
/// Format: 0 Q U 01110 size 10000 opcode 10 Rn Rd (float, size = 0sz)
fn encode_neon_float_cmp_zero(operands: &[Operand], u_bit: u32, size_hi: u32, opcode: u32) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (q, sz) = match arr_d.as_str() {
        "2s" => (0u32, 0u32), "4s" => (1, 0), "2d" => (1, 1),
        _ => return Err(format!("float cmp zero: unsupported: {}", arr_d)),
    };
    let size = (size_hi << 1) | sz;
    let word = (q << 30) | (u_bit << 29) | (0b01110 << 24) | (size << 22)
        | (0b10000 << 17) | (opcode << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON by-element (non-long) ───────────────────────────────────────────
/// MUL/MLA/MLS by element: 0 Q U 01111 size L M Rm opcode H 0 Rn Rd
fn encode_neon_elem(operands: &[Operand], u_bit: u32, opcode: u32) -> Result<EncodeResult, String> {
    if operands.len() < 3 { return Err("NEON by-element requires 3 operands".to_string()); }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, index) = match &operands[2] {
        Operand::RegLane { reg, index, .. } => (parse_reg_num(reg).ok_or("invalid reg")?, *index),
        _ => return Err(format!("expected register lane, got {:?}", operands[2])),
    };
    let (q, size) = neon_arr_to_q_size(&arr_d)?;
    let (h, l, m_bit) = match size {
        0b01 => ((index >> 2) & 1, (index >> 1) & 1, index & 1),
        0b10 => ((index >> 1) & 1, index & 1, (rm >> 4) & 1),
        _ => return Err("unsupported element size for by-element".to_string()),
    };
    let rm_enc = if size == 0b01 { rm & 0xF } else { rm & 0x1F };
    let word = (q << 30) | (u_bit << 29) | (0b01111 << 24) | (size << 22)
        | (l << 21) | (m_bit << 20) | (rm_enc << 16) | (opcode << 12)
        | (h << 11) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON float by-element ────────────────────────────────────────────────
fn encode_neon_float_elem(operands: &[Operand], u_bit: u32, opcode: u32) -> Result<EncodeResult, String> {
    if operands.len() < 3 { return Err("NEON float by-element requires 3 operands".to_string()); }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, index) = match &operands[2] {
        Operand::RegLane { reg, index, .. } => (parse_reg_num(reg).ok_or("invalid reg")?, *index),
        _ => return Err(format!("expected register lane, got {:?}", operands[2])),
    };
    let (q, sz) = match arr_d.as_str() {
        "2s" => (0u32, 0u32), "4s" => (1, 0), "2d" => (1, 1),
        _ => return Err(format!("float by-element: unsupported: {}", arr_d)),
    };
    let (h, l, m_bit) = if sz == 0 {
        ((index >> 1) & 1, index & 1, (rm >> 4) & 1)
    } else {
        (index & 1, 0u32, (rm >> 4) & 1)
    };
    let rm_enc = rm & 0x1F;
    let word = (q << 30) | (u_bit << 29) | (0b01111 << 24) | (sz << 22)
        | (l << 21) | (m_bit << 20) | (rm_enc << 16) | (opcode << 12)
        | (h << 11) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON FCVTL/FCVTN ────────────────────────────────────────────────────
/// FCVTL: half→single or single→double widening float convert
/// Format: 0 Q 0 01110 0 sz 10000 10111 10 Rn Rd
fn encode_neon_fcvtl(operands: &[Operand], is_high: bool) -> Result<EncodeResult, String> {
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let sz = match arr_d.as_str() { "4s" | "2s" => 0u32, "2d" => 1,
        _ => return Err(format!("fcvtl: unsupported dest: {}", arr_d)), };
    let q = if is_high { 1u32 } else { 0 };
    let word = (q << 30) | (0b01110 << 24) | (sz << 22) | (0b10000 << 17)
        | (0b10111 << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

/// FCVTN: single→half or double→single narrowing float convert
fn encode_neon_fcvtn(operands: &[Operand], is_high: bool) -> Result<EncodeResult, String> {
    let (rd, _) = get_neon_reg(operands, 0)?;
    let (rn, arr_n) = get_neon_reg(operands, 1)?;
    let sz = match arr_n.as_str() { "4s" | "2s" => 0u32, "2d" => 1,
        _ => return Err(format!("fcvtn: unsupported source: {}", arr_n)), };
    let q = if is_high { 1u32 } else { 0 };
    let word = (q << 30) | (0b01110 << 24) | (sz << 22) | (0b10000 << 17)
        | (0b10110 << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── BIT/BIF (bitwise insert if true/false) ──────────────────────────────
/// Encodes BIT (size=10) and BIF (size=11) instructions.
/// Same format as BSL but with different size field.
/// Format: 0 Q 1 01110 ss 1 Rm 000111 Rn Rd
fn encode_neon_bitwise_insert(operands: &[Operand], size: u32) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("bit/bif requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let (rm, _) = get_neon_reg(operands, 2)?;
    let q: u32 = if arr_d == "16b" { 1 } else { 0 };
    let word = (q << 30) | (1 << 29) | (0b01110 << 24) | (size << 22) | (1 << 21)
        | (rm << 16) | (0b000111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── FADDP (float pairwise add) ──────────────────────────────────────────
/// FADDP — float add pairwise
/// Vector form: FADDP Vd.T, Vn.T, Vm.T
///   Format: 0 Q 1 01110 0 sz 1 Rm 110101 Rn Rd
/// Scalar form: FADDP Sd, Vn.2S  or FADDP Dd, Vn.2D
///   Format: 01 1 11110 0 sz 11000 01101 10 Rn Rd
fn encode_neon_faddp(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() >= 3 {
        // Vector form: 3 operands
        let (rd, arr_d) = get_neon_reg(operands, 0)?;
        let (rn, _) = get_neon_reg(operands, 1)?;
        let (rm, _) = get_neon_reg(operands, 2)?;
        let (q, sz) = match arr_d.as_str() {
            "2s" => (0u32, 0u32),
            "4s" => (1, 0),
            "2d" => (1, 1),
            _ => return Err(format!("faddp: unsupported arrangement: {}", arr_d)),
        };
        let word = (q << 30) | (1 << 29) | (0b01110 << 24) | (sz << 22) | (1 << 21)
            | (rm << 16) | (0b110101 << 10) | (rn << 5) | rd;
        Ok(EncodeResult::Word(word))
    } else if operands.len() == 2 {
        // Scalar form: FADDP Sd, Vn.2S or FADDP Dd, Vn.2D
        let rd = match &operands[0] {
            Operand::Reg(r) => parse_reg_num(r).ok_or("invalid dest reg")?,
            _ => return Err("faddp scalar: expected register".to_string()),
        };
        let (rn, arr_n) = get_neon_reg(operands, 1)?;
        let sz = match arr_n.as_str() {
            "2s" => 0u32,
            "2d" => 1,
            _ => return Err(format!("faddp scalar: unsupported source: {}", arr_n)),
        };
        // 01 1 11110 0 sz 11000 01101 10 Rn Rd
        let word = (0b01 << 30) | (1 << 29) | (0b11110 << 24) | (sz << 22)
            | (0b11000 << 17) | (0b01101 << 12) | (0b10 << 10) | (rn << 5) | rd;
        Ok(EncodeResult::Word(word))
    } else {
        Err("faddp requires 2 or 3 operands".to_string())
    }
}

// ── SADDLV/UADDLV (signed/unsigned add long across vector) ─────────────
/// Format: 0 Q U 01110 size 11000 00011 10 Rn Rd
fn encode_neon_across_long(operands: &[Operand], u: u32, opcode: u32) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("saddlv/uaddlv requires 2 operands".to_string());
    }
    // Destination is a scalar register (e.g., s16), source is a vector arrangement
    let rd = match &operands[0] {
        Operand::Reg(r) => parse_reg_num(r).ok_or("invalid dest reg")?,
        Operand::RegArrangement { reg, .. } => parse_reg_num(reg).ok_or("invalid dest reg")?,
        _ => return Err("saddlv: expected register".to_string()),
    };
    let (rn, arr_n) = get_neon_reg(operands, 1)?;
    let (q, size) = neon_arr_to_q_size(&arr_n)?;
    let word = (q << 30) | (u << 29) | (0b01110 << 24) | (size << 22)
        | (0b11000 << 17) | (opcode << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON shift left by immediate (SQSHL, UQSHL, SHL, etc.) ─────────────
/// Format: 0 Q U 011110 immh:immb opcode 1 Rn Rd
/// immh:immb encodes both the element size and the shift amount.
fn encode_neon_shift_left_imm(operands: &[Operand], u: u32, opcode: u32) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("shift left immediate requires 3 operands".to_string());
    }
    let (rd, arr_d) = get_neon_reg(operands, 0)?;
    let (rn, _) = get_neon_reg(operands, 1)?;
    let shift = get_imm(operands, 2)? as u32;

    let (q, _immh_base, esize) = match arr_d.as_str() {
        "8b" => (0u32, 0b0001u32, 8u32),
        "16b" => (1, 0b0001, 8),
        "4h" => (0, 0b0010, 16),
        "8h" => (1, 0b0010, 16),
        "2s" => (0, 0b0100, 32),
        "4s" => (1, 0b0100, 32),
        "2d" => (1, 0b1000, 64),
        _ => return Err(format!("shift left imm: unsupported arrangement: {}", arr_d)),
    };

    // immh:immb = esize + shift_amount
    // For 8-bit: immh=0001, shift in 0..7 => immh:immb = 8 + shift
    // For 16-bit: immh=001x, shift in 0..15 => immh:immb = 16 + shift
    // For 32-bit: immh=01xx, shift in 0..31 => immh:immb = 32 + shift
    // For 64-bit: immh=1xxx, shift in 0..63 => immh:immb = 64 + shift
    let immhb = esize + shift;
    let immh = (immhb >> 3) & 0xF;
    let immb = immhb & 0x7;

    let word = (q << 30) | (u << 29) | (0b011110 << 23) | (immh << 19) | (immb << 16)
        | (opcode << 11) | (1 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Helper: detect scalar d-register 3-operand NEON operations ──────────────
fn is_neon_scalar_d_reg_op(operands: &[Operand]) -> bool {
    if operands.len() < 3 { return false; }
    match &operands[0] {
        Operand::Reg(r) => {
            let r = r.to_lowercase();
            r.starts_with('d') && r[1..].parse::<u32>().is_ok()
        }
        _ => false,
    }
}

// ── NEON scalar three-same: ADD/SUB Dd, Dn, Dm ────────────────────────────
/// Encode scalar NEON three-same: 01 U 11110 size 1 Rm opcode 1 Rn Rd
fn encode_neon_scalar_three_same(operands: &[Operand], u_bit: u32, opcode: u32, size: u32) -> Result<EncodeResult, String> {
    if operands.len() < 3 { return Err("scalar three-same requires 3 operands".to_string()); }
    let rd = match &operands[0] { Operand::Reg(r) => parse_reg_num(r).ok_or("invalid reg")?, _ => return Err("expected register".to_string()) };
    let rn = match &operands[1] { Operand::Reg(r) => parse_reg_num(r).ok_or("invalid reg")?, _ => return Err("expected register".to_string()) };
    let rm = match &operands[2] { Operand::Reg(r) => parse_reg_num(r).ok_or("invalid reg")?, _ => return Err("expected register".to_string()) };
    let word = (0b01 << 30) | (u_bit << 29) | (0b11110 << 24) | (size << 22) | (1 << 21)
        | (rm << 16) | (opcode << 11) | (1 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON scalar ADDP: addp Dd, Vn.2d ──────────────────────────────────────
fn encode_neon_scalar_addp(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 { return Err("scalar addp requires 2 operands".to_string()); }
    let rd = match &operands[0] { Operand::Reg(r) => parse_reg_num(r).ok_or("invalid reg")?, _ => return Err("expected d register".to_string()) };
    let rn = match &operands[1] {
        Operand::RegArrangement { reg, arrangement } => {
            if arrangement != "2d" { return Err(format!("scalar addp requires .2d source, got .{}", arrangement)); }
            parse_reg_num(reg).ok_or("invalid reg")?
        }
        _ => return Err("scalar addp: expected Vn.2d source".to_string()),
    };
    // Scalar ADDP: 01 0 11110 11 11000 11011 10 Rn Rd
    let word = (0b01 << 30) | (0b011110 << 24) | (0b11 << 22) | (0b11000 << 17)
        | (0b11011 << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON scalar two-reg misc: SQABS/SQNEG Hd,Hn / Sd,Sn / Dd,Dn ──────────
fn encode_neon_scalar_two_misc(operands: &[Operand], u_bit: u32, opcode: u32) -> Result<EncodeResult, String> {
    if operands.len() < 2 { return Err("scalar two-misc requires 2 operands".to_string()); }
    let (rd, rd_name) = match &operands[0] { Operand::Reg(r) => (parse_reg_num(r).ok_or("invalid reg")?, r.to_lowercase()), _ => return Err("expected register".to_string()) };
    let rn = match &operands[1] { Operand::Reg(r) => parse_reg_num(r).ok_or("invalid reg")?, _ => return Err("expected register".to_string()) };
    let size = if rd_name.starts_with('b') { 0b00u32 }
        else if rd_name.starts_with('h') { 0b01 }
        else if rd_name.starts_with('s') { 0b10 }
        else if rd_name.starts_with('d') { 0b11 }
        else { return Err(format!("scalar two-misc: unsupported register type: {}", rd_name)); };
    // 01 U 11110 size 10000 opcode 10 Rn Rd
    let word = (0b01 << 30) | (u_bit << 29) | (0b11110 << 24) | (size << 22)
        | (0b10000 << 17) | (opcode << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON scalar SQSHRN: sqshrn Hd,Sn,#shift / sqshrn Sd,Dn,#shift ────────
fn encode_neon_scalar_qshrn(operands: &[Operand], u_bit: u32, is_rounding: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 { return Err("scalar qshrn requires 3 operands".to_string()); }
    let (rd, rd_name) = match &operands[0] { Operand::Reg(r) => (parse_reg_num(r).ok_or("invalid reg")?, r.to_lowercase()), _ => return Err("expected register".to_string()) };
    let rn = match &operands[1] { Operand::Reg(r) => parse_reg_num(r).ok_or("invalid reg")?, _ => return Err("expected register".to_string()) };
    let shift = get_imm(operands, 2)? as u32;
    // Determine element bits from destination register type
    let element_bits = if rd_name.starts_with('b') { 8u32 }  // b <- h (narrow from 16-bit)
        else if rd_name.starts_with('h') { 16 }  // h <- s (narrow from 32-bit), immh base = 16
        else if rd_name.starts_with('s') { 32 }  // s <- d (narrow from 64-bit), immh base = 32
        else { return Err(format!("scalar qshrn: unsupported dest: {}", rd_name)); };
    if shift == 0 || shift > element_bits { return Err(format!("scalar qshrn: shift {} out of range", shift)); }
    let immhb = (element_bits * 2) - shift;  // source element bits - shift
    let opcode_bits: u32 = if is_rounding { 0b100111 } else { 0b100101 };
    // 01 U 11110 immh:immb opcode 1 Rn Rd
    let word = (0b01 << 30) | (u_bit << 29) | (0b011110 << 23) | ((immhb >> 3) << 19) | ((immhb & 7) << 16)
        | (opcode_bits << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── NEON addp (integer pairwise add) — already handled in three-same as addp ──
