//! RISC-V instruction encoder.
//!
//! Encodes RISC-V instructions into 32-bit machine code words.
//! This covers the subset of instructions emitted by our codegen (RV64GC).
//!
//! RISC-V base instructions are always 4 bytes (32 bits), little-endian.
//! The encoding uses six main formats: R, I, S, B, U, J.

#![allow(dead_code)]

use super::parser::Operand;

/// Result of encoding an instruction.
#[derive(Debug, Clone)]
pub enum EncodeResult {
    /// Successfully encoded as a 4-byte instruction word
    Word(u32),
    /// Two 4-byte instruction words (e.g., pseudo-instructions like `call`, `li` with large imm)
    Words(Vec<u32>),
    /// Instruction needs a relocation to be applied later
    WordWithReloc {
        word: u32,
        reloc: Relocation,
    },
    /// Multiple words with relocations (e.g., `call` = auipc + jalr)
    WordsWithRelocs(Vec<(u32, Option<Relocation>)>),
    /// Skip this instruction (e.g., pseudo handled elsewhere)
    Skip,
}

/// RISC-V ELF relocation types
#[derive(Debug, Clone)]
pub enum RelocType {
    /// R_RISCV_CALL_PLT (combined auipc+jalr, 8 bytes)
    CallPlt,
    /// R_RISCV_PCREL_HI20 - for AUIPC (high 20 bits of PC-relative)
    PcrelHi20,
    /// R_RISCV_PCREL_LO12_I - for ADDI/LW/LD (low 12 bits of PC-relative, I-type)
    PcrelLo12I,
    /// R_RISCV_PCREL_LO12_S - for SW/SD (low 12 bits of PC-relative, S-type)
    PcrelLo12S,
    /// R_RISCV_HI20 - for LUI (absolute high 20 bits)
    Hi20,
    /// R_RISCV_LO12_I - for ADDI/LW/LD (absolute low 12 bits, I-type)
    Lo12I,
    /// R_RISCV_LO12_S - for SW/SD (absolute low 12 bits, S-type)
    Lo12S,
    /// R_RISCV_BRANCH - 12-bit PC-relative branch (B-type)
    Branch,
    /// R_RISCV_JAL - 20-bit PC-relative jump (J-type)
    Jal,
    /// R_RISCV_64 - 64-bit absolute
    Abs64,
    /// R_RISCV_32 - 32-bit absolute
    Abs32,
    /// R_RISCV_GOT_HI20 - GOT-relative AUIPC
    GotHi20,
    /// R_RISCV_TLS_GD_HI20
    TlsGdHi20,
    /// R_RISCV_TLS_GOT_HI20
    TlsGotHi20,
    /// R_RISCV_TPREL_HI20
    TprelHi20,
    /// R_RISCV_TPREL_LO12_I
    TprelLo12I,
    /// R_RISCV_TPREL_LO12_S
    TprelLo12S,
    /// R_RISCV_TPREL_ADD
    TprelAdd,
    /// R_RISCV_ADD32 - 32-bit addition (for symbol differences)
    Add32,
    /// R_RISCV_SUB32 - 32-bit subtraction (for symbol differences)
    Sub32,
    /// R_RISCV_ADD64 - 64-bit addition (for symbol differences)
    Add64,
    /// R_RISCV_SUB64 - 64-bit subtraction (for symbol differences)
    Sub64,
}

impl RelocType {
    /// Get the ELF relocation type number.
    pub fn elf_type(&self) -> u32 {
        match self {
            RelocType::Branch => 16,      // R_RISCV_BRANCH
            RelocType::Jal => 17,         // R_RISCV_JAL
            RelocType::CallPlt => 19,     // R_RISCV_CALL_PLT
            RelocType::GotHi20 => 20,     // R_RISCV_GOT_HI20
            RelocType::TlsGdHi20 => 22,   // R_RISCV_TLS_GD_HI20
            RelocType::TlsGotHi20 => 21,  // R_RISCV_TLS_GOT_HI20
            RelocType::PcrelHi20 => 23,   // R_RISCV_PCREL_HI20 = 23
            RelocType::PcrelLo12I => 24,  // R_RISCV_PCREL_LO12_I = 24
            RelocType::PcrelLo12S => 25,  // R_RISCV_PCREL_LO12_S = 25
            RelocType::Hi20 => 26,        // R_RISCV_HI20
            RelocType::Lo12I => 27,       // R_RISCV_LO12_I
            RelocType::Lo12S => 28,       // R_RISCV_LO12_S
            RelocType::TprelHi20 => 29,   // R_RISCV_TPREL_HI20
            RelocType::TprelLo12I => 30,  // R_RISCV_TPREL_LO12_I
            RelocType::TprelLo12S => 31,  // R_RISCV_TPREL_LO12_S
            RelocType::TprelAdd => 32,    // R_RISCV_TPREL_ADD
            RelocType::Abs32 => 1,        // R_RISCV_32
            RelocType::Abs64 => 2,        // R_RISCV_64
            RelocType::Add32 => 35,       // R_RISCV_ADD32
            RelocType::Sub32 => 39,       // R_RISCV_SUB32
            RelocType::Add64 => 36,       // R_RISCV_ADD64
            RelocType::Sub64 => 40,       // R_RISCV_SUB64
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

// ── Register encoding ──────────────────────────────────────────────────

/// Parse a register name to its 5-bit encoding number (0-31).
pub fn reg_num(name: &str) -> Option<u32> {
    let name = name.to_lowercase();
    match name.as_str() {
        // ABI names for integer registers
        "zero" => Some(0),
        "ra" => Some(1),
        "sp" => Some(2),
        "gp" => Some(3),
        "tp" => Some(4),
        "t0" => Some(5),
        "t1" => Some(6),
        "t2" => Some(7),
        "s0" | "fp" => Some(8),
        "s1" => Some(9),
        "a0" => Some(10),
        "a1" => Some(11),
        "a2" => Some(12),
        "a3" => Some(13),
        "a4" => Some(14),
        "a5" => Some(15),
        "a6" => Some(16),
        "a7" => Some(17),
        "s2" => Some(18),
        "s3" => Some(19),
        "s4" => Some(20),
        "s5" => Some(21),
        "s6" => Some(22),
        "s7" => Some(23),
        "s8" => Some(24),
        "s9" => Some(25),
        "s10" => Some(26),
        "s11" => Some(27),
        "t3" => Some(28),
        "t4" => Some(29),
        "t5" => Some(30),
        "t6" => Some(31),
        _ => {
            // x0-x31
            if name.starts_with('x') {
                let n: u32 = name[1..].parse().ok()?;
                if n <= 31 { Some(n) } else { None }
            } else {
                None
            }
        }
    }
}

/// Parse a floating-point register name to its 5-bit encoding (0-31).
pub fn freg_num(name: &str) -> Option<u32> {
    let name = name.to_lowercase();
    match name.as_str() {
        "ft0" => Some(0),
        "ft1" => Some(1),
        "ft2" => Some(2),
        "ft3" => Some(3),
        "ft4" => Some(4),
        "ft5" => Some(5),
        "ft6" => Some(6),
        "ft7" => Some(7),
        "fs0" => Some(8),
        "fs1" => Some(9),
        "fa0" => Some(10),
        "fa1" => Some(11),
        "fa2" => Some(12),
        "fa3" => Some(13),
        "fa4" => Some(14),
        "fa5" => Some(15),
        "fa6" => Some(16),
        "fa7" => Some(17),
        "fs2" => Some(18),
        "fs3" => Some(19),
        "fs4" => Some(20),
        "fs5" => Some(21),
        "fs6" => Some(22),
        "fs7" => Some(23),
        "fs8" => Some(24),
        "fs9" => Some(25),
        "fs10" => Some(26),
        "fs11" => Some(27),
        "ft8" => Some(28),
        "ft9" => Some(29),
        "ft10" => Some(30),
        "ft11" => Some(31),
        _ => {
            // f0-f31
            if name.starts_with('f') && !name.starts_with("ft")
                && !name.starts_with("fs") && !name.starts_with("fa")
            {
                let n: u32 = name[1..].parse().ok()?;
                if n <= 31 { Some(n) } else { None }
            } else {
                None
            }
        }
    }
}

/// Try integer register first, then float register
fn any_reg_num(name: &str) -> Option<u32> {
    reg_num(name).or_else(|| freg_num(name))
}

/// Check if a register name is an integer register
fn is_int_reg(name: &str) -> bool {
    reg_num(name).is_some()
}

/// Check if a register name is a floating-point register
fn is_fp_reg(name: &str) -> bool {
    freg_num(name).is_some()
}

// ── Instruction format encoders ──────────────────────────────────────

/// R-type: funct7[31:25] | rs2[24:20] | rs1[19:15] | funct3[14:12] | rd[11:7] | opcode[6:0]
fn encode_r(opcode: u32, rd: u32, funct3: u32, rs1: u32, rs2: u32, funct7: u32) -> u32 {
    (funct7 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | opcode
}

/// I-type: imm[31:20] | rs1[19:15] | funct3[14:12] | rd[11:7] | opcode[6:0]
fn encode_i(opcode: u32, rd: u32, funct3: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    (imm << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | opcode
}

/// S-type: imm[11:5] | rs2[24:20] | rs1[19:15] | funct3[14:12] | imm[4:0] | opcode[6:0]
fn encode_s(opcode: u32, funct3: u32, rs1: u32, rs2: u32, imm: i32) -> u32 {
    let imm = imm as u32;
    let imm_11_5 = (imm >> 5) & 0x7F;
    let imm_4_0 = imm & 0x1F;
    (imm_11_5 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | (imm_4_0 << 7) | opcode
}

/// B-type: imm[12|10:5] | rs2 | rs1 | funct3 | imm[4:1|11] | opcode
fn encode_b(opcode: u32, funct3: u32, rs1: u32, rs2: u32, imm: i32) -> u32 {
    let imm = imm as u32;
    let bit12 = (imm >> 12) & 1;
    let bit11 = (imm >> 11) & 1;
    let bits10_5 = (imm >> 5) & 0x3F;
    let bits4_1 = (imm >> 1) & 0xF;
    (bit12 << 31) | (bits10_5 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12)
        | (bits4_1 << 8) | (bit11 << 7) | opcode
}

/// U-type: imm[31:12] | rd[11:7] | opcode[6:0]
fn encode_u(opcode: u32, rd: u32, imm: u32) -> u32 {
    (imm & 0xFFFFF000) | (rd << 7) | opcode
}

/// J-type: imm[20|10:1|11|19:12] | rd[11:7] | opcode[6:0]
fn encode_j(opcode: u32, rd: u32, imm: i32) -> u32 {
    let imm = imm as u32;
    let bit20 = (imm >> 20) & 1;
    let bits10_1 = (imm >> 1) & 0x3FF;
    let bit11 = (imm >> 11) & 1;
    let bits19_12 = (imm >> 12) & 0xFF;
    (bit20 << 31) | (bits10_1 << 21) | (bit11 << 20) | (bits19_12 << 12) | (rd << 7) | opcode
}

// ── Opcode constants ──────────────────────────────────────────────────

const OP_LUI: u32 = 0b0110111;
const OP_AUIPC: u32 = 0b0010111;
const OP_JAL: u32 = 0b1101111;
const OP_JALR: u32 = 0b1100111;
const OP_BRANCH: u32 = 0b1100011;
const OP_LOAD: u32 = 0b0000011;
const OP_STORE: u32 = 0b0100011;
const OP_OP_IMM: u32 = 0b0010011;
const OP_OP: u32 = 0b0110011;
const OP_OP_IMM_32: u32 = 0b0011011;
const OP_OP_32: u32 = 0b0111011;
const OP_SYSTEM: u32 = 0b1110011;
const OP_MISC_MEM: u32 = 0b0001111;
const OP_AMO: u32 = 0b0101111;
const OP_LOAD_FP: u32 = 0b0000111;
const OP_STORE_FP: u32 = 0b0100111;
const OP_OP_FP: u32 = 0b1010011;
const OP_FMADD: u32 = 0b1000011;
const OP_FMSUB: u32 = 0b1000111;
const OP_FNMSUB: u32 = 0b1001011;
const OP_FNMADD: u32 = 0b1001111;

// ── Helper functions ──────────────────────────────────────────────────

fn get_reg(operands: &[Operand], idx: usize) -> Result<u32, String> {
    match operands.get(idx) {
        Some(Operand::Reg(name)) => {
            reg_num(name).ok_or_else(|| format!("invalid integer register: {}", name))
        }
        other => Err(format!("expected register at operand {}, got {:?}", idx, other)),
    }
}

fn get_freg(operands: &[Operand], idx: usize) -> Result<u32, String> {
    match operands.get(idx) {
        Some(Operand::Reg(name)) => {
            freg_num(name).ok_or_else(|| format!("invalid float register: {}", name))
        }
        other => Err(format!("expected float register at operand {}, got {:?}", idx, other)),
    }
}

fn get_any_reg(operands: &[Operand], idx: usize) -> Result<u32, String> {
    match operands.get(idx) {
        Some(Operand::Reg(name)) => {
            any_reg_num(name).ok_or_else(|| format!("invalid register: {}", name))
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
        other => Err(format!("expected symbol at operand {}, got {:?}", idx, other)),
    }
}

fn get_mem(operands: &[Operand], idx: usize) -> Result<(u32, i64), String> {
    match operands.get(idx) {
        Some(Operand::Mem { base, offset }) => {
            let base_reg = reg_num(base)
                .ok_or_else(|| format!("invalid base register: {}", base))?;
            Ok((base_reg, *offset))
        }
        other => Err(format!("expected memory operand at operand {}, got {:?}", idx, other)),
    }
}

/// Parse a fence ordering string (e.g., "iorw") into a 4-bit mask.
fn parse_fence_bits(s: &str) -> u32 {
    let s = s.to_lowercase();
    let mut bits = 0u32;
    if s.contains('i') { bits |= 8; }
    if s.contains('o') { bits |= 4; }
    if s.contains('r') { bits |= 2; }
    if s.contains('w') { bits |= 1; }
    bits
}

/// Parse a rounding mode to 3-bit encoding.
fn parse_rm(s: &str) -> u32 {
    match s.to_lowercase().as_str() {
        "rne" => 0b000,
        "rtz" => 0b001,
        "rdn" => 0b010,
        "rup" => 0b011,
        "rmm" => 0b100,
        "dyn" => 0b111,
        _ => 0b111, // default to dynamic
    }
}

// ── Main encode function ──────────────────────────────────────────────

/// Encode a RISC-V instruction from its mnemonic and parsed operands.
pub fn encode_instruction(mnemonic: &str, operands: &[Operand], raw_operands: &str) -> Result<EncodeResult, String> {
    let mn = mnemonic.to_lowercase();

    match mn.as_str() {
        // ── RV64I Base Instructions ──

        // U-type
        "lui" => encode_lui(operands),
        "auipc" => encode_auipc(operands),

        // J-type
        "jal" => encode_jal(operands),
        "jalr" => encode_jalr(operands),

        // B-type branches
        "beq" => encode_branch_instr(operands, 0b000),
        "bne" => encode_branch_instr(operands, 0b001),
        "blt" => encode_branch_instr(operands, 0b100),
        "bge" => encode_branch_instr(operands, 0b101),
        "bltu" => encode_branch_instr(operands, 0b110),
        "bgeu" => encode_branch_instr(operands, 0b111),

        // Loads (I-type)
        "lb" => encode_load(operands, 0b000),
        "lh" => encode_load(operands, 0b001),
        "lw" => encode_load(operands, 0b010),
        "ld" => encode_load(operands, 0b011),
        "lbu" => encode_load(operands, 0b100),
        "lhu" => encode_load(operands, 0b101),
        "lwu" => encode_load(operands, 0b110),

        // Stores (S-type)
        "sb" => encode_store(operands, 0b000),
        "sh" => encode_store(operands, 0b001),
        "sw" => encode_store(operands, 0b010),
        "sd" => encode_store(operands, 0b011),

        // Immediate arithmetic (I-type)
        "addi" => encode_alu_imm(operands, 0b000),
        "slti" => encode_alu_imm(operands, 0b010),
        "sltiu" => encode_alu_imm(operands, 0b011),
        "xori" => encode_alu_imm(operands, 0b100),
        "ori" => encode_alu_imm(operands, 0b110),
        "andi" => encode_alu_imm(operands, 0b111),

        // Shifts immediate
        "slli" => encode_shift_imm(operands, 0b001, 0b000000),
        "srli" => encode_shift_imm(operands, 0b101, 0b000000),
        "srai" => encode_shift_imm(operands, 0b101, 0b010000),

        // Register-register arithmetic (R-type)
        "add" => encode_alu_reg(operands, 0b000, 0b0000000),
        "sub" => encode_alu_reg(operands, 0b000, 0b0100000),
        "sll" => encode_alu_reg(operands, 0b001, 0b0000000),
        "slt" => encode_alu_reg(operands, 0b010, 0b0000000),
        "sltu" => encode_alu_reg(operands, 0b011, 0b0000000),
        "xor" => encode_alu_reg(operands, 0b100, 0b0000000),
        "srl" => encode_alu_reg(operands, 0b101, 0b0000000),
        "sra" => encode_alu_reg(operands, 0b101, 0b0100000),
        "or" => encode_alu_reg(operands, 0b110, 0b0000000),
        "and" => encode_alu_reg(operands, 0b111, 0b0000000),

        // RV64I word (32-bit) operations
        "addiw" => encode_alu_imm_w(operands, 0b000),
        "slliw" => encode_shift_imm_w(operands, 0b001, 0b0000000),
        "srliw" => encode_shift_imm_w(operands, 0b101, 0b0000000),
        "sraiw" => encode_shift_imm_w(operands, 0b101, 0b0100000),
        "addw" => encode_alu_reg_w(operands, 0b000, 0b0000000),
        "subw" => encode_alu_reg_w(operands, 0b000, 0b0100000),
        "sllw" => encode_alu_reg_w(operands, 0b001, 0b0000000),
        "srlw" => encode_alu_reg_w(operands, 0b101, 0b0000000),
        "sraw" => encode_alu_reg_w(operands, 0b101, 0b0100000),

        // ── M Extension (multiply/divide) ──
        "mul" => encode_alu_reg(operands, 0b000, 0b0000001),
        "mulh" => encode_alu_reg(operands, 0b001, 0b0000001),
        "mulhsu" => encode_alu_reg(operands, 0b010, 0b0000001),
        "mulhu" => encode_alu_reg(operands, 0b011, 0b0000001),
        "div" => encode_alu_reg(operands, 0b100, 0b0000001),
        "divu" => encode_alu_reg(operands, 0b101, 0b0000001),
        "rem" => encode_alu_reg(operands, 0b110, 0b0000001),
        "remu" => encode_alu_reg(operands, 0b111, 0b0000001),
        "mulw" => encode_alu_reg_w(operands, 0b000, 0b0000001),
        "divw" => encode_alu_reg_w(operands, 0b100, 0b0000001),
        "divuw" => encode_alu_reg_w(operands, 0b101, 0b0000001),
        "remw" => encode_alu_reg_w(operands, 0b110, 0b0000001),
        "remuw" => encode_alu_reg_w(operands, 0b111, 0b0000001),

        // ── A Extension (atomics) ──
        "lr.w" => encode_lr(operands, 0b010),
        "lr.d" => encode_lr(operands, 0b011),
        "sc.w" => encode_sc(operands, 0b010),
        "sc.d" => encode_sc(operands, 0b011),
        "amoswap.w" => encode_amo(operands, 0b010, 0b00001),
        "amoadd.w" => encode_amo(operands, 0b010, 0b00000),
        "amoxor.w" => encode_amo(operands, 0b010, 0b00100),
        "amoand.w" => encode_amo(operands, 0b010, 0b01100),
        "amoor.w" => encode_amo(operands, 0b010, 0b01000),
        "amomin.w" => encode_amo(operands, 0b010, 0b10000),
        "amomax.w" => encode_amo(operands, 0b010, 0b10100),
        "amominu.w" => encode_amo(operands, 0b010, 0b11000),
        "amomaxu.w" => encode_amo(operands, 0b010, 0b11100),
        "amoswap.d" => encode_amo(operands, 0b011, 0b00001),
        "amoadd.d" => encode_amo(operands, 0b011, 0b00000),
        "amoxor.d" => encode_amo(operands, 0b011, 0b00100),
        "amoand.d" => encode_amo(operands, 0b011, 0b01100),
        "amoor.d" => encode_amo(operands, 0b011, 0b01000),
        "amomin.d" => encode_amo(operands, 0b011, 0b10000),
        "amomax.d" => encode_amo(operands, 0b011, 0b10100),
        "amominu.d" => encode_amo(operands, 0b011, 0b11000),
        "amomaxu.d" => encode_amo(operands, 0b011, 0b11100),

        // Handle .aq, .rl, .aqrl suffixes for atomics
        s if s.starts_with("lr.") => encode_lr_suffixed(s, operands),
        s if s.starts_with("sc.") => encode_sc_suffixed(s, operands),
        s if s.starts_with("amo") => encode_amo_suffixed(s, operands),

        // ── System ──
        "ecall" => Ok(EncodeResult::Word(0x00000073)),
        "ebreak" => Ok(EncodeResult::Word(0x00100073)),
        "fence" => encode_fence(operands),
        "fence.i" => Ok(EncodeResult::Word(0x0000100F)),
        "fence.tso" => Ok(EncodeResult::Word(0x8330000F)),

        // ── Privileged instructions ──
        "wfi" => Ok(EncodeResult::Word(0x10500073)),
        "mret" => Ok(EncodeResult::Word(0x30200073)),
        "sret" => Ok(EncodeResult::Word(0x10200073)),
        "sfence.vma" => encode_sfence_vma(operands),

        "csrrw" => encode_csr(operands, 0b001),
        "csrrs" => encode_csr(operands, 0b010),
        "csrrc" => encode_csr(operands, 0b011),
        "csrrwi" => encode_csri(operands, 0b101),
        "csrrsi" => encode_csri(operands, 0b110),
        "csrrci" => encode_csri(operands, 0b111),

        // ── F Extension (single-precision float) ──
        "flw" => encode_float_load(operands, 0b010),
        "fsw" => encode_float_store(operands, 0b010),
        "fadd.s" => encode_fp_arith(operands, 0b0000000),
        "fsub.s" => encode_fp_arith(operands, 0b0000100),
        "fmul.s" => encode_fp_arith(operands, 0b0001000),
        "fdiv.s" => encode_fp_arith(operands, 0b0001100),
        "fsqrt.s" => encode_fp_unary(operands, 0b0101100, 0b00000),
        "fsgnj.s" => encode_fp_sgnj(operands, 0b0010000, 0b000),
        "fsgnjn.s" => encode_fp_sgnj(operands, 0b0010000, 0b001),
        "fsgnjx.s" => encode_fp_sgnj(operands, 0b0010000, 0b010),
        "fmin.s" => encode_fp_sgnj(operands, 0b0010100, 0b000),
        "fmax.s" => encode_fp_sgnj(operands, 0b0010100, 0b001),
        "feq.s" => encode_fp_cmp(operands, 0b1010000, 0b010),
        "flt.s" => encode_fp_cmp(operands, 0b1010000, 0b001),
        "fle.s" => encode_fp_cmp(operands, 0b1010000, 0b000),
        "fclass.s" => encode_fclass(operands, 0b1110000),
        "fcvt.w.s" => encode_fcvt_int(operands, 0b1100000, 0b00000),
        "fcvt.wu.s" => encode_fcvt_int(operands, 0b1100000, 0b00001),
        "fcvt.l.s" => encode_fcvt_int(operands, 0b1100000, 0b00010),
        "fcvt.lu.s" => encode_fcvt_int(operands, 0b1100000, 0b00011),
        "fcvt.s.w" => encode_fcvt_from_int(operands, 0b1101000, 0b00000),
        "fcvt.s.wu" => encode_fcvt_from_int(operands, 0b1101000, 0b00001),
        "fcvt.s.l" => encode_fcvt_from_int(operands, 0b1101000, 0b00010),
        "fcvt.s.lu" => encode_fcvt_from_int(operands, 0b1101000, 0b00011),
        "fmv.x.w" => encode_fmv_x_f(operands, 0b1110000, 0b00),
        "fmv.w.x" => encode_fmv_f_x(operands, 0b1111000, 0b00),

        // ── D Extension (double-precision float) ──
        "fld" => encode_float_load(operands, 0b011),
        "fsd" => encode_float_store(operands, 0b011),
        "fadd.d" => encode_fp_arith_d(operands, 0b0000001),
        "fsub.d" => encode_fp_arith_d(operands, 0b0000101),
        "fmul.d" => encode_fp_arith_d(operands, 0b0001001),
        "fdiv.d" => encode_fp_arith_d(operands, 0b0001101),
        "fsqrt.d" => encode_fp_unary(operands, 0b0101101, 0b00000),
        "fsgnj.d" => encode_fp_sgnj(operands, 0b0010001, 0b000),
        "fsgnjn.d" => encode_fp_sgnj(operands, 0b0010001, 0b001),
        "fsgnjx.d" => encode_fp_sgnj(operands, 0b0010001, 0b010),
        "fmin.d" => encode_fp_sgnj(operands, 0b0010101, 0b000),
        "fmax.d" => encode_fp_sgnj(operands, 0b0010101, 0b001),
        "feq.d" => encode_fp_cmp(operands, 0b1010001, 0b010),
        "flt.d" => encode_fp_cmp(operands, 0b1010001, 0b001),
        "fle.d" => encode_fp_cmp(operands, 0b1010001, 0b000),
        "fclass.d" => encode_fclass(operands, 0b1110001),
        "fcvt.w.d" => encode_fcvt_int(operands, 0b1100001, 0b00000),
        "fcvt.wu.d" => encode_fcvt_int(operands, 0b1100001, 0b00001),
        "fcvt.l.d" => encode_fcvt_int(operands, 0b1100001, 0b00010),
        "fcvt.lu.d" => encode_fcvt_int(operands, 0b1100001, 0b00011),
        "fcvt.d.w" => encode_fcvt_from_int(operands, 0b1101001, 0b00000),
        "fcvt.d.wu" => encode_fcvt_from_int(operands, 0b1101001, 0b00001),
        "fcvt.d.l" => encode_fcvt_from_int(operands, 0b1101001, 0b00010),
        "fcvt.d.lu" => encode_fcvt_from_int(operands, 0b1101001, 0b00011),
        "fcvt.s.d" => encode_fcvt_fp(operands, 0b0100000, 0b00001),
        "fcvt.d.s" => encode_fcvt_fp(operands, 0b0100001, 0b00000),
        "fmv.x.d" => encode_fmv_x_f(operands, 0b1110001, 0b00),
        "fmv.d.x" => encode_fmv_f_x(operands, 0b1111001, 0b00),

        // ── Fused multiply-add ──
        "fmadd.s" => encode_fma(operands, OP_FMADD, 0b00),
        "fmsub.s" => encode_fma(operands, OP_FMSUB, 0b00),
        "fnmsub.s" => encode_fma(operands, OP_FNMSUB, 0b00),
        "fnmadd.s" => encode_fma(operands, OP_FNMADD, 0b00),
        "fmadd.d" => encode_fma(operands, OP_FMADD, 0b01),
        "fmsub.d" => encode_fma(operands, OP_FMSUB, 0b01),
        "fnmsub.d" => encode_fma(operands, OP_FNMSUB, 0b01),
        "fnmadd.d" => encode_fma(operands, OP_FNMADD, 0b01),

        // ── Pseudo-instructions ──
        "nop" => Ok(EncodeResult::Word(encode_i(OP_OP_IMM, 0, 0, 0, 0))), // addi x0, x0, 0
        "li" => encode_li(operands),
        "mv" => encode_mv(operands),
        "not" => encode_not(operands),
        "neg" => encode_neg(operands),
        "negw" => encode_negw(operands),
        "sext.w" => encode_sext_w(operands),
        "seqz" => encode_seqz(operands),
        "snez" => encode_snez(operands),
        "sltz" => encode_sltz(operands),
        "sgtz" => encode_sgtz(operands),

        // Branch pseudo-instructions
        "beqz" => encode_beqz(operands),
        "bnez" => encode_bnez(operands),
        "blez" => encode_blez(operands),
        "bgez" => encode_bgez(operands),
        "bltz" => encode_bltz(operands),
        "bgtz" => encode_bgtz(operands),
        "bgt" => encode_bgt(operands),
        "ble" => encode_ble(operands),
        "bgtu" => encode_bgtu(operands),
        "bleu" => encode_bleu(operands),

        // Jump pseudo-instructions
        "j" => encode_j_pseudo(operands),
        "jr" => encode_jr(operands),
        "ret" => Ok(EncodeResult::Word(encode_i(OP_JALR, 0, 0, 1, 0))), // jalr x0, x1, 0
        "call" => encode_call(operands),
        "tail" => encode_tail(operands),

        // Address pseudo-instructions
        "la" => encode_la(operands),
        "lla" => encode_lla(operands),

        // CSR pseudo-instructions
        "rdcycle" | "rdtime" | "rdinstret" => encode_rdcsr(mnemonic, operands),
        "csrr" => encode_csrr(operands),
        "csrw" => encode_csrw(operands),
        "csrs" => encode_csrs(operands),
        "csrc" => encode_csrc(operands),

        // Misc pseudo-instructions
        "fmv.s" => encode_fmv_s(operands),
        "fmv.d" => encode_fmv_d(operands),
        "fabs.s" => encode_fabs_s(operands),
        "fabs.d" => encode_fabs_d(operands),
        "fneg.s" => encode_fneg_s(operands),
        "fneg.d" => encode_fneg_d(operands),

        // `jump` pseudo-instruction (our codegen emits this)
        "jump" => encode_jump(operands),

        _ => {
            Err(format!("unsupported instruction: {} {}", mnemonic, raw_operands))
        }
    }
}

// ── Instruction encoders ──────────────────────────────────────────────

fn encode_lui(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    match &operands.get(1) {
        Some(Operand::Imm(imm)) => {
            Ok(EncodeResult::Word(encode_u(OP_LUI, rd, (*imm as u32) << 12)))
        }
        Some(Operand::Symbol(s)) => {
            // %hi(symbol)
            Ok(EncodeResult::WordWithReloc {
                word: encode_u(OP_LUI, rd, 0),
                reloc: Relocation {
                    reloc_type: if s.starts_with("%tprel_hi(") {
                        RelocType::TprelHi20
                    } else {
                        RelocType::Hi20
                    },
                    symbol: extract_modifier_symbol(s),
                    addend: 0,
                },
            })
        }
        _ => Err(format!("lui: invalid operands")),
    }
}

fn encode_auipc(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    match &operands.get(1) {
        Some(Operand::Imm(imm)) => {
            Ok(EncodeResult::Word(encode_u(OP_AUIPC, rd, (*imm as u32) << 12)))
        }
        Some(Operand::Symbol(s)) => {
            let (reloc_type, symbol) = parse_reloc_modifier(s);
            Ok(EncodeResult::WordWithReloc {
                word: encode_u(OP_AUIPC, rd, 0),
                reloc: Relocation {
                    reloc_type,
                    symbol,
                    addend: 0,
                },
            })
        }
        _ => Err(format!("auipc: invalid operands")),
    }
}

fn encode_jal(operands: &[Operand]) -> Result<EncodeResult, String> {
    // jal rd, offset  OR  jal offset (rd = ra)
    if operands.len() == 1 {
        // jal offset (implicit rd = ra)
        match &operands[0] {
            Operand::Imm(imm) => {
                Ok(EncodeResult::Word(encode_j(OP_JAL, 1, *imm as i32)))
            }
            Operand::Symbol(s) | Operand::Label(s) => {
                Ok(EncodeResult::WordWithReloc {
                    word: encode_j(OP_JAL, 1, 0),
                    reloc: Relocation {
                        reloc_type: RelocType::Jal,
                        symbol: s.clone(),
                        addend: 0,
                    },
                })
            }
            _ => Err("jal: invalid operand".to_string()),
        }
    } else {
        let rd = get_reg(operands, 0)?;
        match &operands[1] {
            Operand::Imm(imm) => {
                Ok(EncodeResult::Word(encode_j(OP_JAL, rd, *imm as i32)))
            }
            Operand::Symbol(s) | Operand::Label(s) => {
                Ok(EncodeResult::WordWithReloc {
                    word: encode_j(OP_JAL, rd, 0),
                    reloc: Relocation {
                        reloc_type: RelocType::Jal,
                        symbol: s.clone(),
                        addend: 0,
                    },
                })
            }
            _ => Err("jal: invalid operand".to_string()),
        }
    }
}

fn encode_jalr(operands: &[Operand]) -> Result<EncodeResult, String> {
    // jalr rd, rs1, offset  OR  jalr rd, offset(rs1)  OR  jalr rs1
    match operands.len() {
        1 => {
            // jalr rs1 (rd = ra, offset = 0)
            let rs1 = get_reg(operands, 0)?;
            Ok(EncodeResult::Word(encode_i(OP_JALR, 1, 0, rs1, 0)))
        }
        2 => {
            // jalr rd, rs1  (offset = 0)
            let rd = get_reg(operands, 0)?;
            match &operands[1] {
                Operand::Reg(name) => {
                    let rs1 = reg_num(name).ok_or("invalid register")?;
                    Ok(EncodeResult::Word(encode_i(OP_JALR, rd, 0, rs1, 0)))
                }
                Operand::Mem { base, offset } => {
                    let rs1 = reg_num(base).ok_or("invalid base register")?;
                    Ok(EncodeResult::Word(encode_i(OP_JALR, rd, 0, rs1, *offset as i32)))
                }
                _ => Err("jalr: invalid operands".to_string()),
            }
        }
        3 => {
            let rd = get_reg(operands, 0)?;
            let rs1 = get_reg(operands, 1)?;
            let imm = get_imm(operands, 2)?;
            Ok(EncodeResult::Word(encode_i(OP_JALR, rd, 0, rs1, imm as i32)))
        }
        _ => Err("jalr: wrong number of operands".to_string()),
    }
}

fn encode_branch_instr(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rs1 = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;

    match &operands.get(2) {
        Some(Operand::Imm(imm)) => {
            Ok(EncodeResult::Word(encode_b(OP_BRANCH, funct3, rs1, rs2, *imm as i32)))
        }
        Some(Operand::Symbol(s)) | Some(Operand::Label(s)) => {
            Ok(EncodeResult::WordWithReloc {
                word: encode_b(OP_BRANCH, funct3, rs1, rs2, 0),
                reloc: Relocation {
                    reloc_type: RelocType::Branch,
                    symbol: s.clone(),
                    addend: 0,
                },
            })
        }
        _ => Err(format!("branch: expected offset or label as 3rd operand")),
    }
}

fn encode_load(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    match &operands.get(1) {
        Some(Operand::Mem { base, offset }) => {
            let rs1 = reg_num(base).ok_or("invalid base register")?;
            Ok(EncodeResult::Word(encode_i(OP_LOAD, rd, funct3, rs1, *offset as i32)))
        }
        Some(Operand::MemSymbol { base, symbol, .. }) => {
            let rs1 = reg_num(base).ok_or("invalid base register")?;
            let (reloc_type, sym) = parse_reloc_modifier(symbol);
            // Use Lo12I for load-type relocations
            let reloc_type = match reloc_type {
                RelocType::PcrelHi20 => RelocType::PcrelLo12I,
                RelocType::Hi20 => RelocType::Lo12I,
                RelocType::TprelHi20 => RelocType::TprelLo12I,
                other => other,
            };
            Ok(EncodeResult::WordWithReloc {
                word: encode_i(OP_LOAD, rd, funct3, rs1, 0),
                reloc: Relocation {
                    reloc_type,
                    symbol: sym,
                    addend: 0,
                },
            })
        }
        _ => Err("load: expected memory operand".to_string()),
    }
}

fn encode_store(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rs2 = get_reg(operands, 0)?;
    match &operands.get(1) {
        Some(Operand::Mem { base, offset }) => {
            let rs1 = reg_num(base).ok_or("invalid base register")?;
            Ok(EncodeResult::Word(encode_s(OP_STORE, funct3, rs1, rs2, *offset as i32)))
        }
        Some(Operand::MemSymbol { base, symbol, .. }) => {
            let rs1 = reg_num(base).ok_or("invalid base register")?;
            let (reloc_type, sym) = parse_reloc_modifier(symbol);
            let reloc_type = match reloc_type {
                RelocType::PcrelHi20 => RelocType::PcrelLo12S,
                RelocType::Hi20 => RelocType::Lo12S,
                RelocType::TprelHi20 => RelocType::TprelLo12S,
                other => other,
            };
            Ok(EncodeResult::WordWithReloc {
                word: encode_s(OP_STORE, funct3, rs1, rs2, 0),
                reloc: Relocation {
                    reloc_type,
                    symbol: sym,
                    addend: 0,
                },
            })
        }
        _ => Err("store: expected memory operand".to_string()),
    }
}

fn encode_alu_imm(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    match &operands.get(2) {
        Some(Operand::Imm(imm)) => {
            Ok(EncodeResult::Word(encode_i(OP_OP_IMM, rd, funct3, rs1, *imm as i32)))
        }
        Some(Operand::Symbol(s)) => {
            let (reloc_type, sym) = parse_reloc_modifier(s);
            let reloc_type = match reloc_type {
                RelocType::PcrelHi20 => RelocType::PcrelLo12I,
                RelocType::Hi20 => RelocType::Lo12I,
                RelocType::TprelHi20 => RelocType::TprelLo12I,
                other => other,
            };
            Ok(EncodeResult::WordWithReloc {
                word: encode_i(OP_OP_IMM, rd, funct3, rs1, 0),
                reloc: Relocation {
                    reloc_type,
                    symbol: sym,
                    addend: 0,
                },
            })
        }
        _ => Err("alu_imm: expected immediate".to_string()),
    }
}

fn encode_shift_imm(operands: &[Operand], funct3: u32, funct6: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    let shamt = get_imm(operands, 2)? as u32;
    // For RV64, shift amount is 6 bits
    let imm = (funct6 << 6) | (shamt & 0x3F);
    Ok(EncodeResult::Word(encode_i(OP_OP_IMM, rd, funct3, rs1, imm as i32)))
}

fn encode_alu_reg(operands: &[Operand], funct3: u32, funct7: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    let rs2 = get_reg(operands, 2)?;
    Ok(EncodeResult::Word(encode_r(OP_OP, rd, funct3, rs1, rs2, funct7)))
}

fn encode_alu_imm_w(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    let imm = get_imm(operands, 2)? as i32;
    Ok(EncodeResult::Word(encode_i(OP_OP_IMM_32, rd, funct3, rs1, imm)))
}

fn encode_shift_imm_w(operands: &[Operand], funct3: u32, funct7: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    let shamt = get_imm(operands, 2)? as u32;
    // For RV32/W operations, shift amount is 5 bits
    let imm = (funct7 << 5) | (shamt & 0x1F);
    Ok(EncodeResult::Word(encode_i(OP_OP_IMM_32, rd, funct3, rs1, imm as i32)))
}

fn encode_alu_reg_w(operands: &[Operand], funct3: u32, funct7: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    let rs2 = get_reg(operands, 2)?;
    Ok(EncodeResult::Word(encode_r(OP_OP_32, rd, funct3, rs1, rs2, funct7)))
}

// ── Atomics ──

fn encode_lr(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let (rs1, _offset) = get_mem(operands, 1)?;
    // LR: funct7 = 00010 | aq | rl, rs2 = 0
    let funct7 = 0b0001000; // aq=0, rl=0 by default
    Ok(EncodeResult::Word(encode_r(OP_AMO, rd, funct3, rs1, 0, funct7)))
}

fn encode_sc(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    let (rs1, _offset) = get_mem(operands, 2)?;
    let funct7 = 0b0001100; // SC: 00011 | aq=0 | rl=0
    Ok(EncodeResult::Word(encode_r(OP_AMO, rd, funct3, rs1, rs2, funct7)))
}

fn encode_amo(operands: &[Operand], funct3: u32, funct5: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    let (rs1, _offset) = get_mem(operands, 2)?;
    let funct7 = funct5 << 2; // aq=0, rl=0
    Ok(EncodeResult::Word(encode_r(OP_AMO, rd, funct3, rs1, rs2, funct7)))
}

fn encode_lr_suffixed(mnemonic: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    // Parse lr.w, lr.d, lr.w.aq, lr.w.rl, lr.w.aqrl, etc.
    let parts: Vec<&str> = mnemonic.split('.').collect();
    let funct3 = match parts.get(1).map(|s| *s) {
        Some("w") => 0b010,
        Some("d") => 0b011,
        _ => return Err(format!("lr: invalid width: {}", mnemonic)),
    };
    let (aq, rl) = parse_aq_rl(&parts[2..]);
    let rd = get_reg(operands, 0)?;
    let (rs1, _) = get_mem(operands, 1)?;
    let funct7 = (0b00010 << 2) | (aq << 1) | rl;
    Ok(EncodeResult::Word(encode_r(OP_AMO, rd, funct3, rs1, 0, funct7)))
}

fn encode_sc_suffixed(mnemonic: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    let parts: Vec<&str> = mnemonic.split('.').collect();
    let funct3 = match parts.get(1).map(|s| *s) {
        Some("w") => 0b010,
        Some("d") => 0b011,
        _ => return Err(format!("sc: invalid width: {}", mnemonic)),
    };
    let (aq, rl) = parse_aq_rl(&parts[2..]);
    let rd = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    let (rs1, _) = get_mem(operands, 2)?;
    let funct7 = (0b00011 << 2) | (aq << 1) | rl;
    Ok(EncodeResult::Word(encode_r(OP_AMO, rd, funct3, rs1, rs2, funct7)))
}

fn encode_amo_suffixed(mnemonic: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    // Parse e.g. amoswap.w.aqrl, amoadd.d.aq, etc.
    let parts: Vec<&str> = mnemonic.split('.').collect();
    if parts.len() < 2 {
        return Err(format!("amo: invalid mnemonic: {}", mnemonic));
    }

    let op_name = parts[0]; // e.g., "amoswap", "amoadd"
    let funct3 = match parts.get(1).map(|s| *s) {
        Some("w") => 0b010,
        Some("d") => 0b011,
        _ => return Err(format!("amo: invalid width in {}", mnemonic)),
    };
    let (aq, rl) = parse_aq_rl(&parts[2..]);

    let funct5 = match op_name {
        "amoswap" => 0b00001,
        "amoadd" => 0b00000,
        "amoxor" => 0b00100,
        "amoand" => 0b01100,
        "amoor" => 0b01000,
        "amomin" => 0b10000,
        "amomax" => 0b10100,
        "amominu" => 0b11000,
        "amomaxu" => 0b11100,
        _ => return Err(format!("amo: unknown op: {}", op_name)),
    };

    let rd = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    let (rs1, _) = get_mem(operands, 2)?;
    let funct7 = (funct5 << 2) | (aq << 1) | rl;
    Ok(EncodeResult::Word(encode_r(OP_AMO, rd, funct3, rs1, rs2, funct7)))
}

fn parse_aq_rl(suffixes: &[&str]) -> (u32, u32) {
    let mut aq = 0u32;
    let mut rl = 0u32;
    for s in suffixes {
        match *s {
            "aq" => aq = 1,
            "rl" => rl = 1,
            "aqrl" => { aq = 1; rl = 1; }
            _ => {}
        }
    }
    (aq, rl)
}

// ── Fence ──

fn encode_fence(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (pred, succ) = if operands.is_empty() {
        (0xF, 0xF) // fence iorw, iorw
    } else if operands.len() >= 2 {
        let pred = match &operands[0] {
            Operand::FenceArg(s) => parse_fence_bits(s),
            _ => 0xF,
        };
        let succ = match &operands[1] {
            Operand::FenceArg(s) => parse_fence_bits(s),
            _ => 0xF,
        };
        (pred, succ)
    } else {
        (0xF, 0xF)
    };
    let imm = ((pred << 4) | succ) as i32;
    Ok(EncodeResult::Word(encode_i(OP_MISC_MEM, 0, 0, 0, imm)))
}

/// Encode sfence.vma rs1, rs2
/// Format: funct7=0001001 | rs2 | rs1 | funct3=000 | rd=00000 | opcode=1110011
/// If no operands: sfence.vma zero, zero
/// If 1 operand: sfence.vma rs1, zero
/// If 2 operands: sfence.vma rs1, rs2
fn encode_sfence_vma(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs1 = if operands.is_empty() { 0 } else { get_reg(operands, 0)? };
    let rs2 = if operands.len() < 2 { 0 } else { get_reg(operands, 1)? };
    // sfence.vma is encoded as: funct7=0001001(0x09) | rs2 | rs1 | 000 | 00000 | SYSTEM(1110011)
    let word = encode_r(OP_SYSTEM, 0, 0b000, rs1, rs2, 0b0001001);
    Ok(EncodeResult::Word(word))
}

// ── CSR ──

fn encode_csr(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let csr = get_csr_num(operands, 1)?;
    let rs1 = get_reg(operands, 2)?;
    Ok(EncodeResult::Word(encode_i(OP_SYSTEM, rd, funct3, rs1, csr as i32)))
}

fn encode_csri(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let csr = get_csr_num(operands, 1)?;
    let zimm = get_imm(operands, 2)? as u32;
    let rs1 = zimm & 0x1F;
    Ok(EncodeResult::Word(encode_i(OP_SYSTEM, rd, funct3, rs1, csr as i32)))
}

fn get_csr_num(operands: &[Operand], idx: usize) -> Result<u32, String> {
    match operands.get(idx) {
        Some(Operand::Imm(v)) => Ok(*v as u32),
        Some(Operand::Csr(name)) => csr_name_to_num(name),
        Some(Operand::Symbol(name)) => csr_name_to_num(name),
        Some(Operand::Reg(name)) => csr_name_to_num(name), // sometimes CSR names look like regs
        other => Err(format!("expected CSR at operand {}, got {:?}", idx, other)),
    }
}

fn csr_name_to_num(name: &str) -> Result<u32, String> {
    match name.to_lowercase().as_str() {
        "fflags" => Ok(0x001),
        "frm" => Ok(0x002),
        "fcsr" => Ok(0x003),
        "cycle" => Ok(0xC00),
        "time" => Ok(0xC01),
        "instret" => Ok(0xC02),
        "cycleh" => Ok(0xC80),
        "timeh" => Ok(0xC81),
        "instreth" => Ok(0xC82),
        "mstatus" => Ok(0x300),
        "misa" => Ok(0x301),
        "mie" => Ok(0x304),
        "mtvec" => Ok(0x305),
        "mscratch" => Ok(0x340),
        "mepc" => Ok(0x341),
        "mcause" => Ok(0x342),
        "mtval" => Ok(0x343),
        "mip" => Ok(0x344),
        "sstatus" => Ok(0x100),
        "sip" => Ok(0x144),
        "sie" => Ok(0x104),
        "stvec" => Ok(0x105),
        "sscratch" => Ok(0x140),
        "sepc" => Ok(0x141),
        "scause" => Ok(0x142),
        "stval" => Ok(0x143),
        "satp" => Ok(0x180),
        _ => {
            // Try parsing as a number
            if let Ok(v) = name.parse::<u32>() {
                Ok(v)
            } else if name.starts_with("0x") {
                u32::from_str_radix(&name[2..], 16)
                    .map_err(|_| format!("invalid CSR: {}", name))
            } else {
                Err(format!("unknown CSR: {}", name))
            }
        }
    }
}

// ── Floating-point instructions ──

fn encode_float_load(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    match &operands.get(1) {
        Some(Operand::Mem { base, offset }) => {
            let rs1 = reg_num(base).ok_or("invalid base register")?;
            Ok(EncodeResult::Word(encode_i(OP_LOAD_FP, rd, funct3, rs1, *offset as i32)))
        }
        Some(Operand::MemSymbol { base, symbol, .. }) => {
            let rs1 = reg_num(base).ok_or("invalid base register")?;
            let (reloc_type, sym) = parse_reloc_modifier(symbol);
            let reloc_type = match reloc_type {
                RelocType::PcrelHi20 => RelocType::PcrelLo12I,
                RelocType::Hi20 => RelocType::Lo12I,
                other => other,
            };
            Ok(EncodeResult::WordWithReloc {
                word: encode_i(OP_LOAD_FP, rd, funct3, rs1, 0),
                reloc: Relocation {
                    reloc_type,
                    symbol: sym,
                    addend: 0,
                },
            })
        }
        _ => Err("float load: expected memory operand".to_string()),
    }
}

fn encode_float_store(operands: &[Operand], funct3: u32) -> Result<EncodeResult, String> {
    let rs2 = get_freg(operands, 0)?;
    match &operands.get(1) {
        Some(Operand::Mem { base, offset }) => {
            let rs1 = reg_num(base).ok_or("invalid base register")?;
            Ok(EncodeResult::Word(encode_s(OP_STORE_FP, funct3, rs1, rs2, *offset as i32)))
        }
        Some(Operand::MemSymbol { base, symbol, .. }) => {
            let rs1 = reg_num(base).ok_or("invalid base register")?;
            let (reloc_type, sym) = parse_reloc_modifier(symbol);
            let reloc_type = match reloc_type {
                RelocType::PcrelHi20 => RelocType::PcrelLo12S,
                RelocType::Hi20 => RelocType::Lo12S,
                other => other,
            };
            Ok(EncodeResult::WordWithReloc {
                word: encode_s(OP_STORE_FP, funct3, rs1, rs2, 0),
                reloc: Relocation {
                    reloc_type,
                    symbol: sym,
                    addend: 0,
                },
            })
        }
        _ => Err("float store: expected memory operand".to_string()),
    }
}

fn encode_fp_arith(operands: &[Operand], funct7: u32) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    let rs2 = get_freg(operands, 2)?;
    // Check for optional rounding mode
    let rm = if operands.len() > 3 {
        match &operands[3] {
            Operand::RoundingMode(s) => parse_rm(s),
            _ => 0b111, // dynamic
        }
    } else {
        0b111
    };
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, rm, rs1, rs2, funct7)))
}

fn encode_fp_arith_d(operands: &[Operand], funct7: u32) -> Result<EncodeResult, String> {
    encode_fp_arith(operands, funct7)
}

fn encode_fp_unary(operands: &[Operand], funct7: u32, rs2: u32) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    let rm = if operands.len() > 2 {
        match &operands[2] {
            Operand::RoundingMode(s) => parse_rm(s),
            _ => 0b111,
        }
    } else {
        0b111
    };
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, rm, rs1, rs2, funct7)))
}

fn encode_fp_sgnj(operands: &[Operand], funct7: u32, funct3: u32) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    let rs2 = get_freg(operands, 2)?;
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, funct3, rs1, rs2, funct7)))
}

fn encode_fp_cmp(operands: &[Operand], funct7: u32, funct3: u32) -> Result<EncodeResult, String> {
    // Result goes to integer register
    let rd = get_reg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    let rs2 = get_freg(operands, 2)?;
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, funct3, rs1, rs2, funct7)))
}

fn encode_fclass(operands: &[Operand], funct7: u32) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, 0b001, rs1, 0, funct7)))
}

fn encode_fcvt_int(operands: &[Operand], funct7: u32, rs2: u32) -> Result<EncodeResult, String> {
    // Float to integer: result in integer register, source in float register
    let rd = get_reg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    let rm = if operands.len() > 2 {
        match &operands[2] {
            Operand::RoundingMode(s) => parse_rm(s),
            _ => 0b111,
        }
    } else {
        0b111
    };
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, rm, rs1, rs2, funct7)))
}

fn encode_fcvt_from_int(operands: &[Operand], funct7: u32, rs2: u32) -> Result<EncodeResult, String> {
    // Integer to float: result in float register, source in integer register
    let rd = get_freg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    let rm = if operands.len() > 2 {
        match &operands[2] {
            Operand::RoundingMode(s) => parse_rm(s),
            _ => 0b111,
        }
    } else {
        0b111
    };
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, rm, rs1, rs2, funct7)))
}

fn encode_fcvt_fp(operands: &[Operand], funct7: u32, rs2: u32) -> Result<EncodeResult, String> {
    // Float to float conversion (e.g., fcvt.s.d, fcvt.d.s)
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    let rm = if operands.len() > 2 {
        match &operands[2] {
            Operand::RoundingMode(s) => parse_rm(s),
            _ => 0b111,
        }
    } else {
        0b111
    };
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, rm, rs1, rs2, funct7)))
}

fn encode_fmv_x_f(operands: &[Operand], funct7: u32, _fmt: u32) -> Result<EncodeResult, String> {
    // Float to integer register move
    let rd = get_reg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, 0b000, rs1, 0, funct7)))
}

fn encode_fmv_f_x(operands: &[Operand], funct7: u32, _fmt: u32) -> Result<EncodeResult, String> {
    // Integer to float register move
    let rd = get_freg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, 0b000, rs1, 0, funct7)))
}

fn encode_fma(operands: &[Operand], opcode: u32, fmt: u32) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    let rs2 = get_freg(operands, 2)?;
    let rs3 = get_freg(operands, 3)?;
    let rm = if operands.len() > 4 {
        match &operands[4] {
            Operand::RoundingMode(s) => parse_rm(s),
            _ => 0b111,
        }
    } else {
        0b111
    };
    // R4-type: rs3[31:27] | fmt[26:25] | rs2[24:20] | rs1[19:15] | rm[14:12] | rd[11:7] | opcode[6:0]
    let word = (rs3 << 27) | (fmt << 25) | (rs2 << 20) | (rs1 << 15) | (rm << 12) | (rd << 7) | opcode;
    Ok(EncodeResult::Word(word))
}

// ── Pseudo-instruction encoders ──────────────────────────────────────

fn encode_li(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let imm = get_imm(operands, 1)?;

    let words = encode_li_immediate(rd, imm);
    if words.len() == 1 {
        Ok(EncodeResult::Word(words[0]))
    } else {
        Ok(EncodeResult::Words(words))
    }
}

/// Sign-extend a value from `bits` width to i64.
fn sign_extend_li(val: i64, bits: u32) -> i64 {
    let shift = 64 - bits;
    (val << shift) >> shift
}

/// Emit lui + addi (or just lui/addi) for a 32-bit signed value into register `rd`.
fn encode_li_32bit(rd: u32, imm: i32) -> Vec<u32> {
    if imm >= -2048 && imm <= 2047 {
        return vec![encode_i(OP_OP_IMM, rd, 0, 0, imm)]; // addi rd, x0, imm
    }
    let lo = (imm << 20) >> 20; // sign-extend low 12 bits
    let hi = ((imm as u32).wrapping_add(if lo < 0 { 0x1000 } else { 0 })) & 0xFFFFF000;
    let mut words = vec![encode_u(OP_LUI, rd, hi)];
    if lo != 0 {
        words.push(encode_i(OP_OP_IMM, rd, 0, rd, lo)); // addi rd, rd, lo
    }
    words
}

/// Encode `li` pseudo-instruction for an arbitrary 64-bit immediate.
///
/// Decomposes the value into a sequence of lui/addiw/slli/addi instructions.
/// For 64-bit values that don't fit in 32 bits, finds optimal shift amounts
/// such that the value = ((upper << shift1) + lo1) << shift2 + lo2 ...
/// where upper fits in 32 bits and each lo fits in 12 signed bits.
fn encode_li_immediate(rd: u32, imm: i64) -> Vec<u32> {
    // Case 1: fits in 12 bits (addi rd, x0, imm)
    if imm >= -2048 && imm <= 2047 {
        return vec![encode_i(OP_OP_IMM, rd, 0, 0, imm as i32)];
    }

    // Case 2: fits in 32 bits (lui + addi)
    if imm >= -0x80000000 && imm <= 0x7FFFFFFF {
        return encode_li_32bit(rd, imm as i32);
    }

    // Case 3: 64-bit — try single shift: imm = (upper << shift) + lo12
    let lo12 = sign_extend_li(imm & 0xFFF, 12);
    let mut best: Option<Vec<u32>> = None;

    for shift in 12..45 {
        let remainder = imm.wrapping_sub(lo12);
        if remainder & ((1i64 << shift) - 1) != 0 {
            continue;
        }
        let upper = remainder >> shift;
        if upper < -0x80000000 || upper > 0x7FFFFFFF {
            continue;
        }

        let mut words = encode_li_32bit(rd, upper as i32);
        // Convert addi to addiw after lui for proper 64-bit sign extension
        if words.len() == 2 {
            let first_opcode = words[0] & 0x7F;
            let second_opcode = words[1] & 0x7F;
            if first_opcode == OP_LUI && second_opcode == OP_OP_IMM {
                words[1] = (words[1] & !0x7F) | OP_OP_IMM_32;
            }
        }
        words.push(encode_i(OP_OP_IMM, rd, 0b001, rd, shift as i32)); // slli
        if lo12 != 0 {
            words.push(encode_i(OP_OP_IMM, rd, 0, rd, lo12 as i32)); // addi
        }

        if best.is_none() || words.len() < best.as_ref().unwrap().len() {
            best = Some(words);
        }
    }

    if let Some(words) = best {
        return words;
    }

    // Case 4: two-level shift — imm = ((A << shift1) + lo_b) << shift2 + lo_c
    for shift2 in 12..33 {
        let remainder_c = imm.wrapping_sub(lo12);
        if remainder_c & ((1i64 << shift2) - 1) != 0 {
            continue;
        }
        let inner = remainder_c >> shift2;
        let lo12_b = sign_extend_li(inner & 0xFFF, 12);

        for shift1 in 12..33 {
            let remainder_b = inner.wrapping_sub(lo12_b);
            if remainder_b & ((1i64 << shift1) - 1) != 0 {
                continue;
            }
            let upper = remainder_b >> shift1;
            if upper < -0x80000000 || upper > 0x7FFFFFFF {
                continue;
            }

            let mut words = encode_li_32bit(rd, upper as i32);
            if words.len() == 2 {
                let first_opcode = words[0] & 0x7F;
                let second_opcode = words[1] & 0x7F;
                if first_opcode == OP_LUI && second_opcode == OP_OP_IMM {
                    words[1] = (words[1] & !0x7F) | OP_OP_IMM_32;
                }
            }
            words.push(encode_i(OP_OP_IMM, rd, 0b001, rd, shift1 as i32)); // slli
            if lo12_b != 0 {
                words.push(encode_i(OP_OP_IMM, rd, 0, rd, lo12_b as i32)); // addi
            }
            words.push(encode_i(OP_OP_IMM, rd, 0b001, rd, shift2 as i32)); // slli
            if lo12 != 0 {
                words.push(encode_i(OP_OP_IMM, rd, 0, rd, lo12 as i32)); // addi
            }

            if best.is_none() || words.len() < best.as_ref().unwrap().len() {
                best = Some(words);
            }
        }
    }

    if let Some(words) = best {
        return words;
    }

    // Case 5: three-level shift (needed for dense bit patterns across all 64 bits)
    for shift3 in 12..23 {
        let rem_c = imm.wrapping_sub(lo12);
        if rem_c & ((1i64 << shift3) - 1) != 0 {
            continue;
        }
        let v2 = rem_c >> shift3;
        let lo12_b = sign_extend_li(v2 & 0xFFF, 12);

        for shift2 in 12..23 {
            let rem_b = v2.wrapping_sub(lo12_b);
            if rem_b & ((1i64 << shift2) - 1) != 0 {
                continue;
            }
            let v1 = rem_b >> shift2;
            let lo12_a = sign_extend_li(v1 & 0xFFF, 12);

            for shift1 in 12..23 {
                let rem_a = v1.wrapping_sub(lo12_a);
                if rem_a & ((1i64 << shift1) - 1) != 0 {
                    continue;
                }
                let upper = rem_a >> shift1;
                if upper < -0x80000000 || upper > 0x7FFFFFFF {
                    continue;
                }

                let mut words = encode_li_32bit(rd, upper as i32);
                if words.len() == 2 {
                    let first_opcode = words[0] & 0x7F;
                    let second_opcode = words[1] & 0x7F;
                    if first_opcode == OP_LUI && second_opcode == OP_OP_IMM {
                        words[1] = (words[1] & !0x7F) | OP_OP_IMM_32;
                    }
                }
                words.push(encode_i(OP_OP_IMM, rd, 0b001, rd, shift1 as i32));
                if lo12_a != 0 {
                    words.push(encode_i(OP_OP_IMM, rd, 0, rd, lo12_a as i32));
                }
                words.push(encode_i(OP_OP_IMM, rd, 0b001, rd, shift2 as i32));
                if lo12_b != 0 {
                    words.push(encode_i(OP_OP_IMM, rd, 0, rd, lo12_b as i32));
                }
                words.push(encode_i(OP_OP_IMM, rd, 0b001, rd, shift3 as i32));
                if lo12 != 0 {
                    words.push(encode_i(OP_OP_IMM, rd, 0, rd, lo12 as i32));
                }

                if best.is_none() || words.len() < best.as_ref().unwrap().len() {
                    best = Some(words);
                }
            }
        }
    }

    if let Some(words) = best {
        return words;
    }

    // Fallback: lui + addiw + slli 32, then add lower bits via addi chain
    eprintln!("warning: li fallback for 0x{:x}", imm as u64);
    let upper = (imm >> 32) as i32;
    let mut words = encode_li_32bit(rd, upper);
    if words.len() == 2 {
        let first_opcode = words[0] & 0x7F;
        let second_opcode = words[1] & 0x7F;
        if first_opcode == OP_LUI && second_opcode == OP_OP_IMM {
            words[1] = (words[1] & !0x7F) | OP_OP_IMM_32;
        }
    }
    words.push(encode_i(OP_OP_IMM, rd, 0b001, rd, 32)); // slli rd, rd, 32
    let mut remaining = imm as i32 as i64;
    while remaining != 0 {
        let chunk = remaining.max(-2048).min(2047);
        words.push(encode_i(OP_OP_IMM, rd, 0, rd, chunk as i32));
        remaining -= chunk;
    }
    words
}

fn encode_mv(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs = get_reg(operands, 1)?;
    // Use `add rd, x0, rs` instead of `addi rd, rs, 0` so the instruction
    // is eligible for RV64C compression to C.MV (which requires the ADD form).
    Ok(EncodeResult::Word(encode_r(OP_OP, rd, 0b000, 0, rs, 0b0000000))) // add rd, x0, rs
}

fn encode_not(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_i(OP_OP_IMM, rd, 0b100, rs1, -1))) // xori rd, rs1, -1
}

fn encode_neg(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP, rd, 0b000, 0, rs2, 0b0100000))) // sub rd, x0, rs2
}

fn encode_negw(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP_32, rd, 0b000, 0, rs2, 0b0100000)))
}

fn encode_sext_w(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_i(OP_OP_IMM_32, rd, 0, rs1, 0))) // addiw rd, rs1, 0
}

fn encode_seqz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_i(OP_OP_IMM, rd, 0b011, rs1, 1))) // sltiu rd, rs1, 1
}

fn encode_snez(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP, rd, 0b011, 0, rs2, 0b0000000))) // sltu rd, x0, rs2
}

fn encode_sltz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP, rd, 0b010, rs1, 0, 0b0000000))) // slt rd, rs1, x0
}

fn encode_sgtz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP, rd, 0b010, 0, rs2, 0b0000000))) // slt rd, x0, rs2
}

// Branch pseudo-instructions
fn encode_beqz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs1 = get_reg(operands, 0)?;
    let label = get_branch_target(operands, 1)?;
    Ok(EncodeResult::WordWithReloc {
        word: encode_b(OP_BRANCH, 0b000, rs1, 0, 0),
        reloc: Relocation { reloc_type: RelocType::Branch, symbol: label, addend: 0 },
    })
}

fn encode_bnez(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs1 = get_reg(operands, 0)?;
    let label = get_branch_target(operands, 1)?;
    Ok(EncodeResult::WordWithReloc {
        word: encode_b(OP_BRANCH, 0b001, rs1, 0, 0),
        reloc: Relocation { reloc_type: RelocType::Branch, symbol: label, addend: 0 },
    })
}

fn encode_blez(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs2 = get_reg(operands, 0)?;
    let label = get_branch_target(operands, 1)?;
    Ok(EncodeResult::WordWithReloc {
        word: encode_b(OP_BRANCH, 0b101, 0, rs2, 0), // bge x0, rs
        reloc: Relocation { reloc_type: RelocType::Branch, symbol: label, addend: 0 },
    })
}

fn encode_bgez(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs1 = get_reg(operands, 0)?;
    let label = get_branch_target(operands, 1)?;
    Ok(EncodeResult::WordWithReloc {
        word: encode_b(OP_BRANCH, 0b101, rs1, 0, 0), // bge rs, x0
        reloc: Relocation { reloc_type: RelocType::Branch, symbol: label, addend: 0 },
    })
}

fn encode_bltz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs1 = get_reg(operands, 0)?;
    let label = get_branch_target(operands, 1)?;
    Ok(EncodeResult::WordWithReloc {
        word: encode_b(OP_BRANCH, 0b100, rs1, 0, 0), // blt rs, x0
        reloc: Relocation { reloc_type: RelocType::Branch, symbol: label, addend: 0 },
    })
}

fn encode_bgtz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs2 = get_reg(operands, 0)?;
    let label = get_branch_target(operands, 1)?;
    Ok(EncodeResult::WordWithReloc {
        word: encode_b(OP_BRANCH, 0b100, 0, rs2, 0), // blt x0, rs
        reloc: Relocation { reloc_type: RelocType::Branch, symbol: label, addend: 0 },
    })
}

fn encode_bgt(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs1 = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    let label = get_branch_target(operands, 2)?;
    Ok(EncodeResult::WordWithReloc {
        word: encode_b(OP_BRANCH, 0b100, rs2, rs1, 0), // blt rs2, rs1
        reloc: Relocation { reloc_type: RelocType::Branch, symbol: label, addend: 0 },
    })
}

fn encode_ble(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs1 = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    let label = get_branch_target(operands, 2)?;
    Ok(EncodeResult::WordWithReloc {
        word: encode_b(OP_BRANCH, 0b101, rs2, rs1, 0), // bge rs2, rs1
        reloc: Relocation { reloc_type: RelocType::Branch, symbol: label, addend: 0 },
    })
}

fn encode_bgtu(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs1 = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    let label = get_branch_target(operands, 2)?;
    Ok(EncodeResult::WordWithReloc {
        word: encode_b(OP_BRANCH, 0b110, rs2, rs1, 0), // bltu rs2, rs1
        reloc: Relocation { reloc_type: RelocType::Branch, symbol: label, addend: 0 },
    })
}

fn encode_bleu(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs1 = get_reg(operands, 0)?;
    let rs2 = get_reg(operands, 1)?;
    let label = get_branch_target(operands, 2)?;
    Ok(EncodeResult::WordWithReloc {
        word: encode_b(OP_BRANCH, 0b111, rs2, rs1, 0), // bgeu rs2, rs1
        reloc: Relocation { reloc_type: RelocType::Branch, symbol: label, addend: 0 },
    })
}

fn get_branch_target(operands: &[Operand], idx: usize) -> Result<String, String> {
    match operands.get(idx) {
        Some(Operand::Symbol(s)) | Some(Operand::Label(s)) => Ok(s.clone()),
        Some(Operand::Imm(v)) => Ok(format!("{}", v)),
        _ => Err(format!("expected branch target at operand {}", idx)),
    }
}

fn encode_j_pseudo(operands: &[Operand]) -> Result<EncodeResult, String> {
    // j offset -> jal x0, offset
    match &operands[0] {
        Operand::Symbol(s) | Operand::Label(s) => {
            Ok(EncodeResult::WordWithReloc {
                word: encode_j(OP_JAL, 0, 0),
                reloc: Relocation {
                    reloc_type: RelocType::Jal,
                    symbol: s.clone(),
                    addend: 0,
                },
            })
        }
        Operand::Imm(imm) => {
            Ok(EncodeResult::Word(encode_j(OP_JAL, 0, *imm as i32)))
        }
        _ => Err("j: expected offset or label".to_string()),
    }
}

fn encode_jr(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rs1 = get_reg(operands, 0)?;
    Ok(EncodeResult::Word(encode_i(OP_JALR, 0, 0, rs1, 0)))
}

fn encode_call(operands: &[Operand]) -> Result<EncodeResult, String> {
    // call symbol -> auipc ra, %pcrel_hi(symbol) ; jalr ra, %pcrel_lo(symbol)(ra)
    let (symbol, addend) = get_symbol(operands, 0)?;
    Ok(EncodeResult::WordsWithRelocs(vec![
        (encode_u(OP_AUIPC, 1, 0), Some(Relocation {
            reloc_type: RelocType::CallPlt,
            symbol: symbol.clone(),
            addend,
        })),
        (encode_i(OP_JALR, 1, 0, 1, 0), None), // jalr ra, 0(ra)
    ]))
}

fn encode_tail(operands: &[Operand]) -> Result<EncodeResult, String> {
    // tail symbol -> auipc t1, %pcrel_hi(symbol) ; jalr x0, %pcrel_lo(symbol)(t1)
    let (symbol, addend) = get_symbol(operands, 0)?;
    Ok(EncodeResult::WordsWithRelocs(vec![
        (encode_u(OP_AUIPC, 6, 0), Some(Relocation { // t1 = x6
            reloc_type: RelocType::CallPlt,
            symbol: symbol.clone(),
            addend,
        })),
        (encode_i(OP_JALR, 0, 0, 6, 0), None),
    ]))
}

fn encode_jump(operands: &[Operand]) -> Result<EncodeResult, String> {
    // jump label, temp_reg -> auipc temp, %pcrel_hi(label) ; jalr x0, %pcrel_lo(label)(temp)
    // Our codegen emits: jump .LBB42, t6
    let (symbol, addend) = get_symbol(operands, 0)?;
    let temp = if operands.len() > 1 {
        get_reg(operands, 1)?
    } else {
        31 // t6
    };

    Ok(EncodeResult::WordsWithRelocs(vec![
        (encode_u(OP_AUIPC, temp, 0), Some(Relocation {
            reloc_type: RelocType::CallPlt,
            symbol: symbol.clone(),
            addend,
        })),
        (encode_i(OP_JALR, 0, 0, temp, 0), None),
    ]))
}

fn encode_la(operands: &[Operand]) -> Result<EncodeResult, String> {
    // la rd, symbol -> auipc rd, %pcrel_hi(symbol) ; addi rd, rd, %pcrel_lo(symbol)
    // TODO: For PIC, this should use GOT
    encode_lla(operands) // for now, same as lla
}

fn encode_lla(operands: &[Operand]) -> Result<EncodeResult, String> {
    // lla rd, symbol -> auipc rd, %pcrel_hi(symbol) ; addi rd, rd, %pcrel_lo(symbol)
    let rd = get_reg(operands, 0)?;
    let (symbol, addend) = get_symbol(operands, 1)?;

    Ok(EncodeResult::WordsWithRelocs(vec![
        (encode_u(OP_AUIPC, rd, 0), Some(Relocation {
            reloc_type: RelocType::PcrelHi20,
            symbol: symbol.clone(),
            addend,
        })),
        (encode_i(OP_OP_IMM, rd, 0, rd, 0), Some(Relocation {
            reloc_type: RelocType::PcrelLo12I,
            symbol, // TODO: This should reference the auipc label, not the symbol directly
            addend,
        })),
    ]))
}

fn encode_rdcsr(mnemonic: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let csr = match mnemonic {
        "rdcycle" => 0xC00,
        "rdtime" => 0xC01,
        "rdinstret" => 0xC02,
        _ => return Err(format!("unknown CSR pseudo: {}", mnemonic)),
    };
    Ok(EncodeResult::Word(encode_i(OP_SYSTEM, rd, 0b010, 0, csr))) // csrrs rd, csr, x0
}

fn encode_csrr(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_reg(operands, 0)?;
    let csr = get_csr_num(operands, 1)?;
    Ok(EncodeResult::Word(encode_i(OP_SYSTEM, rd, 0b010, 0, csr as i32)))
}

fn encode_csrw(operands: &[Operand]) -> Result<EncodeResult, String> {
    let csr = get_csr_num(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_i(OP_SYSTEM, 0, 0b001, rs1, csr as i32)))
}

fn encode_csrs(operands: &[Operand]) -> Result<EncodeResult, String> {
    let csr = get_csr_num(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_i(OP_SYSTEM, 0, 0b010, rs1, csr as i32)))
}

fn encode_csrc(operands: &[Operand]) -> Result<EncodeResult, String> {
    let csr = get_csr_num(operands, 0)?;
    let rs1 = get_reg(operands, 1)?;
    Ok(EncodeResult::Word(encode_i(OP_SYSTEM, 0, 0b011, rs1, csr as i32)))
}

// Float pseudo-instructions
fn encode_fmv_s(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    // fsgnj.s rd, rs, rs
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, 0b000, rs1, rs1, 0b0010000)))
}

fn encode_fmv_d(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, 0b000, rs1, rs1, 0b0010001)))
}

fn encode_fabs_s(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    // fsgnjx.s rd, rs, rs
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, 0b010, rs1, rs1, 0b0010000)))
}

fn encode_fabs_d(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, 0b010, rs1, rs1, 0b0010001)))
}

fn encode_fneg_s(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    // fsgnjn.s rd, rs, rs
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, 0b001, rs1, rs1, 0b0010000)))
}

fn encode_fneg_d(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rd = get_freg(operands, 0)?;
    let rs1 = get_freg(operands, 1)?;
    Ok(EncodeResult::Word(encode_r(OP_OP_FP, rd, 0b001, rs1, rs1, 0b0010001)))
}

// ── Relocation modifier parsing ──────────────────────────────────────

/// Extract symbol name from %modifier(symbol) expressions
fn extract_modifier_symbol(s: &str) -> String {
    if let Some(start) = s.find('(') {
        if let Some(end) = s.rfind(')') {
            return s[start + 1..end].to_string();
        }
    }
    s.to_string()
}

/// Parse a relocation modifier like %pcrel_hi(symbol) and return (RelocType, symbol)
fn parse_reloc_modifier(s: &str) -> (RelocType, String) {
    if s.starts_with("%pcrel_hi(") {
        (RelocType::PcrelHi20, extract_modifier_symbol(s))
    } else if s.starts_with("%pcrel_lo(") {
        (RelocType::PcrelLo12I, extract_modifier_symbol(s))
    } else if s.starts_with("%hi(") {
        (RelocType::Hi20, extract_modifier_symbol(s))
    } else if s.starts_with("%lo(") {
        (RelocType::Lo12I, extract_modifier_symbol(s))
    } else if s.starts_with("%tprel_hi(") {
        (RelocType::TprelHi20, extract_modifier_symbol(s))
    } else if s.starts_with("%tprel_lo(") {
        (RelocType::TprelLo12I, extract_modifier_symbol(s))
    } else if s.starts_with("%tprel_add(") {
        (RelocType::TprelAdd, extract_modifier_symbol(s))
    } else if s.starts_with("%got_pcrel_hi(") {
        (RelocType::GotHi20, extract_modifier_symbol(s))
    } else if s.starts_with("%tls_ie_pcrel_hi(") {
        (RelocType::TlsGotHi20, extract_modifier_symbol(s))
    } else if s.starts_with("%tls_gd_pcrel_hi(") {
        (RelocType::TlsGdHi20, extract_modifier_symbol(s))
    } else {
        // Plain symbol - use as PC-relative
        (RelocType::PcrelHi20, s.to_string())
    }
}
