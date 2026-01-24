use crate::common::types::IrType;

/// A compilation unit in the IR.
#[derive(Debug)]
pub struct IrModule {
    pub functions: Vec<IrFunction>,
    pub globals: Vec<IrGlobal>,
    pub string_literals: Vec<(String, String)>, // (label, value)
}

/// A global variable.
#[derive(Debug, Clone)]
pub struct IrGlobal {
    pub name: String,
    pub ty: IrType,
    /// Size of the global in bytes (for arrays, this is elem_size * count).
    pub size: usize,
    /// Alignment in bytes.
    pub align: usize,
    /// Initializer for the global variable.
    pub init: GlobalInit,
    /// Whether this is a static (file-scope) variable.
    pub is_static: bool,
    /// Whether this is an extern declaration (no storage emitted).
    pub is_extern: bool,
}

/// Initializer for a global variable.
#[derive(Debug, Clone)]
pub enum GlobalInit {
    /// No initializer (zero-initialized in .bss).
    Zero,
    /// Single scalar constant.
    Scalar(IrConst),
    /// Array of scalar constants.
    Array(Vec<IrConst>),
    /// String literal (stored as bytes with null terminator).
    String(String),
    /// Address of another global (for pointer globals like `const char *s = "hello"`).
    GlobalAddr(String),
    /// Address of a global plus a byte offset (for `&arr[3]`, `&s.field`, etc.).
    GlobalAddrOffset(String, i64),
    /// Compound initializer: a sequence of initializer elements (for arrays/structs
    /// containing address expressions, e.g., `int *ptrs[] = {&a, &b, 0}`).
    Compound(Vec<GlobalInit>),
}

/// An IR function.
#[derive(Debug)]
pub struct IrFunction {
    pub name: String,
    pub return_type: IrType,
    pub params: Vec<IrParam>,
    pub blocks: Vec<BasicBlock>,
    pub is_variadic: bool,
    pub is_declaration: bool, // true if no body (extern)
    pub is_static: bool,      // true if declared with `static` linkage
    pub stack_size: usize,
}

/// A function parameter.
#[derive(Debug, Clone)]
pub struct IrParam {
    pub name: String,
    pub ty: IrType,
}

/// A basic block in the CFG.
#[derive(Debug)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

/// An SSA value reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(pub u32);

/// An IR instruction.
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Allocate stack space: %dest = alloca ty
    Alloca { dest: Value, ty: IrType, size: usize },

    /// Store to memory: store val, ptr (type indicates size of store)
    Store { val: Operand, ptr: Value, ty: IrType },

    /// Load from memory: %dest = load ptr
    Load { dest: Value, ptr: Value, ty: IrType },

    /// Binary operation: %dest = op lhs, rhs
    BinOp { dest: Value, op: IrBinOp, lhs: Operand, rhs: Operand, ty: IrType },

    /// Unary operation: %dest = op src
    UnaryOp { dest: Value, op: IrUnaryOp, src: Operand, ty: IrType },

    /// Comparison: %dest = cmp op lhs, rhs
    Cmp { dest: Value, op: IrCmpOp, lhs: Operand, rhs: Operand, ty: IrType },

    /// Function call: %dest = call func(args...)
    /// `num_fixed_args` is the number of named (non-variadic) parameters in the callee's prototype.
    /// For non-variadic calls, this equals args.len(). For variadic calls, args beyond num_fixed_args
    /// are variadic and may need different calling convention handling (e.g., floats in GP regs on AArch64).
    Call { dest: Option<Value>, func: String, args: Vec<Operand>, arg_types: Vec<IrType>, return_type: IrType, is_variadic: bool, num_fixed_args: usize },

    /// Indirect function call through a pointer: %dest = call_indirect ptr(args...)
    CallIndirect { dest: Option<Value>, func_ptr: Operand, args: Vec<Operand>, arg_types: Vec<IrType>, return_type: IrType, is_variadic: bool, num_fixed_args: usize },

    /// Get element pointer (for arrays/structs)
    GetElementPtr { dest: Value, base: Value, offset: Operand, ty: IrType },

    /// Type cast/conversion
    Cast { dest: Value, src: Operand, from_ty: IrType, to_ty: IrType },

    /// Copy a value
    Copy { dest: Value, src: Operand },

    /// Get address of a global
    GlobalAddr { dest: Value, name: String },

    /// Memory copy: memcpy(dest, src, size)
    Memcpy { dest: Value, src: Value, size: usize },

    /// va_arg: extract the next variadic argument from a va_list.
    /// va_list_ptr is a pointer to the va_list struct/pointer.
    /// result_ty is the type of the argument being extracted.
    VaArg { dest: Value, va_list_ptr: Value, result_ty: IrType },

    /// va_start: initialize a va_list. last_named_param is the pointer to the last named parameter.
    VaStart { va_list_ptr: Value, },

    /// va_end: cleanup a va_list (typically a no-op).
    VaEnd { va_list_ptr: Value },

    /// va_copy: copy one va_list to another.
    VaCopy { dest_ptr: Value, src_ptr: Value },

    /// Atomic read-modify-write: %dest = atomicrmw op ptr, val
    /// Performs: old = *ptr; *ptr = op(old, val); dest = old (fetch_and_*) or dest = op(old, val) (*_and_fetch)
    AtomicRmw {
        dest: Value,
        op: AtomicRmwOp,
        ptr: Operand,
        val: Operand,
        ty: IrType,
        ordering: AtomicOrdering,
    },

    /// Atomic compare-exchange: %dest = cmpxchg ptr, expected, desired
    /// Returns whether the exchange succeeded (as a boolean i8 for __atomic_compare_exchange_n)
    /// or the old value (for __sync_val_compare_and_swap).
    AtomicCmpxchg {
        dest: Value,
        ptr: Operand,
        expected: Operand,
        desired: Operand,
        ty: IrType,
        success_ordering: AtomicOrdering,
        failure_ordering: AtomicOrdering,
        /// If true, dest gets the success/failure boolean; if false, dest gets the old value.
        returns_bool: bool,
    },

    /// Atomic load: %dest = atomic_load ptr
    AtomicLoad {
        dest: Value,
        ptr: Operand,
        ty: IrType,
        ordering: AtomicOrdering,
    },

    /// Atomic store: atomic_store ptr, val
    AtomicStore {
        ptr: Operand,
        val: Operand,
        ty: IrType,
        ordering: AtomicOrdering,
    },

    /// Memory fence
    Fence {
        ordering: AtomicOrdering,
    },

    /// SSA Phi node: merges values from different predecessor blocks.
    /// Each entry in `incoming` is (value, block_label) indicating which value
    /// flows in from which predecessor block.
    Phi {
        dest: Value,
        ty: IrType,
        incoming: Vec<(Operand, String)>,
    },

    /// Get the address of a label (GCC computed goto extension: &&label)
    LabelAddr { dest: Value, label: String },

    /// Inline assembly statement
    InlineAsm {
        /// Assembly template string (with \n\t separators)
        template: String,
        /// Output operands: (constraint, value_ptr, optional_name)
        outputs: Vec<(String, Value, Option<String>)>,
        /// Input operands: (constraint, operand, optional_name)
        inputs: Vec<(String, Operand, Option<String>)>,
        /// Clobber list (register names and "memory", "cc")
        clobbers: Vec<String>,
        /// Types of operands (outputs first, then inputs) for register size selection
        operand_types: Vec<IrType>,
    },
}

/// Block terminator.
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Return from function
    Return(Option<Operand>),

    /// Unconditional branch
    Branch(String),

    /// Conditional branch
    CondBranch { cond: Operand, true_label: String, false_label: String },

    /// Indirect branch (computed goto): goto *addr
    /// possible_targets lists all labels that could be jumped to (for optimization/validation)
    IndirectBranch { target: Operand, possible_targets: Vec<String> },

    /// Unreachable (e.g., after noreturn call)
    Unreachable,
}

/// An operand (either a value reference or a constant).
#[derive(Debug, Clone)]
pub enum Operand {
    Value(Value),
    Const(IrConst),
}

/// IR constants.
#[derive(Debug, Clone)]
pub enum IrConst {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    /// Long double: stored as f64 value, emitted as x87 80-bit extended precision (16 bytes with padding).
    /// On ARM64/RISC-V, emitted as IEEE 754 quad precision approximation (16 bytes).
    LongDouble(f64),
    Zero,
}

/// Hashable representation of IR constants, using bit patterns for floats.
/// This allows constants to be used as HashMap keys for value numbering.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ConstHashKey {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(u32),
    F64(u64),
    Zero,
}

/// Convert an f64 value to IEEE 754 binary128 (quad-precision) encoding (16 bytes, little-endian).
/// Quad format: 1 sign bit, 15 exponent bits (bias 16383), 112 mantissa bits (implicit leading 1).
/// This is used for long double on AArch64 and RISC-V.
pub fn f64_to_f128_bytes(val: f64) -> [u8; 16] {
    let bits = val.to_bits();
    let sign = (bits >> 63) & 1;
    let exp11 = ((bits >> 52) & 0x7FF) as i64;
    let mantissa52 = bits & 0x000F_FFFF_FFFF_FFFF;

    if exp11 == 0 && mantissa52 == 0 {
        // Zero (positive or negative)
        let mut bytes = [0u8; 16];
        if sign == 1 {
            bytes[15] = 0x80; // sign bit in MSB
        }
        return bytes;
    }

    if exp11 == 0x7FF {
        // Infinity or NaN
        let exp15: u16 = 0x7FFF;
        if mantissa52 == 0 {
            // Infinity
            let mut bytes = [0u8; 16];
            bytes[14] = (exp15 & 0xFF) as u8;
            bytes[15] = ((exp15 >> 8) as u8) | ((sign as u8) << 7);
            return bytes;
        } else {
            // NaN - set high mantissa bit
            let mut bytes = [0u8; 16];
            bytes[13] = 0x80; // quiet NaN bit
            bytes[14] = (exp15 & 0xFF) as u8;
            bytes[15] = ((exp15 >> 8) as u8) | ((sign as u8) << 7);
            return bytes;
        }
    }

    // Normal number
    // f64 exponent bias is 1023, f128 exponent bias is 16383
    let exp15 = (exp11 - 1023 + 16383) as u16;
    // f64 has 52-bit mantissa (implicit 1.), f128 has 112-bit mantissa (implicit 1.)
    // Shift the 52-bit mantissa to the top of the 112-bit mantissa field:
    // mantissa112 = mantissa52 << (112 - 52) = mantissa52 << 60
    // The 112-bit mantissa occupies bits [0..111] of the 128-bit value
    // Layout (little-endian): bytes[0..13] = mantissa (112 bits = 14 bytes),
    //   bytes[14..15] = exponent[0..14] (15 bits) + sign (1 bit)
    // But actually: the quad format is:
    //   bit 127 = sign, bits [112..126] = exponent (15 bits), bits [0..111] = mantissa
    // In little-endian u128:
    let mantissa112: u128 = (mantissa52 as u128) << 60;
    let exp_sign: u128 = ((exp15 as u128) << 112) | ((sign as u128) << 127);
    let val128 = mantissa112 | exp_sign;
    val128.to_le_bytes()
}

/// Convert an f64 value to x87 80-bit extended precision encoding (10 bytes, little-endian).
/// x87 format: 1 sign bit, 15 exponent bits (bias 16383), 64 mantissa bits (explicit integer bit).
/// Used for long double on x86-64.
pub fn f64_to_x87_bytes(val: f64) -> [u8; 10] {
    let bits = val.to_bits();
    let sign = (bits >> 63) & 1;
    let exp11 = ((bits >> 52) & 0x7FF) as i64;
    let mantissa52 = bits & 0x000F_FFFF_FFFF_FFFF;

    if exp11 == 0 && mantissa52 == 0 {
        // Zero
        let mut bytes = [0u8; 10];
        if sign == 1 {
            bytes[9] = 0x80;
        }
        return bytes;
    }

    if exp11 == 0x7FF {
        // Infinity or NaN
        if mantissa52 == 0 {
            // Infinity
            let mut bytes = [0u8; 10];
            bytes[7] = 0x80; // integer bit set, mantissa = 0
            let exp15: u16 = 0x7FFF;
            bytes[8] = (exp15 & 0xFF) as u8;
            bytes[9] = ((exp15 >> 8) as u8) | ((sign as u8) << 7);
            return bytes;
        } else {
            // NaN
            let mut bytes = [0xFFu8; 10];
            bytes[8] = 0xFF;
            bytes[9] = 0x7F | ((sign as u8) << 7);
            return bytes;
        }
    }

    // Normal number
    // f64 exponent bias is 1023, x87 exponent bias is 16383
    let exp15 = (exp11 - 1023 + 16383) as u16;
    // f64 has 52-bit mantissa (implicit 1.), x87 has 64-bit mantissa (explicit 1.)
    // Shift mantissa: 52 bits -> 63 bits (bottom), then set bit 63 (integer bit)
    let mantissa64 = (1u64 << 63) | (mantissa52 << 11);

    let mut bytes = [0u8; 10];
    // Bytes 0..7: mantissa (64 bits, little-endian)
    bytes[..8].copy_from_slice(&mantissa64.to_le_bytes());
    // Bytes 8..9: exponent (15 bits) + sign (1 bit)
    bytes[8] = (exp15 & 0xFF) as u8;
    bytes[9] = ((exp15 >> 8) as u8) | ((sign as u8) << 7);
    bytes
}

impl IrConst {
    /// Returns true if this constant is zero (integer or float).
    pub fn is_zero(&self) -> bool {
        match self {
            IrConst::I8(0) | IrConst::I16(0) | IrConst::I32(0) | IrConst::I64(0) => true,
            IrConst::F32(v) => *v == 0.0,
            IrConst::F64(v) => *v == 0.0,
            IrConst::LongDouble(v) => *v == 0.0,
            IrConst::Zero => true,
            _ => false,
        }
    }

    /// Returns true if this constant is one (integer only).
    pub fn is_one(&self) -> bool {
        matches!(self, IrConst::I8(1) | IrConst::I16(1) | IrConst::I32(1) | IrConst::I64(1))
    }

    /// Returns true if this constant is nonzero (for truthiness checks in const eval).
    pub fn is_nonzero(&self) -> bool {
        !self.is_zero()
    }

    /// Convert to a hashable key representation (using bit patterns for floats).
    pub fn to_hash_key(&self) -> ConstHashKey {
        match self {
            IrConst::I8(v) => ConstHashKey::I8(*v),
            IrConst::I16(v) => ConstHashKey::I16(*v),
            IrConst::I32(v) => ConstHashKey::I32(*v),
            IrConst::I64(v) => ConstHashKey::I64(*v),
            IrConst::F32(v) => ConstHashKey::F32(v.to_bits()),
            IrConst::F64(v) => ConstHashKey::F64(v.to_bits()),
            IrConst::LongDouble(v) => ConstHashKey::F64(v.to_bits()),
            IrConst::Zero => ConstHashKey::Zero,
        }
    }

    /// Extract as f64 (works for all numeric types).
    pub fn to_f64(&self) -> Option<f64> {
        match self {
            IrConst::I8(v) => Some(*v as f64),
            IrConst::I16(v) => Some(*v as f64),
            IrConst::I32(v) => Some(*v as f64),
            IrConst::I64(v) => Some(*v as f64),
            IrConst::F32(v) => Some(*v as f64),
            IrConst::F64(v) => Some(*v),
            IrConst::LongDouble(v) => Some(*v),
            IrConst::Zero => Some(0.0),
        }
    }

    /// Cast a float value (as f64) to the target IR type, producing a new IrConst.
    /// Used to deduplicate F32/F64/LongDouble -> target cast logic in const eval.
    pub fn cast_float_to_target(fv: f64, target: IrType) -> Option<IrConst> {
        Some(match target {
            IrType::F64 => IrConst::F64(fv),
            IrType::F128 => IrConst::LongDouble(fv),
            IrType::F32 => IrConst::F32(fv as f32),
            IrType::I8 | IrType::U8 => IrConst::I8(fv as i8),
            IrType::I16 | IrType::U16 => IrConst::I16(fv as i16),
            IrType::I32 | IrType::U32 => IrConst::I32(fv as i32),
            IrType::I64 | IrType::U64 | IrType::Ptr => IrConst::I64(fv as i64),
            _ => return None,
        })
    }

    /// Extract as i64 (integer constants only; floats return None).
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            IrConst::I8(v) => Some(*v as i64),
            IrConst::I16(v) => Some(*v as i64),
            IrConst::I32(v) => Some(*v as i64),
            IrConst::I64(v) => Some(*v),
            IrConst::Zero => Some(0),
            IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(_) => None,
        }
    }

    /// Extract as u64 (integer constants only; floats return None).
    pub fn to_u64(&self) -> Option<u64> {
        match self {
            IrConst::I8(v) => Some(*v as u64),
            IrConst::I16(v) => Some(*v as u64),
            IrConst::I32(v) => Some(*v as u64),
            IrConst::I64(v) => Some(*v as u64),
            IrConst::Zero => Some(0),
            IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(_) => None,
        }
    }

    /// Extract as usize (integer constants only).
    pub fn to_usize(&self) -> Option<usize> {
        self.to_i64().map(|v| v as usize)
    }

    /// Extract as u32 (integer constants only).
    pub fn to_u32(&self) -> Option<u32> {
        self.to_i64().map(|v| v as u32)
    }

    /// Convert to bytes in little-endian format, pushing onto a byte buffer.
    /// Writes `size` bytes for integer types, or full float representation for floats.
    pub fn push_le_bytes(&self, out: &mut Vec<u8>, size: usize) {
        match self {
            IrConst::F32(v) => {
                out.extend_from_slice(&v.to_bits().to_le_bytes());
            }
            IrConst::F64(v) => {
                out.extend_from_slice(&v.to_bits().to_le_bytes());
            }
            IrConst::LongDouble(v) => {
                // Store as IEEE 754 binary128 (quad-precision) for AArch64/RISC-V,
                // or x87 80-bit extended + padding for x86-64.
                // We use quad-precision format which works correctly for ARM64/RISC-V.
                // For x86-64, this also works because the x87 format is handled separately
                // in the x86 backend's global data emission.
                let bytes = f64_to_f128_bytes(*v);
                out.extend_from_slice(&bytes);
            }
            _ => {
                let le_bytes = self.to_i64().unwrap_or(0).to_le_bytes();
                out.extend_from_slice(&le_bytes[..size]);
            }
        }
    }

    /// Construct an IrConst of the given type from an i64 value.
    pub fn from_i64(val: i64, ty: IrType) -> Self {
        match ty {
            IrType::I8 | IrType::U8 => IrConst::I8(val as i8),
            IrType::I16 | IrType::U16 => IrConst::I16(val as i16),
            IrType::I32 | IrType::U32 => IrConst::I32(val as i32),
            IrType::F32 => IrConst::F32(val as f32),
            IrType::F64 => IrConst::F64(val as f64),
            _ => IrConst::I64(val),
        }
    }

    /// Coerce this constant to match a target IrType, with optional source type for signedness.
    pub fn coerce_to_with_src(&self, target_ty: IrType, src_ty: Option<IrType>) -> IrConst {
        // Check if already the right type
        match (self, target_ty) {
            (IrConst::I8(_), IrType::I8 | IrType::U8) => return self.clone(),
            (IrConst::I16(_), IrType::I16 | IrType::U16) => return self.clone(),
            (IrConst::I32(_), IrType::I32 | IrType::U32) => return self.clone(),
            (IrConst::I64(_), IrType::I64 | IrType::U64 | IrType::Ptr) => return self.clone(),
            (IrConst::F32(_), IrType::F32) => return self.clone(),
            (IrConst::F64(_), IrType::F64) => return self.clone(),
            // LongDouble stays as LongDouble when target is F64 or F128
            (IrConst::LongDouble(_), IrType::F64 | IrType::F128) => return self.clone(),
            _ => {}
        }
        // Convert integer types
        if let Some(int_val) = self.to_i64() {
            // For int-to-float conversions, check source signedness
            if target_ty.is_float() {
                let is_unsigned = src_ty.map_or(false, |t| t.is_unsigned());
                if is_unsigned {
                    let uint_val = int_val as u64;
                    return match target_ty {
                        IrType::F32 => IrConst::F32(uint_val as f32),
                        IrType::F64 => IrConst::F64(uint_val as f64),
                        _ => IrConst::I64(int_val),
                    };
                }
            }
            return IrConst::from_i64(int_val, target_ty);
        }
        // Convert float types
        match (self, target_ty) {
            (IrConst::F64(v), IrType::F32) => IrConst::F32(*v as f32),
            (IrConst::F32(v), IrType::F64) => IrConst::F64(*v as f64),
            (IrConst::F64(v), IrType::I8) => IrConst::I8(*v as i8),
            (IrConst::F64(v), IrType::U8) => IrConst::I8(*v as u8 as i8),
            (IrConst::F64(v), IrType::I16) => IrConst::I16(*v as i16),
            (IrConst::F64(v), IrType::U16) => IrConst::I16(*v as u16 as i16),
            (IrConst::F64(v), IrType::I32) => IrConst::I32(*v as i32),
            (IrConst::F64(v), IrType::U32) => IrConst::I32(*v as u32 as i32),
            (IrConst::F64(v), IrType::I64) => IrConst::I64(*v as i64),
            (IrConst::F64(v), IrType::U64) => IrConst::I64(*v as u64 as i64),
            (IrConst::F32(v), IrType::I8) => IrConst::I8(*v as i8),
            (IrConst::F32(v), IrType::U8) => IrConst::I8(*v as u8 as i8),
            (IrConst::F32(v), IrType::I16) => IrConst::I16(*v as i16),
            (IrConst::F32(v), IrType::U16) => IrConst::I16(*v as u16 as i16),
            (IrConst::F32(v), IrType::I32) => IrConst::I32(*v as i32),
            (IrConst::F32(v), IrType::U32) => IrConst::I32(*v as u32 as i32),
            (IrConst::F32(v), IrType::I64) => IrConst::I64(*v as i64),
            (IrConst::F32(v), IrType::U64) => IrConst::I64(*v as u64 as i64),
            // LongDouble conversions
            (IrConst::LongDouble(v), IrType::F64) => IrConst::F64(*v),
            (IrConst::LongDouble(v), IrType::F32) => IrConst::F32(*v as f32),
            (IrConst::LongDouble(v), IrType::I8) => IrConst::I8(*v as i8),
            (IrConst::LongDouble(v), IrType::U8) => IrConst::I8(*v as u8 as i8),
            (IrConst::LongDouble(v), IrType::I16) => IrConst::I16(*v as i16),
            (IrConst::LongDouble(v), IrType::U16) => IrConst::I16(*v as u16 as i16),
            (IrConst::LongDouble(v), IrType::I32) => IrConst::I32(*v as i32),
            (IrConst::LongDouble(v), IrType::U32) => IrConst::I32(*v as u32 as i32),
            (IrConst::LongDouble(v), IrType::I64) => IrConst::I64(*v as i64),
            (IrConst::LongDouble(v), IrType::U64) => IrConst::I64(*v as u64 as i64),
            // Conversions to F128 (long double)
            (IrConst::F64(v), IrType::F128) => IrConst::LongDouble(*v),
            (IrConst::F32(v), IrType::F128) => IrConst::LongDouble(*v as f64),
            (IrConst::I64(v), IrType::F128) => IrConst::LongDouble(*v as f64),
            (IrConst::I32(v), IrType::F128) => IrConst::LongDouble(*v as f64),
            (IrConst::I16(v), IrType::F128) => IrConst::LongDouble(*v as f64),
            (IrConst::I8(v), IrType::F128) => IrConst::LongDouble(*v as f64),
            _ => self.clone(),
        }
    }

    /// Coerce this constant to match a target IrType (assumes signed source for int-to-float).
    pub fn coerce_to(&self, target_ty: IrType) -> IrConst {
        self.coerce_to_with_src(target_ty, None)
    }

    /// Get the zero constant for a given IR type.
    pub fn zero(ty: IrType) -> IrConst {
        match ty {
            IrType::I8 | IrType::U8 => IrConst::I8(0),
            IrType::I16 | IrType::U16 => IrConst::I16(0),
            IrType::I32 | IrType::U32 => IrConst::I32(0),
            IrType::F32 => IrConst::F32(0.0),
            IrType::F64 => IrConst::F64(0.0),
            IrType::F128 => IrConst::LongDouble(0.0),
            _ => IrConst::I64(0),
        }
    }

    /// Get the one constant for a given IR type.
    pub fn one(ty: IrType) -> IrConst {
        match ty {
            IrType::I8 | IrType::U8 => IrConst::I8(1),
            IrType::I16 | IrType::U16 => IrConst::I16(1),
            IrType::I32 | IrType::U32 => IrConst::I32(1),
            IrType::F32 => IrConst::F32(1.0),
            IrType::F64 => IrConst::F64(1.0),
            IrType::F128 => IrConst::LongDouble(1.0),
            _ => IrConst::I64(1),
        }
    }
}

/// Atomic read-modify-write operations.
#[derive(Debug, Clone, Copy)]
pub enum AtomicRmwOp {
    /// Add: *ptr += val
    Add,
    /// Sub: *ptr -= val
    Sub,
    /// And: *ptr &= val
    And,
    /// Or: *ptr |= val
    Or,
    /// Xor: *ptr ^= val
    Xor,
    /// Nand: *ptr = ~(*ptr & val)
    Nand,
    /// Exchange: *ptr = val (returns old value)
    Xchg,
    /// Test and set: *ptr = 1 (returns old value)
    TestAndSet,
}

/// Memory ordering for atomic operations.
#[derive(Debug, Clone, Copy)]
pub enum AtomicOrdering {
    Relaxed,
    Acquire,
    Release,
    AcqRel,
    SeqCst,
}

/// Binary operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrBinOp {
    Add,
    Sub,
    Mul,
    SDiv,
    UDiv,
    SRem,
    URem,
    And,
    Or,
    Xor,
    Shl,
    AShr,
    LShr,
}

impl IrBinOp {
    /// Returns true if this operation is commutative (a op b == b op a).
    pub fn is_commutative(self) -> bool {
        matches!(self, IrBinOp::Add | IrBinOp::Mul | IrBinOp::And | IrBinOp::Or | IrBinOp::Xor)
    }
}

/// Unary operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrUnaryOp {
    Neg,
    Not,
    Clz,
    Ctz,
    Bswap,
    Popcount,
}

/// Comparison operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrCmpOp {
    Eq,
    Ne,
    Slt,
    Sle,
    Sgt,
    Sge,
    Ult,
    Ule,
    Ugt,
    Uge,
}

impl Instruction {
    /// Get the destination value defined by this instruction, if any.
    /// Instructions like Store, Memcpy, VaStart, VaEnd, VaCopy, AtomicStore,
    /// Fence, and InlineAsm produce no value.
    pub fn dest(&self) -> Option<Value> {
        match self {
            Instruction::Alloca { dest, .. }
            | Instruction::Load { dest, .. }
            | Instruction::BinOp { dest, .. }
            | Instruction::UnaryOp { dest, .. }
            | Instruction::Cmp { dest, .. }
            | Instruction::GetElementPtr { dest, .. }
            | Instruction::Cast { dest, .. }
            | Instruction::Copy { dest, .. }
            | Instruction::GlobalAddr { dest, .. }
            | Instruction::VaArg { dest, .. }
            | Instruction::AtomicRmw { dest, .. }
            | Instruction::AtomicCmpxchg { dest, .. }
            | Instruction::AtomicLoad { dest, .. }
            | Instruction::Phi { dest, .. }
            | Instruction::LabelAddr { dest, .. } => Some(*dest),
            Instruction::Call { dest, .. }
            | Instruction::CallIndirect { dest, .. } => *dest,
            Instruction::Store { .. }
            | Instruction::Memcpy { .. }
            | Instruction::VaStart { .. }
            | Instruction::VaEnd { .. }
            | Instruction::VaCopy { .. }
            | Instruction::AtomicStore { .. }
            | Instruction::Fence { .. }
            | Instruction::InlineAsm { .. } => None,
        }
    }
}

impl IrModule {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            globals: Vec::new(),
            string_literals: Vec::new(),
        }
    }

    /// Run a transformation on each defined (non-declaration) function, returning
    /// the total count of changes made. Used by optimization passes.
    pub fn for_each_function<F>(&mut self, mut f: F) -> usize
    where
        F: FnMut(&mut IrFunction) -> usize,
    {
        let mut total = 0;
        for func in &mut self.functions {
            if !func.is_declaration {
                total += f(func);
            }
        }
        total
    }
}

impl IrFunction {
    pub fn new(name: String, return_type: IrType, params: Vec<IrParam>, is_variadic: bool) -> Self {
        Self {
            name,
            return_type,
            params,
            blocks: Vec::new(),
            is_variadic,
            is_declaration: false,
            is_static: false,
            stack_size: 0,
        }
    }
}

impl Default for IrModule {
    fn default() -> Self {
        Self::new()
    }
}
