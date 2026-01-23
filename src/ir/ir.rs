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
    Call { dest: Option<Value>, func: String, args: Vec<Operand>, arg_types: Vec<IrType>, return_type: IrType, is_variadic: bool },

    /// Indirect function call through a pointer: %dest = call_indirect ptr(args...)
    CallIndirect { dest: Option<Value>, func_ptr: Operand, args: Vec<Operand>, arg_types: Vec<IrType>, return_type: IrType, is_variadic: bool },

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
    Zero,
}

impl IrConst {
    /// Extract as i64 (integer constants only; floats return None).
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            IrConst::I8(v) => Some(*v as i64),
            IrConst::I16(v) => Some(*v as i64),
            IrConst::I32(v) => Some(*v as i64),
            IrConst::I64(v) => Some(*v),
            IrConst::Zero => Some(0),
            IrConst::F32(_) | IrConst::F64(_) => None,
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
            IrConst::F32(_) | IrConst::F64(_) => None,
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

    /// Coerce this constant to match a target IrType.
    pub fn coerce_to(&self, target_ty: IrType) -> IrConst {
        // Check if already the right type
        match (self, target_ty) {
            (IrConst::I8(_), IrType::I8 | IrType::U8) => return self.clone(),
            (IrConst::I16(_), IrType::I16 | IrType::U16) => return self.clone(),
            (IrConst::I32(_), IrType::I32 | IrType::U32) => return self.clone(),
            (IrConst::I64(_), IrType::I64 | IrType::U64 | IrType::Ptr) => return self.clone(),
            (IrConst::F32(_), IrType::F32) => return self.clone(),
            (IrConst::F64(_), IrType::F64) => return self.clone(),
            _ => {}
        }
        // Convert integer types
        if let Some(int_val) = self.to_i64() {
            return IrConst::from_i64(int_val, target_ty);
        }
        // Convert float types
        match (self, target_ty) {
            (IrConst::F64(v), IrType::F32) => IrConst::F32(*v as f32),
            (IrConst::F32(v), IrType::F64) => IrConst::F64(*v as f64),
            (IrConst::F64(v), IrType::I8 | IrType::U8) => IrConst::I8(*v as i8),
            (IrConst::F64(v), IrType::I16 | IrType::U16) => IrConst::I16(*v as i16),
            (IrConst::F64(v), IrType::I32 | IrType::U32) => IrConst::I32(*v as i32),
            (IrConst::F64(v), IrType::I64 | IrType::U64) => IrConst::I64(*v as i64),
            (IrConst::F32(v), IrType::I8 | IrType::U8) => IrConst::I8(*v as i8),
            (IrConst::F32(v), IrType::I16 | IrType::U16) => IrConst::I16(*v as i16),
            (IrConst::F32(v), IrType::I32 | IrType::U32) => IrConst::I32(*v as i32),
            (IrConst::F32(v), IrType::I64 | IrType::U64) => IrConst::I64(*v as i64),
            _ => self.clone(),
        }
    }

    /// Get the zero constant for a given IR type.
    pub fn zero(ty: IrType) -> IrConst {
        match ty {
            IrType::I8 | IrType::U8 => IrConst::I8(0),
            IrType::I16 | IrType::U16 => IrConst::I16(0),
            IrType::I32 | IrType::U32 => IrConst::I32(0),
            IrType::F32 => IrConst::F32(0.0),
            IrType::F64 => IrConst::F64(0.0),
            _ => IrConst::I64(0),
        }
    }
}

/// Binary operations.
#[derive(Debug, Clone, Copy)]
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

/// Unary operations.
#[derive(Debug, Clone, Copy)]
pub enum IrUnaryOp {
    Neg,
    Not,
}

/// Comparison operations.
#[derive(Debug, Clone, Copy)]
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

impl IrModule {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            globals: Vec::new(),
            string_literals: Vec::new(),
        }
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
