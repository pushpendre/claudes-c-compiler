use crate::common::types::{AddressSpace, IrType};

/// A basic block identifier. Uses a u32 index for zero-cost copies
/// instead of heap-allocated String labels. The block's assembly label
/// is generated on-the-fly during codegen as ".L{id}".
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub u32);

impl BlockId {
    /// Format this block ID as an assembly label (e.g., ".L5").
    #[inline]
    pub fn as_label(&self) -> String {
        format!(".L{}", self.0)
    }
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".L{}", self.0)
    }
}

/// A compilation unit in the IR.
#[derive(Debug)]
pub struct IrModule {
    pub functions: Vec<IrFunction>,
    pub globals: Vec<IrGlobal>,
    pub string_literals: Vec<(String, String)>, // (label, value)
    /// Wide string literals (L"..."): (label, chars as u32 values including null terminator)
    pub wide_string_literals: Vec<(String, Vec<u32>)>,
    /// char16_t string literals (u"..."): (label, chars as u16 values including null terminator)
    pub char16_string_literals: Vec<(String, Vec<u16>)>,
    pub constructors: Vec<String>, // functions with __attribute__((constructor))
    pub destructors: Vec<String>,  // functions with __attribute__((destructor))
    /// Symbol aliases: (alias_name, target_name, is_weak)
    /// From __attribute__((alias("target"))) and __attribute__((weak))
    pub aliases: Vec<(String, String, bool)>,
    /// Top-level asm("...") directives - emitted verbatim in assembly output
    pub toplevel_asm: Vec<String>,
    /// Symbol attribute directives for extern declarations:
    /// (name, is_weak, visibility) - emitted as .weak/.hidden/.protected directives
    pub symbol_attrs: Vec<(String, bool, Option<String>)>,
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
    /// Whether this has __attribute__((common)) - use COMMON linkage.
    pub is_common: bool,
    /// __attribute__((section("..."))) - place in specific ELF section.
    pub section: Option<String>,
    /// __attribute__((weak)) - emit as a weak symbol (STB_WEAK).
    pub is_weak: bool,
    /// __attribute__((visibility("hidden"|"default"|...))) or #pragma GCC visibility.
    pub visibility: Option<String>,
    /// Whether the user specified an explicit alignment via __attribute__((aligned(N))) or _Alignas.
    /// When true, we respect the user's alignment exactly and don't auto-promote to 16.
    pub has_explicit_align: bool,
    /// Whether this global has const qualification (should be placed in .rodata).
    pub is_const: bool,
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
    /// Wide string literal (stored as array of u32 wchar_t values, no null terminator in vec).
    /// The backend emits each value as .long and adds a null terminator.
    WideString(Vec<u32>),
    /// char16_t string literal (stored as array of u16 values, no null terminator in vec).
    /// The backend emits each value as .short and adds a null terminator.
    Char16String(Vec<u16>),
    /// Address of another global (for pointer globals like `const char *s = "hello"`).
    GlobalAddr(String),
    /// Address of a global plus a byte offset (for `&arr[3]`, `&s.field`, etc.).
    GlobalAddrOffset(String, i64),
    /// Compound initializer: a sequence of initializer elements (for arrays/structs
    /// containing address expressions, e.g., `int *ptrs[] = {&a, &b, 0}`).
    Compound(Vec<GlobalInit>),
    /// Difference of two labels (&&lab1 - &&lab2) for computed goto dispatch tables.
    /// Fields: (label1, label2, byte_size) where byte_size is the width of the
    /// resulting integer (4 for int, 8 for long).
    GlobalLabelDiff(String, String, usize),
}

impl GlobalInit {
    /// Returns true if this initializer contains relocations (addresses of globals/labels).
    /// Used to decide between .rodata (no relocations) and .data.rel.ro (has relocations).
    pub fn has_relocations(&self) -> bool {
        match self {
            GlobalInit::GlobalAddr(_) | GlobalInit::GlobalAddrOffset(_, _) | GlobalInit::GlobalLabelDiff(_, _, _) => true,
            GlobalInit::Compound(inits) => inits.iter().any(|i| i.has_relocations()),
            _ => false,
        }
    }

    /// Returns the byte size of this initializer element in a compound context.
    /// Used when flattening nested Compound elements into a parent Compound.
    pub fn byte_size(&self) -> usize {
        match self {
            GlobalInit::Scalar(_) => 1,
            GlobalInit::GlobalAddr(_) | GlobalInit::GlobalAddrOffset(_, _) => 8,
            GlobalInit::Compound(inner) => inner.len(),
            GlobalInit::Array(vals) => vals.len(),
            GlobalInit::Zero => 0,
            GlobalInit::String(s) => s.len(),
            GlobalInit::WideString(ws) => ws.len() * 4,
            GlobalInit::Char16String(cs) => cs.len() * 2,
            GlobalInit::GlobalLabelDiff(_, _, size) => *size,
        }
    }
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
    pub is_inline: bool,      // true if declared with `inline` (used to skip patchable function entries)
    /// True when __attribute__((always_inline)) is present.
    /// These functions must always be inlined at call sites.
    pub is_always_inline: bool,
    /// True when __attribute__((noinline)) is present.
    /// These functions must never be inlined.
    pub is_noinline: bool,
    pub stack_size: usize,
    /// Cached upper bound on Value IDs: all Value IDs in this function are < next_value_id.
    /// Set by lowering/mem2reg/phi_eliminate to avoid expensive full-IR scans.
    /// A value of 0 means "not yet computed" (will fall back to scanning).
    pub next_value_id: u32,
    /// __attribute__((section("..."))) - place in specific ELF section.
    pub section: Option<String>,
    /// __attribute__((visibility("hidden"|"default"|...)))
    pub visibility: Option<String>,
    /// __attribute__((weak)) - emit as a weak symbol (STB_WEAK).
    pub is_weak: bool,
    /// Set by the inlining pass when call sites were inlined into this function.
    /// Used by post-inlining passes (mem2reg re-run, symbol resolution) to know
    /// that non-entry blocks may contain allocas from inlined callees.
    pub has_inlined_calls: bool,
    /// Values corresponding to the allocas created for function parameters.
    /// Tracked explicitly because lowering creates these allocas, but they may
    /// become unused after optimization. The backend uses this to detect dead
    /// param allocas and skip stack slot allocation, reducing frame size.
    pub param_alloca_values: Vec<Value>,
}

/// A function parameter.
#[derive(Debug, Clone)]
pub struct IrParam {
    pub name: String,
    pub ty: IrType,
    /// If this param is a struct/union passed by value, its byte size. None for non-struct params.
    pub struct_size: Option<usize>,
}

/// A basic block in the CFG.
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: BlockId,
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
    /// `align` is the alignment override (0 means use default platform alignment).
    /// `volatile` prevents mem2reg from promoting this alloca to an SSA register.
    /// This is needed for volatile-qualified locals that must survive setjmp/longjmp.
    Alloca { dest: Value, ty: IrType, size: usize, align: usize, volatile: bool },

    /// Dynamic stack allocation: %dest = dynalloca size_operand, align
    /// Used for __builtin_alloca - adjusts stack pointer at runtime.
    DynAlloca { dest: Value, size: Operand, align: usize },

    /// Store to memory: store val, ptr (type indicates size of store)
    /// seg_override: segment register override for x86 (%gs:/%fs:) from named address spaces.
    Store { val: Operand, ptr: Value, ty: IrType, seg_override: AddressSpace },

    /// Load from memory: %dest = load ptr
    /// seg_override: segment register override for x86 (%gs:/%fs:) from named address spaces.
    Load { dest: Value, ptr: Value, ty: IrType, seg_override: AddressSpace },

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
    /// `struct_arg_sizes` indicates which args are struct/union by-value: Some(size) for struct args, None otherwise.
    Call { dest: Option<Value>, func: String, args: Vec<Operand>, arg_types: Vec<IrType>, return_type: IrType, is_variadic: bool, num_fixed_args: usize, struct_arg_sizes: Vec<Option<usize>> },

    /// Indirect function call through a pointer: %dest = call_indirect ptr(args...)
    /// `struct_arg_sizes` indicates which args are struct/union by-value: Some(size) for struct args, None otherwise.
    CallIndirect { dest: Option<Value>, func_ptr: Operand, args: Vec<Operand>, arg_types: Vec<IrType>, return_type: IrType, is_variadic: bool, num_fixed_args: usize, struct_arg_sizes: Vec<Option<usize>> },

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
    /// Each entry in `incoming` is (value, block_id) indicating which value
    /// flows in from which predecessor block.
    Phi {
        dest: Value,
        ty: IrType,
        incoming: Vec<(Operand, BlockId)>,
    },

    /// Get the address of a label (GCC computed goto extension: &&label)
    LabelAddr { dest: Value, label: BlockId },

    /// Get the second F64 return value from a function call (for _Complex double returns).
    /// On x86-64: reads xmm1, on ARM64: reads d1, on RISC-V: reads fa1.
    /// Must appear immediately after a Call/CallIndirect instruction.
    GetReturnF64Second { dest: Value },

    /// Set the second F64 return value before a return (for _Complex double returns).
    /// On x86-64: writes xmm1, on ARM64: writes d1, on RISC-V: writes fa1.
    /// Must appear immediately before a Return terminator.
    SetReturnF64Second { src: Operand },

    /// Get the second F32 return value from a function call (for _Complex float returns on ARM/RISC-V).
    /// On ARM64: reads s1, on RISC-V: reads fa1 as float.
    /// Must appear immediately after a Call/CallIndirect instruction.
    GetReturnF32Second { dest: Value },

    /// Set the second F32 return value before a return (for _Complex float returns on ARM/RISC-V).
    /// On ARM64: writes s1, on RISC-V: writes fa1 as float.
    /// Must appear immediately before a Return terminator.
    SetReturnF32Second { src: Operand },

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
        /// Goto labels for asm goto: (C label name, resolved block ID)
        goto_labels: Vec<(String, BlockId)>,
        /// Symbol names for input operands with "i" constraints (e.g., function names).
        /// One entry per input; None if the input is not a symbol reference.
        /// Used by %P and %a modifiers to emit raw symbol names in inline asm.
        input_symbols: Vec<Option<String>>,
        /// Per-operand address space overrides (outputs first, then inputs).
        /// Non-Default entries cause segment prefix on memory operands (e.g., %gs:).
        seg_overrides: Vec<AddressSpace>,
    },

    /// Target-independent intrinsic operation (fences, SIMD, CRC32, etc.).
    /// Each backend emits the appropriate native instructions for these operations.
    Intrinsic {
        dest: Option<Value>,
        op: IntrinsicOp,
        /// For store ops: destination pointer
        dest_ptr: Option<Value>,
        /// Operand arguments (varies by op)
        args: Vec<Operand>,
    },

    /// Conditional select: %dest = select cond, true_val, false_val
    /// Equivalent to: cond != 0 ? true_val : false_val
    /// Lowered to cmov on x86, csel on ARM, branch-based on RISC-V.
    /// Unlike a branch diamond, both operands are always evaluated.
    Select {
        dest: Value,
        cond: Operand,
        true_val: Operand,
        false_val: Operand,
        ty: IrType,
    },

    /// Save the current stack pointer: %dest = stacksave
    /// Used to capture the SP before VLA allocations so it can be restored later.
    StackSave { dest: Value },

    /// Restore the stack pointer: stackrestore %ptr
    /// Used to reclaim VLA stack space when jumping backward past VLA declarations.
    StackRestore { ptr: Value },
}

/// Block terminator.
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Return from function
    Return(Option<Operand>),

    /// Unconditional branch
    Branch(BlockId),

    /// Conditional branch
    CondBranch { cond: Operand, true_label: BlockId, false_label: BlockId },

    /// Indirect branch (computed goto): goto *addr
    /// possible_targets lists all labels that could be jumped to (for optimization/validation)
    IndirectBranch { target: Operand, possible_targets: Vec<BlockId> },

    /// Unreachable (e.g., after noreturn call)
    Unreachable,
}

/// An operand (either a value reference or a constant).
#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Value(Value),
    Const(IrConst),
}

/// IR constants.
#[derive(Debug, Clone, Copy)]
pub enum IrConst {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    /// 128-bit integer constant (signed or unsigned, stored as i128).
    I128(i128),
    F32(f32),
    F64(f64),
    /// Long double constant with full precision.
    /// - `f64`: approximate value for computations (lossy, 52-bit mantissa).
    ///   On ARM64/RISC-V, this f64 approximation is also used for data section emission
    ///   and runtime codegen since those backends carry f128 values as f64 internally.
    /// - `[u8; 16]`: raw x87 80-bit extended precision bytes (first 10 bytes used, 6 padding).
    ///   Used by x86 for full-precision `fstpt`/`fldt` data section emission and by
    ///   ARM64/RISC-V backends for function call argument expansion via `f64_to_f128_bytes`.
    LongDouble(f64, [u8; 16]),
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
    I128(i128),
    F32(u32),
    F64(u64),
    LongDouble([u8; 16]),
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
            IrConst::I8(0) | IrConst::I16(0) | IrConst::I32(0) | IrConst::I64(0) | IrConst::I128(0) => true,
            IrConst::F32(v) => *v == 0.0,
            IrConst::F64(v) => *v == 0.0,
            IrConst::LongDouble(v, _) => *v == 0.0,
            IrConst::Zero => true,
            _ => false,
        }
    }

    /// Create a LongDouble constant from an f64 value (low precision - bytes derived from f64).
    /// Use this when no full-precision source text is available.
    pub fn long_double(v: f64) -> IrConst {
        IrConst::LongDouble(v, crate::common::long_double::f64_to_x87_bytes_simple(v))
    }

    /// Create a LongDouble constant with full-precision x87 bytes.
    pub fn long_double_with_bytes(v: f64, bytes: [u8; 16]) -> IrConst {
        IrConst::LongDouble(v, bytes)
    }

    /// Get the raw x87 bytes from a LongDouble constant.
    pub fn long_double_bytes(&self) -> Option<&[u8; 16]> {
        match self {
            IrConst::LongDouble(_, bytes) => Some(bytes),
            _ => None,
        }
    }

    /// Get the x87 80-bit byte representation for any float constant.
    /// For LongDouble, returns the stored full-precision bytes.
    /// For F64/F32, converts to x87 format (widening, zero-fills lower mantissa bits).
    /// For integer types, converts to f64 first then to x87 bytes.
    pub fn x87_bytes(&self) -> [u8; 16] {
        match self {
            IrConst::LongDouble(_, bytes) => *bytes,
            _ => {
                if let Some(v) = self.to_f64() {
                    crate::common::long_double::f64_to_x87_bytes_simple(v)
                } else {
                    [0u8; 16]
                }
            }
        }
    }

    /// Returns true if this constant is one (integer only).
    pub fn is_one(&self) -> bool {
        matches!(self, IrConst::I8(1) | IrConst::I16(1) | IrConst::I32(1) | IrConst::I64(1) | IrConst::I128(1))
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
            IrConst::I128(v) => ConstHashKey::I128(*v),
            IrConst::F32(v) => ConstHashKey::F32(v.to_bits()),
            IrConst::F64(v) => ConstHashKey::F64(v.to_bits()),
            IrConst::LongDouble(_, bytes) => ConstHashKey::LongDouble(*bytes),
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
            IrConst::I128(v) => Some(*v as f64),
            IrConst::F32(v) => Some(*v as f64),
            IrConst::F64(v) => Some(*v),
            IrConst::LongDouble(v, _) => Some(*v),
            IrConst::Zero => Some(0.0),
        }
    }

    /// Cast a float value (as f64) to the target IR type, producing a new IrConst.
    /// For unsigned integer targets, converts via the unsigned type first to get correct
    /// wrapping behavior (e.g., 200.0 as u8 = 200, not saturated to i8 max).
    pub fn cast_float_to_target(fv: f64, target: IrType) -> Option<IrConst> {
        Some(match target {
            IrType::F64 => IrConst::F64(fv),
            IrType::F128 => IrConst::long_double(fv),
            IrType::F32 => IrConst::F32(fv as f32),
            IrType::I8 => IrConst::I8(fv as i8),
            IrType::U8 => IrConst::I8(fv as u8 as i8),
            IrType::I16 => IrConst::I16(fv as i16),
            IrType::U16 => IrConst::I16(fv as u16 as i16),
            IrType::I32 => IrConst::I32(fv as i32),
            IrType::U32 => IrConst::I64(fv as u32 as i64),
            IrType::I64 | IrType::Ptr => IrConst::I64(fv as i64),
            IrType::U64 => IrConst::I64(fv as u64 as i64),
            IrType::I128 => IrConst::I128(fv as i128),
            IrType::U128 => IrConst::I128(fv as u128 as i128),
            _ => return None,
        })
    }

    /// Cast a long double (with raw x87 bytes) to the target IR type.
    /// Uses full 80-bit precision for integer conversions, unlike cast_float_to_target
    /// which only uses the f64 approximation (52-bit mantissa).
    pub fn cast_long_double_to_target(fv: f64, bytes: &[u8; 16], target: IrType) -> Option<IrConst> {
        use crate::common::long_double::{x87_bytes_to_i64, x87_bytes_to_u64, x87_bytes_to_i128, x87_bytes_to_u128};
        Some(match target {
            IrType::F64 => IrConst::F64(fv),
            IrType::F128 => IrConst::long_double_with_bytes(fv, *bytes),
            IrType::F32 => IrConst::F32(fv as f32),
            // For integer targets, use full x87 precision
            IrType::I8 => IrConst::I8(x87_bytes_to_i64(bytes)? as i8),
            IrType::U8 => IrConst::I8(x87_bytes_to_u64(bytes)? as u8 as i8),
            IrType::I16 => IrConst::I16(x87_bytes_to_i64(bytes)? as i16),
            IrType::U16 => IrConst::I16(x87_bytes_to_u64(bytes)? as u16 as i16),
            IrType::I32 => IrConst::I32(x87_bytes_to_i64(bytes)? as i32),
            IrType::U32 => IrConst::I64(x87_bytes_to_u64(bytes)? as u32 as i64),
            IrType::I64 | IrType::Ptr => IrConst::I64(x87_bytes_to_i64(bytes)?),
            IrType::U64 => IrConst::I64(x87_bytes_to_u64(bytes)? as i64),
            IrType::I128 => IrConst::I128(x87_bytes_to_i128(bytes)?),
            IrType::U128 => IrConst::I128(x87_bytes_to_u128(bytes)? as i128),
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
            IrConst::I128(v) => Some(*v as i64),
            IrConst::Zero => Some(0),
            IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(..) => None,
        }
    }

    /// Extract as i128 (integer constants only; floats return None).
    /// Unlike to_i64(), this preserves the full 128-bit value.
    pub fn to_i128(&self) -> Option<i128> {
        match self {
            IrConst::I8(v) => Some(*v as i128),
            IrConst::I16(v) => Some(*v as i128),
            IrConst::I32(v) => Some(*v as i128),
            IrConst::I64(v) => Some(*v as i128),
            IrConst::I128(v) => Some(*v),
            IrConst::Zero => Some(0),
            IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(..) => None,
        }
    }

    /// Extract as u64 (integer constants only; floats return None).
    pub fn to_u64(&self) -> Option<u64> {
        match self {
            IrConst::I8(v) => Some(*v as u64),
            IrConst::I16(v) => Some(*v as u64),
            IrConst::I32(v) => Some(*v as u64),
            IrConst::I64(v) => Some(*v as u64),
            IrConst::I128(v) => Some(*v as u64),
            IrConst::Zero => Some(0),
            IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(..) => None,
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
    /// For long double, emits f64 bit pattern + zero padding (ARM64/RISC-V format).
    /// Use `push_le_bytes_x86` for x86 targets that need x87 80-bit extended precision.
    pub fn push_le_bytes(&self, out: &mut Vec<u8>, size: usize) {
        match self {
            IrConst::F32(v) => {
                out.extend_from_slice(&v.to_bits().to_le_bytes());
            }
            IrConst::F64(v) => {
                out.extend_from_slice(&v.to_bits().to_le_bytes());
            }
            IrConst::LongDouble(f64_val, _) => {
                // ARM64/RISC-V: store f64 approximation in the low 8 bytes of the
                // 16-byte slot. Codegen carries f128 values as f64 internally, so
                // static data must match for consistent load/store behavior.
                // TODO: store full f128 precision when codegen supports it natively.
                out.extend_from_slice(&f64_val.to_bits().to_le_bytes());
                out.extend_from_slice(&[0u8; 8]);
            }
            IrConst::I128(v) => {
                let le_bytes = v.to_le_bytes();
                out.extend_from_slice(&le_bytes[..size.min(16)]);
            }
            _ => {
                let le_bytes = self.to_i64().unwrap_or(0).to_le_bytes();
                if size <= 8 {
                    out.extend_from_slice(&le_bytes[..size]);
                } else {
                    // For sizes > 8 (e.g. __int128), zero-extend
                    out.extend_from_slice(&le_bytes);
                    out.extend_from_slice(&vec![0u8; size - 8]);
                }
            }
        }
    }

    /// Convert to bytes for x86-64 targets, using x87 80-bit extended precision
    /// for long doubles. This ensures correct memory representation when code
    /// type-puns long doubles through unions or integer arrays (e.g., TCC's CValue).
    pub fn push_le_bytes_x86(&self, out: &mut Vec<u8>, size: usize) {
        match self {
            IrConst::LongDouble(_, bytes) => {
                // x86-64: emit stored x87 80-bit bytes (10 bytes) + 6 zero padding bytes.
                out.extend_from_slice(&bytes[..10]);
                out.extend_from_slice(&[0u8; 6]); // pad to 16 bytes
            }
            _ => self.push_le_bytes(out, size),
        }
    }

    /// Construct an IrConst of the given type from an i64 value.
    ///
    /// Store unsigned sub-64-bit types (U8, U16, U32) as I64 with zero-extended
    /// values to preserve unsigned semantics. Storing them in their native
    /// IrConst variants (I8, I16, I32) would cause to_i64() to sign-extend,
    /// turning e.g. U8(255) into -1 instead of 255.
    pub fn from_i64(val: i64, ty: IrType) -> Self {
        match ty {
            IrType::I8 => IrConst::I8(val as i8),
            // U8: store as I64 with zero-extended value to preserve unsigned semantics.
            // Using I8 would sign-extend when to_i64() is called (e.g.,
            // I8(0xFF as i8) = I8(-1) becomes -1 instead of 255).
            IrType::U8 => IrConst::I64(val as u8 as i64),
            IrType::I16 => IrConst::I16(val as i16),
            // U16: same as U8 - store as I64 with zero-extended value.
            IrType::U16 => IrConst::I64(val as u16 as i64),
            IrType::I32 => IrConst::I32(val as i32),
            // U32: store as I64 with zero-extended value to preserve unsigned semantics.
            // Using I32 would sign-extend when loaded as a 64-bit immediate (e.g.,
            // I32(-2147483648) becomes 0xFFFFFFFF80000000 instead of 0x0000000080000000).
            IrType::U32 => IrConst::I64(val as u32 as i64),
            IrType::I128 | IrType::U128 => IrConst::I128(val as i128),
            IrType::F32 => IrConst::F32(val as f32),
            IrType::F64 => IrConst::F64(val as f64),
            IrType::F128 => IrConst::long_double(val as f64),
            _ => IrConst::I64(val),
        }
    }

    /// Coerce this constant to match a target IrType, with optional source type for signedness.
    pub fn coerce_to_with_src(&self, target_ty: IrType, src_ty: Option<IrType>) -> IrConst {
        // Check if already the right type
        match (self, target_ty) {
            (IrConst::I8(_), IrType::I8 | IrType::U8) => return self.clone(),
            (IrConst::I16(_), IrType::I16 | IrType::U16) => return self.clone(),
            (IrConst::I32(_), IrType::I32) => return self.clone(),
            // U32 is stored as I64 (zero-extended), so I32 must be converted
            (IrConst::I64(_), IrType::U32) => return self.clone(),
            (IrConst::I64(_), IrType::I64 | IrType::U64 | IrType::Ptr) => return self.clone(),
            (IrConst::I128(_), IrType::I128 | IrType::U128) => return self.clone(),
            (IrConst::F32(_), IrType::F32) => return self.clone(),
            (IrConst::F64(_), IrType::F64) => return self.clone(),
            (IrConst::LongDouble(..), IrType::F64 | IrType::F128) => return self.clone(),
            _ => {}
        }
        // Convert integer types via from_i64, with unsigned-aware paths
        if let Some(int_val) = self.to_i64() {
            // When the source type is unsigned, we need to zero-extend (not sign-extend)
            // when widening to a larger type. Mask to the source width to get the correct
            // unsigned value (e.g., U32 0xFFFFFFF8 = 4294967288, not -8).
            if src_ty.map_or(false, |t| t.is_unsigned()) {
                // Mask to the source type's width to get the correct unsigned value
                let src_size = src_ty.unwrap().size();
                let uint_val = match src_size {
                    1 => (int_val as u8) as u64,
                    2 => (int_val as u16) as u64,
                    4 => (int_val as u32) as u64,
                    8 => int_val as u64,
                    _ => int_val as u64,
                };
                if target_ty.is_float() {
                    return match target_ty {
                        IrType::F32 => IrConst::F32(uint_val as f32),
                        IrType::F64 => IrConst::F64(uint_val as f64),
                        IrType::F128 => IrConst::long_double(uint_val as f64),
                        _ => IrConst::I64(uint_val as i64),
                    };
                }
                // For integer targets, use the zero-extended value
                return IrConst::from_i64(uint_val as i64, target_ty);
            }
            return IrConst::from_i64(int_val, target_ty);
        }
        // Convert float types: extract as f64 and use cast_float_to_target
        if let Some(fv) = self.to_f64() {
            if let Some(result) = Self::cast_float_to_target(fv, target_ty) {
                return result;
            }
        }
        self.clone()
    }

    /// Coerce this constant to match a target IrType (assumes signed source for int-to-float).
    pub fn coerce_to(&self, target_ty: IrType) -> IrConst {
        self.coerce_to_with_src(target_ty, None)
    }

    /// Normalize a constant for _Bool storage: any nonzero value becomes I8(1), zero becomes I8(0).
    /// Implements C11 6.3.1.2: "When any scalar value is converted to _Bool, the result
    /// is 0 if the value compares equal to 0; otherwise, the result is 1."
    pub fn bool_normalize(&self) -> IrConst {
        IrConst::I8(if self.is_zero() { 0 } else { 1 })
    }

    /// Get the zero constant for a given IR type.
    pub fn zero(ty: IrType) -> IrConst {
        match ty {
            IrType::I8 | IrType::U8 => IrConst::I8(0),
            IrType::I16 | IrType::U16 => IrConst::I16(0),
            IrType::I32 => IrConst::I32(0),
            IrType::U32 => IrConst::I64(0),
            IrType::F32 => IrConst::F32(0.0),
            IrType::F64 => IrConst::F64(0.0),
            IrType::F128 => IrConst::long_double(0.0),
            _ => IrConst::I64(0),
        }
    }

    /// Get the one constant for a given IR type.
    pub fn one(ty: IrType) -> IrConst {
        match ty {
            IrType::I8 | IrType::U8 => IrConst::I8(1),
            IrType::I16 | IrType::U16 => IrConst::I16(1),
            IrType::I32 => IrConst::I32(1),
            IrType::U32 => IrConst::I64(1),
            IrType::F32 => IrConst::F32(1.0),
            IrType::F64 => IrConst::F64(1.0),
            IrType::F128 => IrConst::long_double(1.0),
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

    /// Returns true if this operation can trap at runtime (e.g., divide by zero causes SIGFPE).
    /// Such operations must not be speculatively executed by if-conversion.
    pub fn can_trap(self) -> bool {
        matches!(self, IrBinOp::SDiv | IrBinOp::UDiv | IrBinOp::SRem | IrBinOp::URem)
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

/// Target-independent intrinsic operation kinds.
/// Named after their semantic operation rather than any specific architecture.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicOp {
    /// Memory fence operations (no dest, no args beyond optional ptr)
    Lfence,
    Mfence,
    Sfence,
    Pause,
    Clflush,
    /// Non-temporal stores: movnti (32-bit), movnti64 (64-bit), movntdq (128-bit), movntpd (128-bit double)
    Movnti,
    Movnti64,
    Movntdq,
    Movntpd,
    /// Load/store 128-bit unaligned
    Loaddqu,
    Storedqu,
    /// Compare equal packed bytes (16 bytes)
    Pcmpeqb128,
    /// Compare equal packed dwords (4x32)
    Pcmpeqd128,
    /// Subtract packed unsigned saturated bytes
    Psubusb128,
    /// Bitwise OR/AND/XOR on 128-bit
    Por128,
    Pand128,
    Pxor128,
    /// Move byte mask (pmovmskb) - returns i32
    Pmovmskb128,
    /// Set all bytes to value (splat)
    SetEpi8,
    /// Set all dwords to value (splat)
    SetEpi32,
    /// CRC32 accumulate
    Crc32_8,
    Crc32_16,
    Crc32_32,
    Crc32_64,
    /// __builtin_frame_address(0) - returns current frame pointer
    FrameAddress,
    /// __builtin_return_address(0) - returns current return address
    ReturnAddress,
    /// Scalar square root: sqrtsd/sqrtss on x86, fsqrt on ARM/RISC-V
    /// args[0] = input float value; dest = sqrt result
    SqrtF32,
    SqrtF64,
    /// Scalar absolute value: bitwise AND with sign mask on x86, fabs on ARM/RISC-V
    /// args[0] = input float value; dest = |x|
    FabsF32,
    FabsF64,
}

impl IntrinsicOp {
    /// Returns true if this intrinsic is a pure function (no side effects, result depends
    /// only on inputs). Pure intrinsics can be dead-code eliminated if their result is unused.
    pub fn is_pure(&self) -> bool {
        matches!(self,
            IntrinsicOp::SqrtF32 | IntrinsicOp::SqrtF64 |
            IntrinsicOp::FabsF32 | IntrinsicOp::FabsF64
        )
    }
}

impl Instruction {
    /// Get the destination value defined by this instruction, if any.
    /// Instructions like Store, Memcpy, VaStart, VaEnd, VaCopy, AtomicStore,
    /// Fence, and InlineAsm produce no value.
    /// Returns the result IR type of this instruction, if any.
    /// Used to determine stack slot sizes for 128-bit values.
    pub fn result_type(&self) -> Option<IrType> {
        match self {
            Instruction::Load { ty, .. } => Some(*ty),
            Instruction::BinOp { ty, .. } => Some(*ty),
            Instruction::UnaryOp { ty, .. } => Some(*ty),
            Instruction::Cmp { .. } => Some(IrType::I8), // comparisons produce i8
            Instruction::Cast { to_ty, .. } => Some(*to_ty),
            Instruction::Call { return_type, .. }
            | Instruction::CallIndirect { return_type, .. } => Some(*return_type),
            Instruction::VaArg { result_ty, .. } => Some(*result_ty),
            Instruction::AtomicRmw { ty, .. } => Some(*ty),
            Instruction::AtomicCmpxchg { ty, returns_bool, .. } => {
                if *returns_bool { Some(IrType::I8) } else { Some(*ty) }
            }
            Instruction::AtomicLoad { ty, .. } => Some(*ty),
            // Alloca, GEP, GlobalAddr, Copy, DynAlloca, LabelAddr, StackSave produce pointers or copy types
            Instruction::Alloca { .. } | Instruction::DynAlloca { .. }
            | Instruction::GetElementPtr { .. } | Instruction::GlobalAddr { .. }
            | Instruction::LabelAddr { .. }
            | Instruction::StackSave { .. } => Some(IrType::Ptr),
            Instruction::Copy { .. } => None, // unknown without tracking
            Instruction::Phi { ty, .. } => Some(*ty),
            Instruction::Select { ty, .. } => Some(*ty),
            Instruction::Intrinsic { op, .. } => match op {
                IntrinsicOp::SqrtF32 | IntrinsicOp::FabsF32 => Some(IrType::F32),
                IntrinsicOp::SqrtF64 | IntrinsicOp::FabsF64 => Some(IrType::F64),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn dest(&self) -> Option<Value> {
        match self {
            Instruction::Alloca { dest, .. }
            | Instruction::DynAlloca { dest, .. }
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
            | Instruction::LabelAddr { dest, .. }
            | Instruction::GetReturnF64Second { dest }
            | Instruction::GetReturnF32Second { dest }
            | Instruction::Select { dest, .. }
            | Instruction::StackSave { dest } => Some(*dest),
            Instruction::Call { dest, .. }
            | Instruction::CallIndirect { dest, .. } => *dest,
            Instruction::Intrinsic { dest, .. } => *dest,
            Instruction::Store { .. }
            | Instruction::Memcpy { .. }
            | Instruction::VaStart { .. }
            | Instruction::VaEnd { .. }
            | Instruction::VaCopy { .. }
            | Instruction::AtomicStore { .. }
            | Instruction::Fence { .. }
            | Instruction::SetReturnF64Second { .. }
            | Instruction::SetReturnF32Second { .. }
            | Instruction::InlineAsm { .. }
            | Instruction::StackRestore { .. } => None,
        }
    }
}

impl IrModule {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            globals: Vec::new(),
            string_literals: Vec::new(),
            wide_string_literals: Vec::new(),
            char16_string_literals: Vec::new(),
            constructors: Vec::new(),
            destructors: Vec::new(),
            aliases: Vec::new(),
            toplevel_asm: Vec::new(),
            symbol_attrs: Vec::new(),
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
            is_inline: false,
            is_always_inline: false,
            is_noinline: false,
            stack_size: 0,
            next_value_id: 0,
            section: None,
            visibility: None,
            is_weak: false,
            has_inlined_calls: false,
            param_alloca_values: Vec::new(),
        }
    }

    /// Return the highest Value ID defined (as a destination) in this function, or 0 if empty.
    /// Uses the cached `next_value_id` if available, otherwise falls back to scanning.
    /// Useful for sizing flat lookup tables indexed by Value ID.
    #[inline]
    pub fn max_value_id(&self) -> u32 {
        if self.next_value_id > 0 {
            // next_value_id is the first *unused* ID, so max used is one less
            return self.next_value_id - 1;
        }
        // Fallback: scan all instructions (expensive)
        let mut max_id: u32 = 0;
        for block in &self.blocks {
            for inst in &block.instructions {
                if let Some(v) = inst.dest() {
                    if v.0 > max_id {
                        max_id = v.0;
                    }
                }
            }
        }
        max_id
    }
}

impl Default for IrModule {
    fn default() -> Self {
        Self::new()
    }
}
