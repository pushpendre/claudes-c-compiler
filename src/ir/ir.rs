use crate::common::source::Span;
use crate::common::types::{AddressSpace, EightbyteClass, IrType};

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
    /// __attribute__((used)) - prevent dead code elimination of this symbol.
    pub is_used: bool,
    /// Whether this global has _Thread_local or __thread storage class.
    /// Thread-local globals are placed in .tdata/.tbss and accessed via TLS mechanisms.
    pub is_thread_local: bool,
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
    /// Visit every symbol name referenced by this initializer.
    /// Calls `f` with each global/label name found in GlobalAddr, GlobalAddrOffset,
    /// and GlobalLabelDiff variants, recursing into Compound children.
    pub fn for_each_ref<F: FnMut(&str)>(&self, f: &mut F) {
        match self {
            GlobalInit::GlobalAddr(name) | GlobalInit::GlobalAddrOffset(name, _) => {
                f(name);
            }
            GlobalInit::GlobalLabelDiff(label1, label2, _) => {
                f(label1);
                f(label2);
            }
            GlobalInit::Compound(fields) => {
                for field in fields {
                    field.for_each_ref(f);
                }
            }
            _ => {}
        }
    }

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
            // Use target pointer size: 4 bytes on i686, 8 bytes on 64-bit targets
            GlobalInit::GlobalAddr(_) | GlobalInit::GlobalAddrOffset(_, _) => {
                crate::common::types::target_ptr_size()
            }
            GlobalInit::Compound(inner) => inner.len(),
            GlobalInit::Array(vals) => vals.len(),
            GlobalInit::Zero => 0,
            GlobalInit::String(s) => s.len(),
            GlobalInit::WideString(ws) => ws.len() * 4,
            GlobalInit::Char16String(cs) => cs.len() * 2,
            GlobalInit::GlobalLabelDiff(_, _, size) => *size,
        }
    }

    /// Returns the total number of bytes that will be emitted for this initializer.
    /// Unlike `byte_size()`, this correctly accounts for GlobalAddr entries
    /// (pointer-sized: 4 bytes on i686, 8 bytes on 64-bit) inside Compound initializers.
    pub fn emitted_byte_size(&self) -> usize {
        match self {
            GlobalInit::Scalar(_) => 1,
            // Use target pointer size: 4 bytes on i686, 8 bytes on 64-bit targets
            GlobalInit::GlobalAddr(_) | GlobalInit::GlobalAddrOffset(_, _) => {
                crate::common::types::target_ptr_size()
            }
            GlobalInit::Compound(inner) => inner.iter().map(|e| e.emitted_byte_size()).sum(),
            GlobalInit::Array(vals) => vals.len(),
            GlobalInit::Zero => 0,
            GlobalInit::String(s) => s.len() + 1,
            GlobalInit::WideString(ws) => (ws.len() + 1) * 4,
            GlobalInit::Char16String(cs) => (cs.len() + 1) * 2,
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
    /// __attribute__((used)) - prevent dead code elimination of this symbol.
    pub is_used: bool,
    /// __attribute__((fastcall)) - i386 fastcall calling convention.
    /// First two integer/pointer args passed in ecx/edx instead of stack.
    pub is_fastcall: bool,
    /// Set by the inlining pass when call sites were inlined into this function.
    /// Used by post-inlining passes (mem2reg re-run, symbol resolution) to know
    /// that non-entry blocks may contain allocas from inlined callees.
    pub has_inlined_calls: bool,
    /// Values corresponding to the allocas created for function parameters.
    /// Tracked explicitly because lowering creates these allocas, but they may
    /// become unused after optimization. The backend uses this to detect dead
    /// param allocas and skip stack slot allocation, reducing frame size.
    pub param_alloca_values: Vec<Value>,
    /// True when the function returns a large struct via hidden pointer (sret).
    /// On i386 SysV ABI, such functions must use `ret $4` to pop the hidden
    /// pointer argument from the caller's stack.
    pub uses_sret: bool,
    /// Block IDs referenced by static local variable initializers via &&label.
    /// These blocks must be kept reachable and not merged away by CFG simplify,
    /// since their labels appear in global data (.quad .L3) and must resolve.
    pub global_init_label_blocks: Vec<BlockId>,
}

/// A function parameter.
#[derive(Debug, Clone)]
pub struct IrParam {
    pub name: String,
    pub ty: IrType,
    /// If this param is a struct/union passed by value, its byte size. None for non-struct params.
    pub struct_size: Option<usize>,
    /// Per-eightbyte SysV ABI classification for struct params (x86-64 only).
    /// Empty for non-struct params or when classification is not applicable.
    /// Each entry indicates whether that eightbyte should use SSE or GP registers.
    pub struct_eightbyte_classes: Vec<crate::common::types::EightbyteClass>,
}

/// A basic block in the CFG.
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: BlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
    /// Source location spans for each instruction, parallel to `instructions`.
    /// Used by the backend to emit .file/.loc directives when compiling with -g.
    /// Empty when debug info is not being tracked (non-debug builds).
    pub source_spans: Vec<Span>,
}

/// An SSA value reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(pub u32);

/// Shared call metadata for both direct and indirect function calls.
///
/// This struct consolidates the fields that are common to `Instruction::Call` and
/// `Instruction::CallIndirect`, avoiding duplication across match arms and making
/// it easier to add new call-related fields in the future.
#[derive(Debug, Clone)]
pub struct CallInfo {
    /// Destination value for the return, or None for void calls.
    pub dest: Option<Value>,
    /// Argument operands.
    pub args: Vec<Operand>,
    /// Type of each argument (parallel to `args`).
    pub arg_types: Vec<IrType>,
    /// Return type of the callee.
    pub return_type: IrType,
    /// Whether the callee is variadic.
    pub is_variadic: bool,
    /// Number of named (non-variadic) parameters in the callee's prototype.
    /// For non-variadic calls, this equals args.len().
    pub num_fixed_args: usize,
    /// Which args are struct/union by-value: Some(size) for struct args, None otherwise.
    pub struct_arg_sizes: Vec<Option<usize>>,
    /// Per-eightbyte SysV ABI classification for struct args (for x86-64 SSE-class passing).
    pub struct_arg_classes: Vec<Vec<EightbyteClass>>,
    /// True if the call uses a hidden pointer argument for struct returns (i386 SysV ABI).
    pub is_sret: bool,
    /// True if the callee uses the fastcall calling convention.
    pub is_fastcall: bool,
}

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

    /// Direct function call: %dest = call func(args...)
    Call { func: String, info: CallInfo },

    /// Indirect function call through a pointer: %dest = call_indirect ptr(args...)
    CallIndirect { func_ptr: Operand, info: CallInfo },

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

    /// va_arg for struct/union types: read a struct from the va_list into a
    /// pre-allocated buffer. `dest_ptr` is a pointer to the buffer (an alloca),
    /// `size` is the struct size in bytes. The backend reads the appropriate
    /// number of bytes from the va_list (registers or overflow area) and stores
    /// them at `dest_ptr`, advancing the va_list state appropriately.
    VaArgStruct { dest_ptr: Value, va_list_ptr: Value, size: usize },

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

    /// Get the second F128 return value from a function call (for _Complex long double returns on x86-64).
    /// On x86-64: reads st(0) after the first fstpt has already popped the real part.
    /// Must appear immediately after a Call/CallIndirect instruction.
    GetReturnF128Second { dest: Value },

    /// Set the second F128 return value before a return (for _Complex long double returns on x86-64).
    /// On x86-64: loads an additional value onto the x87 FPU stack as st(1).
    /// Must appear immediately before a Return terminator.
    SetReturnF128Second { src: Operand },

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

    /// Reference to a function parameter value: %dest = paramref param_idx
    /// Represents the incoming value of the function's `param_idx`-th parameter.
    /// Emitted in the entry block alongside param alloca + store to make parameter
    /// initial values visible in the IR, allowing mem2reg to promote param allocas
    /// to SSA and enabling constant propagation through reassigned parameters.
    /// The backend translates this to a load from the appropriate argument register
    /// or stack slot according to the calling convention.
    ParamRef { dest: Value, param_idx: usize, ty: IrType },
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

    /// Switch dispatch via jump table.
    /// Implements dense switch statements efficiently: the backend emits a jump table
    /// instead of a chain of compare-and-branch instructions.
    /// `val` is the switch expression (must be integer type).
    /// `cases` maps case values to target block IDs.
    /// `default` is the fallback block.
    Switch {
        val: Operand,
        cases: Vec<(i64, BlockId)>,
        default: BlockId,
    },

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
    /// - `[u8; 16]`: IEEE 754 binary128 (f128) bytes with full 112-bit mantissa precision.
    ///   For ARM64/RISC-V, these bytes are used directly for data emission.
    ///   For x86, they are converted to x87 80-bit format at emission time.
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
        IrConst::LongDouble(v, crate::common::long_double::f64_to_f128_bytes_lossless(v))
    }

    /// Create a LongDouble constant with full-precision f128 bytes.
    pub fn long_double_with_bytes(v: f64, bytes: [u8; 16]) -> IrConst {
        IrConst::LongDouble(v, bytes)
    }

    /// Create a LongDouble constant from a signed i64 with full precision.
    /// Uses direct integer-to-f128 conversion to preserve all 64 bits of precision.
    pub fn long_double_from_i64(val: i64) -> IrConst {
        let bytes = crate::common::long_double::i64_to_f128_bytes(val);
        IrConst::LongDouble(val as f64, bytes)
    }

    /// Create a LongDouble constant from an unsigned u64 with full precision.
    /// Uses direct integer-to-f128 conversion to preserve all 64 bits of precision.
    pub fn long_double_from_u64(val: u64) -> IrConst {
        let bytes = crate::common::long_double::u64_to_f128_bytes(val);
        IrConst::LongDouble(val as f64, bytes)
    }

    /// Create a LongDouble constant from an unsigned u128 with maximum precision.
    /// Uses direct integer-to-f128 conversion. Values up to 2^112 are exact;
    /// larger values are rounded (f128 has 112-bit mantissa).
    pub fn long_double_from_u128(val: u128) -> IrConst {
        let bytes = crate::common::long_double::u128_to_f128_bytes(val);
        IrConst::LongDouble(val as f64, bytes)
    }

    /// Create a LongDouble constant from a signed i128 with maximum precision.
    /// Uses direct integer-to-f128 conversion. Values with magnitude up to 2^112
    /// are exact; larger values are rounded.
    pub fn long_double_from_i128(val: i128) -> IrConst {
        let bytes = crate::common::long_double::i128_to_f128_bytes(val);
        IrConst::LongDouble(val as f64, bytes)
    }

    /// Get the raw f128 bytes from a LongDouble constant.
    pub fn long_double_bytes(&self) -> Option<&[u8; 16]> {
        match self {
            IrConst::LongDouble(_, bytes) => Some(bytes),
            _ => None,
        }
    }

    /// Get the x87 80-bit byte representation for any float constant.
    /// For LongDouble, converts stored f128 bytes to x87 format (lossy: 112→64 bit mantissa).
    /// For F64/F32, converts to x87 format (widening, zero-fills lower mantissa bits).
    /// For integer types, converts to f64 first then to x87 bytes.
    pub fn x87_bytes(&self) -> [u8; 16] {
        match self {
            IrConst::LongDouble(_, f128_bytes) => {
                crate::common::long_double::f128_bytes_to_x87_bytes(f128_bytes)
            }
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
            IrType::I64 => IrConst::I64(fv as i64),
            IrType::Ptr => IrConst::ptr_int(fv as i64),
            IrType::U64 => IrConst::I64(fv as u64 as i64),
            IrType::I128 => IrConst::I128(fv as i128),
            IrType::U128 => IrConst::I128(fv as u128 as i128),
            _ => return None,
        })
    }

    /// Cast a long double (with f128 bytes) to the target IR type.
    /// Uses full precision for integer conversions.
    pub fn cast_long_double_to_target(fv: f64, bytes: &[u8; 16], target: IrType) -> Option<IrConst> {
        use crate::common::long_double::{f128_bytes_to_i64, f128_bytes_to_u64, f128_bytes_to_i128, f128_bytes_to_u128};
        Some(match target {
            IrType::F64 => IrConst::F64(fv),
            IrType::F128 => IrConst::long_double_with_bytes(fv, *bytes),
            IrType::F32 => IrConst::F32(fv as f32),
            // For integer targets, use full precision via f128 bytes
            IrType::I8 => IrConst::I8(f128_bytes_to_i64(bytes)? as i8),
            IrType::U8 => IrConst::I8(f128_bytes_to_u64(bytes)? as u8 as i8),
            IrType::I16 => IrConst::I16(f128_bytes_to_i64(bytes)? as i16),
            IrType::U16 => IrConst::I16(f128_bytes_to_u64(bytes)? as u16 as i16),
            IrType::I32 => IrConst::I32(f128_bytes_to_i64(bytes)? as i32),
            IrType::U32 => IrConst::I64(f128_bytes_to_u64(bytes)? as u32 as i64),
            IrType::I64 => IrConst::I64(f128_bytes_to_i64(bytes)?),
            IrType::Ptr => IrConst::ptr_int(f128_bytes_to_i64(bytes)?),
            IrType::U64 => IrConst::I64(f128_bytes_to_u64(bytes)? as i64),
            IrType::I128 => IrConst::I128(f128_bytes_to_i128(bytes)?),
            IrType::U128 => IrConst::I128(f128_bytes_to_u128(bytes)? as i128),
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
    /// For long double on ARM64, emits f64 approximation + zero padding (matching the
    /// ARM64 codegen's f64-based internal representation). For RISC-V, use
    /// `push_le_bytes_riscv` which emits full IEEE binary128.
    /// Use `push_le_bytes_x86` for x86 targets that need x87 80-bit extended precision.
    pub fn push_le_bytes(&self, out: &mut Vec<u8>, size: usize) {
        match self {
            IrConst::F32(v) => {
                out.extend_from_slice(&v.to_bits().to_le_bytes());
            }
            IrConst::F64(v) => {
                out.extend_from_slice(&v.to_bits().to_le_bytes());
            }
            IrConst::LongDouble(_, f128_bytes) => {
                // Default: emit full f128 bytes. For architecture-specific emission,
                // use push_le_bytes_x86 (x87 format) or push_le_bytes_riscv (f128 format).
                out.extend_from_slice(f128_bytes);
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
            IrConst::LongDouble(_, f128_bytes) => {
                // x86-64: convert f128 bytes to x87 80-bit format, emit 10 bytes + 6 padding.
                let x87 = crate::common::long_double::f128_bytes_to_x87_bytes(f128_bytes);
                out.extend_from_slice(&x87[..10]);
                out.extend_from_slice(&[0u8; 6]); // pad to 16 bytes
            }
            _ => self.push_le_bytes(out, size),
        }
    }

    /// Convert to bytes for RISC-V/ARM64 targets, using IEEE 754 binary128 format for
    /// long doubles. The f128 bytes are stored directly since they are already in
    /// IEEE binary128 format.
    pub fn push_le_bytes_riscv(&self, out: &mut Vec<u8>, size: usize) {
        match self {
            IrConst::LongDouble(_, f128_bytes) => {
                // ARM64/RISC-V: f128 bytes are already in IEEE binary128 format.
                out.extend_from_slice(f128_bytes);
            }
            _ => self.push_le_bytes(out, size),
        }
    }

    /// Create a pointer-width integer constant: I32 on ILP32 targets, I64 on LP64.
    /// Use this instead of hardcoding `IrConst::I64(val)` for pointer arithmetic
    /// step sizes, element sizes, and other constants in pointer-width operations.
    pub fn ptr_int(val: i64) -> Self {
        if crate::common::types::target_is_32bit() {
            IrConst::I32(val as i32)
        } else {
            IrConst::I64(val)
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
            // Ptr: use target-appropriate width (I32 on ILP32, I64 on LP64)
            IrType::Ptr => IrConst::ptr_int(val),
            // I128: sign-extend i64 to i128 (preserves signed value)
            IrType::I128 => IrConst::I128(val as i128),
            // U128: zero-extend by first reinterpreting i64 as u64, then widening to u128.
            // Using `val as i128` would sign-extend, turning e.g. the unsigned value
            // 0xCAFEBABE12345678 (stored as negative i64) into 0xFFFFFFFFFFFFFFFF_CAFEBABE12345678
            // instead of the correct 0x00000000_00000000_CAFEBABE12345678.
            IrType::U128 => IrConst::I128((val as u64 as u128) as i128),
            IrType::F32 => IrConst::F32(val as f32),
            IrType::F64 => IrConst::F64(val as f64),
            IrType::F128 => IrConst::long_double_from_i64(val),
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
            (IrConst::I64(_), IrType::I64 | IrType::U64) => return self.clone(),
            // Ptr: on LP64 I64 is already correct; on ILP32 we need I32
            (IrConst::I64(v), IrType::Ptr) => {
                if crate::common::types::target_is_32bit() {
                    return IrConst::I32(*v as i32);
                }
                return self.clone();
            }
            (IrConst::I32(_), IrType::Ptr) => {
                if crate::common::types::target_is_32bit() {
                    return self.clone();
                }
                // On LP64, widen I32 to I64 for Ptr
            }
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
                        IrType::F128 => IrConst::long_double_from_u64(uint_val),
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

    /// Narrow a constant to match a target IR type.
    /// If the constant is wider than needed (e.g., I64 for an I32 slot),
    /// truncate it to the correct width. This preserves the numeric value
    /// for values that fit, and truncates for values that don't.
    pub fn narrowed_to(self, ty: IrType) -> IrConst {
        match (&self, ty) {
            // Already the right type or narrower — return as-is
            (IrConst::I8(_), IrType::I8 | IrType::U8) => self,
            (IrConst::I16(_), IrType::I16 | IrType::U16) => self,
            (IrConst::I32(_), IrType::I32) => self,
            (IrConst::F32(_), IrType::F32) => self,
            (IrConst::F64(_), IrType::F64) => self,
            // Wide integer constant being stored to a narrower slot
            (IrConst::I64(v), IrType::I8 | IrType::U8) => IrConst::I8(*v as i8),
            (IrConst::I64(v), IrType::I16 | IrType::U16) => IrConst::I16(*v as i16),
            (IrConst::I64(v), IrType::I32) => IrConst::I32(*v as i32),
            (IrConst::I64(v), IrType::U32) => IrConst::I32(*v as i32),
            (IrConst::I32(v), IrType::I8 | IrType::U8) => IrConst::I8(*v as i8),
            (IrConst::I32(v), IrType::I16 | IrType::U16) => IrConst::I16(*v as i16),
            // Pointer types on 32-bit: I64 -> I32
            (IrConst::I64(v), IrType::Ptr) => {
                if crate::common::types::target_is_32bit() {
                    IrConst::I32(*v as i32)
                } else {
                    self
                }
            }
            // Everything else: keep as-is (F64, I64, I128, etc.)
            _ => self,
        }
    }

    /// Serialize this constant to little-endian bytes.
    /// Returns a Vec containing the value in little-endian byte order.
    pub fn to_le_bytes(&self) -> Vec<u8> {
        match self {
            IrConst::I8(v) => vec![*v as u8],
            IrConst::I16(v) => v.to_le_bytes().to_vec(),
            IrConst::I32(v) => v.to_le_bytes().to_vec(),
            IrConst::I64(v) => v.to_le_bytes().to_vec(),
            IrConst::I128(v) => v.to_le_bytes().to_vec(),
            IrConst::F32(v) => v.to_bits().to_le_bytes().to_vec(),
            IrConst::F64(v) => v.to_bits().to_le_bytes().to_vec(),
            IrConst::LongDouble(_, bytes) => bytes.to_vec(),
            IrConst::Zero => vec![0],
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

    /// Evaluate this binary operation on two i64 operands using wrapping arithmetic.
    ///
    /// Signed operations use Rust's native i64 arithmetic.
    /// Unsigned operations (UDiv, URem, LShr) reinterpret the bits as u64.
    /// Returns None for division/remainder by zero.
    pub fn eval_i64(self, lhs: i64, rhs: i64) -> Option<i64> {
        Some(match self {
            IrBinOp::Add => lhs.wrapping_add(rhs),
            IrBinOp::Sub => lhs.wrapping_sub(rhs),
            IrBinOp::Mul => lhs.wrapping_mul(rhs),
            IrBinOp::And => lhs & rhs,
            IrBinOp::Or => lhs | rhs,
            IrBinOp::Xor => lhs ^ rhs,
            IrBinOp::Shl => lhs.wrapping_shl(rhs as u32),
            IrBinOp::AShr => lhs.wrapping_shr(rhs as u32),
            IrBinOp::LShr => (lhs as u64).wrapping_shr(rhs as u32) as i64,
            IrBinOp::SDiv => {
                if rhs == 0 { return None; }
                lhs.wrapping_div(rhs)
            }
            IrBinOp::UDiv => {
                if rhs == 0 { return None; }
                ((lhs as u64).wrapping_div(rhs as u64)) as i64
            }
            IrBinOp::SRem => {
                if rhs == 0 { return None; }
                lhs.wrapping_rem(rhs)
            }
            IrBinOp::URem => {
                if rhs == 0 { return None; }
                ((lhs as u64).wrapping_rem(rhs as u64)) as i64
            }
        })
    }

    /// Evaluate this binary operation on two i128 operands using wrapping arithmetic.
    ///
    /// Unsigned operations (UDiv, URem, LShr) reinterpret the bits as u128.
    /// Returns None for division/remainder by zero.
    pub fn eval_i128(self, lhs: i128, rhs: i128) -> Option<i128> {
        Some(match self {
            IrBinOp::Add => lhs.wrapping_add(rhs),
            IrBinOp::Sub => lhs.wrapping_sub(rhs),
            IrBinOp::Mul => lhs.wrapping_mul(rhs),
            IrBinOp::And => lhs & rhs,
            IrBinOp::Or => lhs | rhs,
            IrBinOp::Xor => lhs ^ rhs,
            IrBinOp::Shl => lhs.wrapping_shl(rhs as u32),
            IrBinOp::AShr => lhs.wrapping_shr(rhs as u32),
            IrBinOp::LShr => (lhs as u128).wrapping_shr(rhs as u32) as i128,
            IrBinOp::SDiv => {
                if rhs == 0 { return None; }
                lhs.wrapping_div(rhs)
            }
            IrBinOp::UDiv => {
                if rhs == 0 { return None; }
                (lhs as u128).wrapping_div(rhs as u128) as i128
            }
            IrBinOp::SRem => {
                if rhs == 0 { return None; }
                lhs.wrapping_rem(rhs)
            }
            IrBinOp::URem => {
                if rhs == 0 { return None; }
                (lhs as u128).wrapping_rem(rhs as u128) as i128
            }
        })
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
    /// __builtin_constant_p: returns 1 if operand is a compile-time constant, 0 otherwise.
    /// Lowered as an IR instruction so it can be resolved after inlining and constant propagation.
    IsConstant,
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

impl IrCmpOp {
    /// Evaluate this comparison on two i64 operands.
    ///
    /// Signed comparisons use Rust's native i64 ordering.
    /// Unsigned comparisons reinterpret the bits as u64.
    pub fn eval_i64(self, lhs: i64, rhs: i64) -> bool {
        match self {
            IrCmpOp::Eq => lhs == rhs,
            IrCmpOp::Ne => lhs != rhs,
            IrCmpOp::Slt => lhs < rhs,
            IrCmpOp::Sle => lhs <= rhs,
            IrCmpOp::Sgt => lhs > rhs,
            IrCmpOp::Sge => lhs >= rhs,
            IrCmpOp::Ult => (lhs as u64) < (rhs as u64),
            IrCmpOp::Ule => (lhs as u64) <= (rhs as u64),
            IrCmpOp::Ugt => (lhs as u64) > (rhs as u64),
            IrCmpOp::Uge => (lhs as u64) >= (rhs as u64),
        }
    }

    /// Evaluate this comparison on two i128 operands.
    ///
    /// Signed comparisons use Rust's native i128 ordering.
    /// Unsigned comparisons reinterpret the bits as u128.
    pub fn eval_i128(self, lhs: i128, rhs: i128) -> bool {
        match self {
            IrCmpOp::Eq => lhs == rhs,
            IrCmpOp::Ne => lhs != rhs,
            IrCmpOp::Slt => lhs < rhs,
            IrCmpOp::Sle => lhs <= rhs,
            IrCmpOp::Sgt => lhs > rhs,
            IrCmpOp::Sge => lhs >= rhs,
            IrCmpOp::Ult => (lhs as u128) < (rhs as u128),
            IrCmpOp::Ule => (lhs as u128) <= (rhs as u128),
            IrCmpOp::Ugt => (lhs as u128) > (rhs as u128),
            IrCmpOp::Uge => (lhs as u128) >= (rhs as u128),
        }
    }

    /// Evaluate this comparison on two f64 operands using IEEE 754 semantics.
    ///
    /// For floats, signed and unsigned comparison variants are equivalent since
    /// IEEE 754 defines a total ordering (NaN comparisons return false for
    /// ordered ops, true for Ne).
    pub fn eval_f64(self, lhs: f64, rhs: f64) -> bool {
        match self {
            IrCmpOp::Eq => lhs == rhs,
            IrCmpOp::Ne => lhs != rhs,
            IrCmpOp::Slt | IrCmpOp::Ult => lhs < rhs,
            IrCmpOp::Sle | IrCmpOp::Ule => lhs <= rhs,
            IrCmpOp::Sgt | IrCmpOp::Ugt => lhs > rhs,
            IrCmpOp::Sge | IrCmpOp::Uge => lhs >= rhs,
        }
    }
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
    /// AES-NI: aesenc (single round encrypt)
    /// args[0] = state ptr, args[1] = round key ptr; dest_ptr = result ptr
    Aesenc128,
    /// AES-NI: aesenclast (final round encrypt)
    Aesenclast128,
    /// AES-NI: aesdec (single round decrypt)
    Aesdec128,
    /// AES-NI: aesdeclast (final round decrypt)
    Aesdeclast128,
    /// AES-NI: aesimc (inverse mix columns)
    /// args[0] = input ptr; dest_ptr = result ptr
    Aesimc128,
    /// AES-NI: aeskeygenassist with immediate
    /// args[0] = input ptr, args[1] = imm8; dest_ptr = result ptr
    Aeskeygenassist128,
    /// CLMUL: pclmulqdq with immediate
    /// args[0] = src1 ptr, args[1] = src2 ptr, args[2] = imm8; dest_ptr = result ptr
    Pclmulqdq128,
    /// SSE2 byte shift left (PSLLDQ): shift by imm8 bytes
    /// args[0] = src ptr, args[1] = imm8; dest_ptr = result ptr
    Pslldqi128,
    /// SSE2 byte shift right (PSRLDQ): shift by imm8 bytes
    Psrldqi128,
    /// SSE2 bit shift left per 64-bit lane (PSLLQ)
    /// args[0] = src ptr, args[1] = count; dest_ptr = result ptr
    Psllqi128,
    /// SSE2 bit shift right per 64-bit lane (PSRLQ)
    Psrlqi128,
    /// SSE2 shuffle 32-bit integers (PSHUFD)
    /// args[0] = src ptr, args[1] = imm8; dest_ptr = result ptr
    Pshufd128,
    /// Load low 64 bits, zero upper (MOVQ)
    /// args[0] = src ptr; dest_ptr = result ptr
    Loadldi128,

    // --- SSE2 packed 16-bit integer operations ---
    /// Packed 16-bit add (PADDW)
    Paddw128,
    /// Packed 16-bit subtract (PSUBW)
    Psubw128,
    /// Packed 16-bit multiply high (PMULHW)
    Pmulhw128,
    /// Packed 16-bit multiply-add to 32-bit (PMADDWD)
    Pmaddwd128,
    /// Packed 16-bit compare greater-than (PCMPGTW)
    Pcmpgtw128,
    /// Packed 8-bit compare greater-than (PCMPGTB)
    Pcmpgtb128,
    /// Packed 16-bit shift left by imm (PSLLW)
    Psllwi128,
    /// Packed 16-bit shift right logical by imm (PSRLW)
    Psrlwi128,
    /// Packed 16-bit shift right arithmetic by imm (PSRAW)
    Psrawi128,
    /// Packed 32-bit shift right arithmetic by imm (PSRAD)
    Psradi128,
    /// Packed 32-bit shift left by imm (PSLLD)
    Pslldi128,
    /// Packed 32-bit shift right logical by imm (PSRLD)
    Psrldi128,

    // --- SSE2 packed 32-bit integer operations ---
    /// Packed 32-bit add (PADDD)
    Paddd128,
    /// Packed 32-bit subtract (PSUBD)
    Psubd128,

    // --- SSE2 pack/unpack operations ---
    /// Pack 32-bit to 16-bit signed saturate (PACKSSDW)
    Packssdw128,
    /// Pack 16-bit to 8-bit unsigned saturate (PACKUSWB)
    Packuswb128,
    /// Unpack and interleave low 8-bit (PUNPCKLBW)
    Punpcklbw128,
    /// Unpack and interleave high 8-bit (PUNPCKHBW)
    Punpckhbw128,
    /// Unpack and interleave low 16-bit (PUNPCKLWD)
    Punpcklwd128,
    /// Unpack and interleave high 16-bit (PUNPCKHWD)
    Punpckhwd128,

    // --- SSE2 set/insert/extract/convert operations ---
    /// Set all 16-bit lanes to value (splat)
    SetEpi16,
    /// Insert 16-bit value at lane (PINSRW)
    Pinsrw128,
    /// Extract 16-bit value at lane (PEXTRW) - returns scalar i32
    Pextrw128,
    /// Store low 64 bits to memory (MOVQ store)
    Storeldi128,
    /// Convert low 32-bit of __m128i to int (MOVD) - returns scalar i32
    Cvtsi128Si32,
    /// Convert int to __m128i with zero extension (MOVD)
    Cvtsi32Si128,
    /// Convert low 64-bit of __m128i to long long - returns scalar i64
    Cvtsi128Si64,
    /// Shuffle low 16-bit integers (PSHUFLW)
    Pshuflw128,
    /// Shuffle high 16-bit integers (PSHUFHW)
    Pshufhw128,
}

impl IntrinsicOp {
    /// Returns true if this intrinsic is a pure function (no side effects, result depends
    /// only on inputs). Pure intrinsics can be dead-code eliminated if their result is unused.
    pub fn is_pure(&self) -> bool {
        matches!(self,
            IntrinsicOp::SqrtF32 | IntrinsicOp::SqrtF64 |
            IntrinsicOp::FabsF32 | IntrinsicOp::FabsF64 |
            IntrinsicOp::Aesenc128 | IntrinsicOp::Aesenclast128 |
            IntrinsicOp::Aesdec128 | IntrinsicOp::Aesdeclast128 |
            IntrinsicOp::Aesimc128 | IntrinsicOp::Aeskeygenassist128 |
            IntrinsicOp::Pclmulqdq128 |
            IntrinsicOp::Pslldqi128 | IntrinsicOp::Psrldqi128 |
            IntrinsicOp::Psllqi128 | IntrinsicOp::Psrlqi128 |
            IntrinsicOp::Pshufd128 |
            // New SSE2 packed operations are all pure
            IntrinsicOp::Paddw128 | IntrinsicOp::Psubw128 |
            IntrinsicOp::Pmulhw128 | IntrinsicOp::Pmaddwd128 |
            IntrinsicOp::Pcmpgtw128 | IntrinsicOp::Pcmpgtb128 |
            IntrinsicOp::Psllwi128 | IntrinsicOp::Psrlwi128 |
            IntrinsicOp::Psrawi128 | IntrinsicOp::Psradi128 |
            IntrinsicOp::Pslldi128 | IntrinsicOp::Psrldi128 |
            IntrinsicOp::Paddd128 | IntrinsicOp::Psubd128 |
            IntrinsicOp::Packssdw128 | IntrinsicOp::Packuswb128 |
            IntrinsicOp::Punpcklbw128 | IntrinsicOp::Punpckhbw128 |
            IntrinsicOp::Punpcklwd128 | IntrinsicOp::Punpckhwd128 |
            IntrinsicOp::SetEpi16 | IntrinsicOp::Pinsrw128 |
            IntrinsicOp::Pextrw128 | IntrinsicOp::Cvtsi128Si32 |
            IntrinsicOp::Cvtsi32Si128 | IntrinsicOp::Cvtsi128Si64 |
            IntrinsicOp::Pshuflw128 | IntrinsicOp::Pshufhw128
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
            Instruction::Call { info, .. }
            | Instruction::CallIndirect { info, .. } => Some(info.return_type),
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
            Instruction::GetReturnF128Second { .. } => Some(IrType::F128),
            Instruction::ParamRef { ty, .. } => Some(*ty),
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
            | Instruction::GetReturnF128Second { dest }
            | Instruction::Select { dest, .. }
            | Instruction::StackSave { dest }
            | Instruction::ParamRef { dest, .. } => Some(*dest),
            Instruction::Call { info, .. }
            | Instruction::CallIndirect { info, .. } => info.dest,
            Instruction::Intrinsic { dest, .. } => *dest,
            Instruction::Store { .. }
            | Instruction::Memcpy { .. }
            | Instruction::VaStart { .. }
            | Instruction::VaEnd { .. }
            | Instruction::VaCopy { .. }
            | Instruction::VaArgStruct { .. }
            | Instruction::AtomicStore { .. }
            | Instruction::Fence { .. }
            | Instruction::SetReturnF64Second { .. }
            | Instruction::SetReturnF32Second { .. }
            | Instruction::SetReturnF128Second { .. }
            | Instruction::InlineAsm { .. }
            | Instruction::StackRestore { .. } => None,
        }
    }

    /// Collect all Value IDs used (as operands, not defined) by this instruction.
    /// This is the canonical implementation used by inlining validation and other passes.
    /// Note: mem2reg handles Load/Store ptrs specially in its own match before calling this
    /// on other instruction types, so it skips them in the catch-all arm.
    pub fn used_values(&self) -> Vec<u32> {
        let mut used = Vec::new();
        match self {
            Instruction::Alloca { .. } => {}
            Instruction::Load { ptr, .. } => {
                used.push(ptr.0);
            }
            Instruction::Store { val, ptr, .. } => {
                push_operand_value(val, &mut used);
                used.push(ptr.0);
            }
            Instruction::DynAlloca { size, .. } => {
                push_operand_value(size, &mut used);
            }
            Instruction::BinOp { lhs, rhs, .. } => {
                push_operand_value(lhs, &mut used);
                push_operand_value(rhs, &mut used);
            }
            Instruction::UnaryOp { src, .. } => {
                push_operand_value(src, &mut used);
            }
            Instruction::Cmp { lhs, rhs, .. } => {
                push_operand_value(lhs, &mut used);
                push_operand_value(rhs, &mut used);
            }
            Instruction::Call { info, .. } => {
                for arg in &info.args { push_operand_value(arg, &mut used); }
            }
            Instruction::CallIndirect { func_ptr, info } => {
                push_operand_value(func_ptr, &mut used);
                for arg in &info.args { push_operand_value(arg, &mut used); }
            }
            Instruction::GetElementPtr { base, offset, .. } => {
                used.push(base.0);
                push_operand_value(offset, &mut used);
            }
            Instruction::Cast { src, .. } | Instruction::Copy { src, .. } => {
                push_operand_value(src, &mut used);
            }
            Instruction::GlobalAddr { .. } | Instruction::LabelAddr { .. }
            | Instruction::Fence { .. } | Instruction::StackSave { .. } => {}
            Instruction::Memcpy { dest, src, .. } => {
                used.push(dest.0);
                used.push(src.0);
            }
            Instruction::VaArg { va_list_ptr, .. } => {
                used.push(va_list_ptr.0);
            }
            Instruction::VaStart { va_list_ptr } | Instruction::VaEnd { va_list_ptr } => {
                used.push(va_list_ptr.0);
            }
            Instruction::VaCopy { dest_ptr, src_ptr } => {
                used.push(dest_ptr.0);
                used.push(src_ptr.0);
            }
            Instruction::VaArgStruct { dest_ptr, va_list_ptr, .. } => {
                used.push(dest_ptr.0);
                used.push(va_list_ptr.0);
            }
            Instruction::AtomicRmw { ptr, val, .. } => {
                push_operand_value(ptr, &mut used);
                push_operand_value(val, &mut used);
            }
            Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
                push_operand_value(ptr, &mut used);
                push_operand_value(expected, &mut used);
                push_operand_value(desired, &mut used);
            }
            Instruction::AtomicLoad { ptr, .. } => {
                push_operand_value(ptr, &mut used);
            }
            Instruction::AtomicStore { ptr, val, .. } => {
                push_operand_value(ptr, &mut used);
                push_operand_value(val, &mut used);
            }
            Instruction::Phi { incoming, .. } => {
                for (op, _) in incoming {
                    push_operand_value(op, &mut used);
                }
            }
            Instruction::GetReturnF64Second { .. }
            | Instruction::GetReturnF32Second { .. }
            | Instruction::GetReturnF128Second { .. } => {}
            Instruction::SetReturnF64Second { src }
            | Instruction::SetReturnF32Second { src }
            | Instruction::SetReturnF128Second { src } => {
                push_operand_value(src, &mut used);
            }
            Instruction::InlineAsm { outputs, inputs, .. } => {
                for (_, ptr, _) in outputs {
                    used.push(ptr.0);
                }
                for (_, op, _) in inputs {
                    push_operand_value(op, &mut used);
                }
            }
            Instruction::Intrinsic { dest_ptr, args, .. } => {
                if let Some(ptr) = dest_ptr {
                    used.push(ptr.0);
                }
                for arg in args {
                    push_operand_value(arg, &mut used);
                }
            }
            Instruction::Select { cond, true_val, false_val, .. } => {
                push_operand_value(cond, &mut used);
                push_operand_value(true_val, &mut used);
                push_operand_value(false_val, &mut used);
            }
            Instruction::StackRestore { ptr } => {
                used.push(ptr.0);
            }
            Instruction::ParamRef { .. } => {
                // ParamRef has no operand values - it references a parameter by index
            }
        }
        used
    }
}

/// Push a Value ID from an Operand into the collection, if it is a Value (not a Const).
fn push_operand_value(op: &Operand, used: &mut Vec<u32>) {
    if let Operand::Value(v) = op {
        used.push(v.0);
    }
}

impl Terminator {
    /// Collect all Value IDs used by this terminator.
    pub fn used_values(&self) -> Vec<u32> {
        let mut used = Vec::new();
        match self {
            Terminator::Return(Some(op)) => push_operand_value(op, &mut used),
            Terminator::CondBranch { cond, .. } => push_operand_value(cond, &mut used),
            Terminator::IndirectBranch { target, .. } => push_operand_value(target, &mut used),
            Terminator::Switch { val, .. } => push_operand_value(val, &mut used),
            _ => {}
        }
        used
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
            is_used: false,
            has_inlined_calls: false,
            param_alloca_values: Vec::new(),
            uses_sret: false,
            is_fastcall: false,
            global_init_label_blocks: Vec::new(),
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
