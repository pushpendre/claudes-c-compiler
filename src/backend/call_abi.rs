//! Shared call ABI classification and stack space computation.
//!
//! All four backends use the same algorithmic structure to classify function call
//! arguments into register/stack classes. Only the register counts and F128 handling
//! differ per architecture. This module extracts that shared classification so each
//! backend's `emit_call` only needs to handle the instruction emission.

use crate::ir::ir::{IrConst, Operand};
use crate::common::types::IrType;
use super::generation::is_i128_type;

/// Classification of a function call argument for register/stack assignment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallArgClass {
    /// Integer/pointer argument in a GP register. `reg_idx` is the GP register index.
    IntReg { reg_idx: usize },
    /// Float argument in an FP register. `reg_idx` is the FP register index.
    FloatReg { reg_idx: usize },
    /// 128-bit integer in a GP register pair. `base_reg_idx` is the first register.
    I128RegPair { base_reg_idx: usize },
    /// F128 (long double) — handling is arch-specific (x87 on x86, Q-reg on ARM, GP pair on RISC-V).
    F128Reg { reg_idx: usize },
    /// Small struct (<=16 bytes) passed by value in 1-2 GP registers.
    StructByValReg { base_reg_idx: usize, size: usize },
    /// Small struct (<=16 bytes) where all fields are float/double (SSE class per SysV ABI).
    /// Passed in 1-2 XMM registers instead of GP registers.
    /// `lo_fp_idx` is the FP register for the first eightbyte, `hi_fp_idx` for the second (if size > 8).
    StructSseReg { lo_fp_idx: usize, hi_fp_idx: Option<usize>, size: usize },
    /// Small struct where first eightbyte is INTEGER and second is SSE (mixed).
    StructMixedIntSseReg { int_reg_idx: usize, fp_reg_idx: usize, size: usize },
    /// Small struct where first eightbyte is SSE and second is INTEGER (mixed).
    StructMixedSseIntReg { fp_reg_idx: usize, int_reg_idx: usize, size: usize },
    /// Small struct (<=16 bytes) that overflows to the stack.
    StructByValStack { size: usize },
    /// Large struct (>16 bytes) passed on the stack (MEMORY class).
    LargeStructStack { size: usize },
    /// Argument overflows to the stack (normal 8-byte).
    Stack,
    /// F128 argument overflows to the stack (16-byte aligned).
    F128Stack,
    /// I128 argument overflows to the stack (16-byte aligned).
    I128Stack,
    /// Zero-size struct argument (e.g., `struct { char x[0]; }`).
    /// Per GCC behavior, zero-size struct arguments consume no register or stack space.
    ZeroSizeSkip,
}

impl CallArgClass {
    /// Returns true if this argument is passed on the stack (any kind).
    pub fn is_stack(&self) -> bool {
        matches!(self, CallArgClass::Stack | CallArgClass::F128Stack |
                 CallArgClass::I128Stack | CallArgClass::StructByValStack { .. } |
                 CallArgClass::LargeStructStack { .. })
    }

    /// Returns the stack space consumed by this argument (0 if register).
    pub fn stack_bytes(&self) -> usize {
        let slot_size = crate::common::types::target_ptr_size(); // 8 for LP64, 4 for ILP32
        let align_mask = slot_size - 1;
        match self {
            CallArgClass::F128Stack => if slot_size == 4 { 12 } else { 16 }, // i686: x87 long double = 12 bytes
            CallArgClass::I128Stack => 16,
            CallArgClass::StructByValStack { size } | CallArgClass::LargeStructStack { size } => {
                (*size + align_mask) & !align_mask
            }
            CallArgClass::Stack => slot_size,
            _ => 0,
        }
    }
}

/// ABI configuration for call argument classification.
pub struct CallAbiConfig {
    /// Maximum GP registers for arguments (x86: 6, ARM/RISC-V: 8).
    pub max_int_regs: usize,
    /// Maximum FP registers for arguments (all: 8).
    pub max_float_regs: usize,
    /// Whether i128 register pairs must be even-aligned (ARM/RISC-V: true, x86: false).
    pub align_i128_pairs: bool,
    /// Whether F128 uses FP registers (ARM: true) or always goes to stack/x87 (x86: true = stack).
    /// On RISC-V, F128 goes in GP register pairs like i128.
    pub f128_in_fp_regs: bool,
    /// Whether F128 uses GP register pairs (RISC-V: true).
    pub f128_in_gp_pairs: bool,
    /// Whether variadic float args must go in GP registers instead of FP regs (RISC-V: true, x86: false, ARM: false).
    pub variadic_floats_in_gp: bool,
    /// Whether large structs (>16 bytes) are passed by reference (pointer in GP reg).
    /// ARM/RISC-V: true (pointer in GP reg or on stack), x86: false (copy to stack).
    pub large_struct_by_ref: bool,
    /// Whether to use SysV per-eightbyte struct classification (x86-64 only).
    /// When true, struct eightbytes classified as SSE are passed in xmm registers.
    pub use_sysv_struct_classification: bool,
}

/// Result of SysV per-eightbyte struct classification.
/// Describes how a small struct (<=16 bytes) should be passed in registers.
#[derive(Debug, Clone, Copy)]
pub enum SysvStructRegClass {
    /// All eightbytes are INTEGER class -> GP registers only.
    AllInt { gp_count: usize },
    /// All eightbytes are SSE class -> XMM registers only.
    AllSse { fp_count: usize },
    /// First eightbyte INTEGER, second SSE (mixed).
    IntSse,
    /// First eightbyte SSE, second INTEGER (mixed).
    SseInt,
    /// Not enough registers available -> spill to stack.
    Stack,
}

/// Classify a small struct (<=16 bytes) using SysV AMD64 per-eightbyte rules.
///
/// Given the eightbyte classes and current register allocation state, determines
/// whether the struct fits in registers and which class combination to use.
/// Returns the classification and the number of GP/FP registers consumed.
pub fn classify_sysv_struct(
    eb_classes: &[crate::common::types::EightbyteClass],
    int_idx: usize,
    float_idx: usize,
    config: &CallAbiConfig,
) -> (SysvStructRegClass, usize, usize) {
    use crate::common::types::EightbyteClass;
    let n_eightbytes = eb_classes.len();
    let eb0_is_sse = eb_classes.first() == Some(&EightbyteClass::Sse);
    let eb1_is_sse = if n_eightbytes > 1 { eb_classes.get(1) == Some(&EightbyteClass::Sse) } else { false };

    let gp_needed = (if !eb0_is_sse { 1 } else { 0 })
        + (if n_eightbytes > 1 && !eb1_is_sse { 1 } else { 0 });
    let fp_needed = (if eb0_is_sse { 1 } else { 0 })
        + (if n_eightbytes > 1 && eb1_is_sse { 1 } else { 0 });

    if int_idx + gp_needed > config.max_int_regs || float_idx + fp_needed > config.max_float_regs {
        return (SysvStructRegClass::Stack, 0, 0);
    }

    if n_eightbytes == 1 {
        if eb0_is_sse {
            (SysvStructRegClass::AllSse { fp_count: 1 }, 0, 1)
        } else {
            (SysvStructRegClass::AllInt { gp_count: 1 }, 1, 0)
        }
    } else if eb0_is_sse && eb1_is_sse {
        (SysvStructRegClass::AllSse { fp_count: 2 }, 0, 2)
    } else if !eb0_is_sse && eb1_is_sse {
        (SysvStructRegClass::IntSse, 1, 1)
    } else if eb0_is_sse && !eb1_is_sse {
        (SysvStructRegClass::SseInt, 1, 1)
    } else {
        (SysvStructRegClass::AllInt { gp_count: 2 }, 2, 0)
    }
}

/// Classify all arguments for a function call, returning a `CallArgClass` per argument.
/// This captures the shared classification logic used identically by all four backends.
/// `struct_arg_sizes` indicates which args are struct/union by-value: Some(size) for struct
/// args, None otherwise.
/// `struct_arg_aligns` indicates struct alignment: Some(align) for struct args, None otherwise.
/// Used on RISC-V to even-align register pairs for 2×XLEN-aligned structs (e.g., containing
/// long double with 16-byte alignment per the RISC-V psABI).
/// `struct_arg_classes` provides per-eightbyte SysV ABI classification for struct args
/// (used when `config.use_sysv_struct_classification` is true, i.e. x86-64).
pub fn classify_call_args(
    args: &[Operand],
    arg_types: &[IrType],
    struct_arg_sizes: &[Option<usize>],
    struct_arg_aligns: &[Option<usize>],
    struct_arg_classes: &[Vec<crate::common::types::EightbyteClass>],
    is_variadic: bool,
    config: &CallAbiConfig,
) -> Vec<CallArgClass> {
    let mut result = Vec::with_capacity(args.len());
    let mut int_idx = 0usize;
    let mut float_idx = 0usize;

    for (i, _arg) in args.iter().enumerate() {
        let struct_size = struct_arg_sizes.get(i).copied().flatten();
        let arg_ty = if i < arg_types.len() { Some(arg_types[i]) } else { None };
        let is_long_double = arg_ty.map(|t| t.is_long_double()).unwrap_or(false);
        let is_i128 = arg_ty.map(|t| is_i128_type(t)).unwrap_or(false);
        let is_float = if let Some(ty) = arg_ty {
            ty.is_float()
        } else {
            matches!(args[i], Operand::Const(IrConst::F32(_) | IrConst::F64(_)))
        };
        let force_gp = is_variadic && config.variadic_floats_in_gp && is_float && !is_long_double;

        if let Some(size) = struct_size {
            // Zero-size structs (e.g., `struct { char x[0]; }`) consume no register
            // or stack space per GCC behavior. Skip them entirely.
            if size == 0 {
                result.push(CallArgClass::ZeroSizeSkip);
                continue;
            }

            // Get per-eightbyte classification for this struct arg (if available)
            let eb_classes = struct_arg_classes.get(i).map(|v| v.as_slice()).unwrap_or(&[]);

            if size <= 16 && config.use_sysv_struct_classification && !eb_classes.is_empty() {
                // SysV AMD64 ABI: classify per-eightbyte and assign to GP or SSE registers
                let (cls, gp_used, fp_used) = classify_sysv_struct(eb_classes, int_idx, float_idx, config);
                match cls {
                    SysvStructRegClass::AllSse { fp_count } => {
                        let hi = if fp_count > 1 { Some(float_idx + 1) } else { None };
                        result.push(CallArgClass::StructSseReg { lo_fp_idx: float_idx, hi_fp_idx: hi, size });
                    }
                    SysvStructRegClass::AllInt { .. } => {
                        result.push(CallArgClass::StructByValReg { base_reg_idx: int_idx, size });
                    }
                    SysvStructRegClass::IntSse => {
                        result.push(CallArgClass::StructMixedIntSseReg { int_reg_idx: int_idx, fp_reg_idx: float_idx, size });
                    }
                    SysvStructRegClass::SseInt => {
                        result.push(CallArgClass::StructMixedSseIntReg { fp_reg_idx: float_idx, int_reg_idx: int_idx, size });
                    }
                    SysvStructRegClass::Stack => {
                        result.push(CallArgClass::StructByValStack { size });
                        int_idx = config.max_int_regs;
                    }
                }
                int_idx += gp_used;
                float_idx += fp_used;
            } else if size <= 16 {
                // Non-SysV path (ARM, RISC-V): always use GP registers for small structs
                let regs_needed = if size <= 8 { 1 } else { 2 };
                // RISC-V psABI: 2×XLEN-aligned structs (alignment > XLEN) must start
                // at an even-numbered register, matching i128/f128 pair alignment.
                let slot_size = crate::common::types::target_ptr_size();
                if regs_needed == 2 && config.align_i128_pairs {
                    let struct_align = struct_arg_aligns.get(i).copied().flatten().unwrap_or(slot_size);
                    if struct_align > slot_size && int_idx % 2 != 0 {
                        int_idx += 1; // skip to even register
                    }
                }
                if int_idx + regs_needed <= config.max_int_regs {
                    result.push(CallArgClass::StructByValReg { base_reg_idx: int_idx, size });
                    int_idx += regs_needed;
                } else {
                    result.push(CallArgClass::StructByValStack { size });
                    int_idx = config.max_int_regs;
                }
            } else if config.large_struct_by_ref {
                // AAPCS64: composites > 16 bytes are passed by reference.
                // The caller passes a pointer (the IR value is already a struct pointer).
                if int_idx < config.max_int_regs {
                    result.push(CallArgClass::IntReg { reg_idx: int_idx });
                    int_idx += 1;
                } else {
                    result.push(CallArgClass::Stack);
                }
            } else {
                result.push(CallArgClass::LargeStructStack { size });
            }
        } else if is_i128 {
            if config.align_i128_pairs && int_idx % 2 != 0 {
                int_idx += 1;
            }
            if int_idx + 1 < config.max_int_regs {
                result.push(CallArgClass::I128RegPair { base_reg_idx: int_idx });
                int_idx += 2;
            } else {
                result.push(CallArgClass::I128Stack);
                int_idx = config.max_int_regs;
            }
        } else if is_long_double {
            if config.f128_in_fp_regs {
                if float_idx < config.max_float_regs {
                    result.push(CallArgClass::F128Reg { reg_idx: float_idx });
                    float_idx += 1;
                } else {
                    result.push(CallArgClass::F128Stack);
                }
            } else if config.f128_in_gp_pairs {
                if config.align_i128_pairs && int_idx % 2 != 0 {
                    int_idx += 1;
                }
                if int_idx + 1 < config.max_int_regs {
                    result.push(CallArgClass::F128Reg { reg_idx: int_idx });
                    int_idx += 2;
                } else {
                    result.push(CallArgClass::F128Stack);
                    int_idx = config.max_int_regs;
                }
            } else {
                result.push(CallArgClass::F128Stack);
            }
        } else if is_float && !force_gp && float_idx < config.max_float_regs {
            result.push(CallArgClass::FloatReg { reg_idx: float_idx });
            float_idx += 1;
        } else if is_float && !force_gp {
            result.push(CallArgClass::Stack);
        } else if int_idx < config.max_int_regs {
            result.push(CallArgClass::IntReg { reg_idx: int_idx });
            int_idx += 1;
        } else {
            result.push(CallArgClass::Stack);
        }
    }

    result
}

/// Compute the total stack space needed for stack-overflow arguments.
/// Returns the total bytes needed, 16-byte aligned.
/// Use this for ARM and RISC-V which pre-allocate stack space with a single SP adjustment.
pub fn compute_stack_arg_space(arg_classes: &[CallArgClass]) -> usize {
    let mut total: usize = 0;
    for cls in arg_classes {
        if !cls.is_stack() { continue; }
        if matches!(cls, CallArgClass::F128Stack | CallArgClass::I128Stack) {
            total = (total + 15) & !15;
        }
        total += cls.stack_bytes();
    }
    (total + 15) & !15
}

/// Compute per-stack-arg alignment padding needed in the forward layout.
/// Returns a Vec with one entry per `arg_classes` element. Non-stack args get 0.
/// F128Stack and I128Stack args get padding to align to 16 bytes in the overflow area.
pub fn compute_stack_arg_padding(arg_classes: &[CallArgClass]) -> Vec<usize> {
    let mut padding = vec![0usize; arg_classes.len()];
    let mut offset: usize = 0;
    for (i, cls) in arg_classes.iter().enumerate() {
        if !cls.is_stack() { continue; }
        if matches!(cls, CallArgClass::F128Stack | CallArgClass::I128Stack) {
            let align_pad = (16 - (offset % 16)) % 16;
            padding[i] = align_pad;
            offset += align_pad;
        }
        offset += cls.stack_bytes();
    }
    padding
}

/// Compute the raw bytes that will be pushed onto the stack for stack arguments.
/// Unlike `compute_stack_arg_space`, this does NOT apply final 16-byte alignment,
/// because x86 uses individual `pushq` instructions and handles alignment separately.
/// This includes alignment padding for F128/I128 args.
pub fn compute_stack_push_bytes(arg_classes: &[CallArgClass]) -> usize {
    let padding = compute_stack_arg_padding(arg_classes);
    let mut total: usize = 0;
    for (i, cls) in arg_classes.iter().enumerate() {
        if !cls.is_stack() { continue; }
        total += padding[i] + cls.stack_bytes();
    }
    total
}
