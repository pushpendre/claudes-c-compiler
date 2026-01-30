//! Shared callee-side parameter classification for `emit_store_params`.
//!
//! The caller side already has `classify_call_args` in `call_abi.rs`. This module
//! provides the analogous callee-side classification so that each backend's
//! `emit_store_params` only handles the arch-specific register store instructions,
//! not the duplicated ABI classification logic.
//!
//! The `classify_params` function walks a function's parameter list and assigns each
//! parameter to a `ParamClass` (GP register, FP register, i128 pair, struct-by-value,
//! F128, or stack-passed), using the same `CallAbiConfig` that drives `classify_call_args`.

use crate::ir::ir::IrFunction;
use super::call_abi::CallAbiConfig;
use super::generation::is_i128_type;

/// Classification of a function parameter for `emit_store_params`.
///
/// Each variant tells the backend exactly where the parameter arrives and what
/// kind of store logic is needed, without the backend reimplementing the ABI
/// classification algorithm.
#[derive(Debug, Clone, Copy)]
pub enum ParamClass {
    /// Integer/pointer in GP register at `reg_idx`.
    IntReg { reg_idx: usize },
    /// Float/double in FP register at `reg_idx`.
    FloatReg { reg_idx: usize },
    /// i128 in aligned GP register pair starting at `base_reg_idx`.
    I128RegPair { base_reg_idx: usize },
    /// Small struct (<=16 bytes) by value in 1-2 GP registers.
    StructByValReg { base_reg_idx: usize, size: usize },
    /// Small struct where all eightbytes are SSE class -> 1-2 XMM registers.
    StructSseReg { lo_fp_idx: usize, hi_fp_idx: Option<usize>, size: usize },
    /// Small struct: first eightbyte INTEGER, second SSE.
    StructMixedIntSseReg { int_reg_idx: usize, fp_reg_idx: usize, size: usize },
    /// Small struct: first eightbyte SSE, second INTEGER.
    StructMixedSseIntReg { fp_reg_idx: usize, int_reg_idx: usize, size: usize },
    /// F128 (long double) in FP register (ARM: Q-reg).
    F128FpReg { reg_idx: usize },
    /// F128 in GP register pair (RISC-V).
    F128GpPair { lo_reg_idx: usize, hi_reg_idx: usize },
    /// F128 always on stack (x86: x87 convention).
    F128AlwaysStack { offset: i64 },
    /// Regular scalar on the stack.
    StackScalar { offset: i64 },
    /// i128 on the stack (16-byte aligned).
    I128Stack { offset: i64 },
    /// F128 on the stack (overflow from registers).
    F128Stack { offset: i64 },
    /// Small struct that overflowed to the stack.
    StructStack { offset: i64, size: usize },
    /// Large struct (>16 bytes) passed on the stack.
    LargeStructStack { offset: i64, size: usize },
    /// Large struct (>16 bytes) passed by reference in a GP register (AAPCS64).
    /// The register holds a pointer to the struct data; callee must copy from it.
    LargeStructByRefReg { reg_idx: usize, size: usize },
    /// Large struct (>16 bytes) passed by reference on the stack (AAPCS64, overflow case).
    /// The stack slot holds a pointer to the struct data; callee must copy from it.
    LargeStructByRefStack { offset: i64, size: usize },
    /// Zero-size struct parameter (e.g., `struct { char x[0]; }`).
    /// Per GCC behavior, zero-size struct parameters consume no register or stack space.
    ZeroSizeSkip,
}

impl ParamClass {
    /// Returns true if this parameter is passed on the stack.
    pub fn is_stack(&self) -> bool {
        matches!(self,
            ParamClass::StackScalar { .. } | ParamClass::I128Stack { .. } |
            ParamClass::F128Stack { .. } | ParamClass::F128AlwaysStack { .. } |
            ParamClass::StructStack { .. } | ParamClass::LargeStructStack { .. } |
            ParamClass::LargeStructByRefStack { .. }
        )
    }

    /// Returns true if this parameter arrives in a GP register (int, i128 pair, struct, or F128 GP pair).
    pub fn uses_gp_reg(&self) -> bool {
        matches!(self,
            ParamClass::IntReg { .. } | ParamClass::I128RegPair { .. } |
            ParamClass::StructByValReg { .. } | ParamClass::F128GpPair { .. } |
            ParamClass::LargeStructByRefReg { .. } |
            ParamClass::StructMixedIntSseReg { .. } | ParamClass::StructMixedSseIntReg { .. }
        )
    }

    /// Returns the number of stack bytes consumed by this parameter classification.
    /// Used by variadic function handling to compute how many stack bytes named
    /// parameters occupy, so va_start can skip past them.
    pub fn stack_bytes(&self) -> usize {
        let slot_size = crate::common::types::target_ptr_size(); // 8 for LP64, 4 for ILP32
        let align_mask = slot_size - 1;
        match self {
            ParamClass::StackScalar { .. } => slot_size,
            ParamClass::I128Stack { .. } => 16,
            ParamClass::F128Stack { .. } => 16,
            ParamClass::F128AlwaysStack { .. } => if slot_size == 4 { 12 } else { 16 },
            ParamClass::StructStack { size, .. } => (*size + align_mask) & !align_mask,
            ParamClass::LargeStructStack { size, .. } => (*size + align_mask) & !align_mask,
            ParamClass::LargeStructByRefStack { .. } => slot_size, // pointer on stack
            _ => 0, // register-passed params don't consume stack space
        }
    }

    /// Returns the number of GP registers consumed by this parameter classification.
    /// Used by variadic function handling to compute the correct va_start offset.
    pub fn gp_reg_count(&self) -> usize {
        match self {
            ParamClass::IntReg { .. } => 1,
            ParamClass::LargeStructByRefReg { .. } => 1, // pointer in one GP reg
            ParamClass::I128RegPair { .. } => 2,
            ParamClass::StructByValReg { size, .. } => {
                // 1 reg for <=8 bytes, 2 regs for >8 bytes (up to 16)
                if *size <= 8 { 1 } else { 2 }
            }
            ParamClass::F128GpPair { .. } => 2,
            ParamClass::StructMixedIntSseReg { .. } | ParamClass::StructMixedSseIntReg { .. } => 1,
            ParamClass::StructSseReg { .. } => 0, // all SSE, no GP regs
            _ => 0, // FP regs and stack don't consume GP regs
        }
    }
}

/// Result of parameter classification, including the final register allocation state.
/// The `int_reg_idx` field captures the effective GP register index after all named
/// params are classified, which is needed by RISC-V va_start to correctly skip
/// alignment padding gaps (e.g., when an F128 pair couldn't fit and bumped the index).
pub struct ParamClassification {
    pub classes: Vec<ParamClass>,
    /// Final GP register index after classifying all named params.
    /// Includes alignment bumps for I128/F128 pairs. Capped at max_int_regs.
    pub int_reg_idx: usize,
    /// Total stack bytes consumed by all named parameters.
    /// This is the final stack_offset after classification, accounting for
    /// type-specific sizes (e.g., F64/I64 take 8 bytes on ILP32).
    pub total_stack_bytes: usize,
}

/// Classify all parameters of a function for callee-side store emission.
///
/// Uses the same `CallAbiConfig` as `classify_call_args` to ensure caller and callee
/// agree on parameter locations. Returns one `ParamClass` per parameter plus the final
/// register allocation state.
pub fn classify_params_full(func: &IrFunction, config: &CallAbiConfig) -> ParamClassification {
    let mut result = Vec::with_capacity(func.params.len());
    let mut int_reg_idx = 0usize;
    let mut float_reg_idx = 0usize;
    let mut stack_offset: i64 = 0;
    // Minimum stack slot size: 8 bytes on LP64 (x86-64/ARM/RISC-V), 4 bytes on ILP32 (i686).
    let slot_size = crate::common::types::target_ptr_size();
    let slot_align_mask = (slot_size - 1) as i64;

    for param in &func.params {
        let is_float = param.ty.is_float();
        let is_i128 = is_i128_type(param.ty);
        let is_long_double = param.ty.is_long_double();
        let struct_size = param.struct_size;

        // Struct-by-value parameters.
        if let Some(size) = struct_size {
            // Zero-size structs consume no register or stack space per GCC behavior.
            if size == 0 {
                result.push(ParamClass::ZeroSizeSkip);
                continue;
            }

            let eb_classes = &param.struct_eightbyte_classes;

            if size <= 16 && config.use_sysv_struct_classification && !eb_classes.is_empty() {
                use super::call_abi::{classify_sysv_struct, SysvStructRegClass};
                let (cls, gp_used, fp_used) = classify_sysv_struct(eb_classes, int_reg_idx, float_reg_idx, config);
                match cls {
                    SysvStructRegClass::AllSse { fp_count } => {
                        let hi = if fp_count > 1 { Some(float_reg_idx + 1) } else { None };
                        result.push(ParamClass::StructSseReg { lo_fp_idx: float_reg_idx, hi_fp_idx: hi, size });
                    }
                    SysvStructRegClass::AllInt { .. } => {
                        result.push(ParamClass::StructByValReg { base_reg_idx: int_reg_idx, size });
                    }
                    SysvStructRegClass::IntSse => {
                        result.push(ParamClass::StructMixedIntSseReg { int_reg_idx, fp_reg_idx: float_reg_idx, size });
                    }
                    SysvStructRegClass::SseInt => {
                        result.push(ParamClass::StructMixedSseIntReg { fp_reg_idx: float_reg_idx, int_reg_idx, size });
                    }
                    SysvStructRegClass::Stack => {
                        result.push(ParamClass::StructStack { offset: stack_offset, size });
                        stack_offset += (size as i64 + slot_align_mask) & !slot_align_mask;
                        int_reg_idx = config.max_int_regs;
                    }
                }
                int_reg_idx += gp_used;
                float_reg_idx += fp_used;
            } else if size <= 16 {
                // Check RISC-V LP64D hardware floating-point struct classification first
                let rv_class = if config.use_riscv_float_struct_classification {
                    param.riscv_float_class
                } else {
                    None
                };
                let mut classified = false;
                if let Some(rv_fc) = rv_class {
                    use crate::common::types::RiscvFloatClass;
                    match rv_fc {
                        RiscvFloatClass::OneFloat { .. } => {
                            if float_reg_idx < config.max_float_regs {
                                result.push(ParamClass::StructSseReg { lo_fp_idx: float_reg_idx, hi_fp_idx: None, size });
                                float_reg_idx += 1;
                                classified = true;
                            }
                        }
                        RiscvFloatClass::TwoFloats { .. } => {
                            if float_reg_idx + 1 < config.max_float_regs {
                                result.push(ParamClass::StructSseReg { lo_fp_idx: float_reg_idx, hi_fp_idx: Some(float_reg_idx + 1), size });
                                float_reg_idx += 2;
                                classified = true;
                            }
                        }
                        RiscvFloatClass::FloatAndInt { .. } => {
                            if float_reg_idx < config.max_float_regs && int_reg_idx < config.max_int_regs {
                                result.push(ParamClass::StructMixedSseIntReg { fp_reg_idx: float_reg_idx, int_reg_idx, size });
                                float_reg_idx += 1;
                                int_reg_idx += 1;
                                classified = true;
                            }
                        }
                        RiscvFloatClass::IntAndFloat { .. } => {
                            if int_reg_idx < config.max_int_regs && float_reg_idx < config.max_float_regs {
                                result.push(ParamClass::StructMixedIntSseReg { int_reg_idx, fp_reg_idx: float_reg_idx, size });
                                int_reg_idx += 1;
                                float_reg_idx += 1;
                                classified = true;
                            }
                        }
                    }
                }
                if !classified {
                    let regs_needed = if size <= slot_size { 1 } else { size.div_ceil(slot_size) };
                    // RISC-V psABI: 2×XLEN-aligned structs must start at even register.
                    if regs_needed == 2 && config.align_i128_pairs {
                        let struct_align = param.struct_align.unwrap_or(slot_size);
                        if struct_align > slot_size && !int_reg_idx.is_multiple_of(2) {
                            int_reg_idx += 1; // skip to even register
                        }
                    }
                    if int_reg_idx + regs_needed <= config.max_int_regs {
                        result.push(ParamClass::StructByValReg {
                            base_reg_idx: int_reg_idx,
                            size,
                        });
                        int_reg_idx += regs_needed;
                    } else {
                        result.push(ParamClass::StructStack {
                            offset: stack_offset,
                            size,
                        });
                        stack_offset += (size as i64 + slot_align_mask) & !slot_align_mask;
                        int_reg_idx = config.max_int_regs;
                    }
                }
            } else if config.large_struct_by_ref {
                // AAPCS64: large composites arrive as a pointer in a GP register or on stack.
                // The callee must copy from the pointer into the local alloca.
                if int_reg_idx < config.max_int_regs {
                    result.push(ParamClass::LargeStructByRefReg { reg_idx: int_reg_idx, size });
                    int_reg_idx += 1;
                } else {
                    result.push(ParamClass::LargeStructByRefStack { offset: stack_offset, size });
                    stack_offset += slot_size as i64; // stack slot holds a pointer
                }
            } else {
                result.push(ParamClass::LargeStructStack {
                    offset: stack_offset,
                    size,
                });
                stack_offset += (size as i64 + slot_align_mask) & !slot_align_mask;
            }
            continue;
        }

        // i128 parameters.
        if is_i128 {
            if config.align_i128_pairs && !int_reg_idx.is_multiple_of(2) {
                int_reg_idx += 1;
            }
            if int_reg_idx + 1 < config.max_int_regs {
                result.push(ParamClass::I128RegPair {
                    base_reg_idx: int_reg_idx,
                });
                int_reg_idx += 2;
            } else {
                stack_offset = (stack_offset + 15) & !15;
                result.push(ParamClass::I128Stack { offset: stack_offset });
                stack_offset += 16;
                int_reg_idx = config.max_int_regs;
            }
            continue;
        }

        // F128 / long double parameters.
        if is_long_double {
            if config.f128_in_fp_regs {
                // ARM: F128 in Q-register (uses FP register slot).
                if float_reg_idx < config.max_float_regs {
                    result.push(ParamClass::F128FpReg { reg_idx: float_reg_idx });
                    float_reg_idx += 1;
                } else {
                    stack_offset = (stack_offset + 15) & !15;
                    result.push(ParamClass::F128Stack { offset: stack_offset });
                    stack_offset += 16;
                }
            } else if config.f128_in_gp_pairs {
                // RISC-V: F128 in aligned GP pair.
                if config.align_i128_pairs && !int_reg_idx.is_multiple_of(2) {
                    int_reg_idx += 1;
                }
                if int_reg_idx + 1 < config.max_int_regs {
                    result.push(ParamClass::F128GpPair {
                        lo_reg_idx: int_reg_idx,
                        hi_reg_idx: int_reg_idx + 1,
                    });
                    int_reg_idx += 2;
                } else {
                    stack_offset = (stack_offset + 15) & !15;
                    result.push(ParamClass::F128Stack { offset: stack_offset });
                    stack_offset += 16;
                    int_reg_idx = config.max_int_regs;
                }
            } else {
                // x86/i686: F128 always passes on the stack via x87.
                // On i686, x87 long double is 12 bytes (96-bit, 4-byte aligned).
                // On x86-64, it's 16 bytes (16-byte aligned).
                if slot_size == 4 {
                    // i686: long double is 12 bytes, 4-byte aligned
                    result.push(ParamClass::F128AlwaysStack { offset: stack_offset });
                    stack_offset += 12;
                } else {
                    // x86-64: 16 bytes, 16-byte aligned
                    stack_offset = (stack_offset + 15) & !15;
                    result.push(ParamClass::F128AlwaysStack { offset: stack_offset });
                    stack_offset += 16;
                }
            }
            continue;
        }

        // Float/double parameters.
        // On RISC-V variadic functions, float args go in GP registers instead of FP.
        let force_gp = config.variadic_floats_in_gp && is_float && !is_long_double;
        if is_float && !force_gp && float_reg_idx < config.max_float_regs {
            result.push(ParamClass::FloatReg { reg_idx: float_reg_idx });
            float_reg_idx += 1;
            continue;
        }
        // Float that overflowed FP registers goes to stack, not GP registers.
        if is_float && !force_gp {
            result.push(ParamClass::StackScalar { offset: stack_offset });
            // On i686, float is 4 bytes, double is 8 bytes - both use their natural sizes.
            // On LP64, minimum stack slot is 8 bytes.
            if slot_size == 4 {
                let float_stack_size = if param.ty == crate::common::types::IrType::F64 { 8 } else { 4 };
                stack_offset += float_stack_size;
            } else {
                stack_offset += 8;
            }
            continue;
        }

        // GP register or stack overflow.
        if int_reg_idx < config.max_int_regs {
            result.push(ParamClass::IntReg { reg_idx: int_reg_idx });
            int_reg_idx += 1;
        } else {
            result.push(ParamClass::StackScalar { offset: stack_offset });
            // On i686, I64/U64 take 8 bytes on stack, other scalars take 4
            let param_size = param.ty.size() as i64;
            stack_offset += (param_size + slot_align_mask) & !slot_align_mask;
        }
    }

    ParamClassification {
        classes: result,
        int_reg_idx,
        total_stack_bytes: stack_offset as usize,
    }
}

/// Classify all parameters of a function for callee-side store emission.
///
/// Uses the same `CallAbiConfig` as `classify_call_args` to ensure caller and callee
/// agree on parameter locations. Returns one `ParamClass` per parameter.
pub fn classify_params(func: &IrFunction, config: &CallAbiConfig) -> Vec<ParamClass> {
    classify_params_full(func, config).classes
}

/// Compute the total stack space (in bytes) consumed by named parameters that are
/// passed on the stack. This is needed for variadic functions: va_start must set its
/// stack pointer past all named stack-passed args to point at the first variadic arg.
///
/// This correctly accounts for alignment padding (e.g., 16-byte alignment for F128/I128).
pub fn named_params_stack_bytes(param_classes: &[ParamClass]) -> usize {
    let mut total: usize = 0;
    for class in param_classes {
        // Align for 16-byte types before adding their size
        if matches!(class, ParamClass::F128Stack { .. } | ParamClass::I128Stack { .. } | ParamClass::F128AlwaysStack { .. }) {
            total = (total + 15) & !15;
        }
        total += class.stack_bytes();
    }
    total
}
