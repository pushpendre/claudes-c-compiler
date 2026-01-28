//! Inline assembly statement lowering.
//!
//! Processes GCC-style `asm(template : outputs : inputs : clobbers : goto_labels)`
//! statements into IR InlineAsm instructions. Handles:
//! - Output operand processing (including "+" read-write constraints)
//! - Input operand processing (immediate, memory, register constraints)
//! - Register variable constraint rewriting (`register x asm("reg")`)
//! - Symbol name extraction for `%P`/`%c`/`%a` modifiers
//! - Address space detection for segment-override operands
//! - Goto label resolution

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{AddressSpace, IrType};
use super::lowering::Lowerer;
use super::definitions::LValue;
use crate::backend::inline_asm::{constraint_has_immediate_alt, constraint_is_memory_only, constraint_needs_address};

impl Lowerer {
    pub(super) fn lower_inline_asm_stmt(
        &mut self,
        template: &str,
        outputs: &[AsmOperand],
        inputs: &[AsmOperand],
        clobbers: &[String],
        goto_labels: &[String],
    ) {
        let mut ir_outputs = Vec::new();
        let mut ir_inputs = Vec::new();
        let mut operand_types = Vec::new();
        let mut seg_overrides = Vec::new();

        // Process output operands and synthetic "+" inputs
        let mut plus_input_types = Vec::new();
        let mut plus_input_segs = Vec::new();
        let mut plus_input_symbols: Vec<Option<String>> = Vec::new();
        for out in outputs {
            let mut constraint = out.constraint.clone();
            let name = out.name.clone();
            // Rewrite output constraint for register variables with __asm__("regname")
            if let Expr::Identifier(ref var_name, _) = out.expr {
                if let Some(asm_reg) = self.get_asm_register(var_name) {
                    let stripped = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
                    if stripped.contains('r') || stripped == "g" {
                        let prefix: String = constraint.chars().take_while(|c| *c == '=' || *c == '+' || *c == '&').collect();
                        constraint = format!("{}{{{}}}", prefix, asm_reg);
                    }
                }
            }
            let out_ty = IrType::from_ctype(&self.expr_ctype(&out.expr));
            // Detect address space for memory operands (e.g., __seg_gs pointer dereferences)
            let out_seg = self.get_asm_operand_addr_space(&out.expr);
            let ptr = if let Some(lv) = self.lower_lvalue(&out.expr) {
                match lv {
                    LValue::Variable(v) | LValue::Address(v, _) => v,
                }
            } else if let Expr::Identifier(ref var_name, _) = out.expr {
                // Global register variables are pinned to specific hardware registers
                // via constraint rewriting above (e.g., "+r" -> "+{rsp}"). They have no
                // backing storage, so lower_lvalue returns None. Create a temporary alloca
                // to preserve GCC operand numbering -- without this, subsequent operand
                // references (e.g., %P4) would be off-by-one and unresolvable.
                // TODO: The alloca is a placeholder for operand numbering only.
                // The output value stored here is discarded; proper write-back to global
                // register variables is not yet implemented. For "+" constraints, the
                // Load below reads uninitialized memory, but the backend's constraint
                // rewriting to {regname} makes it read from the actual hardware register.
                if self.get_asm_register(var_name).is_some() {
                    let tmp = self.fresh_value();
                    self.emit(Instruction::Alloca {
                        dest: tmp, ty: out_ty, size: out_ty.size(),
                        align: out_ty.align(), volatile: false,
                    });
                    tmp
                } else {
                    continue;
                }
            } else {
                continue;
            };
            if constraint.contains('+') {
                // For global register variables (e.g., `register long x asm("rsp")`),
                // the alloca is just a placeholder and is uninitialized. We must read
                // the current register value via an inline asm instead of loading from
                // the uninitialized alloca, which would corrupt the register (especially
                // critical for stack pointer register variables).
                let is_global_reg = if let Expr::Identifier(ref var_name, _) = out.expr {
                    self.get_asm_register(var_name).is_some() && self.lower_lvalue(&out.expr).is_none()
                } else {
                    false
                };
                let stripped_for_mem_check = constraint.replace('+', "");
                let needs_address = constraint_needs_address(&stripped_for_mem_check);
                let input_operand = if is_global_reg {
                    if let Expr::Identifier(ref var_name, _) = &out.expr {
                        let asm_reg = self.get_asm_register(var_name).unwrap();
                        self.read_global_register(&asm_reg, out_ty)
                    } else {
                        unreachable!()
                    }
                } else if needs_address {
                    // For "+m" (memory-only read-write) and "+A" (RISC-V address for
                    // AMO/LR/SC) constraints, do NOT emit a Load of the current value.
                    // These constraints need the ADDRESS (lvalue), not the value (rvalue).
                    //
                    // For "+m": the inline asm reads/writes memory directly through the
                    // template. Emitting a Load can crash when the memory address is only
                    // valid with a segment prefix (e.g., %gs: for per-CPU variables).
                    //
                    // For "+A": the address is loaded into a register and formatted as
                    // "(reg)" for AMO instructions. Loading the value would cause the
                    // atomic op to use the memory CONTENTS as an address, leading to
                    // crashes (e.g., kernel clear_bit using flag bits as an address).
                    //
                    // We still need a placeholder operand for correct operand numbering.
                    Operand::Value(ptr)
                } else {
                    let cur_val = self.fresh_value();
                    self.emit(Instruction::Load { dest: cur_val, ptr, ty: out_ty, seg_override: out_seg });
                    Operand::Value(cur_val)
                };
                ir_inputs.push((constraint.replace('+', "").to_string(), input_operand, name.clone()));
                plus_input_types.push(out_ty);
                plus_input_segs.push(out_seg);
                // For "+m" constraints, extract the symbol name so the backend can emit
                // direct symbol(%rip) references instead of register-indirect addressing.
                let stripped_constraint = constraint.replace('+', "");
                if constraint_is_memory_only(&stripped_constraint) {
                    plus_input_symbols.push(self.extract_mem_operand_symbol(&out.expr));
                } else {
                    plus_input_symbols.push(None);
                }
            }
            ir_outputs.push((constraint, ptr, name));
            operand_types.push(out_ty);
            seg_overrides.push(out_seg);
        }
        for ty in plus_input_types {
            operand_types.push(ty);
        }
        for seg in plus_input_segs {
            seg_overrides.push(seg);
        }

        // Process input operands
        // Start with symbols for synthetic "+" inputs (from output operands),
        // since those appear first in ir_inputs.
        let mut input_symbols: Vec<Option<String>> = plus_input_symbols;
        for inp in inputs {
            let mut constraint = inp.constraint.clone();
            let name = inp.name.clone();
            // Rewrite constraint for register variables with __asm__("regname"):
            // when the constraint allows "r", pin to the exact requested register.
            if let Expr::Identifier(ref var_name, _) = inp.expr {
                if let Some(asm_reg) = self.get_asm_register(var_name) {
                    let stripped = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
                    if stripped.contains('r') || stripped == "g" {
                        constraint = format!("{{{}}}", asm_reg);
                    }
                }
            }
            let inp_ty = IrType::from_ctype(&self.expr_ctype(&inp.expr));
            let inp_seg = self.get_asm_operand_addr_space(&inp.expr);
            let mut sym_name: Option<String> = None;
            let val = if constraint_has_immediate_alt(&constraint) {
                if let Some(const_val) = self.eval_const_expr(&inp.expr) {
                    Operand::Const(const_val)
                } else if let Some(s) = Self::peel_to_string_literal(&inp.expr) {
                    // String literals (possibly wrapped in casts) have assembly-time-
                    // constant addresses (their .rodata label), so they are valid "i"
                    // constraint immediates. Intern once and use the label as both the
                    // symbol name and the GlobalAddr operand.
                    let label = self.intern_string_literal(&s);
                    sym_name = Some(label.clone());
                    let dest = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest, name: label });
                    Operand::Value(dest)
                } else {
                    sym_name = self.extract_symbol_name(&inp.expr);
                    self.lower_expr(&inp.expr)
                }
            } else if constraint_needs_address(&constraint) {
                // For memory-only and address constraints (m, o, V, Q, A), provide the
                // address (lvalue) rather than the loaded value (rvalue).
                // For memory constraints, also try to extract the symbol name so the
                // backend can emit direct symbol(%rip) references instead of loading
                // addresses into registers, which is critical when the inline asm
                // template adds a segment prefix like %%gs:.
                // For RISC-V "A" constraints (AMO/LR/SC), the address is loaded into
                // a register and formatted as "(reg)" in the template.
                if constraint_is_memory_only(&constraint) {
                    sym_name = self.extract_mem_operand_symbol(&inp.expr);
                }
                if let Some(lv) = self.lower_lvalue(&inp.expr) {
                    let ptr = match lv {
                        LValue::Variable(v) | LValue::Address(v, _) => v,
                    };
                    Operand::Value(ptr)
                } else {
                    self.lower_expr(&inp.expr)
                }
            } else {
                self.lower_expr(&inp.expr)
            };
            ir_inputs.push((constraint, val, name));
            operand_types.push(inp_ty);
            input_symbols.push(sym_name);
            seg_overrides.push(inp_seg);
        }

        // Resolve goto labels
        let ir_goto_labels: Vec<(String, BlockId)> = goto_labels.iter().map(|name| {
            let block = self.get_or_create_user_label(name);
            (name.clone(), block)
        }).collect();

        self.emit(Instruction::InlineAsm {
            template: template.to_string(),
            outputs: ir_outputs,
            inputs: ir_inputs,
            clobbers: clobbers.to_vec(),
            operand_types,
            goto_labels: ir_goto_labels,
            input_symbols,
            seg_overrides,
        });
    }

    /// Extract a global symbol name (possibly with offset) from an expression,
    /// for use with inline asm `"i"` constraint operands and `%P`/`%c`/`%a`
    /// modifiers. Handles:
    /// - `func_name` (bare function identifier that is a known function or global)
    /// - `&var_name` (address-of global variable)
    /// - Casts of the above (e.g., `(void *)func_name`)
    /// - Complex global address expressions like `&((const char *)s.member)[N]`
    ///   which produce `"symbol+offset"` strings (e.g., `"boot_cpu_data+9"`)
    ///
    /// Returns `None` for local variables/parameters, since those are not valid
    /// assembly symbols and would produce invalid assembly if emitted literally.
    fn extract_symbol_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(name, _) => {
                // Only return the name if it is a global symbol or known function,
                // NOT a local variable or function parameter.
                if self.is_global_or_function(name) {
                    Some(name.clone())
                } else {
                    None
                }
            }
            Expr::AddressOf(inner, _) => {
                if let Expr::Identifier(name, _) = inner.as_ref() {
                    if self.is_global_or_function(name) {
                        Some(name.clone())
                    } else {
                        None
                    }
                } else {
                    // Try resolving complex address expressions like
                    // &((const char *)boot_cpu_data.x86_capability)[12 >> 3]
                    // via the global address evaluator.
                    self.extract_symbol_from_global_addr_expr(expr)
                }
            }
            Expr::Cast(_, inner, _) => self.extract_symbol_name(inner),
            _ => None,
        }
    }

    /// Peel through casts to find a narrow string literal. Returns the string
    /// content if the expression is a (possibly cast-wrapped) `StringLiteral`.
    /// Only handles narrow strings; wide/char16 literals are not expected in
    /// inline asm "i" constraint operands.
    fn peel_to_string_literal(expr: &Expr) -> Option<String> {
        match expr {
            Expr::StringLiteral(s, _) => Some(s.clone()),
            Expr::Cast(_, inner, _) => Self::peel_to_string_literal(inner),
            _ => None,
        }
    }

    /// Extract a global symbol name (with optional offset) from an expression used
    /// as a memory ("m") constraint operand in inline assembly.
    /// The expression is typically a dereference like `*(type*)(uintptr_t)(&global.field)`.
    /// We look through dereferences and casts to find the underlying address expression,
    /// then try to resolve it to a symbol+offset.
    fn extract_mem_operand_symbol(&self, expr: &Expr) -> Option<String> {
        match expr {
            // Dereference: look at the pointer expression
            Expr::Deref(inner, _) => {
                // The inner expression is a pointer. Try to get its symbol name as an address.
                self.extract_symbol_from_global_addr_expr(inner)
                    .or_else(|| self.extract_mem_operand_symbol(inner))
            }
            // Cast: look through it
            Expr::Cast(_, inner, _) => self.extract_mem_operand_symbol(inner),
            // Direct identifier: if it's a global, return its name
            Expr::Identifier(name, _) => {
                if self.is_global_or_function(name) {
                    Some(name.clone())
                } else {
                    None
                }
            }
            // Address-of: try to resolve as global address expression
            Expr::AddressOf(_, _) => self.extract_symbol_from_global_addr_expr(expr),
            // Member access on a global struct
            Expr::MemberAccess(base, _, _) | Expr::PointerMemberAccess(base, _, _) => {
                // Try to resolve the full expression as a global address (will include offset)
                self.extract_symbol_from_global_addr_expr(&Expr::AddressOf(Box::new(expr.clone()), expr.span()))
                    .or_else(|| self.extract_mem_operand_symbol(base))
            }
            _ => None,
        }
    }

    /// Try to resolve an expression to a global symbol name with optional offset,
    /// using the global address expression evaluator. Returns strings like
    /// `"boot_cpu_data"` or `"boot_cpu_data+9"`.
    fn extract_symbol_from_global_addr_expr(&self, expr: &Expr) -> Option<String> {
        use crate::ir::ir::GlobalInit;
        let init = self.eval_global_addr_expr(expr)?;
        match init {
            GlobalInit::GlobalAddr(name) => Some(name),
            GlobalInit::GlobalAddrOffset(name, offset) => {
                if offset >= 0 {
                    Some(format!("{}+{}", name, offset))
                } else {
                    Some(format!("{}{}", name, offset))
                }
            }
            _ => None,
        }
    }

    /// Check whether a name refers to a global variable, known function, or
    /// enum constant (i.e., something that is a valid assembly-level symbol or
    /// compile-time constant), as opposed to a local variable or parameter.
    fn is_global_or_function(&self, name: &str) -> bool {
        // Check if it's a local variable/parameter first -- if so, it's NOT global
        if let Some(ref fs) = self.func_state {
            if fs.locals.contains_key(name) {
                return false;
            }
        }
        // It's a global if it's in the globals map, known functions, or enum constants
        self.globals.contains_key(name)
            || self.known_functions.contains(name)
            || self.types.enum_constants.contains_key(name)
    }

    /// Look up the asm register name for a variable declared with
    /// `register <type> <name> __asm__("regname")`.
    /// Checks local variables first, then global register variables.
    pub(super) fn get_asm_register(&self, name: &str) -> Option<String> {
        // Check locals first
        if let Some(reg) = self.func_state.as_ref()
            .and_then(|fs| fs.locals.get(name))
            .and_then(|info| info.asm_register.clone())
        {
            return Some(reg);
        }
        // Check globals (for global register variables like `current_stack_pointer`)
        self.globals.get(name)
            .and_then(|info| info.asm_register.clone())
    }

    /// Detect address space for an inline asm operand expression.
    /// For expressions like `*(typeof(var) __seg_gs *)(uintptr_t)&var`,
    /// returns the address space from the pointer type in the deref.
    fn get_asm_operand_addr_space(&self, expr: &Expr) -> AddressSpace {
        match expr {
            Expr::Deref(inner, _) => self.get_addr_space_of_ptr_expr(inner),
            _ => AddressSpace::Default,
        }
    }
}
