//! Complex number lowering: handles _Complex type operations.
//!
//! Complex types (_Complex float, _Complex double, _Complex long double) are
//! represented in IR as stack-allocated pairs of floats: {real, imag}.
//! A complex value is always passed around as a pointer to this pair.
//!
//! Layout:
//!   - _Complex float:       [f32 real, f32 imag] = 8 bytes, align 4
//!   - _Complex double:      [f64 real, f64 imag] = 16 bytes, align 8
//!   - _Complex long double: [f128 real, f128 imag] = 32 bytes, align 16

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, CType};
use super::lowering::Lowerer;

impl Lowerer {
    /// Get the IR float type for a complex type's component.
    pub(super) fn complex_component_ir_type(ctype: &CType) -> IrType {
        match ctype {
            CType::ComplexFloat => IrType::F32,
            CType::ComplexDouble => IrType::F64,
            CType::ComplexLongDouble => IrType::F128,
            _ => IrType::F64, // fallback
        }
    }

    /// Get the component size in bytes for a complex type.
    pub(super) fn complex_component_size(ctype: &CType) -> usize {
        match ctype {
            CType::ComplexFloat => 4,
            CType::ComplexDouble => 8,
            CType::ComplexLongDouble => 16,
            _ => 8,
        }
    }

    /// Get the zero constant for a complex component type.
    pub(super) fn complex_zero(comp_ty: IrType) -> Operand {
        match comp_ty {
            IrType::F32 => Operand::Const(IrConst::F32(0.0)),
            _ => Operand::Const(IrConst::F64(0.0)),
        }
    }

    /// Allocate stack space for a complex value and return the alloca pointer.
    pub(super) fn alloca_complex(&mut self, ctype: &CType) -> Value {
        let size = ctype.size();
        let alloca = self.fresh_value();
        self.emit(Instruction::Alloca {
            dest: alloca,
            ty: IrType::Ptr,
            size,
        });
        alloca
    }

    /// Store a complex value (real, imag) into an alloca.
    pub(super) fn store_complex_parts(&mut self, ptr: Value, real: Operand, imag: Operand, ctype: &CType) {
        let comp_ty = Self::complex_component_ir_type(ctype);
        let comp_size = Self::complex_component_size(ctype);

        // Store real part at offset 0
        self.emit(Instruction::Store {
            val: real,
            ptr,
            ty: comp_ty,
        });

        // Store imag part at offset comp_size
        let imag_ptr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: imag_ptr,
            base: ptr,
            offset: Operand::Const(IrConst::I64(comp_size as i64)),
            ty: IrType::I8, // byte offset
        });
        self.emit(Instruction::Store {
            val: imag,
            ptr: imag_ptr,
            ty: comp_ty,
        });
    }

    /// Load the real part of a complex value from a pointer.
    pub(super) fn load_complex_real(&mut self, ptr: Value, ctype: &CType) -> Operand {
        let comp_ty = Self::complex_component_ir_type(ctype);
        let dest = self.fresh_value();
        self.emit(Instruction::Load {
            dest,
            ptr,
            ty: comp_ty,
        });
        Operand::Value(dest)
    }

    /// Load the imaginary part of a complex value from a pointer.
    pub(super) fn load_complex_imag(&mut self, ptr: Value, ctype: &CType) -> Operand {
        let comp_ty = Self::complex_component_ir_type(ctype);
        let comp_size = Self::complex_component_size(ctype);
        let imag_ptr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: imag_ptr,
            base: ptr,
            offset: Operand::Const(IrConst::I64(comp_size as i64)),
            ty: IrType::I8,
        });
        let dest = self.fresh_value();
        self.emit(Instruction::Load {
            dest,
            ptr: imag_ptr,
            ty: comp_ty,
        });
        Operand::Value(dest)
    }

    /// Lower an imaginary literal to a complex value on the stack.
    /// Creates {0.0, val} and returns a pointer to it.
    pub(super) fn lower_imaginary_literal(&mut self, val: f64, ctype: &CType) -> Operand {
        let alloca = self.alloca_complex(ctype);
        let comp_ty = Self::complex_component_ir_type(ctype);
        let zero = match comp_ty {
            IrType::F32 => Operand::Const(IrConst::F32(0.0)),
            IrType::F128 => Operand::Const(IrConst::LongDouble(0.0)),
            _ => Operand::Const(IrConst::F64(0.0)),
        };
        let imag_val = match comp_ty {
            IrType::F32 => Operand::Const(IrConst::F32(val as f32)),
            IrType::F128 => Operand::Const(IrConst::LongDouble(val)),
            _ => Operand::Const(IrConst::F64(val)),
        };
        self.store_complex_parts(alloca, zero, imag_val, ctype);
        Operand::Value(alloca)
    }

    /// Lower __real__ expr - extract real part of a complex expression.
    pub(super) fn lower_complex_real_part(&mut self, inner: &Expr) -> Operand {
        let inner_ctype = self.expr_ctype(inner);
        if inner_ctype.is_complex() {
            let ptr = self.lower_expr(inner);
            let ptr_val = self.operand_to_value(ptr);
            self.load_complex_real(ptr_val, &inner_ctype)
        } else {
            // __real__ on a non-complex value just returns the value
            self.lower_expr(inner)
        }
    }

    /// Lower __imag__ expr - extract imaginary part of a complex expression.
    pub(super) fn lower_complex_imag_part(&mut self, inner: &Expr) -> Operand {
        let inner_ctype = self.expr_ctype(inner);
        if inner_ctype.is_complex() {
            let ptr = self.lower_expr(inner);
            let ptr_val = self.operand_to_value(ptr);
            self.load_complex_imag(ptr_val, &inner_ctype)
        } else {
            // __imag__ on a non-complex value returns 0
            let ir_ty = self.get_expr_type(inner);
            if ir_ty.is_float() {
                match ir_ty {
                    IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                    IrType::F128 => Operand::Const(IrConst::LongDouble(0.0)),
                    _ => Operand::Const(IrConst::F64(0.0)),
                }
            } else {
                Operand::Const(IrConst::I64(0))
            }
        }
    }

    /// Lower complex addition: (a+bi) + (c+di) = (a+c) + (b+d)i
    pub(super) fn lower_complex_add(&mut self, lhs_ptr: Value, rhs_ptr: Value, ctype: &CType) -> Operand {
        let comp_ty = Self::complex_component_ir_type(ctype);

        let lr = self.load_complex_real(lhs_ptr, ctype);
        let li = self.load_complex_imag(lhs_ptr, ctype);
        let rr = self.load_complex_real(rhs_ptr, ctype);
        let ri = self.load_complex_imag(rhs_ptr, ctype);

        let real_sum = self.emit_binop_val(IrBinOp::Add, lr, rr, comp_ty);
        let imag_sum = self.emit_binop_val(IrBinOp::Add, li, ri, comp_ty);

        let result = self.alloca_complex(ctype);
        self.store_complex_parts(result, Operand::Value(real_sum), Operand::Value(imag_sum), ctype);
        Operand::Value(result)
    }

    /// Lower complex subtraction: (a+bi) - (c+di) = (a-c) + (b-d)i
    pub(super) fn lower_complex_sub(&mut self, lhs_ptr: Value, rhs_ptr: Value, ctype: &CType) -> Operand {
        let comp_ty = Self::complex_component_ir_type(ctype);

        let lr = self.load_complex_real(lhs_ptr, ctype);
        let li = self.load_complex_imag(lhs_ptr, ctype);
        let rr = self.load_complex_real(rhs_ptr, ctype);
        let ri = self.load_complex_imag(rhs_ptr, ctype);

        let real_diff = self.emit_binop_val(IrBinOp::Sub, lr, rr, comp_ty);
        let imag_diff = self.emit_binop_val(IrBinOp::Sub, li, ri, comp_ty);

        let result = self.alloca_complex(ctype);
        self.store_complex_parts(result, Operand::Value(real_diff), Operand::Value(imag_diff), ctype);
        Operand::Value(result)
    }

    /// Lower complex multiplication: (a+bi)(c+di) = (ac-bd) + (ad+bc)i
    pub(super) fn lower_complex_mul(&mut self, lhs_ptr: Value, rhs_ptr: Value, ctype: &CType) -> Operand {
        let comp_ty = Self::complex_component_ir_type(ctype);

        let a = self.load_complex_real(lhs_ptr, ctype);
        let b = self.load_complex_imag(lhs_ptr, ctype);
        let c = self.load_complex_real(rhs_ptr, ctype);
        let d = self.load_complex_imag(rhs_ptr, ctype);

        // ac
        let ac = self.emit_binop_val(IrBinOp::Mul, a.clone(), c.clone(), comp_ty);
        // bd
        let bd = self.emit_binop_val(IrBinOp::Mul, b.clone(), d.clone(), comp_ty);
        // ad
        let ad = self.emit_binop_val(IrBinOp::Mul, a, d, comp_ty);
        // bc
        let bc = self.emit_binop_val(IrBinOp::Mul, b, c, comp_ty);

        // real = ac - bd
        let real = self.emit_binop_val(IrBinOp::Sub, Operand::Value(ac), Operand::Value(bd), comp_ty);
        // imag = ad + bc
        let imag = self.emit_binop_val(IrBinOp::Add, Operand::Value(ad), Operand::Value(bc), comp_ty);

        let result = self.alloca_complex(ctype);
        self.store_complex_parts(result, Operand::Value(real), Operand::Value(imag), ctype);
        Operand::Value(result)
    }

    /// Lower complex division: (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c²+d²)
    pub(super) fn lower_complex_div(&mut self, lhs_ptr: Value, rhs_ptr: Value, ctype: &CType) -> Operand {
        let comp_ty = Self::complex_component_ir_type(ctype);

        let a = self.load_complex_real(lhs_ptr, ctype);
        let b = self.load_complex_imag(lhs_ptr, ctype);
        let c = self.load_complex_real(rhs_ptr, ctype);
        let d = self.load_complex_imag(rhs_ptr, ctype);

        // denom = c*c + d*d
        let cc = self.emit_binop_val(IrBinOp::Mul, c.clone(), c.clone(), comp_ty);
        let dd = self.emit_binop_val(IrBinOp::Mul, d.clone(), d.clone(), comp_ty);
        let denom = self.emit_binop_val(IrBinOp::Add, Operand::Value(cc), Operand::Value(dd), comp_ty);

        // real_num = a*c + b*d
        let ac = self.emit_binop_val(IrBinOp::Mul, a.clone(), c.clone(), comp_ty);
        let bd = self.emit_binop_val(IrBinOp::Mul, b.clone(), d.clone(), comp_ty);
        let real_num = self.emit_binop_val(IrBinOp::Add, Operand::Value(ac), Operand::Value(bd), comp_ty);

        // imag_num = b*c - a*d
        let bc = self.emit_binop_val(IrBinOp::Mul, b, c, comp_ty);
        let ad = self.emit_binop_val(IrBinOp::Mul, a, d, comp_ty);
        let imag_num = self.emit_binop_val(IrBinOp::Sub, Operand::Value(bc), Operand::Value(ad), comp_ty);

        // real = real_num / denom, imag = imag_num / denom
        let real = self.emit_binop_val(IrBinOp::SDiv, Operand::Value(real_num), Operand::Value(denom), comp_ty);
        let imag = self.emit_binop_val(IrBinOp::SDiv, Operand::Value(imag_num), Operand::Value(denom), comp_ty);

        let result = self.alloca_complex(ctype);
        self.store_complex_parts(result, Operand::Value(real), Operand::Value(imag), ctype);
        Operand::Value(result)
    }

    /// Lower complex negation: -(a+bi) = (-a) + (-b)i
    pub(super) fn lower_complex_neg(&mut self, ptr: Value, ctype: &CType) -> Operand {
        let comp_ty = Self::complex_component_ir_type(ctype);

        let r = self.load_complex_real(ptr, ctype);
        let i = self.load_complex_imag(ptr, ctype);

        let neg_r = self.fresh_value();
        self.emit(Instruction::UnaryOp { dest: neg_r, op: IrUnaryOp::Neg, src: r, ty: comp_ty });
        let neg_i = self.fresh_value();
        self.emit(Instruction::UnaryOp { dest: neg_i, op: IrUnaryOp::Neg, src: i, ty: comp_ty });

        let result = self.alloca_complex(ctype);
        self.store_complex_parts(result, Operand::Value(neg_r), Operand::Value(neg_i), ctype);
        Operand::Value(result)
    }

    /// Lower complex conjugate: conj(a+bi) = a + (-b)i
    pub(super) fn lower_complex_conj(&mut self, inner: &Expr) -> Operand {
        let inner_ctype = self.expr_ctype(inner);
        if inner_ctype.is_complex() {
            let ptr = self.lower_expr(inner);
            let ptr_val = self.operand_to_value(ptr);
            let comp_ty = Self::complex_component_ir_type(&inner_ctype);

            let r = self.load_complex_real(ptr_val, &inner_ctype);
            let i = self.load_complex_imag(ptr_val, &inner_ctype);

            let neg_i = self.fresh_value();
            self.emit(Instruction::UnaryOp { dest: neg_i, op: IrUnaryOp::Neg, src: i, ty: comp_ty });

            let result = self.alloca_complex(&inner_ctype);
            self.store_complex_parts(result, r, Operand::Value(neg_i), &inner_ctype);
            Operand::Value(result)
        } else {
            // conj on a non-complex value just returns the value
            self.lower_expr(inner)
        }
    }

    /// Create a complex value from a real scalar (real part = scalar, imag = 0).
    pub(super) fn real_to_complex(&mut self, real_val: Operand, real_type: &CType, complex_type: &CType) -> Operand {
        let real_ir = IrType::from_ctype(real_type);
        self.scalar_to_complex(real_val, real_ir, complex_type)
    }

    /// Create a complex value from a scalar given its IR type (real part = scalar, imag = 0).
    /// This is preferred over real_to_complex when the CType may be inaccurate (e.g., for
    /// function calls where expr_ctype may not correctly resolve the return type).
    pub(super) fn scalar_to_complex(&mut self, real_val: Operand, src_ir_ty: IrType, complex_type: &CType) -> Operand {
        let comp_ty = Self::complex_component_ir_type(complex_type);
        let zero = match comp_ty {
            IrType::F32 => Operand::Const(IrConst::F32(0.0)),
            IrType::F128 => Operand::Const(IrConst::LongDouble(0.0)),
            _ => Operand::Const(IrConst::F64(0.0)),
        };

        // Convert the real value to the component type if needed
        let converted_real = if src_ir_ty != comp_ty {
            let dest = self.emit_cast_val(real_val, src_ir_ty, comp_ty);
            Operand::Value(dest)
        } else {
            real_val
        };

        let result = self.alloca_complex(complex_type);
        self.store_complex_parts(result, converted_real, zero, complex_type);
        Operand::Value(result)
    }

    /// Convert a complex value from one complex type to another (e.g., ComplexFloat -> ComplexDouble).
    pub(super) fn complex_to_complex(&mut self, ptr: Value, from_type: &CType, to_type: &CType) -> Operand {
        let from_comp = Self::complex_component_ir_type(from_type);
        let to_comp = Self::complex_component_ir_type(to_type);

        let r = self.load_complex_real(ptr, from_type);
        let i = self.load_complex_imag(ptr, from_type);

        let conv_r = if from_comp != to_comp {
            let dest = self.emit_cast_val(r, from_comp, to_comp);
            Operand::Value(dest)
        } else {
            r
        };
        let conv_i = if from_comp != to_comp {
            let dest = self.emit_cast_val(i, from_comp, to_comp);
            Operand::Value(dest)
        } else {
            i
        };

        let result = self.alloca_complex(to_type);
        self.store_complex_parts(result, conv_r, conv_i, to_type);
        Operand::Value(result)
    }

    /// Determine the CType of an expression.
    /// This is needed for complex number operations to know the complex variant.
    pub(super) fn expr_ctype(&self, expr: &Expr) -> CType {
        match expr {
            Expr::ImaginaryLiteral(_, _) => CType::ComplexDouble,
            Expr::ImaginaryLiteralF32(_, _) => CType::ComplexFloat,
            Expr::ImaginaryLiteralLongDouble(_, _) => CType::ComplexLongDouble,
            Expr::FloatLiteral(_, _) => CType::Double,
            Expr::FloatLiteralF32(_, _) => CType::Float,
            Expr::FloatLiteralLongDouble(_, _) => CType::LongDouble,
            Expr::IntLiteral(_, _) | Expr::CharLiteral(_, _) => CType::Int,
            Expr::UIntLiteral(_, _) => CType::UInt,
            Expr::LongLiteral(_, _) => CType::Long,
            Expr::ULongLiteral(_, _) => CType::ULong,
            Expr::Identifier(name, _) => {
                self.get_var_ctype(name)
            }
            Expr::UnaryOp(UnaryOp::RealPart, inner, _) | Expr::UnaryOp(UnaryOp::ImagPart, inner, _) => {
                let inner_ct = self.expr_ctype(inner);
                inner_ct.complex_component_type()
            }
            Expr::UnaryOp(UnaryOp::Neg, inner, _) | Expr::UnaryOp(UnaryOp::Plus, inner, _) => {
                self.expr_ctype(inner)
            }
            Expr::Cast(type_spec, _, _) => {
                self.type_spec_to_ctype(type_spec)
            }
            Expr::BinaryOp(BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div, lhs, rhs, _) => {
                let lt = self.expr_ctype(lhs);
                let rt = self.expr_ctype(rhs);
                if lt.is_complex() || rt.is_complex() {
                    // Result is complex
                    self.common_complex_type(&lt, &rt)
                } else {
                    lt // approximate
                }
            }
            Expr::FunctionCall(callee, args, _) => {
                if let Expr::Identifier(name, _) = callee.as_ref() {
                    // __builtin_complex returns complex type based on argument types
                    if name == "__builtin_complex" {
                        if let Some(first_arg) = args.first() {
                            let arg_ct = self.expr_ctype(first_arg);
                            return match arg_ct {
                                CType::Float => CType::ComplexFloat,
                                CType::LongDouble => CType::ComplexLongDouble,
                                _ => CType::ComplexDouble,
                            };
                        }
                        return CType::ComplexDouble;
                    }
                    self.get_function_return_ctype(name)
                } else {
                    CType::Int
                }
            }
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                self.expr_ctype(lhs)
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                let then_ct = self.expr_ctype(then_expr);
                if then_ct.is_complex() {
                    then_ct
                } else {
                    let else_ct = self.expr_ctype(else_expr);
                    if else_ct.is_complex() { else_ct } else { then_ct }
                }
            }
            Expr::GnuConditional(cond, else_expr, _) => {
                let cond_ct = self.expr_ctype(cond);
                if cond_ct.is_complex() {
                    cond_ct
                } else {
                    let else_ct = self.expr_ctype(else_expr);
                    if else_ct.is_complex() { else_ct } else { cond_ct }
                }
            }
            Expr::MemberAccess(base, field, _) => {
                // Try to resolve struct field type
                let base_ct = self.expr_ctype(base);
                self.resolve_field_type(&base_ct, field)
            }
            Expr::PointerMemberAccess(base, field, _) => {
                let base_ct = self.expr_ctype(base);
                if let CType::Pointer(inner) = &base_ct {
                    self.resolve_field_type(inner, field)
                } else {
                    CType::Int
                }
            }
            Expr::Deref(inner, _) => {
                let inner_ct = self.expr_ctype(inner);
                match inner_ct {
                    CType::Pointer(pointee) => *pointee,
                    // Arrays decay to pointers, so *array yields the element type
                    CType::Array(elem, _) => *elem,
                    _ => CType::Int,
                }
            }
            Expr::ArraySubscript(base, _, _) => {
                let base_ct = self.expr_ctype(base);
                match base_ct {
                    CType::Array(elem, _) | CType::Pointer(elem) => *elem,
                    _ => CType::Int,
                }
            }
            Expr::Comma(_, rhs, _) => self.expr_ctype(rhs),
            Expr::CompoundLiteral(type_spec, _, _) => {
                self.type_spec_to_ctype(type_spec)
            }
            Expr::VaArg(_, type_spec, _) => {
                self.type_spec_to_ctype(type_spec)
            }
            Expr::StmtExpr(compound, _) => {
                // Statement expression: type is the type of the last expression in the block
                if let Some(last) = compound.items.last() {
                    if let BlockItem::Statement(Stmt::Expr(Some(expr))) = last {
                        return self.expr_ctype(expr);
                    }
                }
                CType::Int
            }
            Expr::AddressOf(inner, _) => {
                let inner_ct = self.expr_ctype(inner);
                CType::Pointer(Box::new(inner_ct))
            }
            _ => {
                // Fallback: try get_expr_ctype for more precise type info
                if let Some(ct) = self.get_expr_ctype(expr) {
                    return ct;
                }
                CType::Int
            }
        }
    }

    /// Get variable's CType from symbol table.
    pub(super) fn get_var_ctype(&self, name: &str) -> CType {
        // Check var_ctypes (explicit tracking for complex vars)
        if let Some(ctype) = self.var_ctypes.get(name) {
            return ctype.clone();
        }
        // Check locals/globals shared VarInfo
        if let Some(vi) = self.lookup_var_info(name) {
            if let Some(ref ct) = vi.c_type {
                return ct.clone();
            }
        }
        CType::Int
    }

    /// Get a function's return CType.
    pub(super) fn get_function_return_ctype(&self, name: &str) -> CType {
        // Check complex return types first
        if let Some(ctype) = self.func_return_ctypes.get(name) {
            return ctype.clone();
        }
        // Check pointer/struct return CTypes
        if let Some(ctype) = self.func_meta.return_ctypes.get(name) {
            return ctype.clone();
        }
        // Derive CType from IR return type
        if let Some(&ir_ty) = self.func_meta.return_types.get(name) {
            return match ir_ty {
                IrType::F32 => CType::Float,
                IrType::F64 => CType::Double,
                IrType::F128 => CType::LongDouble,
                IrType::I8 => CType::Char,
                IrType::U8 => CType::UChar,
                IrType::I16 => CType::Short,
                IrType::U16 => CType::UShort,
                IrType::I32 => CType::Int,
                IrType::U32 => CType::UInt,
                IrType::I64 => CType::Long,
                IrType::U64 => CType::ULong,
                IrType::Ptr => CType::Pointer(Box::new(CType::Void)),
                _ => CType::Int,
            };
        }
        CType::Int
    }

    /// Resolve struct field type.
    fn resolve_field_type(&self, struct_type: &CType, field: &str) -> CType {
        match struct_type {
            CType::Struct(st) | CType::Union(st) => {
                for f in &st.fields {
                    if f.name == field {
                        return f.ty.clone();
                    }
                }
                CType::Int
            }
            _ => CType::Int,
        }
    }

    /// Determine the common complex type for binary operations.
    /// Per C11 6.3.1.8: integer types rank below float in the type hierarchy,
    /// so they adopt the complex type of the other operand rather than forcing
    /// promotion to ComplexDouble.
    pub(super) fn common_complex_type(&self, a: &CType, b: &CType) -> CType {
        // Per C11 6.3.1.8: if one operand is complex and the other is real/integer,
        // the result type is determined by the "usual arithmetic conversions" applied
        // to the real type of the complex operand and the type of the other operand.
        // Integer types smaller than double don't force promotion to ComplexDouble;
        // instead, the result keeps the complex type unless the other operand is wider.
        let rank_of = |ct: &CType| -> u8 {
            match ct {
                CType::ComplexLongDouble | CType::LongDouble => 3,
                CType::ComplexDouble | CType::Double => 2,
                CType::ComplexFloat | CType::Float => 1,
                // Integer types: C standard says integer types convert to double
                // when combined with a complex type that is double or wider,
                // but when combined with complex float, they convert to float.
                // However, long long / unsigned long may exceed float precision.
                // For simplicity and correctness with most tests, we use the
                // C rule: "the corresponding real type of the complex" wins unless
                // the real operand has higher floating rank.
                CType::Long | CType::ULong | CType::LongLong | CType::ULongLong => 0,
                _ => 0, // char, short, int, uint -> rank 0 (no floating rank)
            }
        };

        let a_rank = rank_of(a);
        let b_rank = rank_of(b);

        // Result complex type is the wider of the two ranks.
        // If one is complex and the other is integer (rank 0), the complex type wins.
        let max_rank = a_rank.max(b_rank);
        match max_rank {
            3 => CType::ComplexLongDouble,
            2 => CType::ComplexDouble,
            1 => CType::ComplexFloat,
            _ => {
                // Both are integer types - promote to ComplexDouble (C default)
                // But check if one is already complex
                if a.is_complex() { return a.clone(); }
                if b.is_complex() { return b.clone(); }
                CType::ComplexDouble
            }
        }
    }

    /// Convert a value to a complex type, handling both real-to-complex and complex-to-complex.
    pub(super) fn convert_to_complex(&mut self, val: Operand, from_type: &CType, to_type: &CType) -> Operand {
        if from_type.is_complex() {
            if from_type == to_type {
                val
            } else {
                let ptr = self.operand_to_value(val);
                self.complex_to_complex(ptr, from_type, to_type)
            }
        } else {
            // Real to complex
            self.real_to_complex(val, from_type, to_type)
        }
    }

    /// Lower assignment to a complex variable.
    pub(super) fn lower_complex_assign(&mut self, lhs: &Expr, rhs: &Expr, lhs_ct: &CType) -> Operand {
        let rhs_ct = self.expr_ctype(rhs);
        let rhs_val = self.lower_expr(rhs);

        // Convert RHS to the LHS complex type
        let rhs_complex = if rhs_ct.is_complex() {
            if rhs_ct == *lhs_ct {
                rhs_val.clone()
            } else {
                self.convert_to_complex(rhs_val.clone(), &rhs_ct, lhs_ct)
            }
        } else {
            // Real value -> complex
            self.real_to_complex(rhs_val.clone(), &rhs_ct, lhs_ct)
        };

        // Get LHS pointer
        let lhs_ptr = self.lower_complex_lvalue(lhs);

        // Copy real and imag parts from rhs to lhs
        let rhs_ptr = self.operand_to_value(rhs_complex);
        let comp_size = Self::complex_component_size(lhs_ct);
        let total_size = comp_size * 2;
        self.emit(Instruction::Memcpy {
            dest: lhs_ptr,
            src: rhs_ptr,
            size: total_size,
        });

        // Return the lhs pointer
        Operand::Value(lhs_ptr)
    }

    /// Get the lvalue (address) of a complex expression for assignment purposes.
    pub(super) fn lower_complex_lvalue(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Identifier(name, _) => {
                // Look up in locals
                if let Some(info) = self.locals.get(name).cloned() {
                    return info.alloca;
                }
                // Check static locals
                if let Some(mangled) = self.static_local_names.get(name).cloned() {
                    let addr = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest: addr, name: mangled });
                    return addr;
                }
                // Global
                let addr = self.fresh_value();
                self.emit(Instruction::GlobalAddr { dest: addr, name: name.to_string() });
                addr
            }
            Expr::Deref(inner, _) => {
                let ptr = self.lower_expr(inner);
                self.operand_to_value(ptr)
            }
            Expr::ArraySubscript(base, index, _) => {
                let base_val = self.lower_expr(base);
                let idx_val = self.lower_expr(index);
                let elem_size = self.expr_ctype(expr).size();
                let scaled = self.scale_index(idx_val, elem_size);
                let dest = self.emit_binop_val(IrBinOp::Add, base_val, scaled, IrType::I64);
                dest
            }
            Expr::MemberAccess(base, field, _) => {
                // Get base struct address and add field offset
                let base_ct = self.expr_ctype(base);
                if let CType::Struct(ref st) = base_ct {
                    let layout = crate::common::types::StructLayout::for_struct(&st.fields);
                    if let Some((offset, _)) = layout.field_offset(field) {
                        let base_ptr = self.lower_complex_lvalue(base);
                        if offset == 0 {
                            return base_ptr;
                        }
                        let dest = self.fresh_value();
                        self.emit(Instruction::GetElementPtr {
                            dest, base: base_ptr,
                            offset: Operand::Const(IrConst::I64(offset as i64)),
                            ty: IrType::I8,
                        });
                        return dest;
                    }
                }
                // Fallback
                let val = self.lower_expr(expr);
                self.operand_to_value(val)
            }
            Expr::PointerMemberAccess(base, field, _) => {
                let base_val = self.lower_expr(base);
                let base_ptr = self.operand_to_value(base_val);
                let base_ct = self.expr_ctype(base);
                if let CType::Pointer(ref inner) = base_ct {
                    if let CType::Struct(ref st) = **inner {
                        let layout = crate::common::types::StructLayout::for_struct(&st.fields);
                        if let Some((offset, _)) = layout.field_offset(field) {
                            if offset == 0 {
                                return base_ptr;
                            }
                            let dest = self.fresh_value();
                            self.emit(Instruction::GetElementPtr {
                                dest, base: base_ptr,
                                offset: Operand::Const(IrConst::I64(offset as i64)),
                                ty: IrType::I8,
                            });
                            return dest;
                        }
                    }
                }
                base_ptr
            }
            _ => {
                // Fallback: treat expression result as pointer
                let val = self.lower_expr(expr);
                self.operand_to_value(val)
            }
        }
    }

    /// Lower complex equality/inequality comparison.
    /// Two complex numbers are equal iff both real and imaginary parts are equal.
    pub(super) fn lower_complex_comparison(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr, lhs_ct: &CType, rhs_ct: &CType) -> Operand {
        let result_ct = self.common_complex_type(lhs_ct, rhs_ct);
        let comp_ty = Self::complex_component_ir_type(&result_ct);

        // Lower and convert both to common complex type
        let lhs_val = self.lower_expr(lhs);
        let rhs_val = self.lower_expr(rhs);
        let lhs_complex = self.convert_to_complex(lhs_val, lhs_ct, &result_ct);
        let rhs_complex = self.convert_to_complex(rhs_val, rhs_ct, &result_ct);
        let lhs_ptr = self.operand_to_value(lhs_complex);
        let rhs_ptr = self.operand_to_value(rhs_complex);

        // Compare real parts
        let lr = self.load_complex_real(lhs_ptr, &result_ct);
        let rr = self.load_complex_real(rhs_ptr, &result_ct);
        let real_eq = self.emit_cmp_val(IrCmpOp::Eq, lr, rr, comp_ty);

        // Compare imag parts
        let li = self.load_complex_imag(lhs_ptr, &result_ct);
        let ri = self.load_complex_imag(rhs_ptr, &result_ct);
        let imag_eq = self.emit_cmp_val(IrCmpOp::Eq, li, ri, comp_ty);

        // Combine: both must be equal for == (either not equal for !=)
        let result = match op {
            BinOp::Eq => {
                self.emit_binop_val(IrBinOp::And, Operand::Value(real_eq), Operand::Value(imag_eq), IrType::I64)
            }
            BinOp::Ne => {
                // a != b iff !(a.re == b.re && a.im == b.im)
                let both_eq = self.emit_binop_val(IrBinOp::And, Operand::Value(real_eq), Operand::Value(imag_eq), IrType::I64);
                self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(both_eq), Operand::Const(IrConst::I64(0)), IrType::I64)
            }
            _ => unreachable!(),
        };
        Operand::Value(result)
    }

    /// Evaluate a complex expression for global initialization.
    /// Returns a GlobalInit::Array with [real, imag] constant values.
    pub(super) fn eval_complex_global_init(&self, expr: &Expr, target_type: &TypeSpecifier) -> Option<GlobalInit> {
        let (real, imag) = self.eval_complex_const(expr)?;
        match target_type {
            TypeSpecifier::ComplexFloat => Some(GlobalInit::Array(vec![
                IrConst::F32(real as f32),
                IrConst::F32(imag as f32),
            ])),
            TypeSpecifier::ComplexLongDouble => Some(GlobalInit::Array(vec![
                IrConst::LongDouble(real),
                IrConst::LongDouble(imag),
            ])),
            _ => Some(GlobalInit::Array(vec![
                IrConst::F64(real),
                IrConst::F64(imag),
            ])),
        }
    }

    /// Public wrapper for eval_complex_const, used by global_init for complex array elements.
    pub(super) fn eval_complex_const_public(&self, expr: &Expr) -> Option<(f64, f64)> {
        self.eval_complex_const(expr)
    }

    /// Try to evaluate a complex constant expression, returning (real, imag) as f64.
    fn eval_complex_const(&self, expr: &Expr) -> Option<(f64, f64)> {
        match expr {
            // Imaginary literals: 4.0i -> (0, 4.0)
            Expr::ImaginaryLiteral(val, _) => Some((0.0, *val)),
            Expr::ImaginaryLiteralF32(val, _) => Some((0.0, *val)),
            Expr::ImaginaryLiteralLongDouble(val, _) => Some((0.0, *val)),
            // Binary add: real + imag, or complex + complex
            Expr::BinaryOp(BinOp::Add, lhs, rhs, _) => {
                let l = self.eval_complex_const(lhs)?;
                let r = self.eval_complex_const(rhs)?;
                Some((l.0 + r.0, l.1 + r.1))
            }
            // Binary sub: real - imag
            Expr::BinaryOp(BinOp::Sub, lhs, rhs, _) => {
                let l = self.eval_complex_const(lhs)?;
                let r = self.eval_complex_const(rhs)?;
                Some((l.0 - r.0, l.1 - r.1))
            }
            // Binary mul: for simple cases like 2*I or val*I
            Expr::BinaryOp(BinOp::Mul, lhs, rhs, _) => {
                let l = self.eval_complex_const(lhs)?;
                let r = self.eval_complex_const(rhs)?;
                // (a+bi)(c+di) = (ac-bd) + (ad+bc)i
                let real = l.0 * r.0 - l.1 * r.1;
                let imag = l.0 * r.1 + l.1 * r.0;
                Some((real, imag))
            }
            // Unary negation
            Expr::UnaryOp(UnaryOp::Neg, inner, _) => {
                let v = self.eval_complex_const(inner)?;
                Some((-v.0, -v.1))
            }
            // Unary plus
            Expr::UnaryOp(UnaryOp::Plus, inner, _) => {
                self.eval_complex_const(inner)
            }
            // Cast to complex
            Expr::Cast(_, inner, _) => {
                self.eval_complex_const(inner)
            }
            // Real number literal -> (val, 0)
            _ => {
                if let Some(val) = self.eval_const_expr(expr) {
                    let f = match val {
                        IrConst::F64(v) => v,
                        IrConst::F32(v) => v as f64,
                        IrConst::I64(v) => v as f64,
                        IrConst::I32(v) => v as f64,
                        _ => return None,
                    };
                    Some((f, 0.0))
                } else {
                    None
                }
            }
        }
    }

    /// Decompose complex arguments at a call site for ABI compliance.
    /// Replaces complex double/float pointer arguments with (real, imag) pairs.
    /// Per SysV ABI:
    /// - _Complex float: decompose to 2 F32 values (passed in XMM registers)
    /// - _Complex double: decompose to 2 F64 values (passed in XMM registers)
    /// - _Complex long double: keep as pointer (passed on stack)
    pub(super) fn decompose_complex_call_args(
        &mut self,
        arg_vals: &mut Vec<Operand>,
        arg_types: &mut Vec<IrType>,
        param_ctypes: &Option<Vec<CType>>,
        args: &[Expr],
    ) {
        let pctypes = match param_ctypes {
            Some(ref ct) => ct.clone(),
            None => {
                // No param type info available - try to infer from expressions
                args.iter().map(|a| self.expr_ctype(a)).collect()
            }
        };

        let mut new_vals = Vec::with_capacity(arg_vals.len() * 2);
        let mut new_types = Vec::with_capacity(arg_types.len() * 2);

        for (i, (val, ty)) in arg_vals.iter().zip(arg_types.iter()).enumerate() {
            let ctype = pctypes.get(i);
            let should_decompose = match ctype {
                Some(CType::ComplexDouble) | Some(CType::ComplexLongDouble) => true,
                Some(_) => false, // param type is known but not a decomposed complex type
                None => {
                    // No param type info: check if the arg_type is Ptr and the expression has complex type
                    if *ty == IrType::Ptr && i < args.len() {
                        let arg_ct = self.expr_ctype(&args[i]);
                        matches!(arg_ct, CType::ComplexDouble | CType::ComplexLongDouble)
                    } else {
                        false
                    }
                }
            };

            if should_decompose {
                let complex_ct = ctype.cloned().unwrap_or_else(|| {
                    if i < args.len() { self.expr_ctype(&args[i]) } else { CType::ComplexDouble }
                });
                let comp_ty = Self::complex_component_ir_type(&complex_ct);
                let ptr = self.operand_to_value(val.clone());

                // Load real part
                let real = self.load_complex_real(ptr, &complex_ct);
                // Load imag part
                let imag = self.load_complex_imag(ptr, &complex_ct);

                new_vals.push(real);
                new_types.push(comp_ty);
                new_vals.push(imag);
                new_types.push(comp_ty);
            } else {
                new_vals.push(val.clone());
                new_types.push(*ty);
            }
        }

        *arg_vals = new_vals;
        *arg_types = new_types;
    }
}
