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

        let real_sum = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: real_sum, op: IrBinOp::Add,
            lhs: lr, rhs: rr, ty: comp_ty,
        });
        let imag_sum = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: imag_sum, op: IrBinOp::Add,
            lhs: li, rhs: ri, ty: comp_ty,
        });

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

        let real_diff = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: real_diff, op: IrBinOp::Sub,
            lhs: lr, rhs: rr, ty: comp_ty,
        });
        let imag_diff = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: imag_diff, op: IrBinOp::Sub,
            lhs: li, rhs: ri, ty: comp_ty,
        });

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
        let ac = self.fresh_value();
        self.emit(Instruction::BinOp { dest: ac, op: IrBinOp::Mul, lhs: a.clone(), rhs: c.clone(), ty: comp_ty });
        // bd
        let bd = self.fresh_value();
        self.emit(Instruction::BinOp { dest: bd, op: IrBinOp::Mul, lhs: b.clone(), rhs: d.clone(), ty: comp_ty });
        // ad
        let ad = self.fresh_value();
        self.emit(Instruction::BinOp { dest: ad, op: IrBinOp::Mul, lhs: a, rhs: d, ty: comp_ty });
        // bc
        let bc = self.fresh_value();
        self.emit(Instruction::BinOp { dest: bc, op: IrBinOp::Mul, lhs: b, rhs: c, ty: comp_ty });

        // real = ac - bd
        let real = self.fresh_value();
        self.emit(Instruction::BinOp { dest: real, op: IrBinOp::Sub, lhs: Operand::Value(ac), rhs: Operand::Value(bd), ty: comp_ty });
        // imag = ad + bc
        let imag = self.fresh_value();
        self.emit(Instruction::BinOp { dest: imag, op: IrBinOp::Add, lhs: Operand::Value(ad), rhs: Operand::Value(bc), ty: comp_ty });

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
        let cc = self.fresh_value();
        self.emit(Instruction::BinOp { dest: cc, op: IrBinOp::Mul, lhs: c.clone(), rhs: c.clone(), ty: comp_ty });
        let dd = self.fresh_value();
        self.emit(Instruction::BinOp { dest: dd, op: IrBinOp::Mul, lhs: d.clone(), rhs: d.clone(), ty: comp_ty });
        let denom = self.fresh_value();
        self.emit(Instruction::BinOp { dest: denom, op: IrBinOp::Add, lhs: Operand::Value(cc), rhs: Operand::Value(dd), ty: comp_ty });

        // real_num = a*c + b*d
        let ac = self.fresh_value();
        self.emit(Instruction::BinOp { dest: ac, op: IrBinOp::Mul, lhs: a.clone(), rhs: c.clone(), ty: comp_ty });
        let bd = self.fresh_value();
        self.emit(Instruction::BinOp { dest: bd, op: IrBinOp::Mul, lhs: b.clone(), rhs: d.clone(), ty: comp_ty });
        let real_num = self.fresh_value();
        self.emit(Instruction::BinOp { dest: real_num, op: IrBinOp::Add, lhs: Operand::Value(ac), rhs: Operand::Value(bd), ty: comp_ty });

        // imag_num = b*c - a*d
        let bc = self.fresh_value();
        self.emit(Instruction::BinOp { dest: bc, op: IrBinOp::Mul, lhs: b, rhs: c, ty: comp_ty });
        let ad = self.fresh_value();
        self.emit(Instruction::BinOp { dest: ad, op: IrBinOp::Mul, lhs: a, rhs: d, ty: comp_ty });
        let imag_num = self.fresh_value();
        self.emit(Instruction::BinOp { dest: imag_num, op: IrBinOp::Sub, lhs: Operand::Value(bc), rhs: Operand::Value(ad), ty: comp_ty });

        // real = real_num / denom, imag = imag_num / denom
        let real = self.fresh_value();
        self.emit(Instruction::BinOp { dest: real, op: IrBinOp::SDiv, lhs: Operand::Value(real_num), rhs: Operand::Value(denom), ty: comp_ty });
        let imag = self.fresh_value();
        self.emit(Instruction::BinOp { dest: imag, op: IrBinOp::SDiv, lhs: Operand::Value(imag_num), rhs: Operand::Value(denom), ty: comp_ty });

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
        let comp_ty = Self::complex_component_ir_type(complex_type);
        let zero = match comp_ty {
            IrType::F32 => Operand::Const(IrConst::F32(0.0)),
            IrType::F128 => Operand::Const(IrConst::LongDouble(0.0)),
            _ => Operand::Const(IrConst::F64(0.0)),
        };

        // Convert the real value to the component type if needed
        let real_ir = IrType::from_ctype(real_type);
        let converted_real = if real_ir != comp_ty {
            let dest = self.fresh_value();
            self.emit(Instruction::Cast {
                dest,
                src: real_val,
                from_ty: real_ir,
                to_ty: comp_ty,
            });
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
            let dest = self.fresh_value();
            self.emit(Instruction::Cast { dest, src: r, from_ty: from_comp, to_ty: to_comp });
            Operand::Value(dest)
        } else {
            r
        };
        let conv_i = if from_comp != to_comp {
            let dest = self.fresh_value();
            self.emit(Instruction::Cast { dest, src: i, from_ty: from_comp, to_ty: to_comp });
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
            Expr::FunctionCall(callee, _, _) => {
                if let Expr::Identifier(name, _) = callee.as_ref() {
                    self.get_function_return_ctype(name)
                } else {
                    CType::Int
                }
            }
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                self.expr_ctype(lhs)
            }
            Expr::Conditional(_, then_expr, _, _) => {
                self.expr_ctype(then_expr)
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
                if let CType::Pointer(pointee) = inner_ct {
                    *pointee
                } else {
                    CType::Int
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
            _ => {
                // Fallback: use the IR type to approximate
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
        // Check locals (has c_type field)
        if let Some(local) = self.locals.get(name) {
            if let Some(ref ct) = local.c_type {
                return ct.clone();
            }
        }
        // Check globals
        if let Some(global) = self.globals.get(name) {
            if let Some(ref ct) = global.c_type {
                return ct.clone();
            }
        }
        CType::Int
    }

    /// Get a function's return CType.
    pub(super) fn get_function_return_ctype(&self, name: &str) -> CType {
        if let Some(ctype) = self.func_return_ctypes.get(name) {
            ctype.clone()
        } else {
            CType::Int
        }
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
    /// Rules: ComplexLongDouble > ComplexDouble > ComplexFloat
    pub(super) fn common_complex_type(&self, a: &CType, b: &CType) -> CType {
        let a_complex = match a {
            CType::ComplexLongDouble => Some(CType::ComplexLongDouble),
            CType::ComplexDouble => Some(CType::ComplexDouble),
            CType::ComplexFloat => Some(CType::ComplexFloat),
            CType::LongDouble => Some(CType::ComplexLongDouble),
            CType::Double => Some(CType::ComplexDouble),
            CType::Float => Some(CType::ComplexFloat),
            _ => Some(CType::ComplexDouble), // integers promote to complex double
        };
        let b_complex = match b {
            CType::ComplexLongDouble => Some(CType::ComplexLongDouble),
            CType::ComplexDouble => Some(CType::ComplexDouble),
            CType::ComplexFloat => Some(CType::ComplexFloat),
            CType::LongDouble => Some(CType::ComplexLongDouble),
            CType::Double => Some(CType::ComplexDouble),
            CType::Float => Some(CType::ComplexFloat),
            _ => Some(CType::ComplexDouble),
        };

        // Pick the wider type
        match (a_complex, b_complex) {
            (Some(CType::ComplexLongDouble), _) | (_, Some(CType::ComplexLongDouble)) => CType::ComplexLongDouble,
            (Some(CType::ComplexDouble), _) | (_, Some(CType::ComplexDouble)) => CType::ComplexDouble,
            _ => CType::ComplexFloat,
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
                let dest = self.fresh_value();
                self.emit(Instruction::BinOp {
                    dest, op: IrBinOp::Add,
                    lhs: base_val, rhs: scaled,
                    ty: IrType::I64,
                });
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
        let real_eq = self.fresh_value();
        self.emit(Instruction::Cmp {
            dest: real_eq, op: IrCmpOp::Eq, lhs: lr, rhs: rr, ty: comp_ty,
        });

        // Compare imag parts
        let li = self.load_complex_imag(lhs_ptr, &result_ct);
        let ri = self.load_complex_imag(rhs_ptr, &result_ct);
        let imag_eq = self.fresh_value();
        self.emit(Instruction::Cmp {
            dest: imag_eq, op: IrCmpOp::Eq, lhs: li, rhs: ri, ty: comp_ty,
        });

        // Combine: both must be equal for == (either not equal for !=)
        let result = self.fresh_value();
        match op {
            BinOp::Eq => {
                self.emit(Instruction::BinOp {
                    dest: result, op: IrBinOp::And,
                    lhs: Operand::Value(real_eq), rhs: Operand::Value(imag_eq),
                    ty: IrType::I64,
                });
            }
            BinOp::Ne => {
                // a != b iff !(a.re == b.re && a.im == b.im)
                let both_eq = self.fresh_value();
                self.emit(Instruction::BinOp {
                    dest: both_eq, op: IrBinOp::And,
                    lhs: Operand::Value(real_eq), rhs: Operand::Value(imag_eq),
                    ty: IrType::I64,
                });
                self.emit(Instruction::Cmp {
                    dest: result, op: IrCmpOp::Eq,
                    lhs: Operand::Value(both_eq), rhs: Operand::Const(IrConst::I64(0)),
                    ty: IrType::I64,
                });
            }
            _ => unreachable!(),
        }
        Operand::Value(result)
    }

    /// Create a complex value from real scalar for use in expressions like `10.0 + 5.0i`.
    /// This handles the case where a real literal is added to an imaginary literal.
    pub(super) fn make_complex_from_real_scalar(&mut self, val: f64, ctype: &CType) -> Operand {
        let alloca = self.alloca_complex(ctype);
        let comp_ty = Self::complex_component_ir_type(ctype);
        let real_val = match comp_ty {
            IrType::F32 => Operand::Const(IrConst::F32(val as f32)),
            IrType::F128 => Operand::Const(IrConst::LongDouble(val)),
            _ => Operand::Const(IrConst::F64(val)),
        };
        let zero = match comp_ty {
            IrType::F32 => Operand::Const(IrConst::F32(0.0)),
            IrType::F128 => Operand::Const(IrConst::LongDouble(0.0)),
            _ => Operand::Const(IrConst::F64(0.0)),
        };
        self.store_complex_parts(alloca, real_val, zero, ctype);
        Operand::Value(alloca)
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
}
