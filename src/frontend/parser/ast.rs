use crate::common::source::Span;

/// A complete translation unit (one C source file).
#[derive(Debug)]
pub struct TranslationUnit {
    pub decls: Vec<ExternalDecl>,
}

/// Top-level declarations in a translation unit.
#[derive(Debug)]
pub enum ExternalDecl {
    FunctionDef(FunctionDef),
    Declaration(Declaration),
    /// Top-level asm("...") directive - emitted verbatim in assembly output.
    TopLevelAsm(String),
}

/// A function definition.
#[derive(Debug)]
pub struct FunctionDef {
    pub return_type: TypeSpecifier,
    pub name: String,
    pub params: Vec<ParamDecl>,
    pub variadic: bool,
    pub body: CompoundStmt,
    pub is_static: bool,
    pub is_inline: bool,
    pub is_kr: bool,
    pub is_constructor: bool,
    pub is_destructor: bool,
    /// __attribute__((section("..."))) - place in specific ELF section
    pub section: Option<String>,
    pub span: Span,
}

/// A parameter declaration.
#[derive(Debug, Clone)]
pub struct ParamDecl {
    pub type_spec: TypeSpecifier,
    pub name: Option<String>,
    pub span: Span,
    /// For function pointer parameters, the parameter types of the pointed-to function.
    /// E.g., for `float (*func)(float, float)`, this holds the two float param decls.
    pub fptr_params: Option<Vec<ParamDecl>>,
}

/// A variable/type declaration.
#[derive(Debug, Clone)]
pub struct Declaration {
    pub type_spec: TypeSpecifier,
    pub declarators: Vec<InitDeclarator>,
    pub is_static: bool,
    pub is_extern: bool,
    pub is_typedef: bool,
    pub is_const: bool,
    pub is_volatile: bool,
    pub is_common: bool,
    /// Whether __attribute__((transparent_union)) was applied to this typedef.
    pub is_transparent_union: bool,
    /// Alignment override from _Alignas(N) or __attribute__((aligned(N))).
    pub alignment: Option<usize>,
    pub span: Span,
}

impl Declaration {
    /// Create an empty declaration (used for skipped constructs like asm directives).
    pub fn empty() -> Self {
        Self {
            type_spec: TypeSpecifier::Void,
            declarators: Vec::new(),
            is_static: false,
            is_extern: false,
            is_typedef: false,
            is_const: false,
            is_volatile: false,
            is_common: false,
            is_transparent_union: false,
            alignment: None,
            span: Span::dummy(),
        }
    }
}

/// A declarator with optional initializer.
#[derive(Debug, Clone)]
pub struct InitDeclarator {
    pub name: String,
    pub derived: Vec<DerivedDeclarator>,
    pub init: Option<Initializer>,
    pub is_constructor: bool,
    pub is_destructor: bool,
    /// __attribute__((weak)) - emit as a weak symbol
    pub is_weak: bool,
    /// __attribute__((alias("target"))) - this symbol is an alias for target
    pub alias_target: Option<String>,
    /// __attribute__((visibility("hidden"))) etc.
    pub visibility: Option<String>,
    /// __attribute__((section("..."))) - place in specific ELF section
    pub section: Option<String>,
    /// register var __asm__("regname") - pin to specific register for inline asm
    pub asm_register: Option<String>,
    pub span: Span,
}

/// Derived parts of a declarator (pointers, arrays, function params).
#[derive(Debug, Clone)]
pub enum DerivedDeclarator {
    Pointer,
    Array(Option<Box<Expr>>),
    Function(Vec<ParamDecl>, bool), // params, variadic
    /// Function pointer: (*name)(params) - distinguishes from pointer-to-return-type
    FunctionPointer(Vec<ParamDecl>, bool), // params, variadic
}

/// An initializer.
#[derive(Debug, Clone)]
pub enum Initializer {
    Expr(Expr),
    List(Vec<InitializerItem>),
}

/// An item in an initializer list.
#[derive(Debug, Clone)]
pub struct InitializerItem {
    pub designators: Vec<Designator>,
    pub init: Initializer,
}

/// A designator in an initializer.
#[derive(Debug, Clone)]
pub enum Designator {
    Index(Expr),
    /// GCC range designator: [lo ... hi]
    Range(Expr, Expr),
    Field(String),
}

/// Type specifiers.
#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    LongLong,
    Float,
    Double,
    LongDouble,
    Signed,
    Unsigned,
    UnsignedChar,
    UnsignedShort,
    UnsignedInt,
    UnsignedLong,
    UnsignedLongLong,
    Int128,
    UnsignedInt128,
    Bool,
    ComplexFloat,
    ComplexDouble,
    ComplexLongDouble,
    /// Struct: (name, fields, is_packed, max_field_align from #pragma pack, struct-level aligned attribute)
    Struct(Option<String>, Option<Vec<StructFieldDecl>>, bool, Option<usize>, Option<usize>),
    /// Union: (name, fields, is_packed, max_field_align from #pragma pack, struct-level aligned attribute)
    Union(Option<String>, Option<Vec<StructFieldDecl>>, bool, Option<usize>, Option<usize>),
    Enum(Option<String>, Option<Vec<EnumVariant>>),
    TypedefName(String),
    Pointer(Box<TypeSpecifier>),
    Array(Box<TypeSpecifier>, Option<Box<Expr>>),
    /// Function pointer type from cast/sizeof: return_type, params, variadic
    /// E.g., `(jv (*)(void*, jv))` produces FunctionPointer(jv, [void*, jv], false)
    FunctionPointer(Box<TypeSpecifier>, Vec<ParamDecl>, bool),
    /// typeof(expr) - GCC extension: type of an expression
    Typeof(Box<Expr>),
    /// typeof(type-name) - GCC extension: type from a type name
    TypeofType(Box<TypeSpecifier>),
    /// __auto_type - GCC extension: type inferred from initializer expression
    AutoType,
}

/// A field declaration in a struct/union.
#[derive(Debug, Clone)]
pub struct StructFieldDecl {
    pub type_spec: TypeSpecifier,
    pub name: Option<String>,
    pub bit_width: Option<Box<Expr>>,
    /// Derived declarator parts (pointers, arrays, function pointers) from the declarator.
    /// For simple fields like `int x` or `int *p`, this is empty (the pointer is in type_spec).
    /// For complex declarators like `void (*(*fp)(int))(void)`, this carries the full
    /// derived declarator chain that must be applied to type_spec to get the final type.
    pub derived: Vec<DerivedDeclarator>,
    /// Per-field alignment from _Alignas(N) or __attribute__((aligned(N))).
    pub alignment: Option<usize>,
}

/// An enum variant.
#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub value: Option<Box<Expr>>,
}

/// A compound statement (block).
#[derive(Debug, Clone)]
pub struct CompoundStmt {
    pub items: Vec<BlockItem>,
    pub span: Span,
}

/// Items within a block.
#[derive(Debug, Clone)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Stmt),
}

/// Statements.
#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Option<Expr>),
    Return(Option<Expr>, Span),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>, Span),
    While(Expr, Box<Stmt>, Span),
    DoWhile(Box<Stmt>, Expr, Span),
    For(Option<Box<ForInit>>, Option<Expr>, Option<Expr>, Box<Stmt>, Span),
    Compound(CompoundStmt),
    Break(Span),
    Continue(Span),
    Switch(Expr, Box<Stmt>, Span),
    Case(Expr, Box<Stmt>, Span),
    /// GNU case range: `case low ... high:` (GCC extension)
    CaseRange(Expr, Expr, Box<Stmt>, Span),
    Default(Box<Stmt>, Span),
    Goto(String, Span),
    /// Computed goto: goto *expr (GCC extension, labels-as-values)
    GotoIndirect(Box<Expr>, Span),
    Label(String, Box<Stmt>, Span),
    InlineAsm {
        template: String,
        outputs: Vec<AsmOperand>,
        inputs: Vec<AsmOperand>,
        clobbers: Vec<String>,
        /// Goto labels for asm goto (e.g., `asm goto("..." : : : : label1, label2)`)
        goto_labels: Vec<String>,
    },
}

/// An operand in an inline asm statement.
#[derive(Debug, Clone)]
pub struct AsmOperand {
    /// Symbolic name (e.g., [name]) if present
    pub name: Option<String>,
    /// Constraint string (e.g., "=r", "+r", "r")
    pub constraint: String,
    /// The C expression for the operand
    pub expr: Expr,
}

/// For loop initializer.
#[derive(Debug, Clone)]
pub enum ForInit {
    Declaration(Declaration),
    Expr(Expr),
}

/// Expressions.
#[derive(Debug, Clone)]
pub enum Expr {
    IntLiteral(i64, Span),
    UIntLiteral(u64, Span),
    LongLiteral(i64, Span),
    ULongLiteral(u64, Span),
    FloatLiteral(f64, Span),            // double literal (no suffix)
    FloatLiteralF32(f64, Span),         // float literal (f/F suffix)
    FloatLiteralLongDouble(f64, Span),  // long double literal (l/L suffix)
    /// Imaginary literal: value * I (double imaginary, e.g. 1.0i)
    ImaginaryLiteral(f64, Span),
    /// Float imaginary literal (e.g. 1.0fi)
    ImaginaryLiteralF32(f64, Span),
    /// Long double imaginary literal (e.g. 1.0Li)
    ImaginaryLiteralLongDouble(f64, Span),
    StringLiteral(String, Span),
    /// Wide string literal (L"...") - each char is a wchar_t (32-bit int)
    WideStringLiteral(String, Span),
    CharLiteral(char, Span),
    Identifier(String, Span),
    BinaryOp(BinOp, Box<Expr>, Box<Expr>, Span),
    UnaryOp(UnaryOp, Box<Expr>, Span),
    PostfixOp(PostfixOp, Box<Expr>, Span),
    Assign(Box<Expr>, Box<Expr>, Span),
    CompoundAssign(BinOp, Box<Expr>, Box<Expr>, Span),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>, Span),
    /// GNU extension: `cond ? : else_expr` - condition is evaluated once and used as the
    /// then-value if truthy. Semantically: `({ auto __tmp = cond; __tmp ? __tmp : else_expr; })`
    GnuConditional(Box<Expr>, Box<Expr>, Span),
    FunctionCall(Box<Expr>, Vec<Expr>, Span),
    ArraySubscript(Box<Expr>, Box<Expr>, Span),
    MemberAccess(Box<Expr>, String, Span),
    PointerMemberAccess(Box<Expr>, String, Span),
    Cast(TypeSpecifier, Box<Expr>, Span),
    CompoundLiteral(TypeSpecifier, Box<Initializer>, Span),
    StmtExpr(CompoundStmt, Span),
    Sizeof(Box<SizeofArg>, Span),
    /// __builtin_va_arg(ap, type): extract next variadic argument of given type
    VaArg(Box<Expr>, TypeSpecifier, Span),
    Alignof(TypeSpecifier, Span),
    Comma(Box<Expr>, Box<Expr>, Span),
    AddressOf(Box<Expr>, Span),
    Deref(Box<Expr>, Span),
    /// _Generic(controlling_expr, type1: expr1, type2: expr2, ..., default: exprN)
    GenericSelection(Box<Expr>, Vec<GenericAssociation>, Span),
    /// GCC extension: &&label (address of label, for computed goto)
    LabelAddr(String, Span),
    /// GCC extension: __builtin_types_compatible_p(type1, type2)
    /// Compile-time constant: 1 if the two types are compatible, 0 otherwise.
    BuiltinTypesCompatibleP(TypeSpecifier, TypeSpecifier, Span),
}

/// A _Generic association: either a type-expression pair, or a default expression.
#[derive(Debug, Clone)]
pub struct GenericAssociation {
    pub type_spec: Option<TypeSpecifier>, // None for "default"
    pub expr: Expr,
}

/// Sizeof argument can be a type or expression.
#[derive(Debug, Clone)]
pub enum SizeofArg {
    Type(TypeSpecifier),
    Expr(Expr),
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    LogicalAnd,
    LogicalOr,
}

impl BinOp {
    /// Returns true for comparison operators (==, !=, <, <=, >, >=, &&, ||).
    pub fn is_comparison(self) -> bool {
        matches!(self, BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le
            | BinOp::Gt | BinOp::Ge | BinOp::LogicalAnd | BinOp::LogicalOr)
    }
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Neg,
    BitNot,
    LogicalNot,
    PreInc,
    PreDec,
    /// __real__ expr - extract real part of complex number (GCC extension)
    RealPart,
    /// __imag__ expr - extract imaginary part of complex number (GCC extension)
    ImagPart,
}

/// Postfix operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostfixOp {
    PostInc,
    PostDec,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::IntLiteral(_, s) | Expr::UIntLiteral(_, s)
            | Expr::LongLiteral(_, s) | Expr::ULongLiteral(_, s)
            | Expr::FloatLiteral(_, s) | Expr::FloatLiteralF32(_, s) | Expr::FloatLiteralLongDouble(_, s)
            | Expr::ImaginaryLiteral(_, s) | Expr::ImaginaryLiteralF32(_, s) | Expr::ImaginaryLiteralLongDouble(_, s)
            | Expr::StringLiteral(_, s) | Expr::WideStringLiteral(_, s)
            | Expr::CharLiteral(_, s) | Expr::Identifier(_, s)
            | Expr::BinaryOp(_, _, _, s) | Expr::UnaryOp(_, _, s) | Expr::PostfixOp(_, _, s)
            | Expr::Assign(_, _, s) | Expr::CompoundAssign(_, _, _, s) | Expr::Conditional(_, _, _, s)
            | Expr::GnuConditional(_, _, s)
            | Expr::FunctionCall(_, _, s) | Expr::ArraySubscript(_, _, s)
            | Expr::MemberAccess(_, _, s) | Expr::PointerMemberAccess(_, _, s)
            | Expr::Cast(_, _, s) | Expr::CompoundLiteral(_, _, s) | Expr::StmtExpr(_, s)
            | Expr::Sizeof(_, s) | Expr::VaArg(_, _, s) | Expr::Alignof(_, s) | Expr::Comma(_, _, s)
            | Expr::AddressOf(_, s) | Expr::Deref(_, s)
            | Expr::GenericSelection(_, _, s) | Expr::LabelAddr(_, s)
            | Expr::BuiltinTypesCompatibleP(_, _, s) => *s,
        }
    }
}
