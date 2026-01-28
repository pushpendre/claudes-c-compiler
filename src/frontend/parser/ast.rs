use crate::common::source::Span;
use crate::common::types::AddressSpace;

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

/// Attributes that can be applied to function definitions via storage-class
/// specifiers (static, inline, extern) and GCC __attribute__((...)) syntax.
///
/// Consolidates what were previously 13 separate boolean/Option fields on
/// FunctionDef into a single struct, making it easier to add new attributes
/// and pass attribute bundles around.
#[derive(Debug, Clone, Default)]
pub struct FunctionAttributes {
    pub is_static: bool,
    pub is_inline: bool,
    /// True when `extern` storage class was used on the function definition.
    pub is_extern: bool,
    /// True when __attribute__((gnu_inline)) forces GNU89 inline semantics.
    pub is_gnu_inline: bool,
    /// True when __attribute__((always_inline)) is present.
    /// These functions must always be inlined and should not be emitted as standalone.
    pub is_always_inline: bool,
    /// True when __attribute__((noinline)) is present.
    /// These functions must never be inlined by the optimizer.
    pub is_noinline: bool,
    pub is_constructor: bool,
    pub is_destructor: bool,
    /// __attribute__((section("..."))) - place in specific ELF section
    pub section: Option<String>,
    /// __attribute__((visibility("hidden"|"default"|...)))
    pub visibility: Option<String>,
    /// __attribute__((weak)) - emit as a weak symbol
    pub is_weak: bool,
    /// __attribute__((used)) - prevent dead code elimination of this symbol
    pub is_used: bool,
}

/// A function definition.
#[derive(Debug)]
pub struct FunctionDef {
    pub return_type: TypeSpecifier,
    pub name: String,
    pub params: Vec<ParamDecl>,
    pub variadic: bool,
    pub body: CompoundStmt,
    /// Function attributes (storage class, inline hints, GCC __attribute__).
    pub attrs: FunctionAttributes,
    pub is_kr: bool,
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
    /// Whether this parameter's base type has a `const` qualifier.
    /// Used by _Generic matching to distinguish e.g. `const int *` from `int *`.
    pub is_const: bool,
    /// VLA size expressions from the outermost array dimension that was decayed to pointer.
    /// E.g., for `void foo(int a, int b[a++])`, the expression `a++` is stored here
    /// so its side effects can be evaluated at function entry during IR lowering.
    pub vla_size_exprs: Vec<Box<Expr>>,
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
    /// Whether _Thread_local or __thread storage class was specified.
    pub is_thread_local: bool,
    /// Whether __attribute__((transparent_union)) was applied to this typedef.
    pub is_transparent_union: bool,
    /// Alignment override from _Alignas(N) or __attribute__((aligned(N))).
    pub alignment: Option<usize>,
    /// Type specifier from _Alignas(type) that the lowerer should resolve for alignment.
    /// The parser can't resolve typedef names, so when _Alignas takes a type argument,
    /// we store the type specifier here for proper resolution during lowering.
    pub alignas_type: Option<TypeSpecifier>,
    /// Type from `__attribute__((aligned(sizeof(type))))`.  The parser can't compute
    /// sizeof for struct/union types accurately, so we capture the type here and let
    /// sema/lowerer recompute `sizeof(type)` with full layout information.
    pub alignment_sizeof_type: Option<TypeSpecifier>,
    /// Address space qualifier on the variable itself (not on a pointer).
    /// E.g., `extern const struct pcpu_hot __seg_gs const_pcpu_hot;` has SegGs.
    /// Used for x86 per-CPU variable access with %gs:/%fs: segment prefixes.
    pub address_space: AddressSpace,
    /// GCC __attribute__((vector_size(N))): total vector size in bytes.
    /// When present on a typedef, wraps the base type in CType::Vector.
    pub vector_size: Option<usize>,
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
            is_thread_local: false,
            is_transparent_union: false,
            alignment: None,
            alignas_type: None,
            alignment_sizeof_type: None,
            address_space: AddressSpace::Default,
            vector_size: None,
            span: Span::dummy(),
        }
    }
}

/// Attributes that can be applied to individual declarators via GCC
/// __attribute__((...)) syntax.
///
/// Consolidates what were previously 11 separate boolean/Option fields on
/// InitDeclarator into a single struct.
#[derive(Debug, Clone, Default)]
pub struct DeclAttributes {
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
    /// __attribute__((error("msg"))) or __attribute__((warning("msg")))
    /// Functions with this attribute are compile-time assertion traps.
    /// Calls to them should be treated as unreachable.
    pub is_error_attr: bool,
    /// __attribute__((noreturn)) or _Noreturn - function never returns.
    /// Calls to noreturn functions are followed by unreachable.
    pub is_noreturn: bool,
    /// __attribute__((cleanup(func))) - call func(&var) when var goes out of scope.
    /// Used for RAII-style cleanup (e.g., Linux kernel guard()/scoped_guard() for mutex_unlock).
    pub cleanup_fn: Option<String>,
    /// __attribute__((used)) - prevent dead code elimination of this symbol
    pub is_used: bool,
}

/// A declarator with optional initializer.
#[derive(Debug, Clone)]
pub struct InitDeclarator {
    pub name: String,
    pub derived: Vec<DerivedDeclarator>,
    pub init: Option<Initializer>,
    /// Declarator attributes (GCC __attribute__, asm register, etc.).
    pub attrs: DeclAttributes,
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
    /// Enum: (name, variants, is_packed)
    Enum(Option<String>, Option<Vec<EnumVariant>>, bool),
    TypedefName(String),
    Pointer(Box<TypeSpecifier>, AddressSpace),
    Array(Box<TypeSpecifier>, Option<Box<Expr>>),
    /// Function pointer type from cast/sizeof: return_type, params, variadic
    /// E.g., `(jv (*)(void*, jv))` produces FunctionPointer(jv, [void*, jv], false)
    FunctionPointer(Box<TypeSpecifier>, Vec<ParamDecl>, bool),
    /// Bare function type (not a pointer): return_type, params, variadic
    /// This is produced by ctype_to_type_spec for CType::Function, used in typeof
    /// resolution. Unlike FunctionPointer which represents Pointer(Function(...)),
    /// this represents just Function(...) without the pointer wrapper.
    BareFunction(Box<TypeSpecifier>, Vec<ParamDecl>, bool),
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
    /// GNU __label__ declarations: local label names scoped to this block.
    /// When non-empty, label definitions and gotos within this block use
    /// scope-qualified names to avoid collisions (e.g., in statement expressions).
    pub local_labels: Vec<String>,
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
    /// A declaration in statement position (C23: declarations allowed after labels,
    /// and in other statement contexts like `case`/`default`).
    Declaration(Declaration),
    InlineAsm {
        template: String,
        outputs: Vec<AsmOperand>,
        inputs: Vec<AsmOperand>,
        clobbers: Vec<String>,
        /// Goto labels for asm goto (e.g., `asm goto("..." : : : : label1, label2)`)
        goto_labels: Vec<String>,
    },
}

impl Stmt {
    /// Extract the source span from this statement, if available.
    /// Used by IR lowering to propagate source locations for debug info.
    pub fn span(&self) -> Option<Span> {
        match self {
            Stmt::Return(_, span) | Stmt::If(_, _, _, span) | Stmt::While(_, _, span) |
            Stmt::DoWhile(_, _, span) | Stmt::For(_, _, _, _, span) |
            Stmt::Break(span) | Stmt::Continue(span) | Stmt::Switch(_, _, span) |
            Stmt::Case(_, _, span) | Stmt::CaseRange(_, _, _, span) |
            Stmt::Default(_, span) | Stmt::Goto(_, span) |
            Stmt::GotoIndirect(_, span) | Stmt::Label(_, _, span) => Some(*span),
            Stmt::Expr(Some(expr)) => Some(expr.span()),
            Stmt::Declaration(decl) => Some(decl.span),
            Stmt::Expr(None) | Stmt::Compound(_) | Stmt::InlineAsm { .. } => None,
        }
    }
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
    /// Long double literal (l/L suffix). Stores (f64_approx, f128_bytes, span).
    /// f128_bytes is IEEE 754 binary128 with full 112-bit mantissa precision.
    FloatLiteralLongDouble(f64, [u8; 16], Span),
    /// Imaginary literal: value * I (double imaginary, e.g. 1.0i)
    ImaginaryLiteral(f64, Span),
    /// Float imaginary literal (e.g. 1.0fi)
    ImaginaryLiteralF32(f64, Span),
    /// Long double imaginary literal (e.g. 1.0Li). Stores (f64_approx, f128_bytes, span).
    ImaginaryLiteralLongDouble(f64, [u8; 16], Span),
    StringLiteral(String, Span),
    /// Wide string literal (L"...") - each char is a wchar_t (32-bit int)
    WideStringLiteral(String, Span),
    /// char16_t string literal (u"...") - each char is a char16_t (16-bit unsigned)
    Char16StringLiteral(String, Span),
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
    /// __alignof__(expr) - alignment of expression's type (GCC extension)
    AlignofExpr(Box<Expr>, Span),
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
    /// Whether the association type has a const qualifier on the top-level type
    /// (for non-pointer types) or on the pointee (for pointer types like `const int *`).
    /// Used by _Generic matching to distinguish e.g. `const int *` from `int *`,
    /// since CType does not track const/volatile qualifiers.
    pub is_const: bool,
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
            | Expr::FloatLiteral(_, s) | Expr::FloatLiteralF32(_, s) | Expr::FloatLiteralLongDouble(_, _, s)
            | Expr::ImaginaryLiteral(_, s) | Expr::ImaginaryLiteralF32(_, s) | Expr::ImaginaryLiteralLongDouble(_, _, s)
            | Expr::StringLiteral(_, s) | Expr::WideStringLiteral(_, s)
            | Expr::Char16StringLiteral(_, s)
            | Expr::CharLiteral(_, s) | Expr::Identifier(_, s)
            | Expr::BinaryOp(_, _, _, s) | Expr::UnaryOp(_, _, s) | Expr::PostfixOp(_, _, s)
            | Expr::Assign(_, _, s) | Expr::CompoundAssign(_, _, _, s) | Expr::Conditional(_, _, _, s)
            | Expr::GnuConditional(_, _, s)
            | Expr::FunctionCall(_, _, s) | Expr::ArraySubscript(_, _, s)
            | Expr::MemberAccess(_, _, s) | Expr::PointerMemberAccess(_, _, s)
            | Expr::Cast(_, _, s) | Expr::CompoundLiteral(_, _, s) | Expr::StmtExpr(_, s)
            | Expr::Sizeof(_, s) | Expr::VaArg(_, _, s) | Expr::Alignof(_, s)
            | Expr::AlignofExpr(_, s) | Expr::Comma(_, _, s)
            | Expr::AddressOf(_, s) | Expr::Deref(_, s)
            | Expr::GenericSelection(_, _, s) | Expr::LabelAddr(_, s)
            | Expr::BuiltinTypesCompatibleP(_, _, s) => *s,
        }
    }
}
