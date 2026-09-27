use crate::{
    ident_interner::IdentId,
    proc_macro12::{Group, Span, TokenStream, TokenTree, token_stream},
};

pub enum Lazy<T> {
    Eval(T),
    Group(Group),
    TokenStream(TokenStream),
}

pub struct Quote(token_stream::IntoIter);

pub enum QuoteSegment {
    For(QuoteFor),
    Ident(QuoteIdent),
    If(QuoteIf),
    Let(QuoteLet),
    Match(QuoteMatch),
    Tokens(Vec<TokenTree>),
}

pub struct QuoteFor {
    pat: Lazy<Pat>,
    expr: Lazy<Expr>,
    body: Lazy<Quote>,
}

pub struct QuoteIdent {
    pub span: Span,
    pub id: IdentId,
}

pub struct QuoteIf {
    pub segments: Vec<QuoteIfSegment>,
}

pub struct QuoteIfSegment {
    pub condition: Option<Lazy<Expr>>,
    pub branch: Lazy<Quote>,
}

pub struct QuoteLet {
    pub pat: Lazy<Pat>,
    pub expr: Lazy<Expr>,
}

pub struct QuoteMatch {
    pub expr: Lazy<Expr>,
    pub arms: Vec<QuoteMatchArm>,
}

pub struct QuoteMatchArm {
    pub pat: Lazy<Pat>,
    pub body: Lazy<Quote>,
}

pub enum Expr {
    Array(ExprArray),
    Binary(Box<ExprBinary>),
    Bool(ExprBool),
    Ident(ExprIdent),
    Int(ExprInt),
    Unary(Box<ExprUnary>),
    Repeat(Box<ExprRepeat>),
    Str(ExprStr),
    Tuple(ExprTuple),
}

pub struct ExprArray {
    pub span: Span,
    pub elements: Vec<Lazy<Expr>>,
}

pub struct ExprBinary {
    pub span: Span,
    pub lhs: Lazy<Expr>,
    pub op: ExprBinaryOp,
    pub rhs: Lazy<Expr>,
}

pub enum ExprBinaryOp {
    Add,
    And,
    BitAnd,
    BitOr,
    BitXor,
    Div,
    Mul,
    Or,
    Rem,
    Shl,
    Shr,
    Sub,
}

pub struct ExprBool {
    pub span: Span,
    pub value: bool,
}

pub struct ExprIdent {
    pub span: Span,
    pub id: IdentId,
}

pub struct ExprInt {
    pub span: Span,
    pub value: u16,
}

pub struct ExprRepeat {
    pub expr: Lazy<Expr>,
    pub len: Lazy<Expr>,
}

pub struct ExprStr {
    pub span: Span,
    pub value: String,
}

pub struct ExprTuple {
    pub span: Span,
    pub elements: Vec<Lazy<Expr>>,
}

pub struct ExprUnary {
    pub span: Span,
    pub op: ExprUnaryOp,
    pub expr: Lazy<Expr>,
}

pub enum ExprUnaryOp {
    Neg,
    Not,
}

pub enum Pat {}
