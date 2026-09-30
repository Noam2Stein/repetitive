use crate::{
    proc_macro12::{Delimiter, Group, Span, TokenStream},
    str_interner::StrId,
    token_iter::TokenIter,
};

pub struct Quote {
    pub last_span: Span,
    pub stream: TokenStream,
}

pub enum QuoteSegment {
    For(QuoteFor),
    Group(QuoteGroup),
    Ident(QuoteIdent),
    If(QuoteIf),
    Let(QuoteLet),
    Match(QuoteMatch),
    TokenStream(TokenStream),
}

pub struct QuoteFor {
    pub pat: Pat,
    pub expr: Expr,
    pub body: Quote,
}

pub struct QuoteGroup {
    pub delimiter: Delimiter,
    pub span: Span,
    pub stream: Quote,
}

pub struct QuoteIdent {
    pub span: Span,
    pub strid: StrId,
}

pub struct QuoteIf {
    pub segments: Vec<QuoteIfSegment>,
}

pub struct QuoteIfSegment {
    pub condition: Option<Expr>,
    pub branch: Quote,
}

pub struct QuoteLet {
    pub pat: Pat,
    pub expr: Expr,
}

pub struct QuoteMatch {
    pub expr: Expr,
    pub arms: Vec<QuoteMatchArm>,
}

pub struct QuoteMatchArm {
    pub pat: Pat,
    pub body: Quote,
}

pub enum Expr {
    Kind { span: Span, kind: ExprKind },
    Group(Group),
}

pub enum ExprKind {
    Array(Box<ExprArray>),
    Binary(Box<ExprBinary>),
    Bool(bool),
    Ident(StrId),
    Int(u16),
    Unary(Box<ExprUnary>),
    Repeat(Box<ExprRepeat>),
    Str(String),
    Tuple(Box<ExprTuple>),
}

pub struct ExprArray {
    pub first_element: Option<Expr>,
    pub remaining_elements: TokenIter,
}

pub struct ExprBinary {
    pub lhs: Expr,
    pub op: ExprBinaryOp,
    pub rhs: Expr,
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

pub struct ExprRepeat {
    pub expr: Expr,
    pub len: Expr,
}

pub struct ExprTuple {
    pub first_element: Option<Expr>,
    pub remaining_elements: TokenIter,
}

pub struct ExprUnary {
    pub op: ExprUnaryOp,
    pub expr: Expr,
}

pub enum ExprUnaryOp {
    Neg,
    Not,
}

pub enum Pat {
    Kind { span: Span, kind: PatKind },
    Group(Group),
}

pub enum PatKind {}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Kind { span, .. } => *span,
            Expr::Group(variant) => variant.span(),
        }
    }
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::Kind { span, .. } => *span,
            Pat::Group(variant) => variant.span(),
        }
    }
}
