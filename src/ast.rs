use crate::{
    proc_macro12::{Delimiter, Group, Span, TokenStream},
    str_interner::StrId,
    token_iter::TokenIter,
};

pub struct UnparsedQuote {
    pub stream: TokenStream,
    pub last_span: Span,
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
    pub pat: UnparsedPat,
    pub expr: UnparsedExpr,
    pub body: UnparsedQuote,
}

pub struct QuoteGroup {
    pub delimiter: Delimiter,
    pub span: Span,
    pub stream: UnparsedQuote,
}

pub struct QuoteIdent {
    pub span: Span,
    pub strid: StrId,
}

pub struct QuoteIf {
    pub segments: Vec<QuoteIfSegment>,
}

pub struct QuoteIfSegment {
    pub condition: Option<UnparsedExpr>,
    pub branch: UnparsedQuote,
}

pub struct QuoteLet {
    pub pat: UnparsedPat,
    pub expr: UnparsedExpr,
}

pub struct QuoteMatch {
    pub expr: UnparsedExpr,
    pub arms: Vec<QuoteMatchArm>,
}

pub struct QuoteMatchArm {
    pub pat: UnparsedPat,
    pub body: UnparsedQuote,
}

pub enum UnparsedExpr {
    Expr(Expr),
    Group(Group),
}

pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

pub enum ExprKind {
    Array(Box<UnparsedExprArray>),
    Binary(Box<ExprBinary>),
    Bool(bool),
    Ident(StrId),
    Int(u16),
    Unary(Box<ExprUnary>),
    Repeat(Box<ExprRepeat>),
    Str(String),
    Tuple(Box<UnparsedExprTuple>),
}

pub struct UnparsedExprArray {
    pub first_element: Option<UnparsedExpr>,
    pub remaining_elements: TokenIter,
}

pub struct ExprBinary {
    pub lhs: UnparsedExpr,
    pub op: ExprBinaryOp,
    pub rhs: UnparsedExpr,
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
    pub expr: UnparsedExpr,
    pub len: UnparsedExpr,
}

pub struct UnparsedExprTuple {
    pub first_element: Option<UnparsedExpr>,
    pub remaining_elements: TokenIter,
}

pub struct ExprUnary {
    pub op: ExprUnaryOp,
    pub expr: UnparsedExpr,
}

pub enum ExprUnaryOp {
    Neg,
    Not,
}

pub enum UnparsedPat {
    Pat(Pat),
    Group(Group),
}

pub struct Pat {
    pub span: Span,
    pub kind: PatKind,
}

pub enum PatKind {}

impl UnparsedExpr {
    pub fn span(&self) -> Span {
        match self {
            Self::Expr(variant) => variant.span,
            Self::Group(variant) => variant.span(),
        }
    }
}

impl UnparsedPat {
    pub fn span(&self) -> Span {
        match self {
            Self::Group(variant) => variant.span(),
            Self::Pat(variant) => variant.span,
        }
    }
}
