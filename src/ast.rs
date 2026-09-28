use std::iter::Peekable;

use crate::{
    data_structures::ident_interner::IdentId,
    proc_macro12::{Group, Span, TokenStream, TokenTree, token_stream},
};

pub struct UnparsedQuote {
    pub stream: TokenStream,
}

pub enum QuoteSegment {
    For(QuoteFor),
    Ident(QuoteIdent),
    If(QuoteIf),
    Let(QuoteLet),
    Match(QuoteMatch),
    Tokens(Vec<TokenTree>),
}

pub struct QuoteFor {
    pat: UnparsedPat,
    expr: UnparsedExpr,
    body: UnparsedQuote,
}

pub struct QuoteIdent {
    pub span: Span,
    pub id: IdentId,
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

pub enum Expr {
    Array(Box<UnparsedExprArray>),
    Binary(Box<ExprBinary>),
    Bool(ExprBool),
    Ident(ExprIdent),
    Int(ExprInt),
    Unary(Box<ExprUnary>),
    Repeat(Box<ExprRepeat>),
    Str(ExprStr),
    Tuple(Box<UnparsedExprTuple>),
}

pub struct UnparsedExprArray {
    pub span: Span,
    pub first_element: Option<UnparsedExpr>,
    pub remaining_elements: TokenIter,
}

pub struct ExprBinary {
    pub span: Span,
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
    pub expr: UnparsedExpr,
    pub len: UnparsedExpr,
}

pub struct ExprStr {
    pub span: Span,
    pub value: String,
}

pub struct UnparsedExprTuple {
    pub span: Span,
    pub first_element: Option<UnparsedExpr>,
    pub remaining_elements: TokenIter,
}

pub struct ExprUnary {
    pub span: Span,
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

pub enum Pat {}

pub type TokenIter = Peekable<token_stream::IntoIter>;
