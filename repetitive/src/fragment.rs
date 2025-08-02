use proc_macro2::Span;
use syn::{Ident, Token};

use super::*;

#[derive(Debug, Clone)]
pub struct Fragment {
    at_token_span: Span,
    inner: FragmentInner,
}

#[derive(Debug, Clone)]
enum FragmentInner {
    Tokens(Tokens),
    Name(Name),
    Unknown,

    Int(i128),
    Float(f64),
    Bool(bool),
    Ident(IdentStr),
    String(String),
    Char(char),
    List(Vec<FragmentInner>),

    Neg(Vec<FragmentInner>),
    Not(Vec<FragmentInner>),
    Add(Vec<FragmentInner>),
    Sub(Vec<FragmentInner>),
    Mul(Vec<FragmentInner>),
    Div(Vec<FragmentInner>),
    Rem(Vec<FragmentInner>),
    BitAnd(Vec<FragmentInner>),
    BitOr(Vec<FragmentInner>),
    BitXor(Vec<FragmentInner>),
    Shl(Vec<FragmentInner>),
    Shr(Vec<FragmentInner>),

    Eq(Vec<FragmentInner>),
    Ne(Vec<FragmentInner>),
    Lt(Vec<FragmentInner>),
    Gt(Vec<FragmentInner>),
    Le(Vec<FragmentInner>),
    Ge(Vec<FragmentInner>),

    And(Vec<FragmentInner>),
    Or(Vec<FragmentInner>),
    Xor(Vec<FragmentInner>),

    Range(Vec<FragmentInner>),
    RangeInclusive(Vec<FragmentInner>),
    RangeTo(Vec<FragmentInner>),
    RangeToInclusive(Vec<FragmentInner>),
    RangeFrom(Vec<FragmentInner>),
    RangeFull(Vec<FragmentInner>),

    Index(Vec<FragmentInner>),
    Enumerate(Vec<FragmentInner>),
    Zip(Vec<FragmentInner>),
    Chain(Vec<FragmentInner>),
}

impl ContextParse for Fragment {
    fn ctx_parse(input: syn::parse::ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        let at_token = input.parse::<Token![@]>()?;
        let at_token_span = at_token.span;

        let inner = FragmentInner::ctx_parse(input, ctx)?;

        Ok(Self {
            at_token_span,
            inner,
        })
    }
}

impl ContextParse for FragmentInner {
    fn ctx_parse(input: syn::parse::ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        if input.peek(Ident) {
            let name = Name::ctx_parse(input, ctx)?;

            return Ok(Self::Name(name));
        }

        Err(syn::Error::new(input.span(), "expected a fragment"))
    }
}
