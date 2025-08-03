use proc_macro2::{Delimiter, Group, Span, TokenStream};
use quote::ToTokens;
use syn::{
    Ident, Lifetime, Lit, LitBool, LitChar, LitFloat, LitInt, LitStr, Token,
    parse::{Parse, ParseStream, Parser},
};

use super::*;

#[derive(Debug, Clone)]
pub struct Fragment {
    at_token_span: Span,
    inner: FragmentBody,
}

#[derive(Debug, Clone)]
enum FragmentBody {
    Tokens(Tokens),
    Name(Name),
    Unknown,
    Lit(FragmentLit),
    List(Vec<FragmentBody>),

    Neg(Vec<FragmentBody>),
    Not(Vec<FragmentBody>),
    Add(Vec<FragmentBody>),
    Sub(Vec<FragmentBody>),
    Mul(Vec<FragmentBody>),
    Div(Vec<FragmentBody>),
    Rem(Vec<FragmentBody>),
    BitAnd(Vec<FragmentBody>),
    BitOr(Vec<FragmentBody>),
    BitXor(Vec<FragmentBody>),
    Shl(Vec<FragmentBody>),
    Shr(Vec<FragmentBody>),

    Eq(Vec<FragmentBody>),
    Ne(Vec<FragmentBody>),
    Lt(Vec<FragmentBody>),
    Gt(Vec<FragmentBody>),
    Le(Vec<FragmentBody>),
    Ge(Vec<FragmentBody>),

    And(Vec<FragmentBody>),
    Or(Vec<FragmentBody>),
    Xor(Vec<FragmentBody>),

    Range(Vec<FragmentBody>),
    RangeInclusive(Vec<FragmentBody>),
    RangeTo(Vec<FragmentBody>),
    RangeToInclusive(Vec<FragmentBody>),
    RangeFrom(Vec<FragmentBody>),
    RangeFull(Vec<FragmentBody>),

    Index(Vec<FragmentBody>),
    Enumerate(Vec<FragmentBody>),
    Zip(Vec<FragmentBody>),
    Chain(Vec<FragmentBody>),
}

#[derive(Debug, Clone)]
enum FragmentLit {
    Int(i128),
    Float(f64),
    Bool(bool),
    Ident(IdentStr),
    String(String),
    Char(char),
}

impl Fragment {
    pub fn ctx_parse_outer(input: syn::parse::ParseStream, ctx: &mut Context) -> syn::Result<Self> {
        let at_token = input.parse::<Token![@]>()?;
        let at_token_span = at_token.span;

        let inner = FragmentBody::ctx_parse_outer(input, ctx)?;

        Ok(Self {
            at_token_span,
            inner,
        })
    }

    pub fn end(&self, output: &mut TokenStream) {
        match &self.inner {
            FragmentBody::Tokens(tokens) => tokens.end(output),
            FragmentBody::Lit(lit) => lit.to_tokens(self.at_token_span, output),
            FragmentBody::List(val) => val.iter().for_each(|item| {
                Fragment {
                    at_token_span: self.at_token_span,
                    inner: item.clone(),
                }
                .end(output)
            }),

            FragmentBody::Unknown => {}

            _ => panic!("unresolved fragment"),
        }
    }
}

impl FragmentBody {
    fn ctx_parse_outer(input: syn::parse::ParseStream, ctx: &mut Context) -> syn::Result<Self> {
        if input.peek(Ident) {
            let name = Name::ctx_parse(input, ctx)?;

            return Ok(Self::Name(name));
        }

        if let Some(group) = input.parse::<Option<Group>>()? {
            match group.delimiter() {
                Delimiter::Parenthesis => {
                    let inner = ctx_parse2(FragmentBody::ctx_parse_expr_body, group.stream(), ctx)?;

                    return Ok(inner);
                }

                Delimiter::Brace => {}

                Delimiter::Bracket => {}

                Delimiter::None => {
                    return ctx_parse2(FragmentBody::ctx_parse_outer, group.stream(), ctx);
                }
            }
        }

        if input.peek(Lit) {
            return Ok(Self::Lit(FragmentLit::parse(input)?));
        }

        Err(syn::Error::new(input.span(), "expected a fragment"))
    }

    fn ctx_parse_expr(input: syn::parse::ParseStream, ctx: &mut Context) -> syn::Result<Self> {
        FragmentBody::ctx_parse_single_expr(input, ctx)
    }

    fn ctx_parse_single_expr(
        input: syn::parse::ParseStream,
        ctx: &mut Context,
    ) -> syn::Result<Self> {
        FragmentBody::ctx_parse_expr_body(input, ctx)
    }

    fn ctx_parse_expr_body(input: syn::parse::ParseStream, ctx: &mut Context) -> syn::Result<Self> {
        if input.peek(Token![@]) {
            input.parse::<Token![@]>()?;

            return Ok(Self::ctx_parse_outer(input, ctx)?);
        }

        if input.peek(Ident) {
            let name = Name::ctx_parse(input, ctx)?;

            return Ok(Self::Name(name));
        }

        if let Some(group) = input.parse::<Option<Group>>()? {
            match group.delimiter() {
                Delimiter::Parenthesis => {
                    let inner = ctx_parse2(FragmentBody::ctx_parse_expr_body, group.stream(), ctx)?;

                    return Ok(inner);
                }

                Delimiter::Brace => {
                    let tokens = ctx_parse2(Tokens::ctx_parse, group.stream(), ctx)?;

                    return Ok(Self::Tokens(tokens));
                }

                Delimiter::Bracket => {
                    let parse_fn = |input: ParseStream| {
                        let mut items = Vec::new();

                        loop {
                            if input.is_empty() {
                                break;
                            }

                            let item = FragmentBody::ctx_parse_expr(input, ctx)?;
                            items.push(item);

                            if input.is_empty() {
                                break;
                            }

                            input.parse::<Token![,]>()?;
                        }

                        Ok(items)
                    };

                    let items = parse_fn.parse2(group.stream())?;

                    return Ok(Self::List(items));
                }

                Delimiter::None => {
                    return ctx_parse2(FragmentBody::ctx_parse_expr_body, group.stream(), ctx);
                }
            }
        }

        if input.peek(Lit) {
            return Ok(Self::Lit(FragmentLit::parse(input)?));
        }

        Err(syn::Error::new(input.span(), "expected a fragment"))
    }

    fn resolve_

    fn resolve(&mut self, ctx: &mut Context) {
        match self {
            Self::Lit(_) => {}
            Self::Tokens(_) => {}
            Self::Unknown => {}

            Self::Name(name) => {

            }
        }
    }
}

impl Parse for FragmentLit {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Some(token) = input.parse::<Option<LitInt>>()? {
            return Ok(Self::Int(token.base10_digits().parse().unwrap()));
        }

        if let Some(token) = input.parse::<Option<LitFloat>>()? {
            return Ok(Self::Float(token.base10_digits().parse().unwrap()));
        }

        if let Some(token) = input.parse::<Option<LitBool>>()? {
            return Ok(Self::Bool(token.value));
        }

        if let Some(token) = input.parse::<Option<LitStr>>()? {
            return Ok(Self::String(token.value()));
        }

        if let Some(token) = input.parse::<Option<LitChar>>()? {
            return Ok(Self::Char(token.value()));
        }

        if let Some(lifetime) = input.parse::<Option<Lifetime>>()? {
            return Ok(Self::Ident(IdentStr {
                str: lifetime.ident.to_string(),
                span: lifetime.ident.span(),
            }));
        }

        Err(syn::Error::new(input.span(), "expected a fragment literal"))
    }
}

impl FragmentLit {
    fn to_tokens(&self, at_token_span: Span, output: &mut TokenStream) {
        match self {
            Self::Int(val) => {
                LitInt::new(val.to_string().as_str(), at_token_span).to_tokens(output)
            }
            Self::Float(val) => {
                LitFloat::new(val.to_string().as_str(), at_token_span).to_tokens(output)
            }
            Self::Bool(val) => LitBool::new(*val, at_token_span).to_tokens(output),
            Self::String(val) => LitStr::new(val, at_token_span).to_tokens(output),
            Self::Char(val) => LitChar::new(*val, at_token_span).to_tokens(output),
            Self::Ident(val) => val.to_tokens(output),
        }
    }
}
