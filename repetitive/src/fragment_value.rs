use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote_spanned};
use syn::{
    Error, Ident, Lifetime, LitBool, LitChar, LitFloat, LitInt, LitStr, Token, parse::ParseStream,
};

use super::*;

#[derive(Debug, Clone)]
pub enum FragmentValue {
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Ident(String),
    List(Vec<FragmentValue>),
    Tokens(Tokens),
}

#[derive(Debug, Clone)]
pub struct FragmentValueExpr {
    pub span: Span,
    pub value: FragmentValue,
}

impl FragmentValueExpr {
    pub fn option_lit(input: ParseStream) -> syn::Result<Option<Self>> {
        if input.peek(LitInt) {
            return Ok(Some(Self::int_lit(input.parse().unwrap())));
        }
        if input.peek(LitFloat) {
            return Ok(Some(Self::float_lit(input.parse().unwrap())));
        }
        if input.peek(LitBool) {
            return Ok(Some(Self::bool_lit(input.parse().unwrap())));
        }
        if input.peek(LitChar) {
            return Ok(Some(Self::char_lit(input.parse().unwrap())));
        }
        if input.peek(LitStr) {
            return Ok(Some(Self::str_lit(input.parse().unwrap())));
        }
        if input.peek(Lifetime) {
            return Ok(Some(Self::ident_lit(input.parse().unwrap())));
        }
        if input.peek(Token![~]) {
            let punct_span = input.parse::<Token![~]>().unwrap().span;
            let ident = input.parse::<Ident>()?;

            let lifetime = Lifetime {
                apostrophe: punct_span,
                ident,
            };

            return Ok(Some(Self::ident_lit(lifetime)));
        }

        Ok(None)
    }
    pub fn int_lit(lit: LitInt) -> Self {
        Self {
            span: lit.span(),
            value: FragmentValue::Int(lit.base10_digits().parse().unwrap()),
        }
    }
    pub fn float_lit(lit: LitFloat) -> Self {
        Self {
            span: lit.span(),
            value: FragmentValue::Float(lit.base10_digits().parse().unwrap()),
        }
    }
    pub fn bool_lit(lit: LitBool) -> Self {
        Self {
            span: lit.span(),
            value: FragmentValue::Bool(lit.value),
        }
    }
    pub fn char_lit(lit: LitChar) -> Self {
        Self {
            span: lit.span(),
            value: FragmentValue::Char(lit.value()),
        }
    }
    pub fn str_lit(lit: LitStr) -> Self {
        Self {
            span: lit.span(),
            value: FragmentValue::String(lit.value()),
        }
    }
    pub fn ident_lit(lit: Lifetime) -> Self {
        Self {
            span: lit.span(),
            value: FragmentValue::Ident(lit.ident.to_string()),
        }
    }

    pub fn into_expr(self) -> FragmentExpr {
        FragmentExpr {
            span: self.span,
            kind: FragmentExprKind::Value(self.value),
        }
    }
}
impl FragmentValue {
    pub fn kind(&self) -> &str {
        match self {
            FragmentValue::Int(_) => "int",
            FragmentValue::Float(_) => "float",
            FragmentValue::Bool(_) => "bool",
            FragmentValue::String(_) => "string",
            FragmentValue::Char(_) => "char",
            FragmentValue::Ident(_) => "ident",
            FragmentValue::List(_) => "list",
            FragmentValue::Tokens(_) => "tokens",
        }
    }
}

impl Paste for FragmentValueExpr {
    fn paste(
        &self,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> syn::Result<()> {
        Ok(match &self.value {
            FragmentValue::Int(val) => {
                let lit = LitInt::new(val.abs().to_string().as_str(), self.span);
                if *val < 0 {
                    quote_spanned! { self.span => -#lit }.to_tokens(output);
                } else {
                    lit.to_tokens(output);
                }
            }
            FragmentValue::Float(val) => {
                let lit = LitFloat::new(&format!("{:?}", val.abs()), self.span);
                if *val < 0.0 {
                    quote_spanned! { self.span => -#lit }.to_tokens(output);
                } else {
                    lit.to_tokens(output);
                }
            }
            FragmentValue::Bool(val) => LitBool::new(*val, self.span).to_tokens(output),
            FragmentValue::String(val) => LitStr::new(val, self.span).to_tokens(output),
            FragmentValue::Char(val) => LitChar::new(*val, self.span).to_tokens(output),
            FragmentValue::Ident(val) => Ident::new(val, self.span).to_tokens(output),

            FragmentValue::Tokens(val) => val.paste(output, ctx, namespace)?,

            FragmentValue::List(_) => {
                return Err(Error::new(self.span, "cannot paste `list`"));
            }
        })
    }
}
