use std::fmt::Display;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote, quote_spanned};
use syn::{Ident, Lifetime, LitBool, LitChar, LitFloat, LitInt, LitStr, Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone)]
pub struct Value {
    pub span: Span,
    pub kind: ValueKind,
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Ident(String),
    List(Vec<Value>),
    Tokens(Tokens),
    Unknown(UnknownGuard),
}

impl Value {
    pub fn unknown(guard: UnknownGuard) -> Self {
        Self {
            span: Span::call_site(),
            kind: ValueKind::Unknown(guard),
        }
    }

    pub fn peek_lit(input: ParseStream) -> bool {
        input.peek(LitInt)
            || input.peek(LitFloat)
            || input.peek(LitBool)
            || input.peek(LitChar)
            || input.peek(LitStr)
            || input.peek(Lifetime)
            || input.peek(Token![~])
    }

    pub fn ctx_parse_option_lit(
        input: ParseStream,
        ctx: &mut Context,
    ) -> Result<Option<Self>, Error> {
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
            let ident = <Ident>::ctx_parse(input, ctx)?;

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
            kind: ValueKind::Int(lit.base10_digits().parse().unwrap()),
        }
    }
    pub fn float_lit(lit: LitFloat) -> Self {
        Self {
            span: lit.span(),
            kind: ValueKind::Float(lit.base10_digits().parse().unwrap()),
        }
    }
    pub fn bool_lit(lit: LitBool) -> Self {
        Self {
            span: lit.span(),
            kind: ValueKind::Bool(lit.value),
        }
    }
    pub fn char_lit(lit: LitChar) -> Self {
        Self {
            span: lit.span(),
            kind: ValueKind::Char(lit.value()),
        }
    }
    pub fn str_lit(lit: LitStr) -> Self {
        Self {
            span: lit.span(),
            kind: ValueKind::String(lit.value()),
        }
    }
    pub fn ident_lit(lit: Lifetime) -> Self {
        Self {
            span: lit.span(),
            kind: ValueKind::Ident(lit.ident.to_string()),
        }
    }

    pub fn into_expr(self) -> Expr {
        Expr {
            span: self.span,
            kind: ExprKind::Value(self),
        }
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self.kind, ValueKind::Unknown(_))
    }
}
impl ValueKind {
    pub fn kind_str(&self) -> &'static str {
        match self {
            ValueKind::Int(_) => "int",
            ValueKind::Float(_) => "float",
            ValueKind::Bool(_) => "bool",
            ValueKind::String(_) => "string",
            ValueKind::Char(_) => "char",
            ValueKind::Ident(_) => "ident",
            ValueKind::List(_) => "list",
            ValueKind::Tokens(_) => "tokens",
            ValueKind::Unknown(_) => "unknown",
        }
    }
}

impl Expand for Value {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace) {
        match &self.kind {
            ValueKind::Int(val) => {
                let lit = LitInt::new(val.abs().to_string().as_str(), self.span);
                if *val < 0 {
                    quote_spanned! { self.span => -#lit }.to_tokens(output);
                } else {
                    lit.to_tokens(output);
                }
            }

            ValueKind::Float(val) => {
                let lit = LitFloat::new(&format!("{:?}", val.abs()), self.span);
                if *val < 0.0 {
                    quote_spanned! { self.span => -#lit }.to_tokens(output);
                } else {
                    lit.to_tokens(output);
                }
            }

            ValueKind::Bool(val) => LitBool::new(*val, self.span).to_tokens(output),
            ValueKind::String(val) => LitStr::new(val, self.span).to_tokens(output),
            ValueKind::Char(val) => LitChar::new(*val, self.span).to_tokens(output),
            ValueKind::Ident(val) => Ident::new(val, self.span).to_tokens(output),

            ValueKind::Tokens(val) => val.expand(output, ctx, namespace),

            ValueKind::List(val) => {
                let mut items = TokenStream::new();
                for item in val {
                    item.expand(&mut items, ctx, namespace);

                    quote! { , }.to_tokens(&mut items);
                }

                quote_spanned! { self.span => [#items] }.to_tokens(output);
            }

            ValueKind::Unknown(_) => return,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ValueKind::Int(val) => write!(f, "{val:?}"),
            ValueKind::Float(val) => write!(f, "{val:?}"),
            ValueKind::Bool(val) => write!(f, "{val:?}"),
            ValueKind::String(val) => write!(f, "{val:?}"),
            ValueKind::Char(val) => write!(f, "{val:?}"),
            ValueKind::Ident(val) => write!(f, "'{val}"),

            ValueKind::List(val) => {
                write!(f, "[")?;

                for (idx, item) in val.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{item}")?;
                }

                write!(f, "]")
            }

            ValueKind::Tokens(_) => write!(f, "{{tokens}}"),

            ValueKind::Unknown(_) => {
                unreachable!("display should not be called on unknown values")
            }
        }
    }
}
