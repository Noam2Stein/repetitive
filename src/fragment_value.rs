use std::fmt::Display;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote, quote_spanned};
use syn::{Ident, Lifetime, LitBool, LitChar, LitFloat, LitInt, LitStr, Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentValue {
    pub span: Span,
    pub kind: FragmentValueKind,
}

#[derive(Debug, Clone)]
pub enum FragmentValueKind {
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Ident(String),
    List(Vec<FragmentValue>),
    Tokens(Tokens),
    Unknown(UnknownGuard),
}

impl FragmentValue {
    pub fn unknown(guard: UnknownGuard) -> Self {
        Self {
            span: Span::call_site(),
            kind: FragmentValueKind::Unknown(guard),
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
            kind: FragmentValueKind::Int(lit.base10_digits().parse().unwrap()),
        }
    }
    pub fn float_lit(lit: LitFloat) -> Self {
        Self {
            span: lit.span(),
            kind: FragmentValueKind::Float(lit.base10_digits().parse().unwrap()),
        }
    }
    pub fn bool_lit(lit: LitBool) -> Self {
        Self {
            span: lit.span(),
            kind: FragmentValueKind::Bool(lit.value),
        }
    }
    pub fn char_lit(lit: LitChar) -> Self {
        Self {
            span: lit.span(),
            kind: FragmentValueKind::Char(lit.value()),
        }
    }
    pub fn str_lit(lit: LitStr) -> Self {
        Self {
            span: lit.span(),
            kind: FragmentValueKind::String(lit.value()),
        }
    }
    pub fn ident_lit(lit: Lifetime) -> Self {
        Self {
            span: lit.span(),
            kind: FragmentValueKind::Ident(lit.ident.to_string()),
        }
    }

    pub fn into_expr(self) -> FragmentExpr {
        FragmentExpr {
            span: self.span,
            kind: FragmentExprKind::Value(self.kind),
        }
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self.kind, FragmentValueKind::Unknown(_))
    }
}
impl FragmentValueKind {
    pub fn kind_str(&self) -> &'static str {
        match self {
            FragmentValueKind::Int(_) => "int",
            FragmentValueKind::Float(_) => "float",
            FragmentValueKind::Bool(_) => "bool",
            FragmentValueKind::String(_) => "string",
            FragmentValueKind::Char(_) => "char",
            FragmentValueKind::Ident(_) => "ident",
            FragmentValueKind::List(_) => "list",
            FragmentValueKind::Tokens(_) => "tokens",
            FragmentValueKind::Unknown(_) => "unknown",
        }
    }
}

impl Paste for FragmentValue {
    fn paste(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &mut Namespace) {
        match &self.kind {
            FragmentValueKind::Int(val) => {
                let lit = LitInt::new(val.abs().to_string().as_str(), self.span);
                if *val < 0 {
                    quote_spanned! { self.span => -#lit }.to_tokens(output);
                } else {
                    lit.to_tokens(output);
                }
            }
            FragmentValueKind::Float(val) => {
                let lit = LitFloat::new(&format!("{:?}", val.abs()), self.span);
                if *val < 0.0 {
                    quote_spanned! { self.span => -#lit }.to_tokens(output);
                } else {
                    lit.to_tokens(output);
                }
            }
            FragmentValueKind::Bool(val) => LitBool::new(*val, self.span).to_tokens(output),
            FragmentValueKind::String(val) => LitStr::new(val, self.span).to_tokens(output),
            FragmentValueKind::Char(val) => LitChar::new(*val, self.span).to_tokens(output),
            FragmentValueKind::Ident(val) => Ident::new(val, self.span).to_tokens(output),

            FragmentValueKind::Tokens(val) => val.paste(output, ctx, namespace),

            FragmentValueKind::List(val) => {
                let mut items = TokenStream::new();
                for item in val {
                    item.paste(&mut items, ctx, namespace);

                    quote! { , }.to_tokens(&mut items);
                }

                quote_spanned! { self.span => [#items] }.to_tokens(output);
            }

            FragmentValueKind::Unknown(_) => return,
        }
    }
}

impl Display for FragmentValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            FragmentValueKind::Int(val) => write!(f, "{val:?}"),
            FragmentValueKind::Float(val) => write!(f, "{val:?}"),
            FragmentValueKind::Bool(val) => write!(f, "{val:?}"),
            FragmentValueKind::String(val) => write!(f, "{val:?}"),
            FragmentValueKind::Char(val) => write!(f, "{val:?}"),
            FragmentValueKind::Ident(val) => write!(f, "'{val}"),

            FragmentValueKind::List(val) => {
                write!(f, "[")?;

                for (idx, item) in val.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{item}")?;
                }

                write!(f, "]")
            }

            FragmentValueKind::Tokens(_) => write!(f, "{{tokens}}"),

            FragmentValueKind::Unknown(_) => {
                unreachable!("display should not be called on unknown values")
            }
        }
    }
}
