use proc_macro2::Span;
use string_interner::DefaultSymbol;
use syn::{Ident, Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone, Copy)]
pub struct Name {
    pub id: NameId,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameId {
    pub inner: DefaultSymbol,
}

impl Name {
    pub fn peek(input: ParseStream) -> bool {
        input.peek(Ident) || input.peek(Token![_])
    }
}

impl ContextParse for Name {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        if Keyword::peek(input) {
            return Err(syn::Error::new(
                input.span(),
                "expected name, found keyword",
            ));
        }

        let ident = input.parse::<Ident>()?;
        let id = NameId {
            inner: ctx.interner.get_or_intern(ident.to_string()),
        };

        Ok(Self {
            id,
            span: ident.span(),
        })
    }
}
