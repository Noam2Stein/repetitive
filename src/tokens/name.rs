use proc_macro2::Span;
use syn::{Ident, Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone, Copy)]
pub struct Name {
    pub id: NameId,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameId {
    pub strid: StrId,
}

impl ContextParse for Name {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        if Keyword::peek(input) {
            return Err(Error::IsKeyword {
                span: input.span(),
                keyword: input.parse::<Keyword>().unwrap().to_string(),
            });
        }

        let ident = Ident::ctx_parse(input, ctx)?;

        let id = NameId {
            strid: ctx.intern(&ident.to_string()),
        };

        Ok(Self {
            id,
            span: ident.span(),
        })
    }
}

impl Peek for Name {
    fn peek(input: ParseStream) -> bool {
        if Keyword::peek(input) {
            return false;
        }

        input.peek(Ident) && !input.peek(Token![_])
    }
}
