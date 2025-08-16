use syn::parse::ParseStream;

use super::*;

pub trait Peek {
    fn peek(input: ParseStream) -> bool;

    fn ctx_parse_option(input: ParseStream, ctx: &mut Context) -> Result<Option<Self>, Error>
    where
        Self: Sized + ContextParse,
    {
        Ok(if Self::peek(input) {
            Some(Self::ctx_parse(input, ctx)?)
        } else {
            None
        })
    }
}
