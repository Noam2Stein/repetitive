use proc_macro2::{Delimiter, Group, TokenStream};
use syn::{parse::ParseStream, token::Brace};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentTokens {
    pub tokens: Tokens,
}

impl ContextParse for FragmentTokens {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let group = Group::ctx_parse(input, ctx)?;
        if group.delimiter() != Delimiter::Brace {
            return Err(Error::ParseError(syn::Error::new(
                group.span(),
                "expected a brace-delimited block",
            )));
        }

        let tokens = Tokens::ctx_parse.ctx_parse2(group.stream(), ctx)?;

        Ok(Self { tokens })
    }
}

impl Peek for FragmentTokens {
    fn peek(input: ParseStream) -> bool {
        input.peek(Brace)
    }
}

impl Optimize for FragmentTokens {
    fn optimize(&mut self, ctx: &mut Context) {
        self.tokens.optimize(ctx);
    }
}

impl Expand for FragmentTokens {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace) {
        self.tokens.expand(output, ctx, namespace);
    }
}
