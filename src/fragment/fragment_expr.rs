use proc_macro2::{Delimiter, Group, TokenStream};
use syn::{parse::ParseStream, token::Paren};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentExpr {
    pub expr: Expr,
}

impl ContextParse for FragmentExpr {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        if let Some(name) = Name::ctx_parse_option(input, ctx)? {
            return Ok(Self {
                expr: Expr::name(name),
            });
        }

        let group = Group::ctx_parse(input, ctx)?;
        if group.delimiter() != Delimiter::Parenthesis {
            return Err(Error::ParseError(syn::Error::new(
                group.span(),
                "expected a parenthesized expression",
            )));
        }

        let expr = Expr::ctx_parse.ctx_parse2(group.stream(), ctx)?;

        Ok(Self { expr })
    }
}

impl Peek for FragmentExpr {
    fn peek(input: ParseStream) -> bool {
        input.peek(Paren) | Name::peek(input)
    }
}

impl Optimize for FragmentExpr {
    fn optimize(&mut self, ctx: &mut Context) {
        self.expr.optimize(ctx);
    }
}

impl Expand for FragmentExpr {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace) {
        self.expr.expand(output, ctx, namespace);
    }
}
