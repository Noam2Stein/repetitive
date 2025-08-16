use proc_macro2::TokenStream;
use syn::{Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentLet {
    pub let_token: Token![let],
    pub pat: Pattern,
    pub expr: Expr,
}

impl ContextParse for FragmentLet {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let let_token = <Token![let]>::ctx_parse(input, ctx)?;

        let pat = Pattern::ctx_parse(input, ctx)?;

        <Token![=]>::ctx_parse(input, ctx)?;

        let expr = Expr::ctx_parse(input, ctx)?;

        <Token![;]>::ctx_parse(input, ctx)?;

        return Ok(Self {
            let_token,
            pat,
            expr,
        });
    }
}

impl Peek for FragmentLet {
    fn peek(input: ParseStream) -> bool {
        input.peek(Token![let])
    }
}

impl Optimize for FragmentLet {
    fn optimize(&mut self, ctx: &mut Context) {
        self.expr.optimize(ctx);
    }
}

impl ExpandInto for FragmentLet {
    fn expand_into(&self, _output: &mut TokenStream, ctx: &mut Context, namespace: &mut Namespace) {
        let val_expr = self.expr.eval(ctx, namespace);

        namespace.flush();
        self.pat.queue_insert(val_expr, namespace, ctx);
        namespace.flush();
    }
}
