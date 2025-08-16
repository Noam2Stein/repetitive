use proc_macro2::{Delimiter, Group, TokenStream};
use syn::{Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentIf {
    pub expr: Expr,
}

impl ContextParse for FragmentIf {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let if_token = <Token![if]>::ctx_parse(input, ctx)?;

        let cond = Expr::ctx_parse(input, ctx)?;

        let then = {
            let then_group = Group::ctx_parse(input, ctx)?;
            if then_group.delimiter() != Delimiter::Brace {
                return Err(Error::ParseError(syn::Error::new(
                    then_group.span(),
                    "expected a brace-delimited block",
                )));
            }

            Value {
                span: if_token.span,
                kind: ValueKind::Tokens(Tokens::ctx_parse.ctx_parse2(then_group.stream(), ctx)?),
            }
            .into_expr()
        };

        let else_ = if input.peek(Token![else]) {
            let else_token = <Token![else]>::ctx_parse(input, ctx)?;

            if let Some(else_if) = FragmentIf::ctx_parse_option(input, ctx)? {
                else_if.expr
            } else {
                Value {
                    span: else_token.span,
                    kind: ValueKind::Tokens(FragmentTokens::ctx_parse(input, ctx)?.tokens),
                }
                .into_expr()
            }
        } else {
            Value {
                span: if_token.span,
                kind: ValueKind::Tokens(Tokens::default()),
            }
            .into_expr()
        };

        let expr = Expr {
            span: if_token.span,
            kind: ExprKind::Op(FragmentOp {
                op: Op::IfElse(if_token.span),
                args: vec![cond, then, else_],
            }),
        };

        Ok(Self { expr })
    }
}

impl Peek for FragmentIf {
    fn peek(input: ParseStream) -> bool {
        input.peek(Token![if])
    }
}

impl Optimize for FragmentIf {
    fn optimize(&mut self, ctx: &mut Context) {
        self.expr.optimize(ctx);
    }
}

impl Expand for FragmentIf {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace) {
        self.expr.expand(output, ctx, namespace);
    }
}
