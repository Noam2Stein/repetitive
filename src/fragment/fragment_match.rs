use proc_macro2::{Delimiter, Group, TokenStream};
use syn::{Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentMatch {
    pub expr: Expr,
}

impl ContextParse for FragmentMatch {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let match_token = <Token![match]>::ctx_parse(input, ctx)?;

        let expr = Box::new(Expr::ctx_parse(input, ctx)?);

        let group = Group::ctx_parse(input, ctx)?;
        if group.delimiter() != Delimiter::Brace {
            return Err(Error::ParseError(syn::Error::new(
                group.span(),
                "expected a brace-delimited block",
            )));
        }

        let match_arms_parse_fn = |input: ParseStream, ctx: &mut Context| {
            let mut arms = Vec::new();

            while !input.is_empty() {
                let pat_span = input.span();
                let pat = Pattern::ctx_parse(input, ctx)?;

                let condition = if input.peek(Token![if]) {
                    <Token![if]>::ctx_parse(input, ctx)?;

                    Some(Expr::ctx_parse(input, ctx)?)
                } else {
                    None
                };

                <Token![=>]>::ctx_parse(input, ctx)?;

                let body_group = Group::ctx_parse(input, ctx)?;
                if body_group.delimiter() != Delimiter::Brace {
                    return Err(Error::ParseError(syn::Error::new(
                        body_group.span(),
                        "expected a brace-delimited block",
                    )));
                }

                let body = Tokens::ctx_parse.ctx_parse2(body_group.stream(), ctx)?;

                arms.push(ExprMatchArm {
                    pat,
                    condition,
                    body: Value {
                        span: group.span(),
                        kind: ValueKind::Tokens(body),
                    }
                    .into_expr(),
                    unused_arm_warning: ctx.push_warning(Warning::UnusedMatchArm(pat_span)),
                });

                if let Some(token) = <Option<Token![,]>>::ctx_parse(input, ctx)? {
                    ctx.push_warning(Warning::UnnecessaryPunct {
                        span: token.span,
                        punct: ",",
                    });
                }
            }

            Ok::<_, Error>(arms)
        };

        let match_arms = match_arms_parse_fn.ctx_parse2(group.stream(), ctx)?;

        let expr = Expr {
            span: match_token.span,
            kind: ExprKind::Match(ExprMatch {
                match_token,
                expr,
                match_arms,
            }),
        };

        Ok(Self { expr })
    }
}

impl Peek for FragmentMatch {
    fn peek(input: ParseStream) -> bool {
        input.peek(Token![match])
    }
}

impl Optimize for FragmentMatch {
    fn optimize(&mut self, ctx: &mut Context) {
        self.expr.optimize(ctx);
    }
}

impl Expand for FragmentMatch {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace) {
        self.expr.expand(output, ctx, namespace);
    }
}
