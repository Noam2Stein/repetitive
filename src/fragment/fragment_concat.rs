use proc_macro2::{Delimiter, Group, TokenStream};
use syn::{parse::ParseStream, token::Bracket};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentConcat {
    pub expr: Expr,
}

impl ContextParse for FragmentConcat {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let keyword = if Keyword::peek(input) {
            Some(<Keyword>::ctx_parse(input, ctx)?)
        } else {
            None
        };

        let group = Group::ctx_parse(input, ctx)?;
        if group.delimiter() != Delimiter::Bracket {
            return Err(Error::ParseError(syn::Error::new(
                group.span(),
                "expected a bracket-delimited list",
            )));
        }

        let parse_fn = |input: ParseStream, ctx: &mut Context| {
            let mut parts = Vec::new();

            loop {
                if input.is_empty() {
                    break;
                }

                let item = Expr::ctx_parse_single(input, ctx)?;
                parts.push(item);
            }

            Ok::<_, Error>(parts)
        };

        let parts = parse_fn.ctx_parse2(group.stream(), ctx)?;

        let op = if let Some(Keyword::Str(span)) = keyword {
            Op::ConcatString(span)
        } else {
            Op::ConcatIdent(group.span())
        };
        let args = vec![Expr {
            span: group.span(),
            kind: ExprKind::List(parts),
        }];

        let expr = Expr {
            span: group.span(),
            kind: ExprKind::Op(FragmentOp { op, args }),
        };

        Ok(Self { expr })
    }
}

impl Peek for FragmentConcat {
    fn peek(input: ParseStream) -> bool {
        input.peek(Bracket) || Keyword::peek(input)
    }
}

impl Optimize for FragmentConcat {
    fn optimize(&mut self, ctx: &mut Context) {
        self.expr.optimize(ctx);
    }
}

impl Expand for FragmentConcat {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace) {
        self.expr.expand(output, ctx, namespace);
    }
}
