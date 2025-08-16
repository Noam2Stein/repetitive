use proc_macro2::{Delimiter, Group, TokenStream};
use syn::{Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentFor {
    pub for_token: Token![for],
    pub iters: Vec<FragmentForIter>,
    pub body: Tokens,
}

#[derive(Debug, Clone)]
pub struct FragmentForIter {
    pub pat: Pattern,
    pub iter: Expr,
}

impl ContextParse for FragmentFor {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let for_token = <Token![for]>::ctx_parse(input, ctx)?;

        let mut iters = Vec::new();
        while Pattern::peek(input) {
            let iter = FragmentForIter::ctx_parse(input, ctx)?;
            iters.push(iter);

            if !input.peek(Token![,]) {
                break;
            }

            <Token![,]>::ctx_parse(input, ctx)?;
        }

        let body_group = Group::ctx_parse(input, ctx)?;
        if body_group.delimiter() != Delimiter::Brace {
            return Err(Error::ParseError(syn::Error::new(
                body_group.span(),
                "expected a brace-delimited block",
            )));
        }

        let body = Tokens::ctx_parse.ctx_parse2(body_group.stream(), ctx)?;

        return Ok(Self {
            for_token,
            iters,
            body,
        });
    }
}
impl ContextParse for FragmentForIter {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let pat = Pattern::ctx_parse(input, ctx)?;

        <Token![in]>::ctx_parse(input, ctx)?;

        let iter = Expr::ctx_parse(input, ctx)?;

        Ok(Self { pat, iter })
    }
}

impl Peek for FragmentFor {
    fn peek(input: ParseStream) -> bool {
        input.peek(Token![for])
    }
}

impl Optimize for FragmentFor {
    fn optimize(&mut self, ctx: &mut Context) {
        self.body.optimize(ctx);
    }
}

impl Expand for FragmentFor {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace) {
        self.expand_recursive(0, output, ctx, namespace);
    }
}
impl FragmentFor {
    fn expand_recursive(
        &self,
        iter_idx: usize,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &Namespace,
    ) {
        let iter = &self.iters[iter_idx];
        let iter_items = iter.iter.eval(ctx, namespace);

        let iter_items = match iter_items.kind {
            ValueKind::Unknown(_) => return,

            ValueKind::List(val) => val,
            _ => {
                ctx.push_error(Error::ExpectedFound {
                    span: iter_items.span,
                    expected: "list",
                    found: iter_items.kind.kind_str(),
                });
                return;
            }
        };

        for item in iter_items {
            let mut item_namespace = namespace.fork();
            iter.pat.queue_insert(item, &mut item_namespace, ctx);
            item_namespace.flush();

            if self.iters.get(iter_idx + 1).is_some() {
                self.expand_recursive(iter_idx + 1, output, ctx, &mut item_namespace);
            } else {
                self.body.expand(output, ctx, &mut item_namespace);
            }
        }
    }
}
