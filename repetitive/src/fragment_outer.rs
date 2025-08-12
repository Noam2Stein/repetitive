use proc_macro2::{Delimiter, Group, Span, TokenStream};
use syn::{
    Error, Ident, Token,
    parse::{ParseStream, Parser},
    token::{Brace, Bracket, Paren},
};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentOuter {
    #[allow(dead_code)]
    pub at_span: Span,
    pub kind: FragmentOuterKind,
}
#[derive(Debug, Clone)]
pub enum FragmentOuterKind {
    For(FragmentFor),
    Let(FragmentLet),
    Expr(FragmentExpr),
}

#[derive(Debug, Clone)]
pub struct FragmentFor {
    pub for_span: Span,
    pub iters: Vec<FragmentForIter>,
    pub body: Tokens,
}
#[derive(Debug, Clone)]
pub struct FragmentForIter {
    pub pat: Pattern,
    pub iter: FragmentExpr,
}

#[derive(Debug, Clone)]
pub struct FragmentLet {
    pub let_span: Span,
    pub pat: Pattern,
    pub expr: FragmentExpr,
}

impl ContextParse for FragmentOuter {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        let at_token = input.parse::<Token![@]>()?;
        let kind = FragmentOuterKind::ctx_parse(input, ctx)?;

        Ok(Self {
            at_span: at_token.span,
            kind,
        })
    }
}
impl ContextParse for FragmentOuterKind {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        if input.peek(Token![for]) {
            return Ok(Self::For(FragmentFor::ctx_parse(input, ctx)?));
        }

        if input.peek(Token![let]) {
            return Ok(Self::Let(FragmentLet::ctx_parse(input, ctx)?));
        }

        if input.peek(Token![if]) {
            return Ok(Self::ctx_parse_if(input, ctx)?);
        }

        if input.peek(Token![match]) {
            return Ok(Self::ctx_parse_match(input, ctx)?);
        }

        if input.peek(Bracket) || Keyword::peek(input) {
            return Ok(Self::parse_concat(input, ctx)?);
        }

        if input.peek(Ident) {
            let name = Name::ctx_parse(input, ctx)?;

            return Ok(Self::Expr(FragmentExpr::name(name)));
        }

        if input.peek(Paren) {
            let group = input.parse::<Group>()?;

            let expr = FragmentExpr::ctx_parse.ctx_parse2(group.stream(), ctx)?;

            return Ok(Self::Expr(expr));
        }

        if input.peek(Brace) {
            let group = input.parse::<Group>()?;

            let tokens = Tokens::ctx_parse.ctx_parse2(group.stream(), ctx)?;

            return Ok(Self::Expr(FragmentExpr {
                span: group.span(),
                kind: FragmentExprKind::Value(FragmentValueKind::Tokens(tokens)),
            }));
        }

        Err(syn::Error::new(input.span(), "expected a fragment"))
    }
}

impl ContextParse for FragmentFor {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        let for_span = input.parse::<Token![for]>()?.span;

        let mut iters = Vec::new();
        while Pattern::peek(input) {
            let iter = FragmentForIter::ctx_parse(input, ctx)?;
            iters.push(iter);

            if !input.peek(Token![,]) {
                break;
            }

            input.parse::<Token![,]>()?;
        }

        let body_group = input.parse::<Group>()?;
        if body_group.delimiter() != Delimiter::Brace {
            return Err(syn::Error::new(
                body_group.span(),
                "expected a brace-delimited block",
            ));
        }

        let body = Tokens::ctx_parse.ctx_parse2(body_group.stream(), ctx)?;

        return Ok(Self {
            for_span,
            iters,
            body,
        });
    }
}
impl ContextParse for FragmentForIter {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        let pat = Pattern::ctx_parse(input, ctx)?;

        input.parse::<Token![in]>()?;

        let iter = FragmentExpr::ctx_parse(input, ctx)?;

        Ok(Self { pat, iter })
    }
}

impl ContextParse for FragmentLet {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        let let_span = input.parse::<Token![let]>()?.span;

        let pat = Pattern::ctx_parse(input, ctx)?;

        input.parse::<Token![=]>()?;

        let expr = FragmentExpr::ctx_parse(input, ctx)?;

        input.parse::<Token![;]>()?;

        return Ok(Self {
            let_span,
            pat,
            expr,
        });
    }
}

impl FragmentOuterKind {
    fn ctx_parse_if(input: ParseStream, ctx: &mut Context) -> syn::Result<Self> {
        let if_span = input.parse::<Token![if]>()?.span;
        let cond = FragmentExpr::ctx_parse(input, ctx)?;

        let then_group = input.parse::<Group>()?;
        if then_group.delimiter() != Delimiter::Brace {
            return Err(syn::Error::new(
                then_group.span(),
                "expected a brace-delimited block",
            ));
        }

        let then = FragmentExpr {
            span: if_span,
            kind: FragmentExprKind::Value(FragmentValueKind::Tokens(
                Tokens::ctx_parse.ctx_parse2(then_group.stream(), ctx)?,
            )),
        };

        let else_ = if input.peek(Token![else]) {
            input.parse::<Token![else]>()?;

            if input.peek(Token![if]) {
                let else_if = FragmentOuterKind::ctx_parse(input, ctx)?;

                match else_if {
                    FragmentOuterKind::Expr(expr) => expr,
                    _ => unreachable!(),
                }
            } else {
                let else_group = input.parse::<Group>()?;
                if else_group.delimiter() != Delimiter::Brace {
                    return Err(syn::Error::new(
                        else_group.span(),
                        "expected a brace-delimited block",
                    ));
                }

                FragmentExpr {
                    span: if_span,
                    kind: FragmentExprKind::Value(FragmentValueKind::Tokens(
                        Tokens::ctx_parse.ctx_parse2(else_group.stream(), ctx)?,
                    )),
                }
            }
        } else {
            FragmentExpr {
                span: if_span,
                kind: FragmentExprKind::Value(FragmentValueKind::Tokens(Tokens::default())),
            }
        };

        return Ok(Self::Expr(FragmentExpr::op(
            Op::IfElse(if_span),
            vec![cond, then, else_],
            ctx,
        )?));
    }

    fn ctx_parse_match(input: ParseStream, ctx: &mut Context) -> syn::Result<Self> {
        let match_span = input.parse::<Token![match]>()?.span;

        let expr = Box::new(FragmentExpr::ctx_parse(input, ctx)?);

        let group = input.parse::<Group>()?;
        if group.delimiter() != Delimiter::Brace {
            return Err(syn::Error::new(
                group.span(),
                "expected a brace-delimited block",
            ));
        }

        let match_arms_parse_fn = |input: ParseStream, ctx: &mut Context| {
            let mut arms = Vec::new();

            while !input.is_empty() {
                let pat = Pattern::ctx_parse(input, ctx)?;

                let condition = if input.peek(Token![if]) {
                    input.parse::<Token![if]>()?;

                    Some(FragmentExpr::ctx_parse(input, ctx)?)
                } else {
                    None
                };

                input.parse::<Token![=>]>()?;

                let body_group = input.parse::<Group>()?;
                if body_group.delimiter() != Delimiter::Brace {
                    return Err(syn::Error::new(
                        body_group.span(),
                        "expected a brace-delimited block",
                    ));
                }

                let body = Tokens::ctx_parse.ctx_parse2(body_group.stream(), ctx)?;

                arms.push(FragmentMatchArm {
                    pat,
                    condition,
                    body: FragmentExpr {
                        span: group.span(),
                        kind: FragmentExprKind::Value(FragmentValueKind::Tokens(body)),
                    },
                });

                if let Some(token) = input.parse::<Option<Token![,]>>()? {
                    ctx.warnings
                        .push(Error::new_spanned(token, "unnecessary comma"));
                }
            }

            Ok::<_, syn::Error>(arms)
        };

        let match_arms = match_arms_parse_fn.ctx_parse2(group.stream(), ctx)?;

        Ok(Self::Expr(FragmentExpr {
            span: match_span,
            kind: FragmentExprKind::Match(FragmentMatch {
                match_span,
                expr,
                match_arms,
            }),
        }))
    }

    fn parse_concat(input: ParseStream, ctx: &mut Context) -> syn::Result<Self> {
        let keyword = if Keyword::peek(input) {
            Some(input.parse::<Keyword>()?)
        } else {
            None
        };

        let group = input.parse::<Group>()?;
        if group.delimiter() != Delimiter::Bracket {
            return Err(syn::Error::new(
                group.span(),
                "expected a bracket-delimited list",
            ));
        }

        let parse_fn = |input: ParseStream| {
            let mut parts = Vec::new();

            loop {
                if input.is_empty() {
                    break;
                }

                let item = FragmentExpr::ctx_parse_single(input, ctx)?;
                parts.push(item);
            }

            Ok::<_, syn::Error>(parts)
        };

        let parts = parse_fn.parse2(group.stream())?;

        let op = if let Some(Keyword::Str) = keyword {
            Op::ConcatString(group.span())
        } else {
            Op::ConcatIdent(group.span())
        };
        let op_args = vec![FragmentExpr {
            span: group.span(),
            kind: FragmentExprKind::List(parts),
        }];

        Ok(Self::Expr(FragmentExpr::op(op, op_args, ctx)?))
    }
}

impl Paste for FragmentOuter {
    fn paste(
        &self,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> syn::Result<()> {
        Ok(match &self.kind {
            FragmentOuterKind::Expr(frag) => frag.paste(output, ctx, namespace)?,
            FragmentOuterKind::For(frag) => frag.paste(output, ctx, namespace)?,
            FragmentOuterKind::Let(frag) => frag.paste(output, ctx, namespace)?,
        })
    }
}

impl Paste for FragmentFor {
    fn paste(
        &self,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> syn::Result<()> {
        self.paste_from_iter_items(0, output, ctx, namespace)?;

        Ok(())
    }
}
impl FragmentFor {
    fn paste_from_iter_items(
        &self,
        iter_idx: usize,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> syn::Result<()> {
        let iter = &self.iters[iter_idx];
        let iter_items = iter.iter.eval(ctx, namespace)?;

        let iter_items = match iter_items.kind {
            FragmentValueKind::List(val) => val,
            _ => return Err(syn::Error::new(iter_items.span, "expected a list")),
        };

        for item in iter_items {
            let mut item_namespace = namespace.fork();
            iter.pat.queue_insert(item, &mut item_namespace, ctx)?;
            item_namespace.flush();

            if self.iters.get(iter_idx + 1).is_some() {
                self.paste_from_iter_items(iter_idx + 1, output, ctx, &mut item_namespace)?;
            } else {
                self.body.paste(output, ctx, &mut item_namespace)?;
            }
        }

        Ok(())
    }
}

impl FragmentLet {
    pub fn paste(
        &self,
        _output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> syn::Result<()> {
        let val_expr = self.expr.eval(ctx, namespace)?;

        namespace.flush();
        self.pat.queue_insert(val_expr, namespace, ctx)?;
        namespace.flush();

        Ok(())
    }
}
