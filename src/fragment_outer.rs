use proc_macro2::{Delimiter, Group, Span, TokenStream};
use syn::{
    Ident, Token,
    parse::ParseStream,
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
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let at_token = <Token![@]>::ctx_parse(input, ctx)?;
        let kind = FragmentOuterKind::ctx_parse(input, ctx)?;

        Ok(Self {
            at_span: at_token.span,
            kind,
        })
    }
}
impl ContextParse for FragmentOuterKind {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
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
            let group = Group::ctx_parse(input, ctx)?;

            let expr = FragmentExpr::ctx_parse.ctx_parse2(group.stream(), ctx)?;

            return Ok(Self::Expr(expr));
        }

        if input.peek(Brace) {
            let group = Group::ctx_parse(input, ctx)?;

            let tokens = Tokens::ctx_parse.ctx_parse2(group.stream(), ctx)?;

            return Ok(Self::Expr(FragmentExpr {
                span: group.span(),
                kind: FragmentExprKind::Value(FragmentValueKind::Tokens(tokens)),
            }));
        }

        Err(Error::ParseError(syn::Error::new(
            input.span(),
            "expected a fragment",
        )))
    }
}

impl ContextParse for FragmentFor {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let for_span = <Token![for]>::ctx_parse(input, ctx)?.span;

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
            for_span,
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

        let iter = FragmentExpr::ctx_parse(input, ctx)?;

        Ok(Self { pat, iter })
    }
}

impl ContextParse for FragmentLet {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let let_span = <Token![let]>::ctx_parse(input, ctx)?.span;

        let pat = Pattern::ctx_parse(input, ctx)?;

        <Token![=]>::ctx_parse(input, ctx)?;

        let expr = FragmentExpr::ctx_parse(input, ctx)?;

        <Token![;]>::ctx_parse(input, ctx)?;

        return Ok(Self {
            let_span,
            pat,
            expr,
        });
    }
}

impl FragmentOuterKind {
    fn ctx_parse_if(input: ParseStream, ctx: &mut Context) -> Result<Self, Error> {
        let if_span = <Token![if]>::ctx_parse(input, ctx)?.span;
        let cond = FragmentExpr::ctx_parse(input, ctx)?;

        let then_group = Group::ctx_parse(input, ctx)?;
        if then_group.delimiter() != Delimiter::Brace {
            return Err(Error::ParseError(syn::Error::new(
                then_group.span(),
                "expected a brace-delimited block",
            )));
        }

        let then = FragmentExpr {
            span: if_span,
            kind: FragmentExprKind::Value(FragmentValueKind::Tokens(
                Tokens::ctx_parse.ctx_parse2(then_group.stream(), ctx)?,
            )),
        };

        let else_ = if input.peek(Token![else]) {
            <Token![else]>::ctx_parse(input, ctx)?;

            if input.peek(Token![if]) {
                let else_if = FragmentOuterKind::ctx_parse(input, ctx)?;

                match else_if {
                    FragmentOuterKind::Expr(expr) => expr,
                    _ => unreachable!("else if (already peeked) must be an expression"),
                }
            } else {
                let else_group = Group::ctx_parse(input, ctx)?;
                if else_group.delimiter() != Delimiter::Brace {
                    return Err(Error::ParseError(syn::Error::new(
                        else_group.span(),
                        "expected a brace-delimited block",
                    )));
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

    fn ctx_parse_match(input: ParseStream, ctx: &mut Context) -> Result<Self, Error> {
        let match_span = <Token![match]>::ctx_parse(input, ctx)?.span;

        let expr = Box::new(FragmentExpr::ctx_parse(input, ctx)?);

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

                    Some(FragmentExpr::ctx_parse(input, ctx)?)
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

                arms.push(FragmentMatchArm {
                    pat,
                    condition,
                    body: FragmentExpr {
                        span: group.span(),
                        kind: FragmentExprKind::Value(FragmentValueKind::Tokens(body)),
                    },
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

        Ok(Self::Expr(FragmentExpr {
            span: match_span,
            kind: FragmentExprKind::Match(FragmentMatch {
                match_span,
                expr,
                match_arms,
            }),
        }))
    }

    fn parse_concat(input: ParseStream, ctx: &mut Context) -> Result<Self, Error> {
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

                let item = FragmentExpr::ctx_parse_single(input, ctx)?;
                parts.push(item);
            }

            Ok::<_, Error>(parts)
        };

        let parts = parse_fn.ctx_parse2(group.stream(), ctx)?;

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
    fn paste(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &mut Namespace) {
        match &self.kind {
            FragmentOuterKind::Expr(frag) => frag.paste(output, ctx, namespace),
            FragmentOuterKind::For(frag) => frag.paste(output, ctx, namespace),
            FragmentOuterKind::Let(frag) => frag.paste(output, ctx, namespace),
        }
    }
}

impl Paste for FragmentFor {
    fn paste(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &mut Namespace) {
        self.paste_from_iter_items(0, output, ctx, namespace);
    }
}
impl FragmentFor {
    fn paste_from_iter_items(
        &self,
        iter_idx: usize,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) {
        let iter = &self.iters[iter_idx];
        let iter_items = iter.iter.eval(ctx, namespace);

        let iter_items = match iter_items.kind {
            FragmentValueKind::Unknown(_) => return,

            FragmentValueKind::List(val) => val,
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
                self.paste_from_iter_items(iter_idx + 1, output, ctx, &mut item_namespace);
            } else {
                self.body.paste(output, ctx, &mut item_namespace);
            }
        }
    }
}

impl Paste for FragmentLet {
    fn paste(&self, _output: &mut TokenStream, ctx: &mut Context, namespace: &mut Namespace) {
        let val_expr = self.expr.eval(ctx, namespace);

        namespace.flush();
        self.pat.queue_insert(val_expr, namespace, ctx);
        namespace.flush();
    }
}
