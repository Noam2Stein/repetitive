use proc_macro2::{Delimiter, Group, Span, TokenStream};
use syn::{
    Ident, Token,
    parse::{ParseStream, Parser},
    token::Bracket,
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
    pub pat: Pattern,
    pub iter: FragmentExpr,
    pub body: Tokens,
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
            let for_span = input.parse::<Token![for]>()?.span;

            let pat = Pattern::ctx_parse(input, ctx)?;

            input.parse::<Token![in]>()?;

            let iter = FragmentExpr::ctx_parse(input, ctx)?;

            let body_group = input.parse::<Group>()?;
            if body_group.delimiter() != Delimiter::Brace {
                return Err(syn::Error::new(
                    body_group.span(),
                    "expected a brace-delimited block",
                ));
            }

            let body = Tokens::ctx_parse.ctx_parse2(body_group.stream(), ctx)?;

            return Ok(Self::For(FragmentFor {
                for_span,
                pat,
                iter,
                body,
            }));
        }

        if input.peek(Token![let]) {
            let let_span = input.parse::<Token![let]>()?.span;

            let pat = Pattern::ctx_parse(input, ctx)?;

            input.parse::<Token![=]>()?;

            let expr = FragmentExpr::ctx_parse(input, ctx)?;

            input.parse::<Token![;]>()?;

            return Ok(Self::Let(FragmentLet {
                let_span,
                pat,
                expr,
            }));
        }

        if input.peek(Token![if]) {
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
                kind: FragmentExprKind::Value(FragmentValue::Tokens(
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
                        kind: FragmentExprKind::Value(FragmentValue::Tokens(
                            Tokens::ctx_parse.ctx_parse2(else_group.stream(), ctx)?,
                        )),
                    }
                }
            } else {
                FragmentExpr {
                    span: if_span,
                    kind: FragmentExprKind::Value(FragmentValue::Tokens(Tokens::default())),
                }
            };

            return Ok(Self::Expr(FragmentExpr::op(
                Op::IfElse(if_span),
                vec![cond, then, else_],
                ctx,
            )?));
        }

        if Keyword::peek(input) {
            let keyword_span = input.span();
            let keyword = input.parse::<Keyword>()?;

            if input.peek(Bracket) {
                let group = input.parse::<Group>()?;

                let is_to_str = match keyword {
                    Keyword::Str => true,
                };

                return Ok(Self::parse_concat(
                    group.span(),
                    group.stream(),
                    is_to_str,
                    ctx,
                )?);
            }

            return Err(syn::Error::new(keyword_span, "unexpected keyword"));
        }

        if input.peek(Ident) {
            let name = Name::ctx_parse(input, ctx)?;

            return Ok(Self::Expr(FragmentExpr::name(name)));
        }

        if let Some(lit) = FragmentValueExpr::option_lit(input) {
            return Ok(Self::Expr(lit.into_expr()));
        }

        if let Some(group) = input.parse::<Option<Group>>()? {
            match group.delimiter() {
                Delimiter::Parenthesis => {
                    let fragment_expr = FragmentExpr::ctx_parse.ctx_parse2(group.stream(), ctx)?;

                    return Ok(Self::Expr(fragment_expr));
                }

                Delimiter::Brace => {
                    return Ok(Self::Expr(FragmentExpr {
                        span: group.span(),
                        kind: FragmentExprKind::Value(FragmentValue::Tokens(
                            Tokens::ctx_parse.ctx_parse2(group.stream(), ctx)?,
                        )),
                    }));
                }

                Delimiter::Bracket => {
                    return Ok(Self::parse_concat(
                        group.span(),
                        group.stream(),
                        false,
                        ctx,
                    )?);
                }

                Delimiter::None => {
                    return Self::ctx_parse.ctx_parse2(group.stream(), ctx);
                }
            }
        }

        Err(syn::Error::new(input.span(), "expected a fragment"))
    }
}
impl FragmentOuterKind {
    fn parse_concat(
        group_span: Span,
        group_stream: TokenStream,
        is_to_str: bool,
        ctx: &mut Context,
    ) -> syn::Result<Self> {
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

        let parts = parse_fn.parse2(group_stream)?;

        let op = if is_to_str {
            Op::ConcatString(group_span)
        } else {
            Op::ConcatIdent(group_span)
        };
        let op_args = vec![FragmentExpr {
            span: group_span,
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
        let iter_val = self.iter.eval(ctx, namespace)?;
        let items = match iter_val.value {
            FragmentValue::List(val) => val,
            _ => return Err(syn::Error::new(self.iter.span, "expected a list")),
        };

        for item in items {
            let item_expr = FragmentValueExpr {
                span: self.for_span,
                value: item,
            };

            let mut item_namespace = namespace.fork();
            item_namespace.flush();
            self.pat.queue_insert(item_expr, &mut item_namespace, ctx)?;
            item_namespace.flush();

            self.body.paste(output, ctx, &mut item_namespace)?;
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
