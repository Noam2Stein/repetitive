use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone)]
pub struct Fragment {
    pub at_token: Token![@],
    pub kind: FragmentKind,
}

#[derive(Debug, Clone)]
pub enum FragmentKind {
    For(FragmentFor),
    Let(FragmentLet),
    If(FragmentIf),
    Match(FragmentMatch),
    Concat(FragmentConcat),
    Tokens(FragmentTokens),
    Expr(FragmentExpr),
    At(Token![@]),
}

impl ContextParse for Fragment {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let at_token = <Token![@]>::ctx_parse(input, ctx)?;
        let kind = FragmentKind::ctx_parse(input, ctx)?;

        Ok(Self { at_token, kind })
    }
}
impl ContextParse for FragmentKind {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        Ok(
            if let Some(frag) = FragmentFor::ctx_parse_option(input, ctx)? {
                Self::For(frag)
            } else if let Some(frag) = FragmentLet::ctx_parse_option(input, ctx)? {
                Self::Let(frag)
            } else if let Some(frag) = FragmentIf::ctx_parse_option(input, ctx)? {
                Self::If(frag)
            } else if let Some(frag) = FragmentMatch::ctx_parse_option(input, ctx)? {
                Self::Match(frag)
            } else if let Some(frag) = FragmentConcat::ctx_parse_option(input, ctx)? {
                Self::Concat(frag)
            } else if let Some(frag) = FragmentTokens::ctx_parse_option(input, ctx)? {
                Self::Tokens(frag)
            } else if let Some(frag) = FragmentExpr::ctx_parse_option(input, ctx)? {
                Self::Expr(frag)
            } else if let Some(at_token) = Option::<Token![@]>::ctx_parse(input, ctx)? {
                Self::At(at_token)
            } else {
                return Err(Error::ParseError(syn::Error::new(
                    input.span(),
                    "expected a fragment",
                )));
            },
        )
    }
}

impl Peek for Fragment {
    fn peek(input: ParseStream) -> bool {
        input.peek(Token![@])
    }
}

impl Optimize for Fragment {
    fn optimize(&mut self, ctx: &mut Context) {
        match &mut self.kind {
            FragmentKind::For(frag) => frag.optimize(ctx),
            FragmentKind::Let(frag) => frag.optimize(ctx),
            FragmentKind::If(frag) => frag.optimize(ctx),
            FragmentKind::Match(frag) => frag.optimize(ctx),
            FragmentKind::Concat(frag) => frag.optimize(ctx),
            FragmentKind::Tokens(frag) => frag.optimize(ctx),
            FragmentKind::Expr(frag) => frag.optimize(ctx),
            FragmentKind::At(_) => {}
        }
    }
}

impl ExpandInto for Fragment {
    fn expand_into(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &mut Namespace) {
        match &self.kind {
            FragmentKind::For(frag) => frag.expand(output, ctx, namespace),
            FragmentKind::Let(frag) => frag.expand_into(output, ctx, namespace),
            FragmentKind::If(frag) => frag.expand(output, ctx, namespace),
            FragmentKind::Match(frag) => frag.expand(output, ctx, namespace),
            FragmentKind::Concat(frag) => frag.expand(output, ctx, namespace),
            FragmentKind::Tokens(frag) => frag.expand(output, ctx, namespace),
            FragmentKind::Expr(frag) => frag.expand(output, ctx, namespace),
            FragmentKind::At(at) => at.to_tokens(output),
        }
    }
}
