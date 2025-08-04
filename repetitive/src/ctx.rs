use std::ops::Deref;

use proc_macro2::TokenStream;
use string_interner::DefaultStringInterner;
use syn::parse::{Parse, ParseStream, Parser};

use super::*;

#[derive(Debug)]
pub struct Context<'a> {
    pub errors: &'a mut Vec<syn::Error>,
    pub namespace: &'a mut Namespace<'a>,
}

#[derive(Debug)]
pub struct ParseContext<'a> {
    pub interner: &'a mut DefaultStringInterner,
    pub base: Context<'a>,
}

pub trait ContextParse {
    fn ctx_parse(input: ParseStream, ctx: &mut ParseContext) -> syn::Result<Self>
    where
        Self: Sized;
}

pub fn ctx_parse2<T>(
    f: impl FnOnce(ParseStream, &mut ParseContext) -> syn::Result<T>,
    input: TokenStream,
    ctx: &mut ParseContext,
) -> syn::Result<T> {
    (|input: ParseStream| f(input, ctx)).parse2(input)
}

impl<'a> Deref for ParseContext<'a> {
    type Target = Context<'a>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<T: Parse> ContextParse for T {
    fn ctx_parse(input: ParseStream, _ctx: &mut ParseContext) -> syn::Result<Self> {
        T::parse(input)
    }
}
