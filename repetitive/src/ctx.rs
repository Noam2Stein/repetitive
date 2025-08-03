use proc_macro2::TokenStream;
use string_interner::DefaultStringInterner;
use syn::parse::{Parse, ParseStream, Parser};

use super::*;

#[derive(Debug)]
pub struct Context<'a> {
    pub interner: &'a mut DefaultStringInterner,
    pub errors: &'a mut Vec<syn::Error>,
    pub namespace: &'a mut Namespace<'a>,
}

pub trait ContextParse {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized;
}

pub fn ctx_parse2<T>(
    f: impl FnOnce(ParseStream, &mut Context) -> syn::Result<T>,
    input: TokenStream,
    ctx: &mut Context,
) -> syn::Result<T> {
    (|input: ParseStream| f(input, ctx)).parse2(input)
}

impl<T: Parse> ContextParse for T {
    fn ctx_parse(input: ParseStream, _ctx: &mut Context) -> syn::Result<Self> {
        T::parse(input)
    }
}
