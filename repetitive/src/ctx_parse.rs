use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream, Parser};

use super::*;

pub trait ContextParse {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized;
}

pub trait ContextParseExt {
    type Output;

    fn ctx_parse2(self, input: TokenStream, ctx: &mut Context) -> syn::Result<Self::Output>;
}

impl<T: Parse> ContextParse for T {
    fn ctx_parse(input: ParseStream, _ctx: &mut Context) -> syn::Result<Self> {
        T::parse(input)
    }
}

impl<T, F: FnOnce(ParseStream, &mut Context) -> syn::Result<T>> ContextParseExt for F {
    type Output = T;

    fn ctx_parse2(self, input: TokenStream, ctx: &mut Context) -> syn::Result<Self::Output> {
        (|input: ParseStream| {
            let output = self(input, ctx);

            let output = match output {
                Ok(output) => output,
                Err(e) => {
                    return Err(e);
                }
            };

            if !input.is_empty() {
                return Err(input.error("unexpected token"));
            }

            Ok(output)
        })
        .parse2(input)
    }
}
