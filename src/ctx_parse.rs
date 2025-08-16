use proc_macro2::{Span, TokenStream};
use syn::parse::{Parse, ParseStream, Parser};

use super::*;

pub trait ContextParse {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized;
}

pub trait ContextParseExt {
    type Output;

    fn ctx_parse2(self, input: TokenStream, ctx: &mut Context) -> Result<Self::Output, Error>;
}

impl<T: Parse> ContextParse for T {
    fn ctx_parse(input: ParseStream, _ctx: &mut Context) -> Result<Self, Error> {
        T::parse(input).map_err(Error::ParseError)
    }
}

impl<T, F: FnOnce(ParseStream, &mut Context) -> Result<T, Error>> ContextParseExt for F {
    type Output = T;

    fn ctx_parse2(self, input: TokenStream, ctx: &mut Context) -> Result<Self::Output, Error> {
        let mut error = None;

        (|input: ParseStream| {
            let output = self(input, ctx);

            let output = match output {
                Ok(output) => output,
                Err(e) => {
                    error = Some(e);
                    return Err(syn::Error::new(Span::call_site(), "dummy error"));
                }
            };

            if !input.is_empty() {
                error = Some(Error::ParseError(input.error("unexpected token")));
                return Err(syn::Error::new(Span::call_site(), "dummy error"));
            }

            Ok(output)
        })
        .parse2(input)
        .map_err(|_| error.unwrap())
    }
}
