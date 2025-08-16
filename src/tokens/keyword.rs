use std::fmt::Display;

use proc_macro2::Span;
use syn::{
    Ident,
    parse::{Parse, ParseStream},
};

use super::*;

pub enum Keyword {
    Str(Span),
}

impl Parse for Keyword {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;

        match ident.to_string().as_str() {
            "str" => Ok(Keyword::Str(ident.span())),

            _ => Err(syn::Error::new(ident.span(), "expected keyword")),
        }
    }
}

impl Peek for Keyword {
    fn peek(input: ParseStream) -> bool {
        let ident = match input.fork().parse::<Option<Ident>>().unwrap() {
            Some(ident) => ident,
            None => return false,
        };

        match ident.to_string().as_str() {
            "str" => true,
            _ => false,
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Str(_) => write!(f, "str"),
        }
    }
}
