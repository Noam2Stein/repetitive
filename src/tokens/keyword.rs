use std::fmt::Display;

use proc_macro2::Span;
use syn::{
    Ident,
    parse::{Parse, ParseStream},
};

use super::*;

pub enum Keyword {
    Str(Span),
    Include(Span),
}

impl Parse for Keyword {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;

        match ident.to_string().as_str() {
            "str" => Ok(Keyword::Str(ident.span())),
            "include" => Ok(Keyword::Include(ident.span())),

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
            "include" => true,
            _ => false,
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Str(_) => write!(f, "str"),
            Keyword::Include(_) => write!(f, "include"),
        }
    }
}
