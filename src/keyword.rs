use std::fmt::Display;

use syn::{
    Ident,
    parse::{Parse, ParseStream},
};

pub enum Keyword {
    Str,
}

impl Parse for Keyword {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;

        match ident.to_string().as_str() {
            "str" => Ok(Keyword::Str),

            _ => Err(syn::Error::new(ident.span(), "expected keyword")),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Str => write!(f, "str"),
        }
    }
}

impl Keyword {
    pub fn peek(input: ParseStream) -> bool {
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
