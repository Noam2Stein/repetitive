use proc_macro2::Span;
use syn::{
    Ident,
    parse::{Parse, ParseStream},
};

#[derive(Debug, Clone)]
pub struct IdentStr {
    pub str: String,
    pub span: Span,
}

impl Parse for IdentStr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        Ok(Self {
            str: ident.to_string(),
            span: ident.span(),
        })
    }
}
