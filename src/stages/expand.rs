use proc_macro2::TokenStream;
use quote::ToTokens;

use super::*;

pub trait ExpandInto {
    fn expand_into(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &mut Namespace);
}

pub trait Expand {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace);
}

impl Expand for TokenStream {
    fn expand(&self, output: &mut TokenStream, _ctx: &mut Context, _namespace: &Namespace) {
        self.to_tokens(output);
    }
}
