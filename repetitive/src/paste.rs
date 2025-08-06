use proc_macro2::TokenStream;
use quote::ToTokens;

use super::*;

pub trait Paste {
    fn paste(
        &self,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> syn::Result<()>;
}

impl Paste for TokenStream {
    fn paste(
        &self,
        output: &mut TokenStream,
        _ctx: &mut Context,
        _namespace: &mut Namespace,
    ) -> syn::Result<()> {
        self.to_tokens(output);

        Ok(())
    }
}
