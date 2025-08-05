use proc_macro2::TokenStream;

use super::*;

pub trait Paste {
    fn paste(
        &self,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> syn::Result<()>;
}
