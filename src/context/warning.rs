use proc_macro2::{Span, TokenStream};

#[expect(unused_imports)]
use super::*;

#[derive(Debug, Clone)]
pub enum Warning {
    UnnecessaryPunct { span: Span, punct: &'static str },

    UnusedMatchArm(Span),
}

impl Warning {
    pub fn into_syn_warning(self) -> syn::Error {
        match self {
            Warning::UnnecessaryPunct { span, punct } => {
                syn::Error::new(span, format!("unnecessary `{punct}`"))
            }

            Warning::UnusedMatchArm(span) => syn::Error::new(span, "unused match arm"),
        }
    }

    pub fn into_compile_error(self) -> TokenStream {
        self.into_syn_warning().into_compile_error()
    }
}
