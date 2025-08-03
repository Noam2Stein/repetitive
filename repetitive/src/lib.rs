mod ctx;
mod fragment;
mod ident_str;
mod name;
mod tokens;
use ctx::*;
use fragment::*;
use ident_str::*;
use name::*;
use tokens::*;

#[proc_macro]
pub fn repetitive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    main::repetitive(input)
}

mod main {
    use proc_macro2::TokenStream;
    use quote::quote;

    use super::*;

    pub fn repetitive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
        let mut errors = vec![];

        let tokens = ctx_parse2(
            Tokens::ctx_parse,
            input.into(),
            &mut Context {
                interner: &mut Default::default(),
                errors: &mut errors,
                namespace: &mut Namespace::new(),
            },
        );

        let tokens_output = match tokens {
            Ok(tokens) => {
                let mut output = TokenStream::new();
                tokens.end(&mut output);
                output
            }
            Err(err) => err.to_compile_error(),
        };

        let errors_output = errors.iter().map(|err| err.to_compile_error());

        quote! {
            #tokens_output
            #(#errors_output)*
        }
        .into()
    }
}
