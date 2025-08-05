mod ctx;
mod ctx_parse;
mod fragment_expr;
mod fragment_outer;
mod fragment_value;
mod keyword;
mod name;
mod op;
mod paste;
mod pattern;
mod tokens;
use ctx::*;
use ctx_parse::*;
use fragment_expr::*;
use fragment_outer::*;
use fragment_value::*;
use keyword::*;
use name::*;
use op::*;
use paste::*;
use pattern::*;
use tokens::*;

#[proc_macro]
pub fn repetitive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    main::repetitive(input)
}

mod main {
    use proc_macro2::TokenStream;
    use quote::quote;
    use string_interner::DefaultStringInterner;

    use super::*;

    pub fn repetitive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
        let mut ctx = Context {
            interner: DefaultStringInterner::new(),
            errors: vec![],
        };

        let tokens = Tokens::ctx_parse.ctx_parse2(input.into(), &mut ctx);
        let result = 'result: {
            Ok(match tokens {
                Ok(tokens) => {
                    if !ctx.errors.is_empty() {
                        break 'result Err(ctx.errors);
                    }

                    let mut output = TokenStream::new();

                    match tokens.paste(&mut output, &mut ctx, &mut Namespace::new()) {
                        Ok(()) => output,
                        Err(err) => break 'result Err(vec![err]),
                    }
                }
                Err(err) => break 'result Err(ctx.errors.into_iter().chain([err]).collect()),
            })
        };

        match result {
            Ok(output) => output,
            Err(errors) => {
                let errors_output = errors.iter().map(|err| err.to_compile_error());
                quote! {
                    #(#errors_output)*
                }
            }
        }
        .into()
    }
}
