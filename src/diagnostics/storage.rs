use std::{
    cell::UnsafeCell,
    fmt::{Arguments, Write},
};

use crate::proc_macro12::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
};

pub struct Diagnostics(UnsafeCell<Inner>);

pub struct EmittedError(());

struct Inner {
    token_stream: TokenStream,
    message_buffer: String,
    #[cfg(test)]
    errors: Vec<String>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self(UnsafeCell::new(Inner {
            token_stream: TokenStream::new(),
            message_buffer: String::new(),
            #[cfg(test)]
            errors: Vec::new(),
        }))
    }

    pub fn into_token_stream(self) -> TokenStream {
        self.0.into_inner().token_stream
    }

    #[cfg(test)]
    pub fn errors(&mut self) -> impl Iterator<Item = &str> {
        self.0.get_mut().errors.iter().map(String::as_str)
    }

    pub(in crate::diagnostics) fn emit_error(
        &self,
        span: Span,
        message: Arguments,
    ) -> EmittedError {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        inner.message_buffer.clear();
        write!(&mut inner.message_buffer, "{message}").expect("failed to format error message");
        let message = &inner.message_buffer;

        inner.token_stream.extend([
            TokenTree::Ident(Ident::new("compile_error", span)),
            TokenTree::Punct(Punct::new('!', Spacing::Alone)),
            TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenTree::Literal(Literal::string(message)).into(),
            )),
        ]);

        #[cfg(test)]
        inner.errors.push(message.clone());

        EmittedError(())
    }
}
