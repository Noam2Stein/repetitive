//! Functionality for emitting errors, and later warnings when it becomes possible.

use std::borrow::Cow;

use crate::proc_macro12::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
};

pub struct Error {
    pub span: Span,
    pub message: Cow<'static, str>,
}

pub fn emit_diagnostics(stream: TokenStream, errors: Vec<Error>) -> TokenStream {
    if errors.is_empty() {
        stream
    } else {
        errors
            .into_iter()
            .flat_map(|error| {
                [
                    TokenTree::Ident(Ident::new("compile_error", error.span)),
                    TokenTree::Punct(Punct::new('!', Spacing::Alone)),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenTree::Literal(Literal::string(&error.message)).into(),
                    )),
                ]
            })
            .collect()
    }
}

impl Error {
    pub fn new(span: Span, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}
