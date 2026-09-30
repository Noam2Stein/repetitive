//! Functionality for emitting errors, and later warnings when it becomes possible.

use std::{borrow::Cow, cell::Cell};

use crate::{
    error::Error,
    proc_macro12::{
        Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
    },
};

pub struct Diagnostics {
    errors: Cell<Vec<Diagnostic>>,
}

struct Diagnostic {
    span: Span,
    message: Cow<'static, str>,
}

pub struct RecordedError(());

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            errors: Cell::new(Vec::new()),
        }
    }

    pub fn record_error(&self, error: Error) -> RecordedError {
        let mut errors = self.errors.take();
        errors.push(Diagnostic {
            span: error.span(),
            message: error.message(),
        });
        self.errors.set(errors);

        RecordedError(())
    }

    #[cfg(test)]
    pub fn errors(&mut self) -> impl Iterator<Item = &str> {
        self.errors
            .get_mut()
            .iter()
            .map(|diagnostic| diagnostic.message.as_ref())
    }

    pub fn emit_onto(self, output: TokenStream) -> TokenStream {
        let Self { errors } = self;
        let errors = errors.into_inner();

        if errors.is_empty() {
            output
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
}
