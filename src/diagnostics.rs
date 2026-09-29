//! Functionality for emitting errors, and later warnings when it becomes possible.

use std::{borrow::Cow, cell::Cell};

use crate::proc_macro12::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
};

pub struct Diagnostics {
    errors: Cell<Vec<Diagnostic>>,
}

pub struct Diagnostic {
    pub span: Span,
    pub message: Cow<'static, str>,
}

pub struct RecordedError(());

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            errors: Cell::new(Vec::new()),
        }
    }

    pub fn record_error(&self, error: Diagnostic) -> RecordedError {
        let mut errors = self.errors.take();
        errors.push(error);
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

impl Diagnostic {
    pub fn new(span: Span, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use proc_macro2::Span;

    use crate::diagnostics::{Diagnostic, Diagnostics};

    #[test]
    fn test_success() {
        let mut diagnostics = Diagnostics::new();

        assert!(diagnostics.errors().collect_vec().is_empty());
    }

    #[test]
    fn test_errors() {
        let mut diagnostics = Diagnostics::new();
        diagnostics.record_error(Diagnostic::new(Span::call_site(), "insert error message"));

        assert_eq!(
            diagnostics.errors().collect_vec(),
            vec!["insert error message"]
        );
    }
}
