//! A module defining the [`Diagnostics`] data structure.

use std::{cell::UnsafeCell, fmt::Write};

use crate::{
    diagnostics::error_format::Error,
    proc_macro12::{Delimiter, Group, Ident, Literal, Punct, Spacing, TokenStream, TokenTree},
};

/// A data structure storing appropriate information about emitted errors.
///
/// See [`crate::diagnostics`] for an overview of how diagnostics are handled.
///
/// When `cfg(test)` is active, this retains error-message strings and makes
/// them accessible via `Self::errors`.
///
/// Regardless of `cfg(test)`, errors are stored as a token-stream containing
/// calls to [`compile_error`]. The token-stream is accessible via
/// [`Self::into_compile_errors`].
pub struct Diagnostics(UnsafeCell<Inner>);

/// A zero-sized error type indicating an error has been emitted to
/// [`Diagnostics`].
///
/// This type cannot be constructed directly; it is returned from
/// [`Diagnostics::emit_error`].
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

    /// Converts diagnostics into a token-stream containing calls to
    /// [`compile_error`].
    pub fn into_compile_errors(self) -> TokenStream {
        self.0.into_inner().token_stream
    }

    /// Returns all emitted error messages.
    ///
    /// This only works when `cfg(test)` is active. Otherwise, error messages
    /// are not stored directly.
    #[cfg(test)]
    pub fn errors(&mut self) -> impl Iterator<Item = &str> {
        self.0.get_mut().errors.iter().map(String::as_str)
    }

    /// Emits the given error.
    ///
    /// This takes an error defined in the [`crate::diagnostics`] module.
    /// Emitting errors with arbitrary messages it intentionally not supported
    /// in order to force all errors to be defined in this module.
    ///
    /// Currently, this only emits the error to the given [`Diagnostics`]
    /// instance. However, it may affect global state in the future, if the
    /// nightly `proc_macro_diagnostic` API is stabilized.
    ///
    /// See [`crate::diagnostics`] for context.
    pub fn emit_error(&self, error: impl Error) -> EmittedError {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let span = error.span();

        inner.message_buffer.clear();
        write!(
            &mut inner.message_buffer,
            "{}",
            std::fmt::from_fn(|f| error.write_message(f))
        )
        .expect("failed to format error message");
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
