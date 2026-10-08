//! A module defining the format used to represent all of the macro's errors.
//!
//! Currently, errors simply have a span and a message. The [`Error`] trait has
//! methods for getting the span and writing the message to a [`Formatter`]. The
//! [`error!`] macro is used to construct values of `impl Error`.
//!
//! A trait is used instead of a concrete type so that error messages can be
//! formatted into a reusable buffer, instead of a newly allocated [`String`]
//! each time.

use std::fmt::Formatter;

use crate::proc_macro12::Span;

/// A trait for types representing an error.
///
/// Such types usually contain a span and whatever context is needed to write
/// the error message.
pub trait Error {
    fn span(&self) -> Span;

    fn write_message(&self, f: &mut Formatter) -> std::fmt::Result;
}

/// Constructs an anonymous type implementing [`Error`].
///
/// Syntax is `error!(span, "error message", <formatting arguments>)`.
///
/// The returned type stores a span and all values used in formatting arguments.
macro_rules! error {
    ($span:expr, $($args:tt)*) => {
        crate::diagnostics::error_format::FromFn {
            span: $span,
            write_message: move |f| write!(f, $($args)*),
        }
    };
}
pub(crate) use error;

#[doc(hidden)]
pub(in crate::diagnostics) struct FromFn<F>
where
    F: Fn(&mut Formatter) -> std::fmt::Result,
{
    pub span: Span,
    pub write_message: F,
}

impl<F> Error for FromFn<F>
where
    F: Fn(&mut Formatter) -> std::fmt::Result,
{
    #[inline]
    fn span(&self) -> Span {
        self.span
    }

    fn write_message(&self, f: &mut Formatter) -> std::fmt::Result {
        (self.write_message)(f)
    }
}
