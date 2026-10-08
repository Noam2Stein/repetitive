//! A module defining all errors that occur at execution time.
//!
//! See [`crate::diagnostics`] for context.

use crate::{
    diagnostics::error_format::{Error, error},
    proc_macro12::Span,
};

pub fn int_add_overflow(span: Span) -> impl Error {
    error!(span, "attempt to add with overflow")
}

pub fn int_div_overflow(span: Span) -> impl Error {
    error!(span, "attempt to divide with overflow")
}

pub fn int_div_zero(span: Span) -> impl Error {
    error!(span, "attempt to divide by zero")
}

pub fn int_mul_overflow(span: Span) -> impl Error {
    error!(span, "attempt to multiply with overflow")
}

pub fn int_neg_overflow(span: Span) -> impl Error {
    error!(span, "attempt to negate with overflow")
}

pub fn int_rem_overflow(span: Span) -> impl Error {
    error!(span, "attempt to calculate the remainder with overflow")
}

pub fn int_rem_zero(span: Span) -> impl Error {
    error!(
        span,
        "attempt to calculate the remainder with a divisor of zero"
    )
}

pub fn int_sub_overflow(span: Span) -> impl Error {
    error!(span, "attempt to subtract with overflow")
}

pub fn str_emit_invalid(span: Span, val: &str) -> impl Error {
    error!(span, "attempt to emit invalid identifier `{val}`")
}
