//! A module defining all errors that occur at execution time.
//!
//! See [`crate::diagnostics`] for context.

use crate::{
    diagnostics::{Diagnostics, EmittedError},
    proc_macro12::Span,
};

pub fn int_add_overflow(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(span, format_args!("attempt to add with overflow"))
}

pub fn int_div_overflow(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(span, format_args!("attempt to divide with overflow"))
}

pub fn int_div_zero(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(span, format_args!("attempt to divide by zero"))
}

pub fn int_mul_overflow(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(span, format_args!("attempt to multiply with overflow"))
}

pub fn int_neg_overflow(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(span, format_args!("attempt to negate with overflow"))
}

pub fn int_rem_overflow(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("attempt to calculate the remainder with overflow"),
    )
}

pub fn int_rem_zero(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("attempt to calculate the remainder with a divisor of zero"),
    )
}

pub fn int_sub_overflow(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(span, format_args!("attempt to subtract with overflow"))
}

pub fn str_emit_invalid(diagnostics: &Diagnostics, span: Span, val: &str) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("attempt to emit invalid identifier `{val}`"),
    )
}
