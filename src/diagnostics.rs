//! A module that exposes functionality for emitting errors and defines all of
//! the macro's error messages.
//!
//! All of the macro's error messages are defined in this module as free
//! functions that return an anonymous `impl Error` type. These functions take
//! whatever context is needed to construct an error's span and message. Keeping
//! all errors in this module makes them easier to maintain.
//!
//! The [`Diagnostics`] type stores appropriate state about emitted errors. The
//! method [`Diagnostics::emit_error`] takes an `impl Error`, emits the error,
//! and returns the zero-sized type [`EmittedError`] as a "proof" that an error
//! has been emitted. [`EmittedError`] is meant to be used with [`Result`].

pub use self::data_structure::*;

pub mod execution_errors;
pub mod syntax_errors;

mod data_structure;
mod error_format;
