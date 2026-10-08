//! A module that exposes functionality for emitting errors and defines all of
//! the macro's error messages.
//!
//! This module's submodules contain free functions that emit context-specific
//! errors. Such functions take a reference to [`Diagnostics`] which stores
//! appropriate state, and return the zero-sized-type [`EmittedError`] as a
//! "proof" that an error has been emitted.
//!
//! All error messages are defined inside this module. Functionality for
//! emitting errors with arbitrary messages is intentionally not exposed
//! publicly. Keeping all errors in one place makes them easier to maintain.

pub use self::storage::{Diagnostics, EmittedError};

pub mod execute_errors;
pub mod parse_errors;

mod storage;
