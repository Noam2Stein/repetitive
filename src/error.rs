use std::borrow::Cow;

use proc_macro2::Span;

pub struct Error {
    pub span: Span,
    pub message: Cow<'static, str>,
}

impl Error {
    pub fn new(span: Span, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}
