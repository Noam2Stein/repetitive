use std::borrow::Cow;

use crate::proc_macro::Span;

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
