use std::fmt::{Display, Formatter};

use crate::proc_macro12::Span;

pub trait Error {
    fn span(&self) -> Span;

    fn message(self) -> impl Display;
}

macro_rules! error {
    ($span:expr, $($args:tt)*) => {
        crate::diagnostics::error_trait::FromFn {
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

    fn message(self) -> impl Display {
        std::fmt::from_fn(self.write_message)
    }
}
