use proc_macro2::{Span, TokenStream};

use super::*;

#[derive(Debug, Clone)]
pub enum Error {
    ParseError(syn::Error),

    IsKeyword {
        span: Span,
        keyword: String,
    },

    ExpectedFound {
        span: Span,
        expected: &'static str,
        found: &'static str,
    },

    ExpectedRhsFound {
        span: Span,
        lhs: &'static str,
        op: &'static str,
        expected: &'static str,
        found: &'static str,
    },

    CannotPerform {
        span: Span,
        op: &'static str,
        kind: &'static str,
    },

    ArgCount {
        op: &'static str,
        span: Span,
        expected: usize,
        inputs_desc: Option<&'static str>,
        found: usize,
    },

    PatternKindMismatch {
        span: Span,
        expected: &'static str,
        found: &'static str,
    },
    PatternValueMismatch {
        span: Span,
    },
    PatternListLengthMismatch {
        span: Span,
        expected: usize,
        found: usize,
    },

    NoMatches {
        span: Span,
        value: String,
    },

    NameAlreadyExists(Name),
    NameNotFound(Name),
}

impl Error {
    pub fn into_syn_error(self, ctx: &mut Context) -> syn::Error {
        match self {
            Error::ParseError(error) => error,

            Error::IsKeyword { span, keyword } => {
                syn::Error::new(span, format!("expected name, `{keyword}` is a keyword"))
            }

            Error::ExpectedFound {
                span,
                expected,
                found,
            } => syn::Error::new(span, format!("expected `{expected}`, found `{found}`")),

            Error::ExpectedRhsFound {
                span,
                lhs,
                op,
                expected,
                found,
            } => syn::Error::new(
                span,
                format!("expected rhs of `{lhs}` {op} to be `{expected}`, found `{found}`"),
            ),

            Error::ArgCount {
                op,
                span,
                expected,
                inputs_desc,
                found,
            } => {
                if let Some(inputs_desc) = inputs_desc {
                    syn::Error::new(
                        span,
                        format!(
                            "expected {expected} arguments for {op} ({inputs_desc}), found {found}"
                        ),
                    )
                } else {
                    syn::Error::new(
                        span,
                        format!("expected {expected} arguments for {op}, found {found}"),
                    )
                }
            }

            Error::CannotPerform { span, op, kind } => {
                syn::Error::new(span, format!("cannot perform {op} on `{kind}`"))
            }

            Error::PatternKindMismatch {
                span,
                expected,
                found,
            } => syn::Error::new(
                span,
                format!("value does not match pattern. expected `{expected}`, found `{found}`"),
            ),

            Error::PatternValueMismatch { span } => syn::Error::new(
                span,
                "value does not match pattern. expected a specific value",
            ),

            Error::PatternListLengthMismatch {
                span,
                expected,
                found,
            } => syn::Error::new(
                span,
                format!(
                    "value does not match pattern. expected list length {expected}, found {found}"
                ),
            ),

            Error::NoMatches { span, value } => {
                syn::Error::new(span, format!("no matches found for `{value}`"))
            }

            Error::NameAlreadyExists(name) => syn::Error::new(
                name.span,
                format!("name `{}` already exists", ctx.unintern(name.id.strid)),
            ),

            Error::NameNotFound(name) => syn::Error::new(
                name.span,
                format!("name `{}` not found", ctx.unintern(name.id.strid)),
            ),
        }
    }

    pub fn into_compile_error(self, ctx: &mut Context) -> TokenStream {
        self.into_syn_error(ctx).into_compile_error()
    }
}
