//! A module defining all errors related to syntax.
//!
//! See [`crate::diagnostics`] for context.

use crate::{
    diagnostics::error_format::{Error, error},
    proc_macro12::{Delimiter, Span},
};

pub fn expected_char(last_span: Span, char: char) -> impl Error {
    error!(last_span, "expected `{char}` after this")
}

pub fn expected_char_found_punct(span: Span, char: char, found_char: char) -> impl Error {
    error!(span, "expected `{char}`, found `{found_char}`",)
}

pub fn expected_char_found_group(span_open: Span, char: char) -> impl Error {
    error!(span_open, "expected `{char}`, found delimiter",)
}

pub fn expected_char_found_ident(span: Span, char: char) -> impl Error {
    error!(span, "expected `{char}`, found identifier")
}

pub fn expected_char_found_literal(span: Span, char: char) -> impl Error {
    error!(span, "expected `{char}`, found literal")
}

pub fn expected_delimiter(last_span: Span, delimiter: Delimiter) -> impl Error {
    error!(
        last_span,
        "expected {} after this",
        delimiter_noun(delimiter),
    )
}

pub fn expected_delimiter_found_group(
    span_open: Span,
    delimiter: Delimiter,
    found_delimiter: Delimiter,
) -> impl Error {
    error!(
        span_open,
        "expected {}, found {}",
        delimiter_noun(delimiter),
        delimiter_noun(found_delimiter),
    )
}

pub fn expected_delimiter_found_ident(span: Span, delimiter: Delimiter) -> impl Error {
    error!(
        span,
        "expected {}, found an identifier",
        delimiter_noun(delimiter),
    )
}

pub fn expected_delimiter_found_literal(span: Span, delimiter: Delimiter) -> impl Error {
    error!(
        span,
        "expected {}, found a literal",
        delimiter_noun(delimiter),
    )
}

pub fn expected_delimiter_found_punct(span: Span, delimiter: Delimiter) -> impl Error {
    error!(
        span,
        "expected {}, found a punctuation",
        delimiter_noun(delimiter),
    )
}

pub fn expected_expr_found_brace(span_open: Span) -> impl Error {
    error!(span_open, "block expressions are not supported")
}

pub fn expected_joint_spacing_found_alone_spacing(
    span: Span,
    char: char,
    next_char: char,
) -> impl Error {
    error!(
        span,
        "expected `{next_char}` immediately after this `{char}`"
    )
}

pub fn expected_keyword(last_span: Span, keyword: &str) -> impl Error {
    error!(last_span, "expected keyword `{keyword}` after this")
}

pub fn expected_keyword_found_group(span_open: Span, keyword: &str) -> impl Error {
    error!(span_open, "expected keyword `{keyword}`, found delimiter")
}

pub fn expected_keyword_found_ident(span: Span, keyword: &str, found_ident: &str) -> impl Error {
    error!(span, "expected keyword `{keyword}`, found `{found_ident}`")
}

pub fn expected_keyword_found_literal(span: Span, keyword: &str) -> impl Error {
    error!(span, "expected keyword `{keyword}`, found literal")
}

pub fn expected_keyword_found_punct(span: Span, keyword: &str) -> impl Error {
    error!(span, "expected keyword `{keyword}`, found punctuation")
}

pub fn expected_meta(last_span: Span) -> impl Error {
    error!(last_span, "expected meta-construct after `$`")
}

pub fn expected_meta_found_brace(span_open: Span) -> impl Error {
    error!(
        span_open,
        "`${{ ... }}` syntax is not supported (to compute a value, use a `$let` statement)",
    )
}

pub fn expected_meta_found_bracket(span_open: Span) -> impl Error {
    error!(span_open, "invalid syntax `$[...]`")
}

pub fn expected_meta_found_literal(span: Span) -> impl Error {
    error!(span, "invalid syntax `$` followed by literal")
}

pub fn expected_meta_found_none_delimiter(span: Span) -> impl Error {
    error!(span, "meta-constructs pasted from macros are not supported")
}

pub fn expected_meta_found_parenthesis(span: Span) -> impl Error {
    error!(
        span,
        "`$(...)` syntax is not supported (to compute a value, use a `$let` statement)",
    )
}

pub fn expected_meta_found_punct(span: Span, char: char) -> impl Error {
    error!(span, "invalid syntax `$` followed by `{char}`")
}

pub fn leftover_token(span: Span) -> impl Error {
    error!(span, "unexpected token")
}

pub fn unsupported_ident(span: Span, str: &str) -> impl Error {
    error!(span, "unsupported identifier `{str}`")
}

fn delimiter_noun(delimiter: Delimiter) -> &'static str {
    match delimiter {
        Delimiter::Brace => "`{ ... }`",
        Delimiter::Bracket => "`[...]`",
        Delimiter::None => "tokens pasted from macro",
        Delimiter::Parenthesis => "`(...)`",
    }
}
