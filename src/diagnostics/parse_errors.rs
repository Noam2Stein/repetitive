//! A module defining all errors related to syntax.
//!
//! See [`crate::diagnostics`] for context.

use crate::{
    diagnostics::{Diagnostics, EmittedError},
    proc_macro12::{Delimiter, Span},
};

pub fn expected_char(diagnostics: &Diagnostics, last_span: Span, char: char) -> EmittedError {
    diagnostics.emit_error(last_span, format_args!("expected `{char}` after this"))
}

pub fn expected_char_found_punct(
    diagnostics: &Diagnostics,
    span: Span,
    char: char,
    found_char: char,
) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("expected `{char}`, found `{found_char}`"),
    )
}

pub fn expected_char_found_group(
    diagnostics: &Diagnostics,
    span_open: Span,
    char: char,
) -> EmittedError {
    diagnostics.emit_error(
        span_open,
        format_args!("expected `{char}`, found delimiter"),
    )
}

pub fn expected_char_found_ident(
    diagnostics: &Diagnostics,
    span: Span,
    char: char,
) -> EmittedError {
    diagnostics.emit_error(span, format_args!("expected `{char}`, found identifier"))
}

pub fn expected_char_found_literal(
    diagnostics: &Diagnostics,
    span: Span,
    char: char,
) -> EmittedError {
    diagnostics.emit_error(span, format_args!("expected `{char}`, found literal"))
}

pub fn expected_delimiter(
    diagnostics: &Diagnostics,
    last_span: Span,
    delimiter: Delimiter,
) -> EmittedError {
    diagnostics.emit_error(
        last_span,
        format_args!("expected {} after this", delimiter_noun(delimiter)),
    )
}

pub fn expected_delimiter_found_group(
    diagnostics: &Diagnostics,
    span_open: Span,
    delimiter: Delimiter,
    found_delimiter: Delimiter,
) -> EmittedError {
    diagnostics.emit_error(
        span_open,
        format_args!(
            "expected {}, found {}",
            delimiter_noun(delimiter),
            delimiter_noun(found_delimiter)
        ),
    )
}

pub fn expected_delimiter_found_ident(
    diagnostics: &Diagnostics,
    span: Span,
    delimiter: Delimiter,
) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!(
            "expected {}, found an identifier",
            delimiter_noun(delimiter)
        ),
    )
}

pub fn expected_delimiter_found_literal(
    diagnostics: &Diagnostics,
    span: Span,
    delimiter: Delimiter,
) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("expected {}, found a literal", delimiter_noun(delimiter)),
    )
}

pub fn expected_delimiter_found_punct(
    diagnostics: &Diagnostics,
    span: Span,
    delimiter: Delimiter,
) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!(
            "expected {}, found a punctuation",
            delimiter_noun(delimiter)
        ),
    )
}

pub fn expected_expr_found_brace(diagnostics: &Diagnostics, span_open: Span) -> EmittedError {
    diagnostics.emit_error(
        span_open,
        format_args!("block expressions are not supported"),
    )
}

pub fn expected_joint_spacing_found_alone_spacing(
    diagnostics: &Diagnostics,
    span: Span,
    char: char,
    next_char: char,
) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("expected `{next_char}` immediately after this `{char}`"),
    )
}

pub fn expected_keyword(diagnostics: &Diagnostics, last_span: Span, keyword: &str) -> EmittedError {
    diagnostics.emit_error(
        last_span,
        format_args!("expected keyword `{keyword}` after this"),
    )
}

pub fn expected_keyword_found_group(
    diagnostics: &Diagnostics,
    span_open: Span,
    keyword: &str,
) -> EmittedError {
    diagnostics.emit_error(
        span_open,
        format_args!("expected keyword `{keyword}`, found delimiter"),
    )
}

pub fn expected_keyword_found_ident(
    diagnostics: &Diagnostics,
    span: Span,
    keyword: &str,
    found_ident: &str,
) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("expected keyword `{keyword}`, found `{found_ident}`"),
    )
}

pub fn expected_keyword_found_literal(
    diagnostics: &Diagnostics,
    span: Span,
    keyword: &str,
) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("expected keyword `{keyword}`, found literal"),
    )
}

pub fn expected_keyword_found_punct(
    diagnostics: &Diagnostics,
    span: Span,
    keyword: &str,
) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("expected keyword `{keyword}`, found punctuation"),
    )
}

pub fn expected_meta(diagnostics: &Diagnostics, last_span: Span) -> EmittedError {
    diagnostics.emit_error(last_span, format_args!("expected meta-construct after `$`"))
}

pub fn expected_meta_found_brace(diagnostics: &Diagnostics, span_open: Span) -> EmittedError {
    diagnostics.emit_error(
        span_open,
        format_args!(
            "`${{ ... }}` syntax is not supported (to compute a value, use a `$let` statement)",
        ),
    )
}

pub fn expected_meta_found_bracket(diagnostics: &Diagnostics, span_open: Span) -> EmittedError {
    diagnostics.emit_error(span_open, format_args!("invalid syntax `$[...]`"))
}

pub fn expected_meta_found_literal(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(span, format_args!("invalid syntax `$` followed by literal"))
}

pub fn expected_meta_found_none_delimiter(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("meta-constructs pasted from macros are not supported"),
    )
}

pub fn expected_meta_found_parenthesis(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!(
            "`$(...)` syntax is not supported (to compute a value, use a `$let` statement)",
        ),
    )
}

pub fn expected_meta_found_punct(
    diagnostics: &Diagnostics,
    span: Span,
    char: char,
) -> EmittedError {
    diagnostics.emit_error(
        span,
        format_args!("invalid syntax `$` followed by `{char}`"),
    )
}

pub fn leftover_token(diagnostics: &Diagnostics, span: Span) -> EmittedError {
    diagnostics.emit_error(span, format_args!("unexpected token"))
}

pub fn unsupported_ident(diagnostics: &Diagnostics, span: Span, str: &str) -> EmittedError {
    diagnostics.emit_error(span, format_args!("unsupported identifier `{str}`"))
}

fn delimiter_noun(delimiter: Delimiter) -> &'static str {
    match delimiter {
        Delimiter::Brace => "`{ ... }`",
        Delimiter::Bracket => "`[...]`",
        Delimiter::None => "tokens pasted from macro",
        Delimiter::Parenthesis => "`(...)`",
    }
}
