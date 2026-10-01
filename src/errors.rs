use std::borrow::Cow;

use crate::proc_macro12::{Delimiter, Span};

pub struct Error {
    span: Span,
    message: Cow<'static, str>,
}

impl Error {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn message(self) -> Cow<'static, str> {
        self.message
    }
}

/// Syntax errors.
impl Error {
    pub fn parse_char_cutoff(last_span: Span, char: char) -> Self {
        Self {
            span: last_span,
            message: Cow::Owned(format!("expected `{char}` after this")),
        }
    }

    pub fn parse_char_group(span_open: Span, char: char) -> Self {
        Self {
            span: span_open,
            message: Cow::Owned(format!("expected `{char}`, found delimiter")),
        }
    }

    pub fn parse_char_ident(span: Span, char: char) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("expected `{char}`, found identifier")),
        }
    }

    pub fn parse_char_literal(span: Span, char: char) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("expected `{char}`, found literal")),
        }
    }

    pub fn parse_char_wrong_char(span: Span, char: char, found_char: char) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("expected `{char}`, found `{found_char}`")),
        }
    }

    pub fn parse_chars_alone_spacing(span: Span, char: char, next_char: char) -> Self {
        Self {
            span,
            message: Cow::Owned(format!(
                "expected `{next_char}` immediately after this `{char}`"
            )),
        }
    }

    pub fn parse_delimiter_cutoff(last_span: Span, delimiter: Delimiter) -> Self {
        Self {
            span: last_span,
            message: Cow::Owned(format!("expected {} after this", delimiter_noun(delimiter))),
        }
    }

    pub fn parse_delimiter_ident(span: Span, delimiter: Delimiter) -> Self {
        Self {
            span,
            message: Cow::Owned(format!(
                "expected {}, found an identifier",
                delimiter_noun(delimiter)
            )),
        }
    }

    pub fn parse_delimiter_literal(span: Span, delimiter: Delimiter) -> Self {
        Self {
            span,
            message: Cow::Owned(format!(
                "expected {}, found a literal",
                delimiter_noun(delimiter)
            )),
        }
    }

    pub fn parse_delimiter_punct(span: Span, delimiter: Delimiter) -> Self {
        Self {
            span,
            message: Cow::Owned(format!(
                "expected {}, found a punctuation",
                delimiter_noun(delimiter)
            )),
        }
    }

    pub fn parse_delimiter_wrong_delimiter(
        span_open: Span,
        delimiter: Delimiter,
        found_delimiter: Delimiter,
    ) -> Self {
        Self {
            span: span_open,
            message: Cow::Owned(format!(
                "expected {}, found {}",
                delimiter_noun(delimiter),
                delimiter_noun(found_delimiter)
            )),
        }
    }

    pub fn parse_keyword_cutoff(last_span: Span, keyword: &str) -> Self {
        Self {
            span: last_span,
            message: Cow::Owned(format!("expected keyword `{keyword}` after this")),
        }
    }

    pub fn parse_keyword_group(span_open: Span, keyword: &str) -> Self {
        Self {
            span: span_open,
            message: Cow::Owned(format!("expected keyword `{keyword}`, found delimiter")),
        }
    }

    pub fn parse_keyword_ident(span: Span, keyword: &str, ident: &str) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("expected keyword `{keyword}`, found `{ident}`")),
        }
    }

    pub fn parse_keyword_literal(span: Span, keyword: &str) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("expected keyword `{keyword}`, found literal")),
        }
    }

    pub fn parse_keyword_punct(span: Span, keyword: &str) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("expected keyword `{keyword}`, found punctuation")),
        }
    }

    pub fn parse_meta_brace(span_open: Span) -> Self {
        Self {
            span: span_open,
            message: Cow::Borrowed(
                "`${ ... }` syntax is not supported (to compute a value, use a `$let` statement)",
            ),
        }
    }

    pub fn parse_meta_bracket(span_open: Span) -> Self {
        Self {
            span: span_open,
            message: Cow::Borrowed("invalid syntax `$[...]`"),
        }
    }

    pub fn parse_meta_cutoff(last_span: Span) -> Self {
        Self {
            span: last_span,
            message: Cow::Borrowed("expected meta-construct after `$`"),
        }
    }

    pub fn parse_meta_literal(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("invalid syntax `$` followed by literal"),
        }
    }

    pub fn parse_meta_none_delimiter(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("meta-constructs pasted from macros are not supported"),
        }
    }

    pub fn parse_meta_parenthesis(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed(
                "`$(...)` syntax is not supported (to compute a value, use a `$let` statement)",
            ),
        }
    }

    pub fn parse_meta_punct(span: Span, char: char) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("invalid syntax `$` followed by `{char}`")),
        }
    }

    pub fn unsupported_ident(span: Span, str: &str) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("unsupported identifier `{str}`")),
        }
    }
}

/// Execute errors.
impl Error {
    pub fn execute_int_add_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to add with overflow"),
        }
    }

    pub fn execute_int_div_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to divide with overflow"),
        }
    }

    pub fn execute_int_div_zero(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to divide by zero"),
        }
    }

    pub fn execute_int_mul_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to multiply with overflow"),
        }
    }

    pub fn execute_int_neg_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to negate with overflow"),
        }
    }

    pub fn execute_int_rem_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to calculate the remainder with overflow"),
        }
    }

    pub fn execute_int_rem_zero(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to calculate the remainder with a divisor of zero"),
        }
    }

    pub fn execute_int_sub_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to subtract with overflow"),
        }
    }

    pub fn execute_str_emit_invalid(span: Span, val: &str) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("attempt to emit invalid identifier `{val}`")),
        }
    }
}

fn delimiter_noun(delimiter: Delimiter) -> &'static str {
    match delimiter {
        Delimiter::Brace => "`{ ... }`",
        Delimiter::Bracket => "`[...]`",
        Delimiter::None => "tokens pasted from macro",
        Delimiter::Parenthesis => "`(...)`",
    }
}
