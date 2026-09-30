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
    pub fn expected_delimiters_found_delimiters(
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

    pub fn expected_delimiters_found_ident(span: Span, delimiter: Delimiter) -> Self {
        Self {
            span,
            message: Cow::Owned(format!(
                "expected {}, found an identifier",
                delimiter_noun(delimiter)
            )),
        }
    }

    pub fn expected_delimiters_found_literal(span: Span, delimiter: Delimiter) -> Self {
        Self {
            span,
            message: Cow::Owned(format!(
                "expected {}, found a literal",
                delimiter_noun(delimiter)
            )),
        }
    }

    pub fn expected_delimiters_found_punct(span: Span, delimiter: Delimiter) -> Self {
        Self {
            span,
            message: Cow::Owned(format!(
                "expected {}, found a punctuation",
                delimiter_noun(delimiter)
            )),
        }
    }

    pub fn expected_delimiters_found_cutoff(last_span: Span, delimiter: Delimiter) -> Self {
        Self {
            span: last_span,
            message: Cow::Owned(format!("expected {} after this", delimiter_noun(delimiter))),
        }
    }

    pub fn expected_keyword_found_delimiters(span_open: Span, keyword: &str) -> Self {
        Self {
            span: span_open,
            message: Cow::Owned(format!("expected keyword `{keyword}`, found delimiters")),
        }
    }

    pub fn expected_keyword_found_ident(span: Span, keyword: &str, ident: &str) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("expected keyword `{keyword}`, found `{ident}`")),
        }
    }

    pub fn expected_keyword_found_literal(span: Span, keyword: &str) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("expected keyword `{keyword}`, found literal")),
        }
    }

    pub fn expected_keyword_found_punct(span: Span, keyword: &str) -> Self {
        Self {
            span,
            message: Cow::Owned(format!("expected keyword `{keyword}`, found punctuation")),
        }
    }

    pub fn expected_keyword_found_cutoff(last_span: Span, keyword: &str) -> Self {
        Self {
            span: last_span,
            message: Cow::Owned(format!("expected keyword `{keyword}` after this")),
        }
    }

    pub fn meta_braces(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed(
                "`${ ... }` syntax is not supported (to compute a value, use a `$let` statement)",
            ),
        }
    }

    pub fn meta_brackets(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("invalid syntax `$[...]`"),
        }
    }

    pub fn meta_cutoff(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("expected metaprogramming keyword after `$`"),
        }
    }

    pub fn meta_invisible_group(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("metaprogramming segments pasted from macros are not supported"),
        }
    }

    pub fn meta_literal(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("invalid syntax `$` followed by literal"),
        }
    }

    pub fn meta_parentheses(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed(
                "`$(...)` syntax is not supported (to compute a value, use a `$let` statement)",
            ),
        }
    }

    pub fn meta_punct(span: Span, char: char) -> Self {
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
    pub fn int_add_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to add with overflow"),
        }
    }

    pub fn int_div_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to divide with overflow"),
        }
    }

    pub fn int_div_zero(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to divide by zero"),
        }
    }

    pub fn int_mul_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to multiply with overflow"),
        }
    }

    pub fn int_neg_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to negate with overflow"),
        }
    }

    pub fn int_rem_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to calculate the remainder with overflow"),
        }
    }

    pub fn int_rem_zero(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to calculate the remainder with a divisor of zero"),
        }
    }

    pub fn int_sub_overflow(span: Span) -> Self {
        Self {
            span,
            message: Cow::Borrowed("attempt to subtract with overflow"),
        }
    }

    pub fn str_emit_invalid(span: Span, val: &str) -> Self {
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
