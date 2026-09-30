use crate::{
    ast::{
        Expr, QuoteFor, QuoteGroup, QuoteIdent, QuoteIf, QuoteLet, QuoteMatch, QuoteSegment,
        UnparsedExpr, UnparsedExprArray, UnparsedExprTuple, UnparsedPat, UnparsedQuote,
    },
    diagnostics::{Diagnostics, RecordedError},
    proc_macro12::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree},
    str_interner::{StrId, StrInterner},
    token_iter::TokenIter,
};

impl UnparsedQuote {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> impl Iterator<Item = Result<QuoteSegment, RecordedError>> {
        let mut iter = TokenIter::new(self.stream, self.last_span);

        std::iter::from_fn(move || {
            parse_optional_quote_segment(&mut iter, diagnostics, str_interner)
        })
    }
}

fn parse_optional_quote_segment(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Option<Result<QuoteSegment, RecordedError>> {
    let first_token = iter.next()?;

    Some(match first_token {
        TokenTree::Group(first_token) => Ok(QuoteSegment::Group(QuoteGroup {
            delimiter: first_token.delimiter(),
            span: first_token.span(),
            stream: UnparsedQuote {
                stream: first_token.stream(),
                last_span: first_token.span_open(),
            },
        })),
        TokenTree::Punct(first_token) if first_token.as_char() == '$' => {
            parse_quote_dollar(first_token, iter, diagnostics, str_interner)
        }
        TokenTree::Ident(_) | TokenTree::Literal(_) | TokenTree::Punct(_) => {
            let mut result = TokenStream::from_iter([first_token]);
            while let Some(token) = iter.next_if(token_cannot_contain_dollar) {
                result.extend([token]);
            }

            Ok(QuoteSegment::TokenStream(result))
        }
    })
}

fn token_cannot_contain_dollar(token: &TokenTree) -> bool {
    match token {
        TokenTree::Group(_) => false,
        TokenTree::Punct(token) if token.as_char() == '$' => false,
        TokenTree::Ident(_) | TokenTree::Literal(_) | TokenTree::Punct(_) => true,
    }
}

fn parse_quote_dollar(
    dollar: Punct,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<QuoteSegment, RecordedError> {
    let Some(first_token) = iter.next() else {
        return Err(
            diagnostics.record_error(dollar.span(), "expected metaprogramming keyword after `$`")
        );
    };

    match first_token {
        TokenTree::Group(first_token) => Err(diagnostics.record_error(
            first_token.span(),
            quote_dollar_delimiter_error(first_token.delimiter()),
        )),
        TokenTree::Ident(first_token) => {
            parse_quote_dollar_ident(first_token, iter, diagnostics, str_interner)
        }
        TokenTree::Literal(_) => {
            Err(diagnostics
                .record_error(first_token.span(), "invalid syntax `$` followed by literal"))
        }
        TokenTree::Punct(first_token) => Err(diagnostics.record_error(
            first_token.span(),
            format!("invalid syntax `$` followed by `{}`", first_token.as_char()),
        )),
    }
}

fn quote_dollar_delimiter_error(delimiter: Delimiter) -> &'static str {
    match delimiter {
        Delimiter::Brace => {
            "`${ ... }` syntax is not supported (consider writing a `$let` statement)"
        }
        Delimiter::Bracket => "invalid syntax `$[...]`",
        Delimiter::None => "metaprogramming segments pasted from macros are not supported",
        Delimiter::Parenthesis => {
            "`$(...)` syntax is not supported (consider writing a `$let` statement)"
        }
    }
}

fn parse_quote_dollar_ident(
    ident: Ident,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<QuoteSegment, RecordedError> {
    Ok(match ident.to_string().as_str() {
        "for" => QuoteSegment::For(parse_quote_for(iter, diagnostics, str_interner)?),
        "if" => QuoteSegment::If(parse_quote_if(iter, diagnostics, str_interner)?),
        "let" => QuoteSegment::Let(parse_quote_let(iter, diagnostics, str_interner)?),
        "match" => QuoteSegment::Match(parse_quote_match(iter, diagnostics, str_interner)?),
        ident_str => QuoteSegment::Ident(QuoteIdent {
            span: ident.span(),
            strid: validate_ident(ident.span(), ident_str, diagnostics, str_interner)?,
        }),
    })
}

fn parse_quote_for(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<QuoteFor, RecordedError> {
    let pat = partially_parse_pat(iter, diagnostics, str_interner)?;
    parse_keyword("in", iter, diagnostics)?;
    let expr = partially_parse_expr(iter, diagnostics, str_interner)?;
    let body = parse_quote_braces(iter, diagnostics)?;

    Ok(QuoteFor { pat, expr, body })
}

fn parse_quote_if(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<QuoteIf, RecordedError> {
    todo!()
}

fn parse_quote_let(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<QuoteLet, RecordedError> {
    todo!()
}

fn parse_quote_match(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<QuoteMatch, RecordedError> {
    todo!()
}

fn validate_ident(
    span: Span,
    str: &str,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<StrId, RecordedError> {
    let is_valid_ident = str
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && str.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');

    if is_valid_ident {
        Ok(str_interner.intern(str))
    } else {
        Err(diagnostics.record_error(span, format!("unsupported identifier `{str}`")))
    }
}

fn partially_parse_pat(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<UnparsedPat, RecordedError> {
    todo!()
}

fn parse_keyword(
    keyword: &str,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<(), RecordedError> {
    match iter.next() {
        Some(TokenTree::Group(token)) => Err(diagnostics.record_error(
            token.span_open(),
            format!("expected keyword `{keyword}`, found delimiters"),
        )),
        Some(TokenTree::Ident(token)) => {
            let str = token.to_string();
            if str == keyword {
                Ok(())
            } else {
                Err(diagnostics.record_error(
                    token.span(),
                    format!("expected keyword {keyword}, found {str}"),
                ))
            }
        }
        Some(TokenTree::Literal(token)) => Err(diagnostics.record_error(
            token.span(),
            format!("expected keyword `{keyword}`, found literal"),
        )),
        Some(TokenTree::Punct(token)) => Err(diagnostics.record_error(
            token.span(),
            format!("expected keyword `{keyword}`, found punctuation"),
        )),
        None => Err(diagnostics.record_error(
            iter.last_span(),
            format!("expected keyword `{keyword}` after this"),
        )),
    }
}

fn parse_quote_braces(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<UnparsedQuote, RecordedError> {
    let braces = parse_delimiter(Delimiter::Brace, iter, diagnostics)?;
    Ok(UnparsedQuote {
        stream: braces.stream(),
        last_span: braces.span_open(),
    })
}

fn parse_delimiter(
    delimiter: Delimiter,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<Group, RecordedError> {
    match iter.next() {
        Some(TokenTree::Group(token)) => {
            if token.delimiter() == delimiter {
                Ok(token)
            } else {
                Err(diagnostics.record_error(
                    token.span_open(),
                    format!(
                        "expected {}, found {}",
                        delimiter_text(delimiter),
                        delimiter_text(token.delimiter())
                    ),
                ))
            }
        }
        Some(TokenTree::Ident(token)) => Err(diagnostics.record_error(
            token.span(),
            format!("expected {}, found identifier", delimiter_text(delimiter)),
        )),
        Some(TokenTree::Literal(token)) => Err(diagnostics.record_error(
            token.span(),
            format!("expected {}, found literal", delimiter_text(delimiter)),
        )),
        Some(TokenTree::Punct(token)) => Err(diagnostics.record_error(
            token.span(),
            format!("expected {}, found punctuation", delimiter_text(delimiter)),
        )),
        None => Err(diagnostics.record_error(
            iter.last_span(),
            format!("expected {} after this", delimiter_text(delimiter)),
        )),
    }
}

fn partially_parse_expr(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<UnparsedExpr, RecordedError> {
    todo!()
}

fn delimiter_text(delimiter: Delimiter) -> &'static str {
    match delimiter {
        Delimiter::Brace => "`{ ... }`",
        Delimiter::Bracket => "`[...]`",
        Delimiter::None => "tokens pasted from macro",
        Delimiter::Parenthesis => "`(...)`",
    }
}

impl UnparsedExpr {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> Result<Expr, RecordedError> {
        todo!()
    }
}

impl UnparsedExprArray {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> impl Iterator<Item = Result<Expr, RecordedError>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
    }
}

impl UnparsedExprTuple {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> impl Iterator<Item = Result<Expr, RecordedError>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
    }
}

impl UnparsedPat {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> Result<Expr, RecordedError> {
        todo!()
    }
}
