use crate::{
    ast::{
        Expr, ExprArray, ExprKind, ExprTuple, Pat, PatKind, Quote, QuoteFor, QuoteGroup,
        QuoteIdent, QuoteIf, QuoteLet, QuoteMatch, QuoteSegment,
    },
    diagnostics::{Diagnostics, RecordedError},
    error::Error,
    proc_macro12::{Delimiter, Group, Punct, Span, TokenStream, TokenTree},
    str_interner::{StrId, StrInterner},
    token_iter::TokenIter,
};

pub fn parse_ast(input: TokenStream) -> Quote {
    Quote {
        last_span: Span::call_site(),
        stream: input,
    }
}

impl Quote {
    pub fn segments(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> impl Iterator<Item = Result<QuoteSegment, RecordedError>> {
        let mut iter = TokenIter::new(self.last_span, self.stream);

        std::iter::from_fn(move || {
            Some(parse_quote_segment(
                iter.next()?,
                &mut iter,
                diagnostics,
                str_interner,
            ))
        })
    }
}

impl Expr {
    pub fn kind(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> Result<ExprKind, RecordedError> {
        todo!()
    }
}

impl ExprArray {
    pub fn elements(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> impl Iterator<Item = Result<Expr, RecordedError>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
    }
}

impl ExprTuple {
    pub fn fields(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> impl Iterator<Item = Result<Expr, RecordedError>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
    }
}

impl Pat {
    pub fn kind(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> Result<PatKind, RecordedError> {
        todo!()
    }
}

fn parse_quote_segment(
    first_token: TokenTree,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<QuoteSegment, RecordedError> {
    match first_token {
        TokenTree::Group(first_token) => Ok(QuoteSegment::Group(QuoteGroup {
            delimiter: first_token.delimiter(),
            span: first_token.span(),
            stream: Quote {
                last_span: first_token.span_open(),
                stream: first_token.stream(),
            },
        })),
        TokenTree::Punct(first_token) if first_token.as_char() == '$' => {
            parse_quote_dollar(iter, diagnostics, str_interner)
        }
        TokenTree::Ident(_) | TokenTree::Literal(_) | TokenTree::Punct(_) => {
            let mut result = TokenStream::from_iter([first_token]);
            while let Some(token) = iter.next_if(token_cannot_contain_dollar) {
                result.extend([token]);
            }

            Ok(QuoteSegment::TokenStream(result))
        }
    }
}

fn token_cannot_contain_dollar(token: &TokenTree) -> bool {
    match token {
        TokenTree::Group(_) => false,
        TokenTree::Punct(token) if token.as_char() == '$' => false,
        TokenTree::Ident(_) | TokenTree::Literal(_) | TokenTree::Punct(_) => true,
    }
}

fn parse_quote_dollar(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<QuoteSegment, RecordedError> {
    let Some(first_token) = iter.next() else {
        return Err(diagnostics.record_error(Error::meta_cutoff(iter.last_span())));
    };

    Ok(match first_token {
        TokenTree::Group(first_token) => {
            return Err(diagnostics.record_error(match first_token.delimiter() {
                Delimiter::Brace => Error::meta_braces(first_token.span()),
                Delimiter::Bracket => Error::meta_brackets(first_token.span()),
                Delimiter::None => Error::meta_invisible_group(first_token.span()),
                Delimiter::Parenthesis => Error::meta_parentheses(first_token.span()),
            }));
        }
        TokenTree::Ident(first_token) => match first_token.to_string().as_str() {
            "for" => QuoteSegment::For(parse_quote_for(iter, diagnostics, str_interner)?),
            "if" => QuoteSegment::If(parse_quote_if(iter, diagnostics, str_interner)?),
            "let" => QuoteSegment::Let(parse_quote_let(iter, diagnostics, str_interner)?),
            "match" => QuoteSegment::Match(parse_quote_match(iter, diagnostics, str_interner)?),
            ident_str => QuoteSegment::Ident(QuoteIdent {
                span: first_token.span(),
                strid: validate_ident(first_token.span(), ident_str, diagnostics, str_interner)?,
            }),
        },
        TokenTree::Literal(first_token) => {
            return Err(diagnostics.record_error(Error::meta_literal(first_token.span())));
        }
        TokenTree::Punct(first_token) => {
            return Err(diagnostics
                .record_error(Error::meta_punct(first_token.span(), first_token.as_char())));
        }
    })
}

fn parse_quote_for(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<QuoteFor, RecordedError> {
    let pat = parse_pat(iter, diagnostics, str_interner)?;
    parse_keyword("in", iter, diagnostics)?;
    let expr = parse_expr(iter, diagnostics, str_interner)?;
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
    let pat = parse_pat(iter, diagnostics, str_interner)?;
    parse_char('=', iter, diagnostics)?;
    let expr = parse_expr(iter, diagnostics, str_interner)?;
    parse_char(';', iter, diagnostics)?;

    Ok(QuoteLet { pat, expr })
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
        Err(diagnostics.record_error(Error::unsupported_ident(span, str)))
    }
}

fn parse_pat(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<Pat, RecordedError> {
    todo!()
}

fn parse_keyword(
    keyword: &str,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<(), RecordedError> {
    match iter.next() {
        Some(TokenTree::Group(token)) => Err(diagnostics.record_error(
            Error::expected_keyword_found_delimiters(token.span_open(), keyword),
        )),
        Some(TokenTree::Ident(token)) => {
            let ident = token.to_string();
            if ident == keyword {
                Ok(())
            } else {
                Err(
                    diagnostics.record_error(Error::expected_keyword_found_ident(
                        token.span(),
                        keyword,
                        &ident,
                    )),
                )
            }
        }
        Some(TokenTree::Literal(token)) => {
            Err(diagnostics
                .record_error(Error::expected_keyword_found_literal(token.span(), keyword)))
        }
        Some(TokenTree::Punct(token)) => {
            Err(diagnostics
                .record_error(Error::expected_keyword_found_punct(token.span(), keyword)))
        }
        None => Err(
            diagnostics.record_error(Error::expected_keyword_found_cutoff(
                iter.last_span(),
                keyword,
            )),
        ),
    }
}

fn parse_quote_braces(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<Quote, RecordedError> {
    let braces = parse_delimiter(Delimiter::Brace, iter, diagnostics)?;
    Ok(Quote {
        last_span: braces.span_open(),
        stream: braces.stream(),
    })
}

fn parse_delimiter(
    delimiter: Delimiter,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<Group, RecordedError> {
    match iter.next() {
        Some(TokenTree::Group(token)) => {
            let found_delimiter = token.delimiter();
            if found_delimiter == delimiter {
                Ok(token)
            } else {
                Err(
                    diagnostics.record_error(Error::expected_delimiters_found_delimiters(
                        token.span_open(),
                        delimiter,
                        found_delimiter,
                    )),
                )
            }
        }
        Some(TokenTree::Ident(token)) => Err(diagnostics.record_error(
            Error::expected_delimiters_found_ident(token.span(), delimiter),
        )),
        Some(TokenTree::Literal(token)) => Err(diagnostics.record_error(
            Error::expected_delimiters_found_literal(token.span(), delimiter),
        )),
        Some(TokenTree::Punct(token)) => Err(diagnostics.record_error(
            Error::expected_delimiters_found_punct(token.span(), delimiter),
        )),
        None => Err(
            diagnostics.record_error(Error::expected_delimiters_found_cutoff(
                iter.last_span(),
                delimiter,
            )),
        ),
    }
}

fn parse_char(
    char: char,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<(), RecordedError> {
    match iter.next() {
        Some(TokenTree::Group(token)) => {
            Err(diagnostics
                .record_error(token.span(), format!("expected `{char}`, found delimiters")))
        }
        Some(TokenTree::Ident(token)) => {
            Err(diagnostics
                .record_error(token.span(), format!("expected `{char}`, found identifier")))
        }
        Some(TokenTree::Literal(token)) => {
            Err(diagnostics.record_error(token.span(), format!("expected `{char}`, found literal")))
        }
        Some(TokenTree::Punct(token)) => {
            if token.as_char() == char {
                Ok(())
            } else {
                Err(diagnostics.record_error(
                    token.span(),
                    format!("expected `{char}`, found `{}`", token.as_char()),
                ))
            }
        }
        None => {
            Err(diagnostics.record_error(iter.last_span(), format!("expected `{char}` after this")))
        }
    }
}

fn parse_expr(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<Expr, RecordedError> {
    todo!()
}
