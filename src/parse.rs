use crate::{
    ast::{
        Expr, ExprArray, ExprKind, ExprTuple, Meta, MetaFor, MetaIdent, MetaIf, MetaIfSegment,
        MetaLet, MetaMatch, MetaMatchArm, Pat, PatKind, Quote, QuoteGroup, QuoteSegment,
    },
    diagnostics::{Diagnostics, RecordedError},
    errors::Error,
    proc_macro12::{Delimiter, Group, Span, TokenStream, TokenTree},
    str_interner::{StrId, StrInterner},
    token_iter::TokenIter,
};

pub fn parse_ast(input: TokenStream) -> Quote {
    Quote {
        unparsed_segments: TokenIter::new(Span::call_site(), input),
    }
}

impl Quote {
    pub fn segments(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> impl Iterator<Item = Result<QuoteSegment, RecordedError>> {
        let mut iter = self.unparsed_segments;

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

impl MetaMatch {
    pub fn arms(
        self,
        diagnostics: &Diagnostics,
        str_interner: &StrInterner,
    ) -> impl Iterator<Item = Result<MetaMatchArm, RecordedError>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
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

fn parse_brace_with_quote(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<Quote, RecordedError> {
    let brace = parse_delimiter(Delimiter::Brace, iter, diagnostics)?;
    Ok(Quote {
        unparsed_segments: TokenIter::new(brace.span_open(), brace.stream()),
    })
}

fn parse_brace_with_token_iter(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<TokenIter, RecordedError> {
    let brace = parse_delimiter(Delimiter::Brace, iter, diagnostics)?;
    Ok(TokenIter::new(brace.span_open(), brace.stream()))
}

fn parse_char(
    char: char,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<(), RecordedError> {
    match iter.next() {
        Some(TokenTree::Group(token)) => {
            Err(diagnostics.record_error(Error::parse_char_group(token.span_open(), char)))
        }
        Some(TokenTree::Ident(token)) => {
            Err(diagnostics.record_error(Error::parse_char_ident(token.span(), char)))
        }
        Some(TokenTree::Literal(token)) => {
            Err(diagnostics.record_error(Error::parse_char_literal(token.span(), char)))
        }
        Some(TokenTree::Punct(token)) => {
            let found_char = token.as_char();
            if found_char == char {
                Ok(())
            } else {
                Err(diagnostics.record_error(Error::parse_char_wrong_char(
                    token.span(),
                    char,
                    found_char,
                )))
            }
        }
        None => Err(diagnostics.record_error(Error::parse_char_cutoff(iter.last_span(), char))),
    }
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
                    diagnostics.record_error(Error::parse_delimiter_wrong_delimiter(
                        token.span_open(),
                        delimiter,
                        found_delimiter,
                    )),
                )
            }
        }
        Some(TokenTree::Ident(token)) => {
            Err(diagnostics.record_error(Error::parse_delimiter_ident(token.span(), delimiter)))
        }
        Some(TokenTree::Literal(token)) => {
            Err(diagnostics.record_error(Error::parse_delimiter_literal(token.span(), delimiter)))
        }
        Some(TokenTree::Punct(token)) => {
            Err(diagnostics.record_error(Error::parse_delimiter_punct(token.span(), delimiter)))
        }
        None => {
            Err(diagnostics
                .record_error(Error::parse_delimiter_cutoff(iter.last_span(), delimiter)))
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

fn parse_keyword(
    keyword: &str,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
) -> Result<(), RecordedError> {
    match iter.next() {
        Some(TokenTree::Group(token)) => {
            Err(diagnostics.record_error(Error::parse_keyword_group(token.span_open(), keyword)))
        }
        Some(TokenTree::Ident(token)) => {
            let ident = token.to_string();
            if ident == keyword {
                Ok(())
            } else {
                Err(diagnostics.record_error(Error::parse_keyword_ident(
                    token.span(),
                    keyword,
                    &ident,
                )))
            }
        }
        Some(TokenTree::Literal(token)) => {
            Err(diagnostics.record_error(Error::parse_keyword_literal(token.span(), keyword)))
        }
        Some(TokenTree::Punct(token)) => {
            Err(diagnostics.record_error(Error::parse_keyword_punct(token.span(), keyword)))
        }
        None => {
            Err(diagnostics.record_error(Error::parse_keyword_cutoff(iter.last_span(), keyword)))
        }
    }
}

fn parse_meta(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<Meta, RecordedError> {
    let Some(first_token) = iter.next() else {
        return Err(diagnostics.record_error(Error::parse_meta_cutoff(iter.last_span())));
    };

    Ok(match first_token {
        TokenTree::Group(first_token) => {
            return Err(diagnostics.record_error(match first_token.delimiter() {
                Delimiter::Brace => Error::parse_meta_brace(first_token.span()),
                Delimiter::Bracket => Error::parse_meta_bracket(first_token.span()),
                Delimiter::None => Error::parse_meta_none_delimiter(first_token.span()),
                Delimiter::Parenthesis => Error::parse_meta_parenthesis(first_token.span()),
            }));
        }
        TokenTree::Ident(first_token) => match first_token.to_string().as_str() {
            "for" => Meta::For(parse_meta_for(iter, diagnostics, str_interner)?),
            "if" => Meta::If(parse_meta_if(iter, diagnostics, str_interner)?),
            "let" => Meta::Let(parse_meta_let(iter, diagnostics, str_interner)?),
            "match" => Meta::Match(parse_meta_match(iter, diagnostics, str_interner)?),
            ident_str => Meta::Ident(MetaIdent {
                span: first_token.span(),
                strid: validate_ident(first_token.span(), ident_str, diagnostics, str_interner)?,
            }),
        },
        TokenTree::Literal(first_token) => {
            return Err(diagnostics.record_error(Error::parse_meta_literal(first_token.span())));
        }
        TokenTree::Punct(first_token) => {
            return Err(diagnostics.record_error(Error::parse_meta_punct(
                first_token.span(),
                first_token.as_char(),
            )));
        }
    })
}

fn parse_meta_for(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<MetaFor, RecordedError> {
    let pat = parse_pat(iter, diagnostics, str_interner)?;
    parse_keyword("in", iter, diagnostics)?;
    let expr = parse_expr(iter, diagnostics, str_interner)?;
    let body = parse_brace_with_quote(iter, diagnostics)?;

    Ok(MetaFor { pat, expr, body })
}

fn parse_meta_if(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<MetaIf, RecordedError> {
    let mut segments = Vec::new();

    let first_condition = parse_expr(iter, diagnostics, str_interner)?;
    let first_branch = parse_brace_with_quote(iter, diagnostics)?;
    segments.push(MetaIfSegment {
        condition: Some(first_condition),
        branch: first_branch,
    });

    let parse_else = |iter: &mut TokenIter| {
        iter.next_n_if(|[token_0, token_1]| {
            token_is_char(token_0, '$') && token_is_keyword(token_1, "else")
        })
    };
    while parse_else(iter).is_some() {
        let is_else_if = iter
            .next_if(|token| token_is_keyword(token, "if"))
            .is_some();

        let condition = is_else_if
            .then(|| parse_expr(iter, diagnostics, str_interner))
            .transpose()?;

        let branch = parse_brace_with_quote(iter, diagnostics)?;

        segments.push(MetaIfSegment { condition, branch });
    }

    Ok(MetaIf { segments })
}

fn parse_meta_let(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<MetaLet, RecordedError> {
    let pat = parse_pat(iter, diagnostics, str_interner)?;
    parse_char('=', iter, diagnostics)?;
    let expr = parse_expr(iter, diagnostics, str_interner)?;
    parse_char(';', iter, diagnostics)?;

    Ok(MetaLet { pat, expr })
}

fn parse_meta_match(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<MetaMatch, RecordedError> {
    let expr = parse_expr(iter, diagnostics, str_interner)?;
    let unparsed_arms = parse_brace_with_token_iter(iter, diagnostics)?;

    Ok(MetaMatch {
        expr,
        unparsed_arms,
    })
}

fn parse_pat(
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<Pat, RecordedError> {
    todo!()
}

fn parse_quote_segment(
    first_token: TokenTree,
    iter: &mut TokenIter,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<QuoteSegment, RecordedError> {
    Ok(match first_token {
        TokenTree::Group(first_token) => QuoteSegment::Group(QuoteGroup {
            delimiter: first_token.delimiter(),
            span: first_token.span(),
            stream: Quote {
                unparsed_segments: TokenIter::new(first_token.span_open(), first_token.stream()),
            },
        }),
        TokenTree::Punct(first_token) if first_token.as_char() == '$' => {
            QuoteSegment::Meta(parse_meta(iter, diagnostics, str_interner)?)
        }
        TokenTree::Ident(_) | TokenTree::Literal(_) | TokenTree::Punct(_) => {
            let mut result = TokenStream::from_iter([first_token]);
            while let Some(token) = iter.next_if(token_cannot_contain_meta) {
                result.extend([token]);
            }

            QuoteSegment::TokenStream(result)
        }
    })
}

fn token_cannot_contain_meta(token: &TokenTree) -> bool {
    match token {
        TokenTree::Group(_) => false,
        TokenTree::Punct(token) if token.as_char() == '$' => false,
        TokenTree::Ident(_) | TokenTree::Literal(_) | TokenTree::Punct(_) => true,
    }
}

fn token_is_char(token: &TokenTree, char: char) -> bool {
    if let TokenTree::Punct(token) = token
        && token.as_char() == char
    {
        true
    } else {
        false
    }
}

fn token_is_keyword(token: &TokenTree, keyword: &str) -> bool {
    #[allow(clippy::cmp_owned)]
    if let TokenTree::Ident(token) = token
        && token.to_string() == keyword
    {
        true
    } else {
        false
    }
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
