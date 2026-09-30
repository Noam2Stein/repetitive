use std::iter::Peekable;

use crate::proc_macro12::{Span, TokenStream, TokenTree, token_stream};

pub struct TokenIter {
    base: Peekable<token_stream::IntoIter>,
    last_span: Span,
}

impl TokenIter {
    pub fn new(stream: TokenStream, last_span: Span) -> Self {
        Self {
            base: stream.into_iter().peekable(),
            last_span,
        }
    }

    pub fn next(&mut self) -> Option<TokenTree> {
        let next = self.base.next();

        match &next {
            Some(TokenTree::Group(next)) => self.last_span = next.span_close(),
            Some(next) => self.last_span = next.span(),
            None => {}
        }

        next
    }

    pub fn next_if(&mut self, func: impl FnOnce(&TokenTree) -> bool) -> Option<TokenTree> {
        if func(self.base.peek()?) {
            self.next()
        } else {
            None
        }
    }

    pub fn last_span(&self) -> Span {
        self.last_span
    }
}
