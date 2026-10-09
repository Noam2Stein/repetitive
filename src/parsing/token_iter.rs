use std::{collections::VecDeque, mem::transmute};

use crate::{
    diagnostics::{Diagnostics, EmittedError, syntax_errors as errors},
    proc_macro12::{Group, Span, TokenStream, TokenTree, token_stream},
};

/// An iterator over a token-stream used for parsing.
///
/// Compared to [`proc_macro::token_stream::IntoIter`], this iterator has a few
/// additional features:
///
/// - It supports multi-token peeking. This is used, for example, to detect
///   `$else` when parsing meta-if statements.
///
/// - When created from a [`Group`], it remembers the span of the closing
///   delimiter. This is used for errors where a token-stream ends unexpectedly.
///
/// [`TokenIter`] must not be implicitly dropped (its implementation of
/// [`Drop`] panics in order to enforce this). To destruct an iterator, use
/// [`TokenIter::finish`] which emits an error if there are leftover tokens.
/// This prevents bugs where leftover tokens are forgotten about without
/// an error being emitted.
#[derive(Clone)]
#[repr(transparent)]
pub struct TokenIter(Inner);

/// [`TokenIter`] is separated into an inner struct so that its fields can be
/// dropped without its drop glue panicking.
#[derive(Clone)]
struct Inner {
    into_iter: token_stream::IntoIter,
    queue: VecDeque<TokenTree>,
    closing_span: Span,
}

impl Iterator for TokenIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.queue.pop_front().or_else(|| self.0.into_iter.next())
    }
}

impl TokenIter {
    /// Creates a token-iterator from the root token-stream.
    ///
    /// This should not be used with a token-stream that comes from a [`Group`].
    /// For that, use [`Self::from_group`] which retains the span of the group's
    /// delimiters.
    pub fn from_root_stream(stream: TokenStream) -> Self {
        Self(Inner {
            into_iter: stream.into_iter(),
            queue: VecDeque::new(),
            closing_span: Span::call_site(),
        })
    }

    /// Creates a token-iterator over [`Group::stream`] and retains the span of
    /// the group's delimiters.
    pub fn from_group(group: &Group) -> Self {
        Self(Inner {
            into_iter: group.stream().into_iter(),
            queue: VecDeque::new(),
            closing_span: group.span_close(),
        })
    }

    /// Destructs a token-iterator, emitting an error if there are leftover
    /// tokens.
    ///
    /// This is the intended way to destruct a [`TokenIter`] (its implementation
    /// of [`Drop`] panics in order to enforce this).
    pub fn finish(self, diagnostics: &Diagnostics) -> Result<(), EmittedError> {
        // Move the fields of `self` without running its panicking drop glue.
        // There does not seem to be a better way to do this.
        // SAFETY: `TokenIter` is a transparent wrapper of `Inner`.
        let mut inner = unsafe { transmute::<TokenIter, Inner>(self) };

        let leftover_token = inner.queue.pop_front().or_else(|| inner.into_iter.next());

        if let Some(leftover_token) = leftover_token {
            Err(diagnostics.emit_error(errors::leftover_token(leftover_token.span())))
        } else {
            Ok(())
        }
    }

    pub fn peek(&mut self) -> Option<&TokenTree> {
        /*
        This commented-out implementation would be better, but it does not
        compile because of borrow checker limitations. When those limitations
        are gone, this implementation should be used.

        Some(if let Some(peek) = self.queue.front() {
            peek
        } else {
            self.queue.push_back_mut(self.iter.next()?)
        })
        */

        if self.0.queue.is_empty() {
            self.0.queue.push_back(self.0.into_iter.next()?);
        }
        Some(self.0.queue.front().unwrap())
    }

    pub fn peek_n<const N: usize>(&mut self) -> Option<[&TokenTree; N]> {
        while self.0.queue.len() < N {
            self.0.queue.push_back(self.0.into_iter.next()?);
        }

        let mut queue_iter = self.0.queue.iter();
        Some(std::array::from_fn(|_| queue_iter.next().unwrap()))
    }

    pub fn next_if(&mut self, func: impl FnOnce(&TokenTree) -> bool) -> Option<TokenTree> {
        func(self.peek()?).then(|| self.0.queue.pop_front().unwrap())
    }

    pub fn next_n_if<const N: usize>(
        &mut self,
        func: impl FnOnce([&TokenTree; N]) -> bool,
    ) -> Option<[TokenTree; N]> {
        func(self.peek_n()?).then(|| std::array::from_fn(|_| self.0.queue.pop_front().unwrap()))
    }

    /// If the token-iterator originates from a [`Group`], this returns the span
    /// of its closing delimiter. Otherwise, this returns [`Span::call_site`].
    pub fn closing_span(&self) -> Span {
        self.0.closing_span
    }

    #[cfg(test)]
    fn forget(self) {
        // Drop the fields of `self` without running its panicking drop glue.
        // There does not seem to be a better way to do this.
        // SAFETY: `TokenIter` is a transparent wrapper of `Inner`.
        drop(unsafe { transmute::<TokenIter, Inner>(self) });
    }
}

impl Drop for TokenIter {
    fn drop(&mut self) {
        panic!("forgot to call `TokenIter::finish`")
    }
}

#[cfg(test)]
mod tests {
    use proc_macro2::{
        Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
    };

    use crate::parsing::TokenIter;

    #[test]
    fn test_peek() {
        let mut token_iter = TokenIter::from_root_stream(random_token_stream());
        for _ in 0..5 {
            token_iter.next().unwrap();
        }

        for peek_count in 0..5 {
            let pre_call = token_iter.clone();
            for _ in 0..peek_count {
                token_iter.peek().unwrap();
            }

            assert!(token_iter_eq(token_iter.clone(), pre_call));
        }

        token_iter.forget();
    }

    #[test]
    fn test_peek_n() {
        let mut token_iter = TokenIter::from_root_stream(random_token_stream());
        for _ in 0..5 {
            token_iter.next().unwrap();
        }

        macro_rules! test_n {
            ($N:literal) => {
                let pre_call = token_iter.clone();
                token_iter.peek_n::<$N>().unwrap();

                assert!(token_iter_eq(token_iter.clone(), pre_call));
            };
        }
        test_n!(1);
        test_n!(3);
        test_n!(2);
        test_n!(4);

        token_iter.forget();
    }

    #[test]
    fn test_next_if() {
        let mut token_iter = TokenIter::from_root_stream(random_token_stream());
        for _ in 0..5 {
            token_iter.next().unwrap();
        }

        for peek_count in 0..5 {
            let pre_call = token_iter.clone();
            for _ in 0..peek_count {
                token_iter.next_if(|_| false);
            }

            assert!(token_iter_eq(token_iter.clone(), pre_call));

            let mut call_next = token_iter.clone();
            call_next.next().unwrap();
            token_iter.next_if(|_| true).unwrap();
            assert!(token_iter_eq(token_iter.clone(), call_next));
        }

        token_iter.forget();
    }

    #[test]
    fn test_next_n_if() {
        let mut token_iter = TokenIter::from_root_stream(random_token_stream());
        for _ in 0..5 {
            token_iter.next().unwrap();
        }

        macro_rules! test_n {
            ($N:literal) => {
                let pre_call = token_iter.clone();
                token_iter.next_n_if::<$N>(|_| false);
                assert!(token_iter_eq(token_iter.clone(), pre_call));

                let mut call_next_n_times = token_iter.clone();
                for _ in 0..$N {
                    call_next_n_times.next().unwrap();
                }
                token_iter.next_n_if::<$N>(|_| true).unwrap();
                assert!(token_iter_eq(token_iter.clone(), call_next_n_times));
            };
        }
        test_n!(1);
        test_n!(3);
        test_n!(2);
        test_n!(4);

        token_iter.forget();
    }

    fn random_token_stream() -> TokenStream {
        let random_numbers = [1, 2, 3, 4, 0, 2, 1, 3, 0, 3, 4, 2, 1, 4, 1, 0]
            .into_iter()
            .cycle()
            .take(1000);

        let random_tokens = random_numbers.map(|number: u32| match number {
            0 => TokenTree::Group(Group::new(Delimiter::Brace, TokenStream::new())),
            1 => TokenTree::Ident(Ident::new("string", Span::call_site())),
            2 => TokenTree::Literal(Literal::string("string")),
            3 => TokenTree::Punct(Punct::new('!', Spacing::Alone)),
            4 => TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
            5.. => unreachable!("the input numbers are between `0` and `4`"),
        });

        random_tokens.collect()
    }

    fn token_eq(a: TokenTree, b: TokenTree) -> bool {
        match (a, b) {
            (TokenTree::Group(a), TokenTree::Group(b)) => a.delimiter() == b.delimiter(),
            (TokenTree::Group(_), _) | (_, TokenTree::Group(_)) => false,
            (TokenTree::Ident(a), TokenTree::Ident(b)) => a == b,
            (TokenTree::Ident(_), _) | (_, TokenTree::Ident(_)) => false,
            (TokenTree::Literal(a), TokenTree::Literal(b)) => a.to_string() == b.to_string(),
            (TokenTree::Literal(_), _) | (_, TokenTree::Literal(_)) => false,
            (TokenTree::Punct(a), TokenTree::Punct(b)) => a.as_char() == b.as_char(),
        }
    }

    fn token_iter_eq(mut a: TokenIter, mut b: TokenIter) -> bool {
        let result = loop {
            match (a.next(), b.next()) {
                (Some(a), Some(b)) => {
                    if !token_eq(a, b) {
                        break false;
                    }
                }
                (Some(_), None) | (None, Some(_)) => break false,
                (None, None) => break true,
            }
        };

        a.forget();
        b.forget();

        result
    }

    #[test]
    fn test_token_eq() {
        for token in random_token_stream() {
            assert!(token_eq(token.clone(), token));
        }
    }

    #[test]
    fn test_token_iter_eq() {
        let token_iter = TokenIter::from_root_stream(random_token_stream());
        assert!(token_iter_eq(token_iter.clone(), token_iter));

        let a = TokenIter::from_root_stream(random_token_stream());
        let b = {
            let mut b = a.clone();
            let _ = b.next();
            b
        };
        assert!(!token_iter_eq(a, b));
    }
}
