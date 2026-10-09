use std::{collections::VecDeque, mem::transmute};

use crate::{
    diagnostics::{Diagnostics, EmittedError, syntax_errors as errors},
    proc_macro12::{Group, Span, TokenStream, TokenTree, token_stream},
};

#[derive(Clone)]
#[repr(transparent)]
pub struct TokenIter(Inner);

#[derive(Clone)]
struct Inner {
    raw_iter: token_stream::IntoIter,
    next_queue: VecDeque<TokenTree>,
    last_span: Span,
}

impl Iterator for TokenIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        let result = if let Some(result) = self.0.next_queue.pop_front() {
            result
        } else {
            self.0.raw_iter.next()?
        };

        self.update_last_span(&result);

        Some(result)
    }
}

impl TokenIter {
    pub fn from_root_stream(stream: TokenStream) -> Self {
        Self(Inner {
            raw_iter: stream.into_iter(),
            next_queue: VecDeque::new(),
            last_span: Span::call_site(),
        })
    }

    pub fn from_group(group: &Group) -> Self {
        Self(Inner {
            raw_iter: group.stream().into_iter(),
            next_queue: VecDeque::new(),
            last_span: group.span_open(),
        })
    }

    pub fn finish(self, diagnostics: &Diagnostics) -> Result<(), EmittedError> {
        // SAFETY: `TokenIter` is a transparent wrapper of `Inner`.
        let mut inner = unsafe { transmute::<TokenIter, Inner>(self) };

        let leftover_token = inner
            .next_queue
            .pop_front()
            .or_else(|| inner.raw_iter.next());

        if let Some(leftover_token) = leftover_token {
            Err(diagnostics.emit_error(errors::leftover_token(leftover_token.span())))
        } else {
            Ok(())
        }
    }

    pub fn peek(&mut self) -> Option<&TokenTree> {
        /*
        This commented-out implementation would be better, but it does not
        compile because of borrow checker limitations.

        Some(if let Some(peek) = self.queue.front() {
            peek
        } else {
            self.queue.push_back_mut(self.iter.next()?)
        })
        */

        if self.0.next_queue.is_empty() {
            self.0.next_queue.push_back(self.0.raw_iter.next()?);
        }
        self.0.next_queue.front()
    }

    pub fn peek_n<const N: usize>(&mut self) -> Option<[&TokenTree; N]> {
        while self.0.next_queue.len() < N {
            self.0.next_queue.push_back(self.0.raw_iter.next()?);
        }

        let mut queue_iter = self.0.next_queue.iter();
        Some(std::array::from_fn(|_| queue_iter.next().unwrap()))
    }

    pub fn next_if(&mut self, func: impl FnOnce(&TokenTree) -> bool) -> Option<TokenTree> {
        func(self.peek()?).then(|| {
            let result = self.0.next_queue.pop_front().unwrap();
            self.update_last_span(&result);
            result
        })
    }

    pub fn next_n_if<const N: usize>(
        &mut self,
        func: impl FnOnce([&TokenTree; N]) -> bool,
    ) -> Option<[TokenTree; N]> {
        func(self.peek_n()?).then(|| {
            let result = std::array::from_fn(|_| self.0.next_queue.pop_front().unwrap());

            if let Some(result_last) = result.last() {
                self.update_last_span(result_last);
            }

            result
        })
    }

    pub fn last_span(&self) -> Span {
        self.0.last_span
    }

    fn update_last_span(&mut self, last_token: &TokenTree) {
        self.0.last_span = match &last_token {
            TokenTree::Group(token) => token.span_close(),
            token => token.span(),
        };
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
    fn test_next() {
        let stream = random_token_stream();

        let expected_iter = stream.clone().into_iter();
        let actual_iter = TokenIter::from_root_stream(stream);

        assert!(token_iter_eq(expected_iter, actual_iter));
    }

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

                let mut call_next_n = token_iter.clone();
                for _ in 0..$N {
                    call_next_n.next().unwrap();
                }
                token_iter.next_n_if::<$N>(|_| true).unwrap();
                assert!(token_iter_eq(token_iter.clone(), call_next_n));
            };
        }
        test_n!(1);
        test_n!(3);
        test_n!(2);
        test_n!(4);
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

    fn token_iter_eq(
        mut a: impl Iterator<Item = TokenTree>,
        mut b: impl Iterator<Item = TokenTree>,
    ) -> bool {
        loop {
            match (a.next(), b.next()) {
                (Some(a), Some(b)) => {
                    if !token_eq(a, b) {
                        return false;
                    }
                }
                (Some(_), None) | (None, Some(_)) => return false,
                (None, None) => return true,
            }
        }
    }
}
