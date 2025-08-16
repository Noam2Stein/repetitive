use std::mem::take;

use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt};
use syn::{
    Token,
    parse::ParseStream,
    token::{Brace, Bracket, Paren},
};

use super::*;

#[derive(Debug, Clone, Default)]
pub struct Tokens {
    segments: Vec<TokensSegment>,
}

#[derive(Debug, Clone)]
enum TokensSegment {
    TokenStream(TokenStream),
    Group(TokensGroup),
    Fragment(Fragment),
}

#[derive(Debug, Clone)]
struct TokensGroup {
    delimiter: Delimiter,
    tokens: Tokens,
}

impl ContextParse for Tokens {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let mut segments = Vec::new();

        while !input.is_empty() {
            let segment = TokensSegment::ctx_parse(input, ctx)?;
            segments.push(segment);
        }

        Ok(Self { segments })
    }
}
impl ContextParse for TokensSegment {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        if let Some(group) = Option::<Group>::ctx_parse(input, ctx)? {
            let tokens = Tokens::ctx_parse.ctx_parse2(group.stream(), ctx)?;

            Ok(TokensSegment::Group(TokensGroup {
                delimiter: group.delimiter(),
                tokens,
            }))
        } else if input.peek(Token![@]) {
            let fragment = Fragment::ctx_parse(input, ctx)?;
            Ok(TokensSegment::Fragment(fragment))
        } else {
            let mut stream = TokenStream::new();

            while !input.is_empty()
                && !input.peek(Token![@])
                && !input.peek(Paren)
                && !input.peek(Brace)
                && !input.peek(Bracket)
            {
                stream.append(TokenTree::ctx_parse(input, ctx)?);
            }

            Ok(TokensSegment::TokenStream(stream))
        }
    }
}

impl Optimize for Tokens {
    fn optimize(&mut self, ctx: &mut Context) {
        let mut new_self = Self::default();

        for mut segment in self.segments.drain(..) {
            segment.optimize(ctx);

            if let Some(TokensSegment::TokenStream(last_segment)) = new_self.segments.last_mut()
                && let TokensSegment::TokenStream(segment) = segment
            {
                segment.to_tokens(last_segment);
            } else {
                new_self.segments.push(segment);
            }
        }

        *self = new_self;
    }
}
impl Optimize for TokensSegment {
    fn optimize(&mut self, ctx: &mut Context) {
        match self {
            TokensSegment::TokenStream(_) => {}

            TokensSegment::Group(group) => {
                group.tokens.optimize(ctx);

                if let [TokensSegment::TokenStream(_)] = group.tokens.segments.as_slice() {
                    let Some(TokensSegment::TokenStream(stream)) =
                        take(&mut group.tokens.segments).into_iter().nth(0)
                    else {
                        unreachable!("single token stream segment group");
                    };

                    let group = Group::new(group.delimiter, stream);
                    *self = TokensSegment::TokenStream(group.to_token_stream());
                }
            }

            TokensSegment::Fragment(frag) => {
                frag.optimize(ctx);
            }
        }
    }
}

impl Expand for Tokens {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace) {
        let mut namespace_fork = namespace.fork();

        for segment in &self.segments {
            match segment {
                TokensSegment::TokenStream(seg) => seg.expand(output, ctx, &namespace_fork),
                TokensSegment::Group(seg) => seg.expand(output, ctx, &namespace_fork),
                TokensSegment::Fragment(seg) => seg.expand_into(output, ctx, &mut namespace_fork),
            }
        }
    }
}
impl Expand for TokensGroup {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace) {
        let mut group_tokens = TokenStream::new();
        self.tokens.expand(&mut group_tokens, ctx, namespace);

        let group = Group::new(self.delimiter, group_tokens);
        group.to_tokens(output);
    }
}
