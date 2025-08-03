use proc_macro2::{Delimiter, Group, TokenStream};
use quote::ToTokens;
use syn::Token;

use super::*;

#[derive(Debug, Clone)]
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
    fn ctx_parse(input: syn::parse::ParseStream, ctx: &mut crate::Context) -> syn::Result<Self>
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
    fn ctx_parse(input: syn::parse::ParseStream, ctx: &mut crate::Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        if let Some(group) = input.parse::<Option<Group>>()? {
            let tokens = ctx_parse2(Tokens::ctx_parse, group.stream(), ctx)?;

            Ok(TokensSegment::Group(TokensGroup {
                delimiter: group.delimiter(),
                tokens,
            }))
        } else if input.peek(Token![@]) {
            let fragment = Fragment::ctx_parse_outer(input, ctx)?;
            Ok(TokensSegment::Fragment(fragment))
        } else {
            let tokens = input.parse::<TokenStream>()?;
            Ok(TokensSegment::TokenStream(tokens))
        }
    }
}

impl Tokens {
    pub fn end(&self, tokens: &mut TokenStream) {
        for segment in &self.segments {
            match segment {
                TokensSegment::TokenStream(stream) => stream.to_tokens(tokens),

                TokensSegment::Group(group) => {
                    let mut group_tokens = TokenStream::new();
                    group.tokens.end(&mut group_tokens);
                    let group = Group::new(group.delimiter, group_tokens);
                    group.to_tokens(tokens);
                }

                TokensSegment::Fragment(fragment) => fragment.end(tokens),
            }
        }
    }
}
