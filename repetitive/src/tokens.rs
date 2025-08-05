use proc_macro2::{Delimiter, Group, TokenStream};
use quote::ToTokens;
use syn::{Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone, Default)]
pub struct Tokens {
    segments: Vec<TokensSegment>,
}

#[derive(Debug, Clone)]
enum TokensSegment {
    TokenStream(TokenStream),
    Group(TokensGroup),
    Fragment(FragmentOuter),
}

#[derive(Debug, Clone)]
struct TokensGroup {
    delimiter: Delimiter,
    tokens: Tokens,
}

impl ContextParse for Tokens {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
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
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        if let Some(group) = input.parse::<Option<Group>>()? {
            let tokens = Tokens::ctx_parse.ctx_parse2(group.stream(), ctx)?;

            Ok(TokensSegment::Group(TokensGroup {
                delimiter: group.delimiter(),
                tokens,
            }))
        } else if input.peek(Token![@]) {
            let fragment = FragmentOuter::ctx_parse(input, ctx)?;
            Ok(TokensSegment::Fragment(fragment))
        } else {
            let tokens = input.parse::<TokenStream>()?;
            Ok(TokensSegment::TokenStream(tokens))
        }
    }
}

impl Tokens {
    pub fn paste(
        &self,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> syn::Result<()> {
        for segment in &self.segments {
            match segment {
                TokensSegment::TokenStream(stream) => stream.to_tokens(output),

                TokensSegment::Group(group) => {
                    let mut group_tokens = TokenStream::new();
                    group.tokens.paste(&mut group_tokens, ctx, namespace)?;
                    let group = Group::new(group.delimiter, group_tokens);
                    group.to_tokens(output);
                }

                TokensSegment::Fragment(fragment) => {
                    fragment.clone().paste(output, ctx, namespace)?;
                }
            }
        }

        Ok(())
    }
}
