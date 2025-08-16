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
    Fragment(FragmentOuter),
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
            let fragment = FragmentOuter::ctx_parse(input, ctx)?;
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

impl Paste for Tokens {
    fn paste(
        &self,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> Result<(), Error> {
        for segment in &self.segments {
            match segment {
                TokensSegment::TokenStream(seg) => seg.paste(output, ctx, namespace)?,
                TokensSegment::Group(seg) => seg.paste(output, ctx, namespace)?,
                TokensSegment::Fragment(seg) => seg.paste(output, ctx, namespace)?,
            }
        }

        Ok(())
    }
}
impl Paste for TokensGroup {
    fn paste(
        &self,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> Result<(), Error> {
        let mut group_namespace = namespace.fork();

        let mut group_tokens = TokenStream::new();
        self.tokens
            .paste(&mut group_tokens, ctx, &mut group_namespace)?;

        let group = Group::new(self.delimiter, group_tokens);
        group.to_tokens(output);

        Ok(())
    }
}
