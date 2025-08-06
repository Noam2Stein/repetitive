use proc_macro2::Group;
use syn::{Token, parse::ParseStream, token::Bracket};

use super::*;

#[derive(Debug, Clone)]
pub enum Pattern {
    Name(Name),
    List(Vec<Pattern>),
}

impl Pattern {
    pub fn peek(input: ParseStream) -> bool {
        input.peek(Bracket) || Name::peek(input)
    }

    pub fn queue_insert(
        &self,
        value_expr: FragmentValueExpr,
        namespace: &mut Namespace,
        ctx: &mut Context,
    ) -> syn::Result<()> {
        Ok(match self {
            Pattern::Name(name) => {
                namespace.queue_insert(*name, value_expr.value, ctx)?;
            }

            Pattern::List(pat) => {
                let value = match &value_expr.value {
                    FragmentValue::List(list) => {
                        list.iter().map(|item| item.clone()).collect::<Vec<_>>()
                    }

                    _ => {
                        return Err(syn::Error::new(value_expr.span, "expected a list"));
                    }
                };

                if pat.len() != value.len() {
                    return Err(syn::Error::new(
                        value_expr.span,
                        "non compatible list length",
                    ));
                }

                for (pat, value) in pat.iter().zip(value) {
                    let value_expr = FragmentValueExpr {
                        span: value_expr.span,
                        value,
                    };

                    pat.queue_insert(value_expr, namespace, ctx)?;
                }
            }
        })
    }
}

impl ContextParse for Pattern {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        if input.peek(Bracket) {
            let group = input.parse::<Group>()?;

            return Ok(Self::List(
                ctx_parse_punctuated.ctx_parse2(group.stream(), ctx)?,
            ));
        }

        Ok(Self::Name(Name::ctx_parse(input, ctx)?))
    }
}

fn ctx_parse_punctuated<T: ContextParse>(
    input: ParseStream,
    ctx: &mut Context,
) -> syn::Result<Vec<T>> {
    let mut items = Vec::new();

    while !input.is_empty() {
        let item = T::ctx_parse(input, ctx)?;
        items.push(item);

        if input.is_empty() {
            break;
        }

        input.parse::<Token![,]>()?;
    }

    Ok(items)
}
