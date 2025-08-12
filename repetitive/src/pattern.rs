use proc_macro2::Group;
use syn::{Token, parse::ParseStream, token::Bracket};

use super::*;

#[derive(Debug, Clone)]
pub enum Pattern {
    Name(Name),
    Literal(FragmentValueExpr),
    List(Vec<Pattern>),
}

impl Pattern {
    pub fn peek(input: ParseStream) -> bool {
        input.peek(Bracket) || Name::peek(input) || FragmentExpr::peek(input)
    }

    pub fn matches(
        &self,
        value: &FragmentValueExpr,
        ctx: &mut Context,
    ) -> syn::Result<syn::Result<()>> {
        Ok(match self {
            Self::Name(_) => Ok(()),

            Self::Literal(lit) => {
                let eq =
                    Op::Eq(value.span).compute(&[lit.value.clone(), value.value.clone()], ctx)?;

                let FragmentValue::Bool(eq) = eq else {
                    unreachable!();
                };

                match eq {
                    true => Ok(()),
                    false => Err(syn::Error::new(value.span, "value does not match pattern")),
                }
            }

            Self::List(pat) => {
                let FragmentValue::List(value_list) = &value.value else {
                    return Ok(Err(syn::Error::new(
                        value.span,
                        "value does not match pattern. pattern is a list",
                    )));
                };

                if pat.len() != value_list.len() {
                    return Ok(Err(syn::Error::new(
                        value.span,
                        "value does not match pattern. incorrect list length",
                    )));
                }

                pat.iter()
                    .zip(value_list)
                    .map(|(pat_item, value_item)| {
                        pat_item.matches(
                            &FragmentValueExpr {
                                span: value.span,
                                value: value_item.clone(),
                            },
                            ctx,
                        )
                    })
                    .collect::<syn::Result<Vec<syn::Result<_>>>>()?
                    .into_iter()
                    .collect::<syn::Result<Vec<_>>>()
                    .map(|_| ())
            }
        })
    }

    pub fn queue_insert(
        &self,
        value_expr: FragmentValueExpr,
        namespace: &mut Namespace,
        ctx: &mut Context,
    ) -> syn::Result<()> {
        if let Err(e) = self.matches(&value_expr, ctx)? {
            return Err(e);
        }

        Ok(match self {
            Pattern::Name(name) => {
                namespace.queue_insert(*name, value_expr.value, ctx)?;
            }

            Pattern::Literal(_) => {}

            Pattern::List(pat) => {
                let FragmentValue::List(value) = value_expr.value else {
                    unreachable!();
                };

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

        if Name::peek(input) {
            return Ok(Self::Name(Name::ctx_parse(input, ctx)?));
        }

        if let Some(lit) = FragmentValueExpr::option_lit(input)? {
            return Ok(Self::Literal(lit));
        }

        Err(syn::Error::new(input.span(), "expected pattern"))
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
