use proc_macro2::Group;
use syn::{Token, parse::ParseStream, token::Bracket};

use super::*;

#[derive(Debug, Clone)]
pub enum Pattern {
    Empty,
    Name(Name),
    Literal(FragmentValue),
    List(Vec<Pattern>),
}

impl Pattern {
    pub fn peek(input: ParseStream) -> bool {
        input.peek(Bracket)
            || Name::peek(input)
            || FragmentExpr::peek(input)
            || input.peek(Token![_])
    }

    pub fn matches(
        &self,
        value: &FragmentValue,
        ctx: &mut Context,
    ) -> syn::Result<syn::Result<()>> {
        Ok(match self {
            Self::Empty => Ok(()),
            Self::Name(_) => Ok(()),

            Self::Literal(lit) => {
                let is_same_kind = match (&lit.kind, &value.kind) {
                    (FragmentValueKind::Bool(_), FragmentValueKind::Bool(_)) => true,
                    (FragmentValueKind::Int(_), FragmentValueKind::Int(_)) => true,
                    (FragmentValueKind::Float(_), FragmentValueKind::Float(_)) => true,
                    (FragmentValueKind::String(_), FragmentValueKind::String(_)) => true,
                    (FragmentValueKind::Char(_), FragmentValueKind::Char(_)) => true,
                    (FragmentValueKind::Ident(_), FragmentValueKind::Ident(_)) => true,
                    _ => false,
                };

                if !is_same_kind {
                    return Ok(Err(syn::Error::new(
                        value.span,
                        "value does not match pattern. different types",
                    )));
                }

                let eq = Op::Eq(value.span).compute(&[lit.clone(), value.clone()], ctx)?;

                let FragmentValueKind::Bool(eq) = eq.kind else {
                    unreachable!();
                };

                match eq {
                    true => Ok(()),
                    false => Err(syn::Error::new(value.span, "value does not match pattern")),
                }
            }

            Self::List(pat) => {
                let FragmentValueKind::List(value_list) = &value.kind else {
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
                    .map(|(pat_item, value_item)| pat_item.matches(value_item, ctx))
                    .collect::<syn::Result<Vec<syn::Result<_>>>>()?
                    .into_iter()
                    .collect::<syn::Result<Vec<_>>>()
                    .map(|_| ())
            }
        })
    }

    pub fn queue_insert(
        &self,
        value_expr: FragmentValue,
        namespace: &mut Namespace,
        ctx: &mut Context,
    ) -> syn::Result<()> {
        if let Err(e) = self.matches(&value_expr, ctx)? {
            return Err(e);
        }

        Ok(match self {
            Pattern::Name(name) => {
                namespace.queue_insert(*name, value_expr, ctx)?;
            }

            Pattern::Empty => {}
            Pattern::Literal(_) => {}

            Pattern::List(pat) => {
                let FragmentValueKind::List(value) = value_expr.kind else {
                    unreachable!();
                };

                for (pat, value) in pat.iter().zip(value) {
                    pat.queue_insert(value, namespace, ctx)?;
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

        if let Some(lit) = FragmentValue::option_lit(input)? {
            return Ok(Self::Literal(lit));
        }

        if let Some(_) = input.parse::<Option<Token![_]>>()? {
            return Ok(Self::Empty);
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
