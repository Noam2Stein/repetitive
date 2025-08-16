use proc_macro2::Group;
use syn::{Token, parse::ParseStream, token::Bracket};

use super::*;

#[derive(Debug, Clone)]
pub enum Pattern {
    Empty,
    Name(Name),
    Literal(Value),
    List(Vec<Pattern>),
}

pub enum PatternMatches {
    Matches,
    Mismatched(Error),
    Unknown(UnknownGuard),
}

impl Pattern {
    pub fn peek(input: ParseStream) -> bool {
        input.peek(Bracket) || Name::peek(input) || Expr::peek(input) || input.peek(Token![_])
    }

    pub fn matches(&self, value: &Value, ctx: &mut Context) -> PatternMatches {
        match self {
            Self::Empty => PatternMatches::Matches,
            Self::Name(_) => PatternMatches::Matches,

            Self::Literal(lit) => {
                let is_same_kind = match (&lit.kind, &value.kind) {
                    (ValueKind::Bool(_), ValueKind::Bool(_)) => true,
                    (ValueKind::Int(_), ValueKind::Int(_)) => true,
                    (ValueKind::Float(_), ValueKind::Float(_)) => true,
                    (ValueKind::String(_), ValueKind::String(_)) => true,
                    (ValueKind::Char(_), ValueKind::Char(_)) => true,
                    (ValueKind::Ident(_), ValueKind::Ident(_)) => true,
                    _ => false,
                };

                if !is_same_kind {
                    return PatternMatches::Mismatched(Error::PatternKindMismatch {
                        span: value.span,
                        expected: lit.kind.kind_str(),
                        found: value.kind.kind_str(),
                    });
                }

                let eq = match Op::Eq(value.span).compute(&[lit.clone(), value.clone()], ctx) {
                    Ok(val) => val,
                    Err(err) => {
                        return PatternMatches::Unknown(ctx.push_error(err).unknown_guard());
                    }
                };

                let ValueKind::Bool(eq) = eq.kind else {
                    if let ValueKind::Unknown(guard) = &eq.kind {
                        return PatternMatches::Unknown(*guard);
                    }

                    unreachable!("pattern literal eq must be a bool");
                };

                match eq {
                    true => PatternMatches::Matches,
                    false => {
                        PatternMatches::Mismatched(Error::PatternValueMismatch { span: value.span })
                    }
                }
            }

            Self::List(pat) => {
                let ValueKind::List(value_list) = &value.kind else {
                    return PatternMatches::Mismatched(Error::PatternKindMismatch {
                        span: value.span,
                        expected: "list",
                        found: value.kind.kind_str(),
                    });
                };

                if pat.len() != value_list.len() {
                    return PatternMatches::Mismatched(Error::PatternListLengthMismatch {
                        span: value.span,
                        expected: pat.len(),
                        found: value_list.len(),
                    });
                }

                for (pat_item, value_item) in pat.iter().zip(value_list) {
                    match pat_item.matches(value_item, ctx) {
                        PatternMatches::Matches => {}
                        PatternMatches::Mismatched(err) => return PatternMatches::Mismatched(err),
                        PatternMatches::Unknown(guard) => return PatternMatches::Unknown(guard),
                    }
                }

                PatternMatches::Matches
            }
        }
    }

    pub fn queue_insert(&self, value_expr: Value, namespace: &mut Namespace, ctx: &mut Context) {
        match self.matches(&value_expr, ctx) {
            PatternMatches::Matches => {}
            PatternMatches::Mismatched(err) => {
                ctx.push_error(err);
                return;
            }
            PatternMatches::Unknown(_) => return,
        }

        match self {
            Pattern::Name(name) => {
                namespace.queue_insert(*name, value_expr, ctx);
            }

            Pattern::Empty => {}
            Pattern::Literal(_) => {}

            Pattern::List(pat) => {
                let ValueKind::List(value) = value_expr.kind else {
                    unreachable!("pattern list must be a list");
                };

                for (pat, value) in pat.iter().zip(value) {
                    pat.queue_insert(value, namespace, ctx);
                }
            }
        }
    }
}

impl ContextParse for Pattern {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        if input.peek(Bracket) {
            let group = Group::ctx_parse(input, ctx)?;

            return Ok(Self::List(
                ctx_parse_punctuated.ctx_parse2(group.stream(), ctx)?,
            ));
        }

        if Name::peek(input) {
            return Ok(Self::Name(Name::ctx_parse(input, ctx)?));
        }

        if let Some(lit) = Value::ctx_parse_option_lit(input, ctx)? {
            return Ok(Self::Literal(lit));
        }

        if let Some(_) = <Option<Token![_]>>::ctx_parse(input, ctx)? {
            return Ok(Self::Empty);
        }

        Err(Error::ParseError(input.error("expected pattern")))
    }
}

fn ctx_parse_punctuated<T: ContextParse>(
    input: ParseStream,
    ctx: &mut Context,
) -> Result<Vec<T>, Error> {
    let mut items = Vec::new();

    while !input.is_empty() {
        let item = T::ctx_parse(input, ctx)?;
        items.push(item);

        if input.is_empty() {
            break;
        }

        <Token![,]>::ctx_parse(input, ctx)?;
    }

    Ok(items)
}
