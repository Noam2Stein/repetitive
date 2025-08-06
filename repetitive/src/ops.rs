use proc_macro2::Span;
use syn::{Error, Ident, Token, parse::ParseStream, spanned::Spanned};

use super::*;

#[derive(Debug, Clone, Copy)]
pub enum Op {
    IfElse(Span),

    Neg(Span),
    Not(Span),

    Add(Span),
    Sub(Span),
    Mul(Span),
    Div(Span),
    Rem(Span),
    BitAnd(Span),
    BitOr(Span),
    BitXor(Span),
    Shl(Span),
    Shr(Span),

    Eq(Span),
    Ne(Span),
    Lt(Span),
    Gt(Span),
    Le(Span),
    Ge(Span),

    And(Span),
    Or(Span),
    #[allow(dead_code)]
    Xor(Span),

    Range(Span),
    RangeInclusive(Span),

    Len(Span),
    Index(Span),
    Enumerate(Span),
    Zip(Span),
    Chain(Span),

    ConcatIdent(Span),
    ConcatString(Span),
}

impl Op {
    pub fn span(&self) -> Span {
        match self {
            Self::IfElse(span) => *span,
            Self::Neg(span) => *span,
            Self::Not(span) => *span,
            Self::Add(span) => *span,
            Self::Sub(span) => *span,
            Self::Mul(span) => *span,
            Self::Div(span) => *span,
            Self::Rem(span) => *span,
            Self::BitAnd(span) => *span,
            Self::BitOr(span) => *span,
            Self::BitXor(span) => *span,
            Self::Shl(span) => *span,
            Self::Shr(span) => *span,
            Self::Eq(span) => *span,
            Self::Ne(span) => *span,
            Self::Lt(span) => *span,
            Self::Gt(span) => *span,
            Self::Le(span) => *span,
            Self::Ge(span) => *span,
            Self::And(span) => *span,
            Self::Or(span) => *span,
            Self::Xor(span) => *span,
            Self::Range(span) => *span,
            Self::RangeInclusive(span) => *span,
            Self::Len(span) => *span,
            Self::Index(span) => *span,
            Self::Enumerate(span) => *span,
            Self::Zip(span) => *span,
            Self::Chain(span) => *span,
            Self::ConcatIdent(span) => *span,
            Self::ConcatString(span) => *span,
        }
    }
}

impl Op {
    pub fn parse_option_un(input: ParseStream) -> Option<Self> {
        if let Some(tok) = input.parse::<Option<Token![!]>>().unwrap() {
            Some(Op::Not(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![-]>>().unwrap() {
            Some(Op::Neg(tok.span()))
        } else {
            None
        }
    }

    pub fn parse_option_bin(input: ParseStream) -> Option<Self> {
        if let Some(tok) = input.parse::<Option<Token![+]>>().unwrap() {
            Some(Op::Add(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![-]>>().unwrap() {
            Some(Op::Sub(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![*]>>().unwrap() {
            Some(Op::Mul(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![/]>>().unwrap() {
            Some(Op::Div(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![%]>>().unwrap() {
            Some(Op::Rem(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![|]>>().unwrap() {
            Some(Op::BitOr(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![^]>>().unwrap() {
            Some(Op::BitXor(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![&]>>().unwrap() {
            Some(Op::BitAnd(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![==]>>().unwrap() {
            Some(Op::Eq(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![!=]>>().unwrap() {
            Some(Op::Ne(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![<]>>().unwrap() {
            Some(Op::Lt(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![>]>>().unwrap() {
            Some(Op::Gt(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![<=]>>().unwrap() {
            Some(Op::Le(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![>=]>>().unwrap() {
            Some(Op::Ge(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![..=]>>().unwrap() {
            Some(Op::RangeInclusive(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![..]>>().unwrap() {
            Some(Op::Range(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![||]>>().unwrap() {
            Some(Op::Or(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![&&]>>().unwrap() {
            Some(Op::And(tok.span()))
        } else {
            None
        }
    }

    pub fn parse_method(ident: Ident) -> syn::Result<Self> {
        let variant_fn = match ident.to_string().as_str() {
            "not" => Op::Not,
            "neg" => Op::Neg,

            "add" => Op::Add,
            "sub" => Op::Sub,
            "mul" => Op::Mul,
            "div" => Op::Div,
            "rem" => Op::Rem,
            "bitand" => Op::BitAnd,
            "bitor" => Op::BitOr,
            "bitxor" => Op::BitXor,
            "shl" => Op::Shl,
            "shr" => Op::Shr,

            "eq" => Op::Eq,
            "ne" => Op::Ne,
            "lt" => Op::Lt,
            "gt" => Op::Gt,
            "le" => Op::Le,
            "ge" => Op::Ge,

            "len" => Op::Len,
            "index" => Op::Index,
            "enumerate" => Op::Enumerate,
            "zip" => Op::Zip,
            "chain" => Op::Chain,

            "concat_ident" => Op::ConcatIdent,
            "concat_string" => Op::ConcatString,

            _ => {
                return Err(syn::Error::new(
                    ident.span(),
                    format!("method `{}` not found", ident.to_string()),
                ));
            }
        };

        Ok(variant_fn(ident.span()))
    }

    pub fn compute(self, args: &[FragmentValue], ctx: &mut Context) -> syn::Result<FragmentValue> {
        Ok(match self {
            Self::IfElse(span) => {
                let [cond, then, otherwise] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 3 arguments, found {}", args.len()),
                    ));
                };

                let cond = match cond {
                    FragmentValue::Bool(cond) => cond,
                    _ => unreachable!(),
                };

                if *cond {
                    then.clone()
                } else {
                    otherwise.clone()
                }
            }

            Self::Neg(span) => {
                let [arg] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 1 argument, found {}", args.len()),
                    ));
                };

                match arg {
                    FragmentValue::Int(val) => FragmentValue::Int(-val),
                    FragmentValue::Float(val) => FragmentValue::Float(-val),

                    FragmentValue::Bool(_) => {
                        return Err(Error::new(span, "cannot negate `bool`"));
                    }
                    FragmentValue::Ident(_) => {
                        return Err(Error::new(span, "cannot negate `ident`"));
                    }
                    FragmentValue::String(_) => {
                        return Err(Error::new(span, "cannot negate `string`"));
                    }
                    FragmentValue::Char(_) => {
                        return Err(Error::new(span, "cannot negate `char`"));
                    }
                    FragmentValue::List(_) => {
                        return Err(Error::new(span, "cannot negate `list`"));
                    }
                    FragmentValue::Tokens(_) => {
                        return Err(Error::new(span, "cannot negate `tokens`"));
                    }
                }
            }

            Self::Not(span) => {
                let [arg] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 1 argument, found {}", args.len()),
                    ));
                };

                match arg {
                    FragmentValue::Bool(val) => FragmentValue::Bool(!val),

                    FragmentValue::Int(_) => {
                        return Err(Error::new(span, "cannot not `int`"));
                    }
                    FragmentValue::Float(_) => {
                        return Err(Error::new(span, "cannot not `float`"));
                    }
                    FragmentValue::Ident(_) => {
                        return Err(Error::new(span, "cannot not `ident`"));
                    }
                    FragmentValue::String(_) => {
                        return Err(Error::new(span, "cannot not `string`"));
                    }
                    FragmentValue::Char(_) => {
                        return Err(Error::new(span, "cannot not `char`"));
                    }
                    FragmentValue::List(_) => {
                        return Err(Error::new(span, "cannot not `list`"));
                    }
                    FragmentValue::Tokens(_) => {
                        return Err(Error::new(span, "cannot not `tokens`"));
                    }
                }
            }

            Self::Add(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Int(lhs + rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(lhs), _) => match rhs {
                        FragmentValue::Float(rhs) => FragmentValue::Float(lhs + rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    (FragmentValue::String(lhs), _) => match rhs {
                        FragmentValue::String(rhs) => FragmentValue::String(format!("{lhs}{rhs}")),
                        FragmentValue::Ident(rhs) => FragmentValue::String(format!("{lhs}{rhs}")),
                        FragmentValue::Char(rhs) => FragmentValue::String(format!("{lhs}{rhs}")),
                        _ => return Err(Error::new(span, "expected rhs to be `string`")),
                    },

                    (FragmentValue::Ident(lhs), _) => match rhs {
                        FragmentValue::Ident(rhs) => FragmentValue::Ident(format!("{lhs}{rhs}")),
                        FragmentValue::String(rhs) => FragmentValue::Ident(format!("{lhs}{rhs}")),
                        FragmentValue::Char(rhs) => FragmentValue::Ident(format!("{lhs}{rhs}")),
                        _ => return Err(Error::new(span, "expected rhs to be `ident`")),
                    },

                    (FragmentValue::Bool(_), _) => {
                        return Err(Error::new(span, "cannot add to `bool`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot add to `char`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot add to `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot add to `tokens`"));
                    }
                }
            }

            Self::Sub(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Int(lhs - rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(lhs), _) => match rhs {
                        FragmentValue::Float(rhs) => FragmentValue::Float(lhs - rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    (FragmentValue::String(_), _) => {
                        return Err(Error::new(span, "cannot subtract from `string`"));
                    }
                    (FragmentValue::Ident(_), _) => {
                        return Err(Error::new(span, "cannot subtract from `ident`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot subtract from `char`"));
                    }
                    (FragmentValue::Bool(_), _) => {
                        return Err(Error::new(span, "cannot subtract from `bool`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot subtract from `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot subtract from `tokens`"));
                    }
                }
            }

            Self::Mul(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Int(lhs * rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(lhs), _) => match rhs {
                        FragmentValue::Float(rhs) => FragmentValue::Float(lhs * rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    (FragmentValue::String(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::String(lhs.repeat(*rhs as usize)),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Ident(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Ident(lhs.repeat(*rhs as usize)),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Char(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => {
                            FragmentValue::String(lhs.to_string().repeat(*rhs as usize))
                        }
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Bool(_), _) => {
                        return Err(Error::new(span, "cannot multiply `bool`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot multiply `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot multiply `tokens`"));
                    }
                }
            }

            Self::Div(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Int(lhs / rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(lhs), _) => match rhs {
                        FragmentValue::Float(rhs) => FragmentValue::Float(lhs / rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    (FragmentValue::String(_), _) => {
                        return Err(Error::new(span, "cannot divide `string`"));
                    }
                    (FragmentValue::Ident(_), _) => {
                        return Err(Error::new(span, "cannot divide `ident`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot divide `char`"));
                    }
                    (FragmentValue::Bool(_), _) => {
                        return Err(Error::new(span, "cannot divide `bool`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot divide `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot divide `tokens`"));
                    }
                }
            }

            Self::Rem(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Int(lhs % rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(lhs), _) => match rhs {
                        FragmentValue::Float(rhs) => FragmentValue::Float(lhs % rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    (FragmentValue::String(_), _) => {
                        return Err(Error::new(span, "cannot divide `string`"));
                    }
                    (FragmentValue::Ident(_), _) => {
                        return Err(Error::new(span, "cannot divide `ident`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot divide `char`"));
                    }
                    (FragmentValue::Bool(_), _) => {
                        return Err(Error::new(span, "cannot divide `bool`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot divide `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot divide `tokens`"));
                    }
                }
            }

            Self::BitAnd(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Int(lhs & rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Bool(lhs), _) => match rhs {
                        FragmentValue::Bool(rhs) => FragmentValue::Bool(lhs & rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    (FragmentValue::Float(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `float`"));
                    }
                    (FragmentValue::String(_), _) => {
                        return Err(Error::new(span, "cannot divide `string`"));
                    }
                    (FragmentValue::Ident(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `ident`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `char`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `tokens`"));
                    }
                }
            }

            Self::BitOr(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Int(lhs | rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Bool(lhs), _) => match rhs {
                        FragmentValue::Bool(rhs) => FragmentValue::Bool(lhs | rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    (FragmentValue::String(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-or `string`"));
                    }
                    (FragmentValue::Float(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `float`"));
                    }
                    (FragmentValue::Ident(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-or `ident`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-or `char`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-or `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-or `tokens`"));
                    }
                }
            }

            Self::BitXor(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Int(lhs ^ rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Bool(lhs), _) => match rhs {
                        FragmentValue::Bool(rhs) => FragmentValue::Bool(lhs ^ rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    (FragmentValue::Float(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `float`"));
                    }
                    (FragmentValue::String(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `string`"));
                    }
                    (FragmentValue::Ident(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `ident`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `char`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `tokens`"));
                    }
                }
            }

            Self::Shl(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Int(lhs << rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(_), _) => {
                        return Err(Error::new(span, "cannot shift-left `float`"));
                    }
                    (FragmentValue::String(_), _) => {
                        return Err(Error::new(span, "cannot shift-left `string`"));
                    }
                    (FragmentValue::Ident(_), _) => {
                        return Err(Error::new(span, "cannot shift-left `ident`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot shift-left `char`"));
                    }
                    (FragmentValue::Bool(_), _) => {
                        return Err(Error::new(span, "cannot shift-left `bool`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot shift-left `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot shift-left `tokens`"));
                    }
                }
            }

            Self::Shr(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Int(lhs >> rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(_), _) => {
                        return Err(Error::new(span, "cannot shift-right `float`"));
                    }
                    (FragmentValue::String(_), _) => {
                        return Err(Error::new(span, "cannot shift-right `string`"));
                    }
                    (FragmentValue::Ident(_), _) => {
                        return Err(Error::new(span, "cannot shift-right `ident`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot shift-right `char`"));
                    }
                    (FragmentValue::Bool(_), _) => {
                        return Err(Error::new(span, "cannot shift-right `bool`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot shift-right `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot shift-right `tokens`"));
                    }
                }
            }

            Self::Eq(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(lhs), _) => match rhs {
                        FragmentValue::Float(rhs) => FragmentValue::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    (FragmentValue::String(lhs), _) => match rhs {
                        FragmentValue::String(rhs) => FragmentValue::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `string`")),
                    },

                    (FragmentValue::Ident(lhs), _) => match rhs {
                        FragmentValue::Ident(rhs) => FragmentValue::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `ident`")),
                    },

                    (FragmentValue::Char(lhs), _) => match rhs {
                        FragmentValue::Char(rhs) => FragmentValue::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `char`")),
                    },

                    (FragmentValue::Bool(lhs), _) => match rhs {
                        FragmentValue::Bool(rhs) => FragmentValue::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    (FragmentValue::List(lhs), _) => match rhs {
                        FragmentValue::List(rhs) => FragmentValue::Bool('list_eq: {
                            if lhs.len() != rhs.len() {
                                break 'list_eq false;
                            }

                            for (a, b) in lhs.iter().zip(rhs.iter()) {
                                let eq = Op::Eq(span).compute(&[a.clone(), b.clone()], ctx)?;

                                match eq {
                                    FragmentValue::Bool(val) => {
                                        if !val {
                                            break 'list_eq false;
                                        }
                                    }

                                    _ => unreachable!(),
                                }
                            }

                            true
                        }),

                        _ => return Err(Error::new(span, "expected rhs to be `list`")),
                    },

                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot compare `tokens`"));
                    }
                }
            }

            Self::Ne(span) => {
                let eq = Self::Eq(span).compute(args, ctx)?;

                match eq {
                    FragmentValue::Bool(val) => FragmentValue::Bool(!val),
                    _ => unreachable!(),
                }
            }

            Self::Lt(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(lhs), _) => match rhs {
                        FragmentValue::Float(rhs) => FragmentValue::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    (FragmentValue::String(lhs), _) => match rhs {
                        FragmentValue::String(rhs) => FragmentValue::Bool(lhs < rhs),
                        FragmentValue::Ident(rhs) => FragmentValue::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `string`")),
                    },

                    (FragmentValue::Ident(lhs), _) => match rhs {
                        FragmentValue::Ident(rhs) => FragmentValue::Bool(lhs < rhs),
                        FragmentValue::String(rhs) => FragmentValue::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `ident`")),
                    },

                    (FragmentValue::Char(lhs), _) => match rhs {
                        FragmentValue::Char(rhs) => FragmentValue::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `char`")),
                    },

                    (FragmentValue::Bool(lhs), _) => match rhs {
                        FragmentValue::Bool(rhs) => FragmentValue::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot compare `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot compare `tokens`"));
                    }
                }
            }

            Self::Gt(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Int(lhs), _) => match rhs {
                        FragmentValue::Int(rhs) => FragmentValue::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(lhs), _) => match rhs {
                        FragmentValue::Float(rhs) => FragmentValue::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    (FragmentValue::Char(lhs), _) => match rhs {
                        FragmentValue::Char(rhs) => FragmentValue::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `char`")),
                    },

                    (FragmentValue::String(lhs), _) => match rhs {
                        FragmentValue::String(rhs) => FragmentValue::Bool(lhs > rhs),
                        FragmentValue::Ident(rhs) => FragmentValue::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `string`")),
                    },

                    (FragmentValue::Ident(lhs), _) => match rhs {
                        FragmentValue::Ident(rhs) => FragmentValue::Bool(lhs > rhs),
                        FragmentValue::String(rhs) => FragmentValue::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `ident`")),
                    },

                    (FragmentValue::Bool(lhs), _) => match rhs {
                        FragmentValue::Bool(rhs) => FragmentValue::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot compare `list`"));
                    }

                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot compare `tokens`"));
                    }
                }
            }

            Self::Le(span) => {
                let gt = Self::Gt(span).compute(args, ctx)?;

                match gt {
                    FragmentValue::Bool(val) => FragmentValue::Bool(!val),
                    _ => unreachable!(),
                }
            }

            Self::Ge(span) => {
                let lt = Self::Lt(span).compute(args, ctx)?;

                match lt {
                    FragmentValue::Bool(val) => FragmentValue::Bool(!val),
                    _ => unreachable!(),
                }
            }

            Self::And(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Bool(lhs), _) => match rhs {
                        FragmentValue::Bool(rhs) => FragmentValue::Bool(lhs & rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    _ => return Err(Error::new(span, "expected `bool`")),
                }
            }

            Self::Or(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Bool(lhs), _) => match rhs {
                        FragmentValue::Bool(rhs) => FragmentValue::Bool(lhs | rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    _ => return Err(Error::new(span, "expected `bool`")),
                }
            }

            Self::Xor(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::Bool(lhs), _) => match rhs {
                        FragmentValue::Bool(rhs) => FragmentValue::Bool(lhs ^ rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    _ => return Err(Error::new(span, "expected `bool`")),
                }
            }

            Self::Range(span) => {
                let [start, end] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (start, end) {
                    (FragmentValue::Int(lhs), _) => match end {
                        FragmentValue::Int(rhs) => FragmentValue::List(
                            (*lhs..*rhs).map(|i| FragmentValue::Int(i)).collect(),
                        ),

                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(_), _) => {
                        return Err(Error::new(span, "cannot range `float`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot range `char`"));
                    }
                    (FragmentValue::String(_), _) => {
                        return Err(Error::new(span, "cannot range `string`"));
                    }
                    (FragmentValue::Ident(_), _) => {
                        return Err(Error::new(span, "cannot range `ident`"));
                    }
                    (FragmentValue::Bool(_), _) => {
                        return Err(Error::new(span, "cannot range `bool`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot range `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot range `tokens`"));
                    }
                }
            }

            Self::RangeInclusive(span) => {
                let [start, end] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (start, end) {
                    (FragmentValue::Int(lhs), _) => match end {
                        FragmentValue::Int(rhs) => FragmentValue::List(
                            (*lhs..=*rhs).map(|i| FragmentValue::Int(i)).collect(),
                        ),

                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    (FragmentValue::Float(_), _) => {
                        return Err(Error::new(span, "cannot range `float`"));
                    }
                    (FragmentValue::Char(_), _) => {
                        return Err(Error::new(span, "cannot range `char`"));
                    }
                    (FragmentValue::String(_), _) => {
                        return Err(Error::new(span, "cannot range `string`"));
                    }
                    (FragmentValue::Ident(_), _) => {
                        return Err(Error::new(span, "cannot range `ident`"));
                    }
                    (FragmentValue::Bool(_), _) => {
                        return Err(Error::new(span, "cannot range `bool`"));
                    }
                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(span, "cannot range `list`"));
                    }
                    (FragmentValue::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot range `tokens`"));
                    }
                }
            }

            Self::Len(span) => {
                let [arg] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 1 argument, found {}", args.len()),
                    ));
                };

                match arg {
                    FragmentValue::List(list) => FragmentValue::Int(list.len() as i128),

                    FragmentValue::String(string) => FragmentValue::Int(string.len() as i128),

                    FragmentValue::Ident(ident) => FragmentValue::Int(ident.len() as i128),

                    _ => {
                        return Err(Error::new(
                            span,
                            format!(
                                "expected `list`,`string` or `ident`, found `{}`",
                                arg.kind()
                            ),
                        ));
                    }
                }
            }

            Self::Index(span) => {
                let [list, index] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                let items = match list {
                    FragmentValue::List(list) => list,
                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", list.kind()),
                        ));
                    }
                };

                match index {
                    FragmentValue::Int(index) => items[*index as usize].clone(),

                    FragmentValue::List(indicies) => FragmentValue::List(
                        indicies
                            .iter()
                            .map(|index| {
                                Op::Index(span).compute(&[list.clone(), index.clone()], ctx)
                            })
                            .flatten()
                            .collect(),
                    ),

                    _ => return Err(Error::new(span, "expected `int`")),
                }
            }

            Self::Enumerate(span) => {
                let [list] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 1 argument, found {}", args.len()),
                    ));
                };

                match list {
                    FragmentValue::List(list) => FragmentValue::List(
                        list.iter()
                            .enumerate()
                            .map(|(index, item)| {
                                FragmentValue::List(vec![
                                    FragmentValue::Int(index as i128),
                                    item.clone(),
                                ])
                            })
                            .collect(),
                    ),

                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", list.kind()),
                        ));
                    }
                }
            }

            Self::Zip(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::List(lhs), FragmentValue::List(rhs)) => FragmentValue::List(
                        lhs.iter()
                            .zip(rhs.iter())
                            .map(|(lhs, rhs)| FragmentValue::List(vec![lhs.clone(), rhs.clone()]))
                            .collect(),
                    ),

                    (FragmentValue::List(_), _) => {
                        return Err(Error::new(
                            span,
                            format!("expected rhs to be `list`, found `{}`", rhs.kind()),
                        ));
                    }

                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", lhs.kind()),
                        ));
                    }
                }
            }

            Self::Chain(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match (lhs, rhs) {
                    (FragmentValue::List(lhs), FragmentValue::List(rhs)) => {
                        FragmentValue::List(lhs.iter().chain(rhs.iter()).cloned().collect())
                    }

                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", lhs.kind()),
                        ));
                    }
                }
            }

            Self::ConcatIdent(span) => {
                let [parts] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 1 argument, found {}", args.len()),
                    ));
                };

                let parts = match parts {
                    FragmentValue::List(parts) => parts,
                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", parts.kind()),
                        ));
                    }
                };

                let mut output_str = String::new();
                for part in parts {
                    match part {
                        FragmentValue::Ident(ident) => output_str.push_str(ident.as_str()),
                        FragmentValue::String(string) => output_str.push_str(string.as_str()),
                        FragmentValue::Char(char) => output_str.push(*char),
                        FragmentValue::Int(int) => output_str.push_str(int.to_string().as_str()),
                        FragmentValue::Float(float) => {
                            output_str.push_str(float.to_string().as_str())
                        }
                        FragmentValue::Bool(bool) => output_str.push_str(bool.to_string().as_str()),

                        FragmentValue::List(_) => {
                            return Err(Error::new(span, "cannot stringify `list`"));
                        }
                        FragmentValue::Tokens(_) => {
                            return Err(Error::new(span, "cannot stringify `tokens`"));
                        }
                    }
                }

                FragmentValue::Ident(output_str)
            }

            Self::ConcatString(span) => {
                let [parts] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 1 argument, found {}", args.len()),
                    ));
                };

                let parts = match parts {
                    FragmentValue::List(parts) => parts,
                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", parts.kind()),
                        ));
                    }
                };

                let mut output_str = String::new();
                for part in parts {
                    match part {
                        FragmentValue::Ident(ident) => output_str.push_str(ident.as_str()),
                        FragmentValue::String(string) => output_str.push_str(string.as_str()),
                        FragmentValue::Char(char) => output_str.push(*char),
                        FragmentValue::Int(int) => output_str.push_str(int.to_string().as_str()),
                        FragmentValue::Float(float) => {
                            output_str.push_str(float.to_string().as_str())
                        }
                        FragmentValue::Bool(bool) => output_str.push_str(bool.to_string().as_str()),

                        FragmentValue::List(_) => {
                            return Err(Error::new(span, "cannot stringify `list`"));
                        }
                        FragmentValue::Tokens(_) => {
                            return Err(Error::new(span, "cannot stringify `tokens`"));
                        }
                    }
                }

                FragmentValue::String(output_str)
            }
        })
    }
}
