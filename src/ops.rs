use proc_macro2::Span;
use syn::{Error, Token, parse::ParseStream, spanned::Spanned};

use super::*;

#[derive(Debug, Clone, Copy)]
pub enum Op {
    IfElse(Span),

    // Unary Operators
    Neg(Span),
    Not(Span),

    // Binary Operators
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

    // Comparison Operators
    Eq(Span),
    Ne(Span),
    Lt(Span),
    Gt(Span),
    Le(Span),
    Ge(Span),

    Min(Span),
    Max(Span),
    Clamp(Span),

    // Logical Operators
    And(Span),
    Or(Span),
    #[allow(dead_code)]
    Xor(Span),

    // Range Operators
    Range(Span),
    RangeInclusive(Span),

    // List
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
            Self::Min(span) => *span,
            Self::Max(span) => *span,
            Self::Clamp(span) => *span,
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
        // 3 punctuated ops
        if let Some(tok) = input.parse::<Option<Token![..=]>>().unwrap() {
            Some(Op::RangeInclusive(tok.span()))
        }
        // 2 punctuated ops
        else if let Some(tok) = input.parse::<Option<Token![||]>>().unwrap() {
            Some(Op::Or(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![&&]>>().unwrap() {
            Some(Op::And(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![<=]>>().unwrap() {
            Some(Op::Le(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![>=]>>().unwrap() {
            Some(Op::Ge(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![==]>>().unwrap() {
            Some(Op::Eq(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![!=]>>().unwrap() {
            Some(Op::Ne(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![..]>>().unwrap() {
            Some(Op::Range(tok.span()))
        }
        // 1 punctuated op
        else if let Some(tok) = input.parse::<Option<Token![+]>>().unwrap() {
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
        } else if let Some(tok) = input.parse::<Option<Token![<]>>().unwrap() {
            Some(Op::Lt(tok.span()))
        } else if let Some(tok) = input.parse::<Option<Token![>]>>().unwrap() {
            Some(Op::Gt(tok.span()))
        } else {
            None
        }
    }

    pub fn from_method_ident(ident: Method) -> Self {
        match ident {
            Method::Not(span) => Op::Not(span),
            Method::Neg(span) => Op::Neg(span),
            Method::Add(span) => Op::Add(span),
            Method::Sub(span) => Op::Sub(span),
            Method::Mul(span) => Op::Mul(span),
            Method::Div(span) => Op::Div(span),
            Method::Rem(span) => Op::Rem(span),
            Method::BitAnd(span) => Op::BitAnd(span),
            Method::BitOr(span) => Op::BitOr(span),
            Method::BitXor(span) => Op::BitXor(span),
            Method::Shl(span) => Op::Shl(span),
            Method::Shr(span) => Op::Shr(span),
            Method::Eq(span) => Op::Eq(span),
            Method::Ne(span) => Op::Ne(span),
            Method::Lt(span) => Op::Lt(span),
            Method::Gt(span) => Op::Gt(span),
            Method::Le(span) => Op::Le(span),
            Method::Ge(span) => Op::Ge(span),
            Method::Min(span) => Op::Min(span),
            Method::Max(span) => Op::Max(span),
            Method::Clamp(span) => Op::Clamp(span),
            Method::Len(span) => Op::Len(span),
            Method::Index(span) => Op::Index(span),
            Method::Enumerate(span) => Op::Enumerate(span),
            Method::Zip(span) => Op::Zip(span),
            Method::Chain(span) => Op::Chain(span),
            Method::ConcatIdent(span) => Op::ConcatIdent(span),
            Method::ConcatString(span) => Op::ConcatString(span),
        }
    }

    pub fn compute(self, args: &[FragmentValue], ctx: &mut Context) -> syn::Result<FragmentValue> {
        let output_kind = match self {
            Self::IfElse(span) => {
                let [cond, then, otherwise] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 3 arguments, found {}", args.len()),
                    ));
                };

                let cond = match &cond.kind {
                    FragmentValueKind::Bool(cond) => *cond,
                    _ => unreachable!(),
                };

                if cond {
                    then.kind.clone()
                } else {
                    otherwise.kind.clone()
                }
            }

            Self::Neg(span) => {
                let [arg] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 1 argument, found {}", args.len()),
                    ));
                };

                match &arg.kind {
                    FragmentValueKind::Int(val) => FragmentValueKind::Int(-val),
                    FragmentValueKind::Float(val) => FragmentValueKind::Float(-val),

                    FragmentValueKind::Bool(_) => {
                        return Err(Error::new(span, "cannot negate `bool`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot negate `ident`"));
                    }
                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot negate `string`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot negate `char`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot negate `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &arg.kind {
                    FragmentValueKind::Bool(val) => FragmentValueKind::Bool(!val),

                    FragmentValueKind::Int(_) => {
                        return Err(Error::new(span, "cannot not `int`"));
                    }
                    FragmentValueKind::Float(_) => {
                        return Err(Error::new(span, "cannot not `float`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot not `ident`"));
                    }
                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot not `string`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot not `char`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot not `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs + rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Float(lhs + rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    FragmentValueKind::String(lhs) => match &rhs.kind {
                        FragmentValueKind::String(rhs) => {
                            FragmentValueKind::String(format!("{lhs}{rhs}"))
                        }
                        FragmentValueKind::Ident(rhs) => {
                            FragmentValueKind::String(format!("{lhs}{rhs}"))
                        }
                        FragmentValueKind::Char(rhs) => {
                            FragmentValueKind::String(format!("{lhs}{rhs}"))
                        }
                        _ => return Err(Error::new(span, "expected rhs to be `string`")),
                    },

                    FragmentValueKind::Ident(lhs) => match &rhs.kind {
                        FragmentValueKind::Ident(rhs) => {
                            FragmentValueKind::Ident(format!("{lhs}{rhs}"))
                        }
                        FragmentValueKind::String(rhs) => {
                            FragmentValueKind::Ident(format!("{lhs}{rhs}"))
                        }
                        FragmentValueKind::Char(rhs) => {
                            FragmentValueKind::Ident(format!("{lhs}{rhs}"))
                        }
                        _ => return Err(Error::new(span, "expected rhs to be `ident`")),
                    },

                    FragmentValueKind::Bool(_) => {
                        return Err(Error::new(span, "cannot add to `bool`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot add to `char`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot add to `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs - rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Float(lhs - rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot subtract from `string`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot subtract from `ident`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot subtract from `char`"));
                    }
                    FragmentValueKind::Bool(_) => {
                        return Err(Error::new(span, "cannot subtract from `bool`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot subtract from `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs * rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Float(lhs * rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    FragmentValueKind::String(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => {
                            FragmentValueKind::String(lhs.repeat(*rhs as usize))
                        }
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Ident(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => {
                            FragmentValueKind::Ident(lhs.repeat(*rhs as usize))
                        }
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Char(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => {
                            FragmentValueKind::String(lhs.to_string().repeat(*rhs as usize))
                        }
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Bool(_) => {
                        return Err(Error::new(span, "cannot multiply `bool`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot multiply `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs / rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Float(lhs / rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot divide `string`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot divide `ident`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot divide `char`"));
                    }
                    FragmentValueKind::Bool(_) => {
                        return Err(Error::new(span, "cannot divide `bool`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot divide `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs % rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Float(lhs % rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot divide `string`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot divide `ident`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot divide `char`"));
                    }
                    FragmentValueKind::Bool(_) => {
                        return Err(Error::new(span, "cannot divide `bool`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot divide `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs & rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs & rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    FragmentValueKind::Float(_) => {
                        return Err(Error::new(span, "cannot bitwise-and `float`"));
                    }
                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot divide `string`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot bitwise-and `ident`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot bitwise-and `char`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot bitwise-and `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs | rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs | rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot bitwise-or `string`"));
                    }
                    FragmentValueKind::Float(_) => {
                        return Err(Error::new(span, "cannot bitwise-and `float`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot bitwise-or `ident`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot bitwise-or `char`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot bitwise-or `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs ^ rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs ^ rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    FragmentValueKind::Float(_) => {
                        return Err(Error::new(span, "cannot bitwise-xor `float`"));
                    }
                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot bitwise-xor `string`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot bitwise-xor `ident`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot bitwise-xor `char`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot bitwise-xor `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs << rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(_) => {
                        return Err(Error::new(span, "cannot shift-left `float`"));
                    }
                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot shift-left `string`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot shift-left `ident`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot shift-left `char`"));
                    }
                    FragmentValueKind::Bool(_) => {
                        return Err(Error::new(span, "cannot shift-left `bool`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot shift-left `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs >> rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(_) => {
                        return Err(Error::new(span, "cannot shift-right `float`"));
                    }
                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot shift-right `string`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot shift-right `ident`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot shift-right `char`"));
                    }
                    FragmentValueKind::Bool(_) => {
                        return Err(Error::new(span, "cannot shift-right `bool`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot shift-right `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    FragmentValueKind::String(lhs) => match &rhs.kind {
                        FragmentValueKind::String(rhs) => FragmentValueKind::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `string`")),
                    },

                    FragmentValueKind::Ident(lhs) => match &rhs.kind {
                        FragmentValueKind::Ident(rhs) => FragmentValueKind::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `ident`")),
                    },

                    FragmentValueKind::Char(lhs) => match &rhs.kind {
                        FragmentValueKind::Char(rhs) => FragmentValueKind::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `char`")),
                    },

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs == rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    FragmentValueKind::List(lhs) => match &rhs.kind {
                        FragmentValueKind::List(rhs) => FragmentValueKind::Bool('list_eq: {
                            if lhs.len() != rhs.len() {
                                break 'list_eq false;
                            }

                            for (a, b) in lhs.iter().zip(rhs.iter()) {
                                let eq = Op::Eq(span).compute(&[a.clone(), b.clone()], ctx)?;

                                match &eq.kind {
                                    FragmentValueKind::Bool(val) => {
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

                    FragmentValueKind::Tokens(_) => {
                        return Err(Error::new(span, "cannot compare `tokens`"));
                    }
                }
            }

            Self::Ne(span) => {
                let eq = Self::Eq(span).compute(args, ctx)?;

                match &eq.kind {
                    FragmentValueKind::Bool(val) => FragmentValueKind::Bool(!val),
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    FragmentValueKind::String(lhs) => match &rhs.kind {
                        FragmentValueKind::String(rhs) => FragmentValueKind::Bool(lhs < rhs),
                        FragmentValueKind::Ident(rhs) => FragmentValueKind::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `string`")),
                    },

                    FragmentValueKind::Ident(lhs) => match &rhs.kind {
                        FragmentValueKind::Ident(rhs) => FragmentValueKind::Bool(lhs < rhs),
                        FragmentValueKind::String(rhs) => FragmentValueKind::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `ident`")),
                    },

                    FragmentValueKind::Char(lhs) => match &rhs.kind {
                        FragmentValueKind::Char(rhs) => FragmentValueKind::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `char`")),
                    },

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs < rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot compare `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `float`")),
                    },

                    FragmentValueKind::Char(lhs) => match &rhs.kind {
                        FragmentValueKind::Char(rhs) => FragmentValueKind::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `char`")),
                    },

                    FragmentValueKind::String(lhs) => match &rhs.kind {
                        FragmentValueKind::String(rhs) => FragmentValueKind::Bool(lhs > rhs),
                        FragmentValueKind::Ident(rhs) => FragmentValueKind::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `string`")),
                    },

                    FragmentValueKind::Ident(lhs) => match &rhs.kind {
                        FragmentValueKind::Ident(rhs) => FragmentValueKind::Bool(lhs > rhs),
                        FragmentValueKind::String(rhs) => FragmentValueKind::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `ident`")),
                    },

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs > rhs),
                        _ => return Err(Error::new(span, "expected rhs to be `bool`")),
                    },

                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot compare `list`"));
                    }

                    FragmentValueKind::Tokens(_) => {
                        return Err(Error::new(span, "cannot compare `tokens`"));
                    }
                }
            }

            Self::Le(span) => {
                let gt = Self::Gt(span).compute(args, ctx)?;

                match &gt.kind {
                    FragmentValueKind::Bool(val) => FragmentValueKind::Bool(!val),
                    _ => unreachable!(),
                }
            }

            Self::Ge(span) => {
                let lt = Self::Lt(span).compute(args, ctx)?;

                match &lt.kind {
                    FragmentValueKind::Bool(val) => FragmentValueKind::Bool(!val),
                    _ => unreachable!(),
                }
            }

            Self::Min(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                let lhs_is_greater = Self::Gt(span).compute(&[lhs.clone(), rhs.clone()], ctx)?;
                let lhs_is_greater = match &lhs_is_greater.kind {
                    FragmentValueKind::Bool(val) => *val,
                    _ => unreachable!(),
                };

                if lhs_is_greater {
                    rhs.kind.clone()
                } else {
                    lhs.kind.clone()
                }
            }

            Self::Max(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                let lhs_is_greater = Self::Gt(span).compute(&[lhs.clone(), rhs.clone()], ctx)?;
                let lhs_is_greater = match &lhs_is_greater.kind {
                    FragmentValueKind::Bool(val) => *val,
                    _ => unreachable!(),
                };

                if lhs_is_greater {
                    lhs.kind.clone()
                } else {
                    rhs.kind.clone()
                }
            }

            Self::Clamp(span) => {
                let [value, min, max] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 3 arguments, found {}", args.len()),
                    ));
                };

                let is_too_low = Self::Lt(span).compute(&[value.clone(), min.clone()], ctx)?;
                let is_too_low = match &is_too_low.kind {
                    FragmentValueKind::Bool(val) => *val,
                    _ => unreachable!(),
                };

                let is_too_high = Self::Gt(span).compute(&[value.clone(), max.clone()], ctx)?;
                let is_too_high = match &is_too_high.kind {
                    FragmentValueKind::Bool(val) => *val,
                    _ => unreachable!(),
                };

                if is_too_low {
                    min.kind.clone()
                } else if is_too_high {
                    max.kind.clone()
                } else {
                    value.kind.clone()
                }
            }

            Self::And(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 2 arguments, found {}", args.len()),
                    ));
                };

                match &lhs.kind {
                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs & rhs),
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

                match &lhs.kind {
                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs | rhs),
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

                match &lhs.kind {
                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs ^ rhs),
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

                match &start.kind {
                    FragmentValueKind::Int(lhs) => match &end.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::List(
                            (*lhs..*rhs)
                                .map(|i| FragmentValue {
                                    span: span,
                                    kind: FragmentValueKind::Int(i),
                                })
                                .collect(),
                        ),

                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(_) => {
                        return Err(Error::new(span, "cannot range `float`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot range `char`"));
                    }
                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot range `string`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot range `ident`"));
                    }
                    FragmentValueKind::Bool(_) => {
                        return Err(Error::new(span, "cannot range `bool`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot range `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &start.kind {
                    FragmentValueKind::Int(lhs) => match &end.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::List(
                            (*lhs..=*rhs)
                                .map(|i| FragmentValue {
                                    span: span,
                                    kind: FragmentValueKind::Int(i),
                                })
                                .collect(),
                        ),

                        _ => return Err(Error::new(span, "expected rhs to be `int`")),
                    },

                    FragmentValueKind::Float(_) => {
                        return Err(Error::new(span, "cannot range `float`"));
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::new(span, "cannot range `char`"));
                    }
                    FragmentValueKind::String(_) => {
                        return Err(Error::new(span, "cannot range `string`"));
                    }
                    FragmentValueKind::Ident(_) => {
                        return Err(Error::new(span, "cannot range `ident`"));
                    }
                    FragmentValueKind::Bool(_) => {
                        return Err(Error::new(span, "cannot range `bool`"));
                    }
                    FragmentValueKind::List(_) => {
                        return Err(Error::new(span, "cannot range `list`"));
                    }
                    FragmentValueKind::Tokens(_) => {
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

                match &arg.kind {
                    FragmentValueKind::List(list) => FragmentValueKind::Int(list.len() as i128),

                    FragmentValueKind::String(string) => {
                        FragmentValueKind::Int(string.len() as i128)
                    }

                    FragmentValueKind::Ident(ident) => FragmentValueKind::Int(ident.len() as i128),

                    _ => {
                        return Err(Error::new(
                            span,
                            format!(
                                "expected `list`,`string` or `ident`, found `{}`",
                                arg.kind.kind()
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

                let items = match &list.kind {
                    FragmentValueKind::List(list) => list,
                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", list.kind.kind()),
                        ));
                    }
                };

                match &index.kind {
                    FragmentValueKind::Int(index) => items[*index as usize].kind.clone(),

                    FragmentValueKind::List(indicies) => FragmentValueKind::List(
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

                match &list.kind {
                    FragmentValueKind::List(list) => FragmentValueKind::List(
                        list.iter()
                            .enumerate()
                            .map(|(index, item)| FragmentValue {
                                span: span,
                                kind: FragmentValueKind::List(vec![
                                    FragmentValue {
                                        span: self.span(),
                                        kind: FragmentValueKind::Int(index as i128),
                                    },
                                    item.clone(),
                                ]),
                            })
                            .collect(),
                    ),

                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", list.kind.kind()),
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

                match (&lhs.kind, &rhs.kind) {
                    (FragmentValueKind::List(lhs), FragmentValueKind::List(rhs)) => {
                        FragmentValueKind::List(
                            lhs.iter()
                                .zip(rhs.iter())
                                .map(|(lhs, rhs)| FragmentValue {
                                    span: span,
                                    kind: FragmentValueKind::List(vec![lhs.clone(), rhs.clone()]),
                                })
                                .collect(),
                        )
                    }

                    (FragmentValueKind::List(_), _) => {
                        return Err(Error::new(
                            span,
                            format!("expected rhs to be `list`, found `{}`", rhs.kind.kind()),
                        ));
                    }

                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", lhs.kind.kind()),
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

                match (&lhs.kind, &rhs.kind) {
                    (FragmentValueKind::List(lhs), FragmentValueKind::List(rhs)) => {
                        FragmentValueKind::List(lhs.iter().chain(rhs.iter()).cloned().collect())
                    }

                    (FragmentValueKind::List(_), _) => {
                        return Err(Error::new(
                            span,
                            format!("expected rhs to be `list`, found `{}`", rhs.kind.kind()),
                        ));
                    }

                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", lhs.kind.kind()),
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

                let parts = match &parts.kind {
                    FragmentValueKind::List(parts) => parts,
                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", parts.kind.kind()),
                        ));
                    }
                };

                let mut output_str = String::new();
                for part in parts {
                    match &part.kind {
                        FragmentValueKind::Ident(ident) => output_str.push_str(ident.as_str()),
                        FragmentValueKind::String(string) => output_str.push_str(string.as_str()),
                        FragmentValueKind::Char(char) => output_str.push(*char),
                        FragmentValueKind::Int(int) => {
                            output_str.push_str(int.to_string().as_str())
                        }
                        FragmentValueKind::Float(float) => {
                            output_str.push_str(float.to_string().as_str())
                        }
                        FragmentValueKind::Bool(bool) => {
                            output_str.push_str(bool.to_string().as_str())
                        }

                        FragmentValueKind::List(_) => {
                            return Err(Error::new(span, "cannot stringify `list`"));
                        }
                        FragmentValueKind::Tokens(_) => {
                            return Err(Error::new(span, "cannot stringify `tokens`"));
                        }
                    }
                }

                FragmentValueKind::Ident(output_str)
            }

            Self::ConcatString(span) => {
                let [parts] = args else {
                    return Err(Error::new(
                        span,
                        format!("expected 1 argument, found {}", args.len()),
                    ));
                };

                let parts = match &parts.kind {
                    FragmentValueKind::List(parts) => parts,
                    _ => {
                        return Err(Error::new(
                            span,
                            format!("expected `list`, found `{}`", parts.kind.kind()),
                        ));
                    }
                };

                let mut output_str = String::new();
                for part in parts {
                    match &part.kind {
                        FragmentValueKind::Ident(ident) => output_str.push_str(ident.as_str()),
                        FragmentValueKind::String(string) => output_str.push_str(string.as_str()),
                        FragmentValueKind::Char(char) => output_str.push(*char),
                        FragmentValueKind::Int(int) => {
                            output_str.push_str(int.to_string().as_str())
                        }
                        FragmentValueKind::Float(float) => {
                            output_str.push_str(float.to_string().as_str())
                        }
                        FragmentValueKind::Bool(bool) => {
                            output_str.push_str(bool.to_string().as_str())
                        }

                        FragmentValueKind::List(_) => {
                            return Err(Error::new(span, "cannot stringify `list`"));
                        }
                        FragmentValueKind::Tokens(_) => {
                            return Err(Error::new(span, "cannot stringify `tokens`"));
                        }
                    }
                }

                FragmentValueKind::String(output_str)
            }
        };

        Ok(FragmentValue {
            span: self.span(),
            kind: output_kind,
        })
    }
}
