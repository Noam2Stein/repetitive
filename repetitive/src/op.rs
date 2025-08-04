use proc_macro2::Span;
use syn::Error;

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Op {
    For,
    If,
    IfElse,
    Let,

    Neg,
    Not,

    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,

    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,

    And,
    Or,
    #[allow(dead_code)]
    Xor,

    Range,
    RangeInclusive,

    Len,
    Index,
    Enumerate,
    Zip,
    Chain,
}

impl Op {
    pub fn compute(
        self,
        span: Span,
        args: &[FragmentValue],
        ctx: &mut Context,
    ) -> Result<FragmentValue, Error> {
        Ok(match self {
            Self::For => {
                let [iter, body] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                let iter = match iter {
                    FragmentValue::List(list) => list,
                    _ => unreachable!(),
                };
                let body = match body {
                    FragmentValue::Tokens(tokens) => tokens,
                    _ => unreachable!(),
                };

                let mut tokens = Tokens::default();
                for item in iter {
                    body.paste(&mut tokens, ctx);
                }

                FragmentValue::Tokens(tokens)
            }

            Self::Neg => {
                let [arg] = args else {
                    return Err(Error::new(span, "expected a single argument"));
                };

                match arg {
                    FragmentBody::Lit(FragmentValue::Int(val)) => {
                        FragmentBody::Lit(FragmentValue::Int(-val))
                    }
                    FragmentBody::Lit(FragmentValue::Float(val)) => {
                        FragmentBody::Lit(FragmentValue::Float(-val))
                    }

                    FragmentBody::Lit(FragmentValue::Bool(_)) => {
                        return Err(Error::new(span, "cannot negate `bool`"));
                    }
                    FragmentBody::Lit(FragmentValue::Ident(_)) => {
                        return Err(Error::new(span, "cannot negate `ident`"));
                    }
                    FragmentBody::Lit(FragmentValue::String(_)) => {
                        return Err(Error::new(span, "cannot negate `string`"));
                    }
                    FragmentBody::Lit(FragmentValue::Char(_)) => {
                        return Err(Error::new(span, "cannot negate `char`"));
                    }
                    FragmentBody::List(_) => {
                        return Err(Error::new(span, "cannot negate `list`"));
                    }
                    FragmentBody::Tokens(_) => {
                        return Err(Error::new(span, "cannot negate `tokens`"));
                    }

                    FragmentBody::Name(_) => unreachable!(),
                    FragmentBody::Op(_) => unreachable!(),
                }
            }

            Self::Not => {
                let [arg] = args else {
                    return Err(Error::new(span, "expected a single argument"));
                };

                match arg {
                    FragmentBody::Lit(FragmentValue::Bool(val)) => {
                        FragmentBody::Lit(FragmentValue::Bool(!val))
                    }

                    FragmentBody::Lit(FragmentValue::Int(_)) => {
                        return Err(Error::new(span, "cannot not `int`"));
                    }
                    FragmentBody::Lit(FragmentValue::Float(_)) => {
                        return Err(Error::new(span, "cannot not `float`"));
                    }
                    FragmentBody::Lit(FragmentValue::Ident(_)) => {
                        return Err(Error::new(span, "cannot not `ident`"));
                    }
                    FragmentBody::Lit(FragmentValue::String(_)) => {
                        return Err(Error::new(span, "cannot not `string`"));
                    }
                    FragmentBody::Lit(FragmentValue::Char(_)) => {
                        return Err(Error::new(span, "cannot not `char`"));
                    }
                    FragmentBody::List(_) => {
                        return Err(Error::new(span, "cannot not `list`"));
                    }
                    FragmentBody::Tokens(_) => {
                        return Err(Error::new(span, "cannot not `tokens`"));
                    }

                    FragmentBody::Name(_) => unreachable!(),
                    FragmentBody::Op(_) => unreachable!(),
                }
            }

            Self::Add => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Int(lhs + rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Float(lhs)),
                        FragmentBody::Lit(FragmentValue::Float(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Float(lhs + rhs)),

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `float`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::String(lhs)),
                        FragmentBody::Lit(FragmentValue::String(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::String(format!("{lhs}{rhs}"))),

                    (
                        FragmentBody::Lit(FragmentValue::String(lhs)),
                        FragmentBody::Lit(FragmentValue::Ident(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::String(format!("{lhs}{rhs}"))),

                    (
                        FragmentBody::Lit(FragmentValue::String(lhs)),
                        FragmentBody::Lit(FragmentValue::Char(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::String(format!("{lhs}{rhs}"))),

                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(
                            span,
                            "expected rhs to be `string`, `ident` or `char`",
                        ));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Ident(lhs)),
                        FragmentBody::Lit(FragmentValue::Ident(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Ident(format!("{lhs}{rhs}"))),

                    (
                        FragmentBody::Lit(FragmentValue::Ident(lhs)),
                        FragmentBody::Lit(FragmentValue::String(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Ident(format!("{lhs}{rhs}"))),

                    (
                        FragmentBody::Lit(FragmentValue::Ident(lhs)),
                        FragmentBody::Lit(FragmentValue::Char(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Ident(format!("{lhs}{rhs}"))),

                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(
                            span,
                            "expected rhs to be `ident`, `string` or `char`",
                        ));
                    }

                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "cannot add to `bool`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot add to `char`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot add to `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot add to `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Sub => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Int(lhs - rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Float(lhs)),
                        FragmentBody::Lit(FragmentValue::Float(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Float(lhs - rhs)),

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `float`"));
                    }

                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "cannot subtract from `string`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "cannot subtract from `ident`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot subtract from `char`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "cannot subtract from `bool`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot subtract from `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot subtract from `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Mul => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Int(lhs * rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Float(lhs)),
                        FragmentBody::Lit(FragmentValue::Float(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Float(lhs * rhs)),

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `float`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::String(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::String(lhs.repeat(*rhs as usize))),

                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Ident(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Ident(lhs.repeat(*rhs as usize))),

                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Char(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::String(
                        lhs.to_string().repeat(*rhs as usize),
                    )),

                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "cannot multiply `bool`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot multiply `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot multiply `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Div => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Int(lhs / rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Float(lhs)),
                        FragmentBody::Lit(FragmentValue::Float(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Float(lhs / rhs)),

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `float`"));
                    }

                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "cannot divide `string`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "cannot divide `ident`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot divide `char`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "cannot divide `bool`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot divide `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot divide `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Rem => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Int(lhs % rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `float`"));
                    }

                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "cannot divide `string`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "cannot divide `ident`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot divide `char`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "cannot divide `bool`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot divide `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot divide `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::BitAnd => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Int(lhs & rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Bool(lhs)),
                        FragmentBody::Lit(FragmentValue::Bool(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs & rhs)),

                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `bool`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `float`"));
                    }
                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "cannot divide `string`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `ident`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `char`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::BitOr => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Int(lhs | rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Bool(lhs)),
                        FragmentBody::Lit(FragmentValue::Bool(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs | rhs)),

                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `bool`"));
                    }

                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-or `string`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-and `float`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-or `ident`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-or `char`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-or `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-or `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::BitXor => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Int(lhs ^ rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Bool(lhs)),
                        FragmentBody::Lit(FragmentValue::Bool(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs ^ rhs)),

                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `bool`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `float`"));
                    }
                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `string`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `ident`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `char`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot bitwise-xor `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Shl => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Int(lhs << rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "cannot shift-left `float`"));
                    }
                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "cannot shift-left `string`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "cannot shift-left `ident`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot shift-left `char`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "cannot shift-left `bool`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot shift-left `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot shift-left `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Shr => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Int(lhs >> rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "cannot shift-right `float`"));
                    }
                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "cannot shift-right `string`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "cannot shift-right `ident`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot shift-right `char`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "cannot shift-right `bool`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot shift-right `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot shift-right `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Eq => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs == rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Float(lhs)),
                        FragmentBody::Lit(FragmentValue::Float(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs == rhs)),

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `float`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::String(lhs)),
                        FragmentBody::Lit(FragmentValue::String(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs == rhs)),

                    (
                        FragmentBody::Lit(FragmentValue::String(lhs)),
                        FragmentBody::Lit(FragmentValue::Ident(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs == rhs)),

                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `string`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Ident(lhs)),
                        FragmentBody::Lit(FragmentValue::Ident(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs == rhs)),

                    (
                        FragmentBody::Lit(FragmentValue::Ident(lhs)),
                        FragmentBody::Lit(FragmentValue::String(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs == rhs)),

                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `ident`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Char(lhs)),
                        FragmentBody::Lit(FragmentValue::Char(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs == rhs)),

                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `char`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Bool(lhs)),
                        FragmentBody::Lit(FragmentValue::Bool(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs == rhs)),

                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `bool`"));
                    }

                    (FragmentBody::List(lhs), FragmentBody::List(rhs)) => {
                        FragmentBody::Lit(FragmentValue::Bool('list_eq: {
                            if lhs.len() != rhs.len() {
                                break 'list_eq false;
                            }

                            for (a, b) in lhs.iter().zip(rhs.iter()) {
                                let eq = Op::Eq.compute(span, &[a.clone(), b.clone()])?;

                                match eq {
                                    FragmentBody::Lit(FragmentValue::Bool(val)) => {
                                        if !val {
                                            break 'list_eq false;
                                        }
                                    }

                                    _ => unreachable!(),
                                }
                            }

                            true
                        }))
                    }

                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "expected rhs to be `list`"));
                    }

                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot compare `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Ne => {
                let eq = Self::Eq.compute(span, args)?;

                match eq {
                    FragmentBody::Lit(FragmentValue::Bool(val)) => {
                        FragmentBody::Lit(FragmentValue::Bool(!val))
                    }

                    _ => unreachable!(),
                }
            }

            Self::Lt => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs < rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Float(lhs)),
                        FragmentBody::Lit(FragmentValue::Float(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs < rhs)),

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `float`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Char(lhs)),
                        FragmentBody::Lit(FragmentValue::Char(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs < rhs)),

                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `char`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::String(lhs)),
                        FragmentBody::Lit(FragmentValue::String(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs < rhs)),

                    (
                        FragmentBody::Lit(FragmentValue::String(lhs)),
                        FragmentBody::Lit(FragmentValue::Ident(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs < rhs)),

                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `string`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Ident(lhs)),
                        FragmentBody::Lit(FragmentValue::Ident(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs < rhs)),

                    (
                        FragmentBody::Lit(FragmentValue::Ident(lhs)),
                        FragmentBody::Lit(FragmentValue::String(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs < rhs)),

                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `ident`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Bool(lhs)),
                        FragmentBody::Lit(FragmentValue::Bool(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs < rhs)),

                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `bool`"));
                    }

                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot compare `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot compare `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Gt => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs > rhs)),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Float(lhs)),
                        FragmentBody::Lit(FragmentValue::Float(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs > rhs)),

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `float`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Char(lhs)),
                        FragmentBody::Lit(FragmentValue::Char(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs > rhs)),

                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `char`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::String(lhs)),
                        FragmentBody::Lit(FragmentValue::String(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs > rhs)),

                    (
                        FragmentBody::Lit(FragmentValue::String(lhs)),
                        FragmentBody::Lit(FragmentValue::Ident(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs > rhs)),

                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `string`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Ident(lhs)),
                        FragmentBody::Lit(FragmentValue::Ident(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs > rhs)),

                    (
                        FragmentBody::Lit(FragmentValue::Ident(lhs)),
                        FragmentBody::Lit(FragmentValue::String(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs > rhs)),

                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `ident`"));
                    }

                    (
                        FragmentBody::Lit(FragmentValue::Bool(lhs)),
                        FragmentBody::Lit(FragmentValue::Bool(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs > rhs)),

                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `bool`"));
                    }

                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot compare `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot compare `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Le => {
                let gt = Self::Gt.compute(span, args)?;

                match gt {
                    FragmentBody::Lit(FragmentValue::Bool(val)) => {
                        FragmentBody::Lit(FragmentValue::Bool(!val))
                    }

                    _ => unreachable!(),
                }
            }

            Self::Ge => {
                let lt = Self::Lt.compute(span, args)?;

                match lt {
                    FragmentBody::Lit(FragmentValue::Bool(val)) => {
                        FragmentBody::Lit(FragmentValue::Bool(!val))
                    }

                    _ => unreachable!(),
                }
            }

            Self::And => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Bool(lhs)),
                        FragmentBody::Lit(FragmentValue::Bool(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs & rhs)),

                    _ => return Err(Error::new(span, "expected `bool`")),
                }
            }

            Self::Or => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Bool(lhs)),
                        FragmentBody::Lit(FragmentValue::Bool(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs | rhs)),

                    _ => return Err(Error::new(span, "expected `bool`")),
                }
            }

            Self::Xor => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (
                        FragmentBody::Lit(FragmentValue::Bool(lhs)),
                        FragmentBody::Lit(FragmentValue::Bool(rhs)),
                    ) => FragmentBody::Lit(FragmentValue::Bool(lhs ^ rhs)),

                    _ => return Err(Error::new(span, "expected `bool`")),
                }
            }

            Self::Range => {
                let [start, end] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (start, end) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::List(
                        (*lhs..*rhs)
                            .map(|i| FragmentBody::Lit(FragmentValue::Int(i)))
                            .collect(),
                    ),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "cannot range `float`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot range `char`"));
                    }

                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "cannot range `string`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "cannot range `ident`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "cannot range `bool`"));
                    }

                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot range `list`"));
                    }

                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot range `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::RangeInclusive => {
                let [start, end] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (start, end) {
                    (
                        FragmentBody::Lit(FragmentValue::Int(lhs)),
                        FragmentBody::Lit(FragmentValue::Int(rhs)),
                    ) => FragmentBody::List(
                        (*lhs..=*rhs)
                            .map(|i| FragmentBody::Lit(FragmentValue::Int(i)))
                            .collect(),
                    ),

                    (FragmentBody::Lit(FragmentValue::Int(_)), _) => {
                        return Err(Error::new(span, "expected rhs to be `int`"));
                    }

                    (FragmentBody::Lit(FragmentValue::Float(_)), _) => {
                        return Err(Error::new(span, "cannot range `float`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Char(_)), _) => {
                        return Err(Error::new(span, "cannot range `char`"));
                    }
                    (FragmentBody::Lit(FragmentValue::String(_)), _) => {
                        return Err(Error::new(span, "cannot range `string`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Ident(_)), _) => {
                        return Err(Error::new(span, "cannot range `ident`"));
                    }
                    (FragmentBody::Lit(FragmentValue::Bool(_)), _) => {
                        return Err(Error::new(span, "cannot range `bool`"));
                    }
                    (FragmentBody::List(_), _) => {
                        return Err(Error::new(span, "cannot range `list`"));
                    }
                    (FragmentBody::Tokens(_), _) => {
                        return Err(Error::new(span, "cannot range `tokens`"));
                    }

                    (FragmentBody::Name(_), _) => unreachable!(),
                    (_, FragmentBody::Name(_)) => unreachable!(),
                    (_, FragmentBody::Op(_)) => unreachable!(),
                    (FragmentBody::Op(_), _) => unreachable!(),
                }
            }

            Self::Len => {
                let [arg] = args else {
                    return Err(Error::new(span, "expected one argument"));
                };

                match arg {
                    FragmentBody::List(list) => {
                        FragmentBody::Lit(FragmentValue::Int(list.len() as i128))
                    }

                    FragmentBody::Lit(FragmentValue::String(string)) => {
                        FragmentBody::Lit(FragmentValue::Int(string.len() as i128))
                    }

                    FragmentBody::Lit(FragmentValue::Ident(ident)) => {
                        FragmentBody::Lit(FragmentValue::Int(ident.len() as i128))
                    }

                    _ => return Err(Error::new(span, "expected `list`,`string` or `ident`")),
                }
            }

            Self::Index => {
                let [list, index] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                let items = match list {
                    FragmentBody::List(list) => list,
                    _ => return Err(Error::new(span, "expected `list`")),
                };

                match index {
                    FragmentBody::Lit(FragmentValue::Int(index)) => items[*index as usize].clone(),

                    FragmentBody::List(indicies) => FragmentBody::List(
                        indicies
                            .iter()
                            .map(|index| Op::Index.compute(span, &[list.clone(), index.clone()]))
                            .flatten()
                            .collect(),
                    ),

                    _ => return Err(Error::new(span, "expected `int`")),
                }
            }

            Self::Enumerate => {
                let [list] = args else {
                    return Err(Error::new(span, "expected one argument"));
                };

                match list {
                    FragmentBody::List(list) => FragmentBody::List(
                        list.iter()
                            .enumerate()
                            .map(|(index, item)| {
                                FragmentBody::List(vec![
                                    FragmentBody::Lit(FragmentValue::Int(index as i128)),
                                    item.clone(),
                                ])
                            })
                            .collect(),
                    ),

                    _ => return Err(Error::new(span, "expected `list`")),
                }
            }

            Self::Zip => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (FragmentBody::List(lhs), FragmentBody::List(rhs)) => FragmentBody::List(
                        lhs.iter()
                            .zip(rhs.iter())
                            .map(|(lhs, rhs)| FragmentBody::List(vec![lhs.clone(), rhs.clone()]))
                            .collect(),
                    ),

                    _ => return Err(Error::new(span, "expected `list`")),
                }
            }

            Self::Chain => {
                let [lhs, rhs] = args else {
                    return Err(Error::new(span, "expected two arguments"));
                };

                match (lhs, rhs) {
                    (FragmentBody::List(lhs), FragmentBody::List(rhs)) => {
                        FragmentBody::List(lhs.iter().chain(rhs.iter()).cloned().collect())
                    }

                    _ => return Err(Error::new(span, "expected `list`")),
                }
            }
        })
    }
}
