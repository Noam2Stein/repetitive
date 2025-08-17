use proc_macro2::Span;
use syn::{Token, parse::ParseStream, spanned::Spanned};

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
    Contains(Span),
    ConcatIdent(Span),
    ConcatString(Span),

    // Conversion
    ToFloat(Span),

    // Rounding
    Round(Span),
    Floor(Span),
    Ceil(Span),
    Trunc(Span),
    Atrunc(Span),
    IRound(Span),
    IFloor(Span),
    ICeil(Span),
    ITrunc(Span),
    IAtrunc(Span),

    // Log
    Log(Span),
    Log2(Span),
    Log10(Span),
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
            Self::Contains(span) => *span,
            Self::ConcatIdent(span) => *span,
            Self::ConcatString(span) => *span,
            Self::ToFloat(span) => *span,
            Self::Round(span) => *span,
            Self::Floor(span) => *span,
            Self::Ceil(span) => *span,
            Self::Trunc(span) => *span,
            Self::Atrunc(span) => *span,
            Self::IRound(span) => *span,
            Self::IFloor(span) => *span,
            Self::ICeil(span) => *span,
            Self::ITrunc(span) => *span,
            Self::IAtrunc(span) => *span,
            Self::Log(span) => *span,
            Self::Log2(span) => *span,
            Self::Log10(span) => *span,
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
            Method::Contains(span) => Op::Contains(span),
            Method::ConcatIdent(span) => Op::ConcatIdent(span),
            Method::ConcatString(span) => Op::ConcatString(span),
            Method::ToFloat(span) => Op::ToFloat(span),
            Method::Round(span) => Op::Round(span),
            Method::Floor(span) => Op::Floor(span),
            Method::Ceil(span) => Op::Ceil(span),
            Method::Trunc(span) => Op::Trunc(span),
            Method::Atrunc(span) => Op::Atrunc(span),
            Method::IRound(span) => Op::IRound(span),
            Method::IFloor(span) => Op::IFloor(span),
            Method::ICeil(span) => Op::ICeil(span),
            Method::ITrunc(span) => Op::ITrunc(span),
            Method::IAtrunc(span) => Op::IAtrunc(span),
            Method::Log(span) => Op::Log(span),
            Method::Log2(span) => Op::Log2(span),
            Method::Log10(span) => Op::Log10(span),
        }
    }

    pub fn compute(self, args: &[Value], ctx: &mut Context) -> Result<Value, Error> {
        if args.iter().any(|arg| arg.is_unknown()) {
            return Ok(Value::unknown(
                args.iter()
                    .find_map(|arg| {
                        if let ValueKind::Unknown(guard) = arg.kind {
                            Some(guard)
                        } else {
                            None
                        }
                    })
                    .unwrap(),
            ));
        }

        let output_kind = match self {
            Self::IfElse(span) => {
                let [cond, then, otherwise] = args else {
                    return Err(Error::ArgCount {
                        op: "if-else",
                        span,
                        expected: 3,
                        inputs_desc: Some("condition, then, otherwise"),
                        found: args.len(),
                    });
                };

                let cond = match &cond.kind {
                    ValueKind::Bool(cond) => *cond,
                    _ => unreachable!("if-else condition must be a bool"),
                };

                if cond {
                    then.kind.clone()
                } else {
                    otherwise.kind.clone()
                }
            }

            Self::Neg(span) => {
                let [arg] = args else {
                    return Err(Error::ArgCount {
                        op: "neg",
                        span,
                        expected: 1,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &arg.kind {
                    ValueKind::Int(val) => ValueKind::Int(-val),
                    ValueKind::Float(val) => ValueKind::Float(-val),

                    ValueKind::Bool(_)
                    | ValueKind::Ident(_)
                    | ValueKind::String(_)
                    | ValueKind::Char(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "neg",
                            kind: arg.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute neg"),
                }
            }

            Self::Not(span) => {
                let [arg] = args else {
                    return Err(Error::ArgCount {
                        op: "not",
                        span,
                        expected: 1,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &arg.kind {
                    ValueKind::Bool(val) => ValueKind::Bool(!val),

                    ValueKind::Int(_)
                    | ValueKind::Float(_)
                    | ValueKind::Ident(_)
                    | ValueKind::String(_)
                    | ValueKind::Char(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "not",
                            kind: arg.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute not"),
                }
            }

            Self::Add(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "add",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Int(lhs + rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "+",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(lhs) => match &rhs.kind {
                        ValueKind::Float(rhs) => ValueKind::Float(lhs + rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "float",
                                op: "+",
                                expected: "float",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::String(lhs) => match &rhs.kind {
                        ValueKind::String(rhs) => ValueKind::String(format!("{lhs}{rhs}")),
                        ValueKind::Ident(rhs) => ValueKind::String(format!("{lhs}{rhs}")),
                        ValueKind::Char(rhs) => ValueKind::String(format!("{lhs}{rhs}")),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "string",
                                op: "+",
                                expected: "string, ident, or char",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Ident(lhs) => match &rhs.kind {
                        ValueKind::Ident(rhs) => ValueKind::Ident(format!("{lhs}{rhs}")),
                        ValueKind::String(rhs) => ValueKind::Ident(format!("{lhs}{rhs}")),
                        ValueKind::Char(rhs) => ValueKind::Ident(format!("{lhs}{rhs}")),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "ident",
                                op: "+",
                                expected: "ident, string, or char",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Bool(_)
                    | ValueKind::Char(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "add",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute add"),
                }
            }

            Self::Sub(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "sub",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Int(lhs - rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "-",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(lhs) => match &rhs.kind {
                        ValueKind::Float(rhs) => ValueKind::Float(lhs - rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "float",
                                op: "-",
                                expected: "float",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::String(_)
                    | ValueKind::Ident(_)
                    | ValueKind::Char(_)
                    | ValueKind::Bool(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "sub",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute sub"),
                }
            }

            Self::Mul(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "mul",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Int(lhs * rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "*",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(lhs) => match &rhs.kind {
                        ValueKind::Float(rhs) => ValueKind::Float(lhs * rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "float",
                                op: "*",
                                expected: "float",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::String(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::String(lhs.repeat(*rhs as usize)),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "ident",
                                op: "*",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Ident(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Ident(lhs.repeat(*rhs as usize)),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "ident",
                                op: "*",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Char(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => {
                            ValueKind::String(lhs.to_string().repeat(*rhs as usize))
                        }
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "char",
                                op: "*",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Bool(_) | ValueKind::List(_) | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "mul",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute mul"),
                }
            }

            Self::Div(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "div",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Int(lhs / rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "/",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(lhs) => match &rhs.kind {
                        ValueKind::Float(rhs) => ValueKind::Float(lhs / rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "float",
                                op: "/",
                                expected: "float",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::String(_)
                    | ValueKind::Ident(_)
                    | ValueKind::Char(_)
                    | ValueKind::Bool(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "div",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute div"),
                }
            }

            Self::Rem(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "rem",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Int(lhs % rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "rem",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(lhs) => match &rhs.kind {
                        ValueKind::Float(rhs) => ValueKind::Float(lhs % rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "float",
                                op: "rem",
                                expected: "float",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::String(_)
                    | ValueKind::Ident(_)
                    | ValueKind::Char(_)
                    | ValueKind::Bool(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "rem",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute rem"),
                }
            }

            Self::BitAnd(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "bitand",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Int(lhs & rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "bitand",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Bool(lhs) => match &rhs.kind {
                        ValueKind::Bool(rhs) => ValueKind::Bool(lhs & rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "bool",
                                op: "bitand",
                                expected: "bool",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(_)
                    | ValueKind::String(_)
                    | ValueKind::Ident(_)
                    | ValueKind::Char(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "bitand",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute bitand"),
                }
            }

            Self::BitOr(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "bitor",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Int(lhs | rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "bitor",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Bool(lhs) => match &rhs.kind {
                        ValueKind::Bool(rhs) => ValueKind::Bool(lhs | rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "bool",
                                op: "bitor",
                                expected: "bool",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(_)
                    | ValueKind::String(_)
                    | ValueKind::Ident(_)
                    | ValueKind::Char(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "bitor",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute bitor"),
                }
            }

            Self::BitXor(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "bitxor",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Int(lhs ^ rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "bitxor",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Bool(lhs) => match &rhs.kind {
                        ValueKind::Bool(rhs) => ValueKind::Bool(lhs ^ rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "bool",
                                op: "bitxor",
                                expected: "bool",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(_)
                    | ValueKind::String(_)
                    | ValueKind::Ident(_)
                    | ValueKind::Char(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "bitxor",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute bitxor"),
                }
            }

            Self::Shl(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "shl",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Int(lhs << rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "shl",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(_)
                    | ValueKind::String(_)
                    | ValueKind::Ident(_)
                    | ValueKind::Char(_)
                    | ValueKind::Bool(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "shl",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute shl"),
                }
            }

            Self::Shr(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "shr",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Int(lhs >> rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "shr",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(_)
                    | ValueKind::String(_)
                    | ValueKind::Ident(_)
                    | ValueKind::Char(_)
                    | ValueKind::Bool(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "shr",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute shr"),
                }
            }

            Self::Eq(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "eq",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Bool(lhs == rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "eq",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(lhs) => match &rhs.kind {
                        ValueKind::Float(rhs) => ValueKind::Bool(lhs == rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "float",
                                op: "eq",
                                expected: "float",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::String(lhs) => match &rhs.kind {
                        ValueKind::String(rhs) => ValueKind::Bool(lhs == rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "string",
                                op: "eq",
                                expected: "string",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Ident(lhs) => match &rhs.kind {
                        ValueKind::Ident(rhs) => ValueKind::Bool(lhs == rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "ident",
                                op: "eq",
                                expected: "ident",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Char(lhs) => match &rhs.kind {
                        ValueKind::Char(rhs) => ValueKind::Bool(lhs == rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "char",
                                op: "eq",
                                expected: "char",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Bool(lhs) => match &rhs.kind {
                        ValueKind::Bool(rhs) => ValueKind::Bool(lhs == rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "bool",
                                op: "eq",
                                expected: "bool",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::List(lhs) => match &rhs.kind {
                        ValueKind::List(rhs) => ValueKind::Bool('list_eq: {
                            if lhs.len() != rhs.len() {
                                break 'list_eq false;
                            }

                            for (a, b) in lhs.iter().zip(rhs.iter()) {
                                let eq = Op::Eq(span).compute(&[a.clone(), b.clone()], ctx)?;

                                match &eq.kind {
                                    ValueKind::Unknown(guard) => {
                                        return Ok(Value::unknown(*guard));
                                    }

                                    ValueKind::Bool(val) => {
                                        if !val {
                                            break 'list_eq false;
                                        }
                                    }

                                    _ => unreachable!("list eq must be a bool"),
                                }
                            }

                            true
                        }),

                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "list",
                                op: "eq",
                                expected: "list",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "eq",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute eq"),
                }
            }

            Self::Ne(span) => {
                let eq = Self::Eq(span).compute(args, ctx)?;

                match &eq.kind {
                    ValueKind::Bool(val) => ValueKind::Bool(!val),
                    _ => unreachable!("ne must be a bool"),
                }
            }

            Self::Lt(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "lt",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Bool(lhs < rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "lt",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(lhs) => match &rhs.kind {
                        ValueKind::Float(rhs) => ValueKind::Bool(lhs < rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "float",
                                op: "lt",
                                expected: "float",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::String(lhs) => match &rhs.kind {
                        ValueKind::String(rhs) => ValueKind::Bool(lhs < rhs),
                        ValueKind::Ident(rhs) => ValueKind::Bool(lhs < rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "string",
                                op: "lt",
                                expected: "string",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Ident(lhs) => match &rhs.kind {
                        ValueKind::Ident(rhs) => ValueKind::Bool(lhs < rhs),
                        ValueKind::String(rhs) => ValueKind::Bool(lhs < rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "ident",
                                op: "lt",
                                expected: "ident",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Char(lhs) => match &rhs.kind {
                        ValueKind::Char(rhs) => ValueKind::Bool(lhs < rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "char",
                                op: "lt",
                                expected: "char",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Bool(lhs) => match &rhs.kind {
                        ValueKind::Bool(rhs) => ValueKind::Bool(lhs < rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "bool",
                                op: "lt",
                                expected: "bool",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::List(_) | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "lt",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute lt"),
                }
            }

            Self::Gt(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "gt",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Int(lhs) => match &rhs.kind {
                        ValueKind::Int(rhs) => ValueKind::Bool(lhs > rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "gt",
                                expected: "int",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(lhs) => match &rhs.kind {
                        ValueKind::Float(rhs) => ValueKind::Bool(lhs > rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "float",
                                op: "gt",
                                expected: "float",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Char(lhs) => match &rhs.kind {
                        ValueKind::Char(rhs) => ValueKind::Bool(lhs > rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "char",
                                op: "gt",
                                expected: "char",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::String(lhs) => match &rhs.kind {
                        ValueKind::String(rhs) => ValueKind::Bool(lhs > rhs),
                        ValueKind::Ident(rhs) => ValueKind::Bool(lhs > rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "string",
                                op: "gt",
                                expected: "string",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Ident(lhs) => match &rhs.kind {
                        ValueKind::Ident(rhs) => ValueKind::Bool(lhs > rhs),
                        ValueKind::String(rhs) => ValueKind::Bool(lhs > rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "ident",
                                op: "gt",
                                expected: "ident",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Bool(lhs) => match &rhs.kind {
                        ValueKind::Bool(rhs) => ValueKind::Bool(lhs > rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "bool",
                                op: "gt",
                                expected: "bool",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::List(_) | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "gt",
                            kind: lhs.kind.kind_str(),
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute gt"),
                }
            }

            Self::Le(span) => {
                let gt = Self::Gt(span).compute(args, ctx)?;

                match &gt.kind {
                    ValueKind::Unknown(guard) => {
                        return Ok(Value::unknown(*guard));
                    }

                    ValueKind::Bool(val) => ValueKind::Bool(!val),
                    _ => unreachable!("le must be a bool"),
                }
            }

            Self::Ge(span) => {
                let lt = Self::Lt(span).compute(args, ctx)?;

                match &lt.kind {
                    ValueKind::Unknown(guard) => {
                        return Ok(Value::unknown(*guard));
                    }

                    ValueKind::Bool(val) => ValueKind::Bool(!val),
                    _ => unreachable!("ge must be a bool"),
                }
            }

            Self::Min(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "min",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                let lhs_is_greater = Self::Gt(span).compute(&[lhs.clone(), rhs.clone()], ctx)?;
                let lhs_is_greater = match &lhs_is_greater.kind {
                    ValueKind::Unknown(guard) => {
                        return Ok(Value::unknown(*guard));
                    }

                    ValueKind::Bool(val) => *val,
                    _ => unreachable!("min must be a bool"),
                };

                if lhs_is_greater {
                    rhs.kind.clone()
                } else {
                    lhs.kind.clone()
                }
            }

            Self::Max(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "max",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                let lhs_is_greater = Self::Gt(span).compute(&[lhs.clone(), rhs.clone()], ctx)?;
                let lhs_is_greater = match &lhs_is_greater.kind {
                    ValueKind::Unknown(guard) => {
                        return Ok(Value::unknown(*guard));
                    }

                    ValueKind::Bool(val) => *val,
                    _ => unreachable!("max must be a bool"),
                };

                if lhs_is_greater {
                    lhs.kind.clone()
                } else {
                    rhs.kind.clone()
                }
            }

            Self::Clamp(span) => {
                let [value, min, max] = args else {
                    return Err(Error::ArgCount {
                        op: "clamp",
                        span,
                        expected: 3,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                let is_too_low = Self::Lt(span).compute(&[value.clone(), min.clone()], ctx)?;
                let is_too_low = match &is_too_low.kind {
                    ValueKind::Unknown(guard) => {
                        return Ok(Value::unknown(*guard));
                    }

                    ValueKind::Bool(val) => *val,
                    _ => unreachable!("clamp must be a bool"),
                };

                let is_too_high = Self::Gt(span).compute(&[value.clone(), max.clone()], ctx)?;
                let is_too_high = match &is_too_high.kind {
                    ValueKind::Unknown(guard) => {
                        return Ok(Value::unknown(*guard));
                    }

                    ValueKind::Bool(val) => *val,
                    _ => unreachable!("clamp must be a bool"),
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
                    return Err(Error::ArgCount {
                        op: "and",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Bool(lhs) => match &rhs.kind {
                        ValueKind::Bool(rhs) => ValueKind::Bool(lhs & rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "bool",
                                op: "and",
                                expected: "bool",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "bool",
                            found: lhs.kind.kind_str(),
                        });
                    }
                }
            }

            Self::Or(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "or",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Bool(lhs) => match &rhs.kind {
                        ValueKind::Bool(rhs) => ValueKind::Bool(lhs | rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "bool",
                                op: "or",
                                expected: "bool",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "bool",
                            found: lhs.kind.kind_str(),
                        });
                    }
                }
            }

            Self::Xor(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "xor",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::Bool(lhs) => match &rhs.kind {
                        ValueKind::Bool(rhs) => ValueKind::Bool(lhs ^ rhs),
                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "bool",
                                op: "xor",
                                expected: "bool",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "bool",
                            found: lhs.kind.kind_str(),
                        });
                    }
                }
            }

            Self::Range(span) => {
                let [start, end] = args else {
                    return Err(Error::ArgCount {
                        op: "range",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &start.kind {
                    ValueKind::Int(lhs) => match &end.kind {
                        ValueKind::Int(rhs) => ValueKind::List(
                            (*lhs..*rhs)
                                .map(|i| Value {
                                    span: span,
                                    kind: ValueKind::Int(i),
                                })
                                .collect(),
                        ),

                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "range",
                                expected: "int",
                                found: end.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "range",
                            kind: "float",
                        });
                    }
                    ValueKind::Char(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "range",
                            kind: "char",
                        });
                    }
                    ValueKind::String(_)
                    | ValueKind::Ident(_)
                    | ValueKind::Bool(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "range",
                            kind: "string",
                        });
                    }

                    ValueKind::Unknown(_) => unreachable!("unknown in compute range"),
                }
            }

            Self::RangeInclusive(span) => {
                let [start, end] = args else {
                    return Err(Error::ArgCount {
                        op: "range_inclusive",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &start.kind {
                    ValueKind::Int(lhs) => match &end.kind {
                        ValueKind::Int(rhs) => ValueKind::List(
                            (*lhs..=*rhs)
                                .map(|i| Value {
                                    span: span,
                                    kind: ValueKind::Int(i),
                                })
                                .collect(),
                        ),

                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "int",
                                op: "range_inclusive",
                                expected: "int",
                                found: end.kind.kind_str(),
                            });
                        }
                    },

                    ValueKind::Float(_)
                    | ValueKind::String(_)
                    | ValueKind::Char(_)
                    | ValueKind::Ident(_)
                    | ValueKind::Bool(_)
                    | ValueKind::List(_)
                    | ValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "range_inclusive",
                            kind: "float",
                        });
                    }

                    ValueKind::Unknown(_) => {
                        unreachable!("unknown in compute range_inclusive")
                    }
                }
            }

            Self::Len(span) => {
                let [arg] = args else {
                    return Err(Error::ArgCount {
                        op: "len",
                        span,
                        expected: 1,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &arg.kind {
                    ValueKind::List(list) => ValueKind::Int(list.len() as i128),

                    ValueKind::String(string) => ValueKind::Int(string.len() as i128),

                    ValueKind::Ident(ident) => ValueKind::Int(ident.len() as i128),

                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list, string, or ident",
                            found: arg.kind.kind_str(),
                        });
                    }
                }
            }

            Self::Index(span) => {
                let [list, index] = args else {
                    return Err(Error::ArgCount {
                        op: "index",
                        span,
                        expected: 2,
                        inputs_desc: Some("list and index"),
                        found: args.len(),
                    });
                };

                let items = match &list.kind {
                    ValueKind::List(list) => list,
                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list",
                            found: list.kind.kind_str(),
                        });
                    }
                };

                match &index.kind {
                    ValueKind::Int(index) => items[*index as usize].kind.clone(),

                    ValueKind::List(indicies) => ValueKind::List(
                        indicies
                            .iter()
                            .map(|index| {
                                Op::Index(span).compute(&[list.clone(), index.clone()], ctx)
                            })
                            .flatten()
                            .collect(),
                    ),

                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "int",
                            found: index.kind.kind_str(),
                        });
                    }
                }
            }

            Self::Enumerate(span) => {
                let [list] = args else {
                    return Err(Error::ArgCount {
                        op: "enumerate",
                        span,
                        expected: 1,
                        inputs_desc: Some("list"),
                        found: args.len(),
                    });
                };

                match &list.kind {
                    ValueKind::List(list) => ValueKind::List(
                        list.iter()
                            .enumerate()
                            .map(|(index, item)| Value {
                                span: span,
                                kind: ValueKind::List(vec![
                                    Value {
                                        span: self.span(),
                                        kind: ValueKind::Int(index as i128),
                                    },
                                    item.clone(),
                                ]),
                            })
                            .collect(),
                    ),

                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list",
                            found: list.kind.kind_str(),
                        });
                    }
                }
            }

            Self::Zip(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "zip",
                        span,
                        expected: 2,
                        inputs_desc: Some("two lists"),
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::List(lhs) => match &rhs.kind {
                        ValueKind::List(rhs) => ValueKind::List(
                            lhs.iter()
                                .zip(rhs.iter())
                                .map(|(lhs, rhs)| Value {
                                    span: span,
                                    kind: ValueKind::List(vec![lhs.clone(), rhs.clone()]),
                                })
                                .collect(),
                        ),

                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "list",
                                op: "zip",
                                expected: "list",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list",
                            found: lhs.kind.kind_str(),
                        });
                    }
                }
            }

            Self::Chain(span) => {
                let [lhs, rhs] = args else {
                    return Err(Error::ArgCount {
                        op: "chain",
                        span,
                        expected: 2,
                        inputs_desc: Some("two lists"),
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    ValueKind::List(lhs) => match &rhs.kind {
                        ValueKind::List(rhs) => {
                            ValueKind::List(lhs.iter().chain(rhs.iter()).cloned().collect())
                        }

                        _ => {
                            return Err(Error::ExpectedRhsFound {
                                span,
                                lhs: "list",
                                op: "chain",
                                expected: "list",
                                found: rhs.kind.kind_str(),
                            });
                        }
                    },

                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list",
                            found: lhs.kind.kind_str(),
                        });
                    }
                }
            }

            Self::Contains(span) => {
                let [list, value] = args else {
                    return Err(Error::ArgCount {
                        op: "contains",
                        span,
                        expected: 2,
                        inputs_desc: Some("list and value"),
                        found: args.len(),
                    });
                };

                match &list.kind {
                    ValueKind::List(list) => ValueKind::Bool('contains: {
                        for item in list {
                            let eq = Op::Eq(span).compute(&[item.clone(), value.clone()], ctx)?;

                            match &eq.kind {
                                ValueKind::Unknown(guard) => {
                                    return Ok(Value::unknown(*guard));
                                }

                                ValueKind::Bool(val) => {
                                    if *val {
                                        break 'contains true;
                                    }
                                }

                                _ => unreachable!("contains eq must be a bool"),
                            }
                        }

                        false
                    }),

                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list",
                            found: list.kind.kind_str(),
                        });
                    }
                }
            }

            Self::ConcatIdent(span) => {
                let [parts] = args else {
                    return Err(Error::ArgCount {
                        op: "concat_ident",
                        span,
                        expected: 1,
                        inputs_desc: Some("list to concat"),
                        found: args.len(),
                    });
                };

                let parts = match &parts.kind {
                    ValueKind::List(parts) => parts,
                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list",
                            found: parts.kind.kind_str(),
                        });
                    }
                };

                if parts.iter().any(|part| part.is_unknown()) {
                    return Ok(Value::unknown(
                        parts
                            .iter()
                            .find_map(|part| {
                                if let ValueKind::Unknown(guard) = part.kind {
                                    Some(guard)
                                } else {
                                    None
                                }
                            })
                            .unwrap(),
                    ));
                }

                let mut output_str = String::new();
                for part in parts {
                    match &part.kind {
                        ValueKind::Ident(ident) => output_str.push_str(ident.as_str()),
                        ValueKind::String(string) => output_str.push_str(string.as_str()),
                        ValueKind::Char(char) => output_str.push(*char),
                        ValueKind::Int(int) => output_str.push_str(int.to_string().as_str()),
                        ValueKind::Float(float) => output_str.push_str(float.to_string().as_str()),
                        ValueKind::Bool(bool) => output_str.push_str(bool.to_string().as_str()),

                        ValueKind::List(_) | ValueKind::Tokens(_) => {
                            return Err(Error::CannotPerform {
                                span,
                                op: "stringify",
                                kind: part.kind.kind_str(),
                            });
                        }

                        ValueKind::Unknown(_) => {
                            unreachable!("unknown in compute concat_ident")
                        }
                    }
                }

                ValueKind::Ident(output_str)
            }

            Self::ConcatString(span) => {
                let [parts] = args else {
                    return Err(Error::ArgCount {
                        op: "concat_string",
                        span,
                        expected: 1,
                        inputs_desc: Some("list to concat"),
                        found: args.len(),
                    });
                };

                let parts = match &parts.kind {
                    ValueKind::List(parts) => parts,
                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list",
                            found: parts.kind.kind_str(),
                        });
                    }
                };

                if parts.iter().any(|part| part.is_unknown()) {
                    return Ok(Value::unknown(
                        parts
                            .iter()
                            .find_map(|part| {
                                if let ValueKind::Unknown(guard) = part.kind {
                                    Some(guard)
                                } else {
                                    None
                                }
                            })
                            .unwrap(),
                    ));
                }

                let mut output_str = String::new();
                for part in parts {
                    match &part.kind {
                        ValueKind::Ident(ident) => output_str.push_str(ident.as_str()),
                        ValueKind::String(string) => output_str.push_str(string.as_str()),
                        ValueKind::Char(char) => output_str.push(*char),
                        ValueKind::Int(int) => output_str.push_str(int.to_string().as_str()),
                        ValueKind::Float(float) => output_str.push_str(float.to_string().as_str()),
                        ValueKind::Bool(bool) => output_str.push_str(bool.to_string().as_str()),

                        ValueKind::List(_) | ValueKind::Tokens(_) => {
                            return Err(Error::CannotPerform {
                                span,
                                op: "stringify",
                                kind: part.kind.kind_str(),
                            });
                        }

                        ValueKind::Unknown(_) => {
                            unreachable!("unknown in compute concat_string")
                        }
                    }
                }

                ValueKind::String(output_str)
            }

            Self::ToFloat(span) => {
                let [value] = args else {
                    return Err(Error::ArgCount {
                        op: "to_float",
                        span,
                        expected: 1,
                        inputs_desc: Some("value to convert"),
                        found: args.len(),
                    });
                };

                match &value.kind {
                    ValueKind::Int(int) => ValueKind::Float(*int as f64),
                    ValueKind::Float(float) => ValueKind::Float(*float),
                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "int",
                            found: value.kind.kind_str(),
                        });
                    }
                }
            }

            Self::Round(span)
            | Self::Floor(span)
            | Self::Ceil(span)
            | Self::Trunc(span)
            | Self::Atrunc(span)
            | Self::IRound(span)
            | Self::IFloor(span)
            | Self::ICeil(span)
            | Self::ITrunc(span)
            | Self::IAtrunc(span) => {
                let [value] = args else {
                    return Err(Error::ArgCount {
                        op: "round",
                        span,
                        expected: 1,
                        inputs_desc: Some("value to round"),
                        found: args.len(),
                    });
                };

                let ValueKind::Float(float) = value.kind else {
                    return Err(Error::ExpectedFound {
                        span,
                        expected: "float",
                        found: value.kind.kind_str(),
                    });
                };

                let float_output = match self {
                    Self::Round(_) | Self::IRound(_) => float.round(),
                    Self::Floor(_) | Self::IFloor(_) => float.floor(),
                    Self::Ceil(_) | Self::ICeil(_) => float.ceil(),
                    Self::Trunc(_) | Self::ITrunc(_) => float.trunc(),
                    Self::Atrunc(_) | Self::IAtrunc(_) => {
                        if float.is_sign_negative() {
                            float.floor()
                        } else {
                            float.ceil()
                        }
                    }
                    _ => unreachable!("round op guarenteed"),
                };

                let is_to_int = matches!(
                    self,
                    Self::IRound(_)
                        | Self::IFloor(_)
                        | Self::ICeil(_)
                        | Self::ITrunc(_)
                        | Self::IAtrunc(_)
                );

                if is_to_int {
                    ValueKind::Int(float_output as i128)
                } else {
                    ValueKind::Float(float_output)
                }
            }

            Self::Log(span) => {
                let [value, base] = args else {
                    return Err(Error::ArgCount {
                        op: "log",
                        span,
                        expected: 2,
                        inputs_desc: Some("value and base"),
                        found: args.len(),
                    });
                };

                let ValueKind::Float(float) = value.kind else {
                    return Err(Error::ExpectedFound {
                        span,
                        expected: "float",
                        found: value.kind.kind_str(),
                    });
                };

                let ValueKind::Float(base) = base.kind else {
                    return Err(Error::ExpectedFound {
                        span,
                        expected: "float",
                        found: base.kind.kind_str(),
                    });
                };

                ValueKind::Float(float.log(base))
            }

            Self::Log2(span) => {
                let [value] = args else {
                    return Err(Error::ArgCount {
                        op: "log2",
                        span,
                        expected: 1,
                        inputs_desc: Some("value"),
                        found: args.len(),
                    });
                };

                let ValueKind::Float(float) = value.kind else {
                    return Err(Error::ExpectedFound {
                        span,
                        expected: "float",
                        found: value.kind.kind_str(),
                    });
                };

                ValueKind::Float(float.log2())
            }

            Self::Log10(span) => {
                let [value] = args else {
                    return Err(Error::ArgCount {
                        op: "log10",
                        span,
                        expected: 1,
                        inputs_desc: Some("value"),
                        found: args.len(),
                    });
                };

                let ValueKind::Float(float) = value.kind else {
                    return Err(Error::ExpectedFound {
                        span,
                        expected: "float",
                        found: value.kind.kind_str(),
                    });
                };

                ValueKind::Float(float.log10())
            }
        };

        Ok(Value {
            span: self.span(),
            kind: output_kind,
        })
    }
}
