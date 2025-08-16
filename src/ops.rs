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
        }
    }

    pub fn compute(
        self,
        args: &[FragmentValue],
        ctx: &mut Context,
    ) -> Result<FragmentValue, Error> {
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
                    return Err(Error::ArgCount {
                        op: "neg",
                        span,
                        expected: 1,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &arg.kind {
                    FragmentValueKind::Int(val) => FragmentValueKind::Int(-val),
                    FragmentValueKind::Float(val) => FragmentValueKind::Float(-val),

                    FragmentValueKind::Bool(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::String(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "neg",
                            kind: arg.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Bool(val) => FragmentValueKind::Bool(!val),

                    FragmentValueKind::Int(_)
                    | FragmentValueKind::Float(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::String(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "not",
                            kind: arg.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs + rhs),
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

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Float(lhs + rhs),
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

                    FragmentValueKind::Bool(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "add",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs - rhs),
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

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Float(lhs - rhs),
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

                    FragmentValueKind::String(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::Bool(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "sub",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs * rhs),
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

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Float(lhs * rhs),
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

                    FragmentValueKind::String(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => {
                            FragmentValueKind::String(lhs.repeat(*rhs as usize))
                        }
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

                    FragmentValueKind::Ident(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => {
                            FragmentValueKind::Ident(lhs.repeat(*rhs as usize))
                        }
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

                    FragmentValueKind::Char(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => {
                            FragmentValueKind::String(lhs.to_string().repeat(*rhs as usize))
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

                    FragmentValueKind::Bool(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "mul",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs / rhs),
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

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Float(lhs / rhs),
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

                    FragmentValueKind::String(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::Bool(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "div",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs % rhs),
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

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Float(lhs % rhs),
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

                    FragmentValueKind::String(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::Bool(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "rem",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs & rhs),
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

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs & rhs),
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

                    FragmentValueKind::Float(_)
                    | FragmentValueKind::String(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "bitand",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs | rhs),
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

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs | rhs),
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

                    FragmentValueKind::Float(_)
                    | FragmentValueKind::String(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "bitor",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs ^ rhs),
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

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs ^ rhs),
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

                    FragmentValueKind::Float(_)
                    | FragmentValueKind::String(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "bitxor",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs << rhs),
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

                    FragmentValueKind::Float(_)
                    | FragmentValueKind::String(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::Bool(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "shl",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Int(lhs >> rhs),
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

                    FragmentValueKind::Float(_)
                    | FragmentValueKind::String(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::Bool(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "shr",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Bool(lhs == rhs),
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

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Bool(lhs == rhs),
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

                    FragmentValueKind::String(lhs) => match &rhs.kind {
                        FragmentValueKind::String(rhs) => FragmentValueKind::Bool(lhs == rhs),
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

                    FragmentValueKind::Ident(lhs) => match &rhs.kind {
                        FragmentValueKind::Ident(rhs) => FragmentValueKind::Bool(lhs == rhs),
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

                    FragmentValueKind::Char(lhs) => match &rhs.kind {
                        FragmentValueKind::Char(rhs) => FragmentValueKind::Bool(lhs == rhs),
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

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs == rhs),
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

                    FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "eq",
                            kind: lhs.kind.kind_str(),
                        });
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
                    return Err(Error::ArgCount {
                        op: "lt",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Bool(lhs < rhs),
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

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Bool(lhs < rhs),
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

                    FragmentValueKind::String(lhs) => match &rhs.kind {
                        FragmentValueKind::String(rhs) => FragmentValueKind::Bool(lhs < rhs),
                        FragmentValueKind::Ident(rhs) => FragmentValueKind::Bool(lhs < rhs),
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

                    FragmentValueKind::Ident(lhs) => match &rhs.kind {
                        FragmentValueKind::Ident(rhs) => FragmentValueKind::Bool(lhs < rhs),
                        FragmentValueKind::String(rhs) => FragmentValueKind::Bool(lhs < rhs),
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

                    FragmentValueKind::Char(lhs) => match &rhs.kind {
                        FragmentValueKind::Char(rhs) => FragmentValueKind::Bool(lhs < rhs),
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

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs < rhs),
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

                    FragmentValueKind::List(_) | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "lt",
                            kind: lhs.kind.kind_str(),
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &rhs.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::Bool(lhs > rhs),
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

                    FragmentValueKind::Float(lhs) => match &rhs.kind {
                        FragmentValueKind::Float(rhs) => FragmentValueKind::Bool(lhs > rhs),
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

                    FragmentValueKind::Char(lhs) => match &rhs.kind {
                        FragmentValueKind::Char(rhs) => FragmentValueKind::Bool(lhs > rhs),
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

                    FragmentValueKind::String(lhs) => match &rhs.kind {
                        FragmentValueKind::String(rhs) => FragmentValueKind::Bool(lhs > rhs),
                        FragmentValueKind::Ident(rhs) => FragmentValueKind::Bool(lhs > rhs),
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

                    FragmentValueKind::Ident(lhs) => match &rhs.kind {
                        FragmentValueKind::Ident(rhs) => FragmentValueKind::Bool(lhs > rhs),
                        FragmentValueKind::String(rhs) => FragmentValueKind::Bool(lhs > rhs),
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

                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs > rhs),
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

                    FragmentValueKind::List(_) | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "gt",
                            kind: lhs.kind.kind_str(),
                        });
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
                    return Err(Error::ArgCount {
                        op: "and",
                        span,
                        expected: 2,
                        inputs_desc: None,
                        found: args.len(),
                    });
                };

                match &lhs.kind {
                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs & rhs),
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
                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs | rhs),
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
                    FragmentValueKind::Bool(lhs) => match &rhs.kind {
                        FragmentValueKind::Bool(rhs) => FragmentValueKind::Bool(lhs ^ rhs),
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
                    FragmentValueKind::Int(lhs) => match &end.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::List(
                            (*lhs..*rhs)
                                .map(|i| FragmentValue {
                                    span: span,
                                    kind: FragmentValueKind::Int(i),
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

                    FragmentValueKind::Float(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "range",
                            kind: "float",
                        });
                    }
                    FragmentValueKind::Char(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "range",
                            kind: "char",
                        });
                    }
                    FragmentValueKind::String(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::Bool(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "range",
                            kind: "string",
                        });
                    }
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
                    FragmentValueKind::Int(lhs) => match &end.kind {
                        FragmentValueKind::Int(rhs) => FragmentValueKind::List(
                            (*lhs..=*rhs)
                                .map(|i| FragmentValue {
                                    span: span,
                                    kind: FragmentValueKind::Int(i),
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

                    FragmentValueKind::Float(_)
                    | FragmentValueKind::String(_)
                    | FragmentValueKind::Char(_)
                    | FragmentValueKind::Ident(_)
                    | FragmentValueKind::Bool(_)
                    | FragmentValueKind::List(_)
                    | FragmentValueKind::Tokens(_) => {
                        return Err(Error::CannotPerform {
                            span,
                            op: "range_inclusive",
                            kind: "float",
                        });
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
                    FragmentValueKind::List(list) => FragmentValueKind::Int(list.len() as i128),

                    FragmentValueKind::String(string) => {
                        FragmentValueKind::Int(string.len() as i128)
                    }

                    FragmentValueKind::Ident(ident) => FragmentValueKind::Int(ident.len() as i128),

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
                    FragmentValueKind::List(list) => list,
                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list",
                            found: list.kind.kind_str(),
                        });
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
                    FragmentValueKind::List(lhs) => match &rhs.kind {
                        FragmentValueKind::List(rhs) => FragmentValueKind::List(
                            lhs.iter()
                                .zip(rhs.iter())
                                .map(|(lhs, rhs)| FragmentValue {
                                    span: span,
                                    kind: FragmentValueKind::List(vec![lhs.clone(), rhs.clone()]),
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
                    FragmentValueKind::List(lhs) => match &rhs.kind {
                        FragmentValueKind::List(rhs) => {
                            FragmentValueKind::List(lhs.iter().chain(rhs.iter()).cloned().collect())
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
                    FragmentValueKind::List(list) => FragmentValueKind::Bool(
                        list.iter()
                            .map(|item| Op::Eq(span).compute(&[item.clone(), value.clone()], ctx))
                            .collect::<Result<Vec<_>, _>>()?
                            .iter()
                            .any(|eq| match eq.kind {
                                FragmentValueKind::Bool(bool) => bool,
                                _ => unreachable!(),
                            }),
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
                    FragmentValueKind::List(parts) => parts,
                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list",
                            found: parts.kind.kind_str(),
                        });
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

                        FragmentValueKind::List(_) | FragmentValueKind::Tokens(_) => {
                            return Err(Error::CannotPerform {
                                span,
                                op: "stringify",
                                kind: part.kind.kind_str(),
                            });
                        }
                    }
                }

                FragmentValueKind::Ident(output_str)
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
                    FragmentValueKind::List(parts) => parts,
                    _ => {
                        return Err(Error::ExpectedFound {
                            span,
                            expected: "list",
                            found: parts.kind.kind_str(),
                        });
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

                        FragmentValueKind::List(_) | FragmentValueKind::Tokens(_) => {
                            return Err(Error::CannotPerform {
                                span,
                                op: "stringify",
                                kind: part.kind.kind_str(),
                            });
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
