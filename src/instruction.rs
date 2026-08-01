use std::cell::Cell;

use crate::proc_macro::{Span, TokenStream};

pub enum Instruction<'a> {
    BoolAnd {
        lhs: &'a Cell<bool>,
        rhs: &'a Cell<bool>,
        dst: &'a Cell<bool>,
    },
    BoolCopy {
        val: &'a Cell<bool>,
        dst: &'a Cell<bool>,
    },
    BoolDisplay {
        val: &'a Cell<bool>,
        dst: &'a Cell<String>,
    },
    BoolEmit {
        val: &'a Cell<bool>,
        dst: &'a Cell<TokenStream>,
        span: Span,
    },
    BoolNot {
        val: &'a Cell<bool>,
        dst: &'a Cell<bool>,
    },
    BoolOr {
        lhs: &'a Cell<bool>,
        rhs: &'a Cell<bool>,
        dst: &'a Cell<bool>,
    },
    BoolXor {
        lhs: &'a Cell<bool>,
        rhs: &'a Cell<bool>,
        dst: &'a Cell<bool>,
    },
    IntAdd {
        lhs: &'a Cell<i32>,
        rhs: &'a Cell<i32>,
        dst: &'a Cell<i32>,
        span: Span,
    },
    IntCopy {
        val: &'a Cell<i32>,
        dst: &'a Cell<i32>,
    },
    IntDisplay {
        val: &'a Cell<i32>,
        dst: &'a Cell<String>,
    },
    IntDiv {
        lhs: &'a Cell<i32>,
        rhs: &'a Cell<i32>,
        dst: &'a Cell<i32>,
        span: Span,
    },
    IntEmit {
        val: &'a Cell<i32>,
        dst: &'a Cell<TokenStream>,
        span: Span,
    },
    IntMul {
        lhs: &'a Cell<i32>,
        rhs: &'a Cell<i32>,
        dst: &'a Cell<i32>,
        span: Span,
    },
    IntNeg {
        val: &'a Cell<i32>,
        dst: &'a Cell<i32>,
        span: Span,
    },
    IntRem {
        lhs: &'a Cell<i32>,
        rhs: &'a Cell<i32>,
        dst: &'a Cell<i32>,
        span: Span,
    },
    IntSub {
        lhs: &'a Cell<i32>,
        rhs: &'a Cell<i32>,
        dst: &'a Cell<i32>,
        span: Span,
    },
    StrCopy {
        val: &'a Cell<String>,
        dst: &'a Cell<String>,
    },
    StrDisplay {
        val: &'a Cell<String>,
        dst: &'a Cell<String>,
    },
    StrEmit {
        val: &'a Cell<String>,
        dst: &'a Cell<TokenStream>,
        span: Span,
    },
    StrEmitStr {
        val: &'a Cell<String>,
        dst: &'a Cell<TokenStream>,
        span: Span,
    },
}
