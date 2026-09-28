use std::cell::Cell;

use crate::proc_macro12::{Span, TokenTree};

pub enum Instruction<'ctx> {
    BoolAnd {
        lhs: &'ctx Cell<bool>,
        rhs: &'ctx Cell<bool>,
        dst: &'ctx Cell<bool>,
    },
    BoolCopy {
        val: &'ctx Cell<bool>,
        dst: &'ctx Cell<bool>,
    },
    BoolDisplay {
        val: &'ctx Cell<bool>,
        dst: &'ctx Cell<String>,
    },
    BoolEmit {
        val: &'ctx Cell<bool>,
        dst: &'ctx Cell<Vec<TokenTree>>,
        span: Span,
    },
    BoolNot {
        val: &'ctx Cell<bool>,
        dst: &'ctx Cell<bool>,
    },
    BoolOr {
        lhs: &'ctx Cell<bool>,
        rhs: &'ctx Cell<bool>,
        dst: &'ctx Cell<bool>,
    },
    BoolXor {
        lhs: &'ctx Cell<bool>,
        rhs: &'ctx Cell<bool>,
        dst: &'ctx Cell<bool>,
    },
    IntAdd {
        lhs: &'ctx Cell<i32>,
        rhs: &'ctx Cell<i32>,
        dst: &'ctx Cell<i32>,
        span: Span,
    },
    IntCopy {
        val: &'ctx Cell<i32>,
        dst: &'ctx Cell<i32>,
    },
    IntDisplay {
        val: &'ctx Cell<i32>,
        dst: &'ctx Cell<String>,
    },
    IntDiv {
        lhs: &'ctx Cell<i32>,
        rhs: &'ctx Cell<i32>,
        dst: &'ctx Cell<i32>,
        span: Span,
    },
    IntEmit {
        val: &'ctx Cell<i32>,
        dst: &'ctx Cell<Vec<TokenTree>>,
        span: Span,
    },
    IntMul {
        lhs: &'ctx Cell<i32>,
        rhs: &'ctx Cell<i32>,
        dst: &'ctx Cell<i32>,
        span: Span,
    },
    IntNeg {
        val: &'ctx Cell<i32>,
        dst: &'ctx Cell<i32>,
        span: Span,
    },
    IntRem {
        lhs: &'ctx Cell<i32>,
        rhs: &'ctx Cell<i32>,
        dst: &'ctx Cell<i32>,
        span: Span,
    },
    IntSub {
        lhs: &'ctx Cell<i32>,
        rhs: &'ctx Cell<i32>,
        dst: &'ctx Cell<i32>,
        span: Span,
    },
    StrCopy {
        val: &'ctx Cell<String>,
        dst: &'ctx Cell<String>,
    },
    StrDisplay {
        val: &'ctx Cell<String>,
        dst: &'ctx Cell<String>,
    },
    StrEmit {
        val: &'ctx Cell<String>,
        dst: &'ctx Cell<Vec<TokenTree>>,
        span: Span,
    },
}
