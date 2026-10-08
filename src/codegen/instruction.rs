use std::cell::Cell;

use crate::proc_macro12::{Span, TokenStream};

pub enum Instruction<'storage> {
    BoolAnd {
        lhs: &'storage Cell<bool>,
        rhs: &'storage Cell<bool>,
        dst: &'storage Cell<bool>,
    },
    BoolCopy {
        val: &'storage Cell<bool>,
        dst: &'storage Cell<bool>,
    },
    BoolDisplay {
        val: &'storage Cell<bool>,
        dst: &'storage Cell<String>,
    },
    BoolEmit {
        val: &'storage Cell<bool>,
        dst: &'storage Cell<TokenStream>,
        span: Span,
    },
    BoolLoad {
        val: bool,
        dst: &'storage Cell<bool>,
    },
    BoolNot {
        val: &'storage Cell<bool>,
        dst: &'storage Cell<bool>,
    },
    BoolOr {
        lhs: &'storage Cell<bool>,
        rhs: &'storage Cell<bool>,
        dst: &'storage Cell<bool>,
    },
    BoolXor {
        lhs: &'storage Cell<bool>,
        rhs: &'storage Cell<bool>,
        dst: &'storage Cell<bool>,
    },
    IntAdd {
        lhs: &'storage Cell<i32>,
        rhs: &'storage Cell<i32>,
        dst: &'storage Cell<i32>,
        span: Span,
    },
    IntCopy {
        val: &'storage Cell<i32>,
        dst: &'storage Cell<i32>,
    },
    IntDisplay {
        val: &'storage Cell<i32>,
        dst: &'storage Cell<String>,
    },
    IntDiv {
        lhs: &'storage Cell<i32>,
        rhs: &'storage Cell<i32>,
        dst: &'storage Cell<i32>,
        span: Span,
    },
    IntEmit {
        val: &'storage Cell<i32>,
        dst: &'storage Cell<TokenStream>,
        span: Span,
    },
    IntLoad {
        val: i32,
        dst: &'storage Cell<i32>,
    },
    IntMul {
        lhs: &'storage Cell<i32>,
        rhs: &'storage Cell<i32>,
        dst: &'storage Cell<i32>,
        span: Span,
    },
    IntNeg {
        val: &'storage Cell<i32>,
        dst: &'storage Cell<i32>,
        span: Span,
    },
    IntRem {
        lhs: &'storage Cell<i32>,
        rhs: &'storage Cell<i32>,
        dst: &'storage Cell<i32>,
        span: Span,
    },
    IntSub {
        lhs: &'storage Cell<i32>,
        rhs: &'storage Cell<i32>,
        dst: &'storage Cell<i32>,
        span: Span,
    },
    StrCopy {
        val: &'storage Cell<String>,
        dst: &'storage Cell<String>,
    },
    StrDisplay {
        val: &'storage Cell<String>,
        dst: &'storage Cell<String>,
    },
    StrEmit {
        val: &'storage Cell<String>,
        dst: &'storage Cell<TokenStream>,
        span: Span,
    },
    TokenStreamEmit {
        val: &'storage Cell<TokenStream>,
        dst: &'storage Cell<TokenStream>,
    },
}
