use std::cell::Cell;

use crate::proc_macro12::TokenStream;

pub enum Val<'ctx> {
    Array(ValArray<'ctx>),
    Bool(&'ctx Cell<bool>),
    Int(&'ctx Cell<i32>),
    Range(Box<ValRange<'ctx>>),
    RangeFrom(Box<ValRangeFrom<'ctx>>),
    RangeFull,
    RangeInclusive(Box<ValRangeInclusive<'ctx>>),
    RangeTo(Box<ValRangeTo<'ctx>>),
    RangeToInclusive(Box<ValRangeToInclusive<'ctx>>),
    Str,
    TokenStream(&'ctx Cell<TokenStream>),
    Tuple(ValTuple<'ctx>),
}

pub struct ValArray<'ctx> {
    pub elements: Vec<Val<'ctx>>,
}

pub struct ValRange<'ctx> {
    pub start: Val<'ctx>,
    pub end: Val<'ctx>,
}

pub struct ValRangeFrom<'ctx> {
    pub start: Val<'ctx>,
}

pub struct ValRangeInclusive<'ctx> {
    pub start: Val<'ctx>,
    pub last: Val<'ctx>,
}

pub struct ValRangeTo<'ctx> {
    pub end: Val<'ctx>,
}

pub struct ValRangeToInclusive<'ctx> {
    pub last: Val<'ctx>,
}

pub struct ValTuple<'ctx> {
    pub elements: Vec<Val<'ctx>>,
}
