use std::cell::Cell;

use crate::proc_macro12::TokenTree;

pub enum Val<'a> {
    Array(ValArray<'a>),
    Bool(&'a Cell<bool>),
    Int(&'a Cell<i32>),
    Tokens(&'a Cell<Vec<TokenTree>>),
    Range(Box<ValRange<'a>>),
    RangeFrom(Box<ValRangeFrom<'a>>),
    RangeFull,
    RangeInclusive(Box<ValRangeInclusive<'a>>),
    RangeTo(Box<ValRangeTo<'a>>),
    RangeToInclusive(Box<ValRangeToInclusive<'a>>),
    Str,
    Tuple(ValTuple<'a>),
}

pub struct ValArray<'a> {
    pub elements: Vec<Val<'a>>,
}

pub struct ValRange<'a> {
    pub start: Val<'a>,
    pub end: Val<'a>,
}

pub struct ValRangeFrom<'a> {
    pub start: Val<'a>,
}

pub struct ValRangeInclusive<'a> {
    pub start: Val<'a>,
    pub last: Val<'a>,
}

pub struct ValRangeTo<'a> {
    pub end: Val<'a>,
}

pub struct ValRangeToInclusive<'a> {
    pub last: Val<'a>,
}

pub struct ValTuple<'a> {
    pub elements: Vec<Val<'a>>,
}
