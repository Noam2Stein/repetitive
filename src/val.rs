use std::cell::Cell;

use crate::proc_macro12::TokenStream;

pub enum Val<'storage> {
    Array(ValArray<'storage>),
    Bool(&'storage Cell<bool>),
    Int(&'storage Cell<i32>),
    Range(Box<ValRange<'storage>>),
    RangeFrom(Box<ValRangeFrom<'storage>>),
    RangeFull,
    RangeInclusive(Box<ValRangeInclusive<'storage>>),
    RangeTo(Box<ValRangeTo<'storage>>),
    RangeToInclusive(Box<ValRangeToInclusive<'storage>>),
    Str,
    TokenStream(&'storage Cell<TokenStream>),
    Tuple(ValTuple<'storage>),
}

pub struct ValArray<'storage> {
    pub elements: Vec<Val<'storage>>,
}

pub struct ValRange<'storage> {
    pub start: Val<'storage>,
    pub end: Val<'storage>,
}

pub struct ValRangeFrom<'storage> {
    pub start: Val<'storage>,
}

pub struct ValRangeInclusive<'storage> {
    pub start: Val<'storage>,
    pub last: Val<'storage>,
}

pub struct ValRangeTo<'storage> {
    pub end: Val<'storage>,
}

pub struct ValRangeToInclusive<'storage> {
    pub last: Val<'storage>,
}

pub struct ValTuple<'storage> {
    pub elements: Vec<Val<'storage>>,
}
