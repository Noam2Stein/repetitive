use crate::{
    data_structures::{reservation_stack::ReservationStack, stable_storage::StableStorage},
    diagnostics::Diagnostics,
    proc_macro12::TokenTree,
};

pub struct Context {
    pub bool_stack: ReservationStack<bool>,
    pub diagnostics: Diagnostics,
    pub int_stack: ReservationStack<i32>,
    pub str_constants: StableStorage<str>,
    pub str_stack: ReservationStack<String>,
    pub tokenstream_stack: ReservationStack<Vec<TokenTree>>,
}
