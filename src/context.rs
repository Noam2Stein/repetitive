use std::cell::Cell;

use crate::{
    constants::Constants, diagnostics::Diagnostics, ident_interner::IdentInterner,
    proc_macro12::TokenStream, stack::Stack,
};

pub struct Context {
    pub bool_stack: Stack<Cell<bool>>,
    pub diagnostics: Diagnostics,
    pub ident_interner: IdentInterner,
    pub int_stack: Stack<Cell<i32>>,
    pub str_constants: Constants<Cell<String>>,
    pub str_stack: Stack<Cell<String>>,
    pub token_stream_constants: Constants<Cell<TokenStream>>,
    pub token_stream_stack: Stack<Cell<TokenStream>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            bool_stack: Stack::new(),
            diagnostics: Diagnostics::new(),
            ident_interner: IdentInterner::new(),
            int_stack: Stack::new(),
            str_constants: Constants::new(),
            str_stack: Stack::new(),
            token_stream_constants: Constants::new(),
            token_stream_stack: Stack::new(),
        }
    }
}
