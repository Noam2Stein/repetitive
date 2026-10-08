use std::cell::Cell;

use crate::{
    proc_macro12::TokenStream,
    storage::{arena::Arena, stack::Stack},
};

pub struct Storage {
    pub bool_stack: Stack<Cell<bool>>,
    pub int_stack: Stack<Cell<i32>>,
    pub string_arena: Arena<String>,
    pub string_stack: Stack<Cell<String>>,
    pub token_stream_arena: Arena<TokenStream>,
    pub token_stream_stack: Stack<Cell<TokenStream>>,
    pub u8_arena: Arena<u8>,
}

impl Storage {
    pub fn new() -> Self {
        Storage {
            bool_stack: Stack::new(),
            int_stack: Stack::new(),
            string_arena: Arena::new(),
            string_stack: Stack::new(),
            token_stream_arena: Arena::new(),
            token_stream_stack: Stack::new(),
            u8_arena: Arena::new(),
        }
    }
}
