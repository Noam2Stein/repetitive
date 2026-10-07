use std::cell::Cell;

use crate::{
    arena::{Arena, MixedArena},
    diagnostics::Diagnostics,
    proc_macro12::TokenStream,
    stack::Stack,
    str_interner::StrInterner,
};

pub struct Context<'ctx> {
    pub bool_stack: Stack<Cell<bool>>,
    pub diagnostics: Diagnostics,
    pub int_stack: Stack<Cell<i32>>,
    pub mixed_arena: MixedArena,
    pub str_interner: StrInterner<'ctx>,
    pub string_arena: Arena<String>,
    pub string_stack: Stack<Cell<String>>,
    pub token_stream_arena: Arena<TokenStream>,
    pub token_stream_stack: Stack<Cell<TokenStream>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new() -> Self {
        Context {
            bool_stack: Stack::new(),
            diagnostics: Diagnostics::new(),
            int_stack: Stack::new(),
            mixed_arena: Arena::new(),
            str_interner: StrInterner::new(),
            string_arena: Arena::new(),
            string_stack: Stack::new(),
            token_stream_arena: Arena::new(),
            token_stream_stack: Stack::new(),
        }
    }
}
