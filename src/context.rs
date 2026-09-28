use crate::{diagnostics::Diagnostics, proc_macro12::TokenStream, stack::Stack};

pub struct Context {
    pub bool_stack: Stack<bool>,
    pub diagnostics: Diagnostics,
    pub int_stack: Stack<i32>,
    pub str_stack: Stack<String>,
    pub token_stream_stack: Stack<TokenStream>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            bool_stack: Stack::new(),
            diagnostics: Diagnostics::new(),
            int_stack: Stack::new(),
            str_stack: Stack::new(),
            token_stream_stack: Stack::new(),
        }
    }
}
