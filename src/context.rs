use crate::{
    constants::Constants, diagnostics::Diagnostics, proc_macro12::TokenTree, stack::Stack,
};

pub struct Context {
    pub bool_stack: Stack<bool>,
    pub diagnostics: Diagnostics,
    pub int_stack: Stack<i32>,
    pub str_constants: Constants<str>,
    pub str_stack: Stack<String>,
    pub tokens_stack: Stack<Vec<TokenTree>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            bool_stack: Stack::new(),
            diagnostics: Diagnostics::new(),
            int_stack: Stack::new(),
            str_constants: Constants::new(),
            str_stack: Stack::new(),
            tokens_stack: Stack::new(),
        }
    }
}
