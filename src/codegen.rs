use std::cell::Cell;

use crate::{
    entrypoint::Context,
    instruction::Instruction,
    proc_macro12::{TokenStream, TokenTree},
};

pub struct CodegenResult<'ctx> {
    pub instructions: Vec<Instruction<'ctx>>,
    pub output_slot: &'ctx Cell<Vec<TokenTree>>,
}

pub fn codegen(input: TokenStream, ctx: &Context) -> CodegenResult<'_> {
    todo!()
}
