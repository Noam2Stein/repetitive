use std::cell::Cell;

use crate::{
    ast::UnparsedQuote, context::Context, instruction::Instruction, proc_macro12::TokenTree,
};

pub struct CodegenContext<'a> {
    pub ctx: &'a Context,
    pub instructions: Vec<Instruction<'a>>,
}

pub fn eval_quote<'ctx>(
    quote: UnparsedQuote,
    ctx: &mut CodegenContext<'ctx>,
) -> &'ctx Cell<Vec<TokenTree>> {
    todo!()
}
