use std::cell::Cell;

use crate::{
    ast::UnparsedQuote,
    diagnostics::{Diagnostics, RecordedError},
    instruction::{Instruction, InstructionStorage},
    proc_macro12::TokenStream,
    str_interner::StrInterner,
};

pub struct CompileResult<'storage> {
    pub instructions: Vec<Instruction<'storage>>,
    pub output_slot: &'storage Cell<TokenStream>,
}

pub fn compile<'storage>(
    input: TokenStream,
    diagnostics: &Diagnostics,
    instruction_storage: &'storage InstructionStorage,
) -> Result<CompileResult<'storage>, RecordedError> {
    let str_interner = StrInterner::new();
    let _ = UnparsedQuote { stream: input }.parse(diagnostics, &str_interner);
    todo!()
}
