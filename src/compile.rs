use std::cell::Cell;

use crate::{
    diagnostics::{Diagnostics, RecordedError},
    instruction::{Instruction, InstructionStorage},
    proc_macro12::TokenStream,
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
    todo!()
}
