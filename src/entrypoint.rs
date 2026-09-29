use crate::{
    compile::{CompileResult, compile},
    diagnostics::{Diagnostics, RecordedError},
    execute::execute,
    instruction::InstructionStorage,
    proc_macro12::TokenStream,
};

pub struct RepetitiveResult {
    pub output: TokenStream,
    pub diagnostics: Diagnostics,
}

/// The entry point of the macro.
///
/// This returns diagnostics separately from the output token stream, whereas
/// the public `repetitive` function embeds diagnostics inside the token stream.
/// This approach is required for unit tests.
pub fn repetitive(input: TokenStream) -> RepetitiveResult {
    let diagnostics = Diagnostics::new();
    let instruction_storage = InstructionStorage::new();

    // This should be replaced with a `try` block once they are stabilized
    let output = (|| {
        let CompileResult {
            instructions,
            output_slot,
        } = compile(input, &diagnostics, &instruction_storage)?;

        execute(&instructions, &diagnostics)?;

        Ok(output_slot.take())
    })()
    .unwrap_or_else(|_: RecordedError| TokenStream::new());

    RepetitiveResult {
        output,
        diagnostics,
    }
}
