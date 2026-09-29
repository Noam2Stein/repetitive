use crate::{
    codegen::{CodegenResult, codegen},
    context::Context,
    diagnostics::Diagnostics,
    execute::execute,
    proc_macro12::TokenStream,
};

pub struct RepetitiveResult {
    pub stream: TokenStream,
    pub diagnostics: Diagnostics,
}

/// The entry point of the macro.
///
/// This returns diagnostics separately from the output token stream, whereas
/// the public `repetitive` function embeds diagnostics inside the token stream.
/// This approach is required for unit tests.
pub fn repetitive(input: TokenStream) -> RepetitiveResult {
    let ctx = Context::new();

    let CodegenResult {
        instructions,
        output_slot,
    } = codegen(input, &ctx);

    let execute_result = execute(&instructions);

    let output = match execute_result {
        Ok(()) => TokenStream::from_iter(output_slot.take()),
        Err(error) => {
            ctx.diagnostics.push_error(error);
            TokenStream::new()
        }
    };

    RepetitiveResult {
        stream: output,
        diagnostics: ctx.diagnostics,
    }
}
