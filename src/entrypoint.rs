use crate::{
    compile::{CompileResult, compile},
    context::Context,
    diagnostics::RecordedError,
    execute::execute,
    proc_macro12::TokenStream,
};

/// The entry point of the macro.
///
/// This returns diagnostics separately from the output token stream, whereas
/// the public `repetitive` function embeds diagnostics inside the token stream.
/// This approach is required for unit tests.
pub fn repetitive(input: TokenStream, ctx: &Context) -> Result<TokenStream, RecordedError> {
    let CompileResult {
        instructions,
        output_slot,
    } = compile(input, &ctx)?;

    execute(&instructions, &ctx)?;

    Ok(output_slot.take())
}
