use crate::{
    ast::UnparsedQuote,
    codegen::{CodegenContext, eval_quote},
    context::Context,
    data_structures::{reservation_stack::ReservationStack, stable_storage::StableStorage},
    diagnostics::Diagnostics,
    instruction_executor::execute_instructions,
    proc_macro12::TokenStream,
};

/// The entry point of the macro.
///
/// This returns diagnostics separately from the output tokenstream, whereas the
/// public `repetitive` function embeds diagnostics inside the tokenstream. This
/// approach is currently required for unit tests.
pub fn repetitive(input: TokenStream) -> (TokenStream, Diagnostics) {
    let ctx = Context {
        bool_stack: ReservationStack::new(),
        diagnostics: Diagnostics::new(),
        int_stack: ReservationStack::new(),
        str_constants: StableStorage::new(),
        str_stack: ReservationStack::new(),
        tokenstream_stack: ReservationStack::new(),
    };
    let mut codegen_context = CodegenContext {
        ctx: &ctx,
        instructions: Vec::new(),
    };

    let input = UnparsedQuote { stream: input };
    let output_slot = eval_quote(input, &mut codegen_context);

    let output = match execute_instructions(&codegen_context.instructions) {
        Ok(()) => TokenStream::from_iter(output_slot.take()),
        Err(error) => {
            ctx.diagnostics.push_error(error);
            TokenStream::new()
        }
    };

    (output, ctx.diagnostics)
}
