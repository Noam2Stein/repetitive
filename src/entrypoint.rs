use std::cell::Cell;

use crate::{
    codegen::{CodegenResult, codegen},
    constants::Constants,
    diagnostics::Diagnostics,
    execute::execute,
    ident_interner::IdentInterner,
    proc_macro12::TokenStream,
    stack::Stack,
};

pub struct RepetitiveResult {
    pub stream: TokenStream,
    pub diagnostics: Diagnostics,
}

pub struct Context {
    pub bool_stack: Stack<Cell<bool>>,
    pub diagnostics: Diagnostics,
    pub ident_interner: IdentInterner,
    pub int_stack: Stack<Cell<i32>>,
    pub str_constants: Constants<Cell<String>>,
    pub str_stack: Stack<Cell<String>>,
    pub token_stream_constants: Constants<Cell<TokenStream>>,
    pub token_stream_stack: Stack<Cell<TokenStream>>,
}

/// The entry point of the macro.
///
/// This returns diagnostics separately from the output token stream, whereas
/// the public `repetitive` function embeds diagnostics inside the token stream.
/// This approach is required for unit tests.
pub fn repetitive(input: TokenStream) -> RepetitiveResult {
    let ctx = Context {
        bool_stack: Stack::new(),
        diagnostics: Diagnostics::new(),
        ident_interner: IdentInterner::new(),
        int_stack: Stack::new(),
        str_constants: Constants::new(),
        str_stack: Stack::new(),
        token_stream_constants: Constants::new(),
        token_stream_stack: Stack::new(),
    };

    let Ok(CodegenResult {
        instructions,
        output_slot,
    }) = codegen(input, &ctx)
    else {
        return RepetitiveResult {
            stream: TokenStream::new(),
            diagnostics: ctx.diagnostics,
        };
    };

    let output_stream = match execute(&instructions) {
        Ok(()) => output_slot.take(),
        Err(error) => {
            ctx.diagnostics.push_error(error);
            TokenStream::new()
        }
    };

    RepetitiveResult {
        stream: output_stream,
        diagnostics: ctx.diagnostics,
    }
}
