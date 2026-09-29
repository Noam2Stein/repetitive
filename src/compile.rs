use std::cell::Cell;

use crate::{
    ast::{Expr, QuoteSegment, UnparsedQuote},
    bindings::Bindings,
    diagnostics::{Diagnostics, RecordedError},
    entrypoint::Context,
    instruction::{Instruction, InstructionStorage},
    proc_macro12::TokenStream,
    val::Val,
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
    let mut ctx = CodegenContext {
        ctx,
        bindings: Bindings::new(),
        instructions: Vec::new(),
    };

    let output = eval_quote(UnparsedQuote { stream: input }.parse(ctx.ctx), &mut ctx)?;
    let Val::TokenStream(output_slot) = output else {
        unreachable!("the result of `eval_quote` should be a token stream");
    };

    Ok(CompileResult {
        instructions: ctx.instructions,
        output_slot,
    })
}

struct CodegenContext<'ctx> {
    ctx: &'ctx Context,
    bindings: Bindings<'ctx>,
    instructions: Vec<Instruction<'ctx>>,
}

fn eval_quote<'ctx>(
    quote: impl Iterator<Item = Result<QuoteSegment, ()>>,
    ctx: &mut CodegenContext<'ctx>,
) -> Result<Val<'ctx>, ()> {
    todo!()
}

fn eval_expr<'ctx>(expr: Expr, ctx: &mut CodegenContext<'ctx>) -> Result<Val<'ctx>, ()> {
    todo!()
}
