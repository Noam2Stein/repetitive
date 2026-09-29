use std::cell::Cell;

use crate::{
    ast::{Expr, QuoteSegment, UnparsedQuote},
    bindings::Bindings,
    entrypoint::Context,
    instruction::Instruction,
    proc_macro12::TokenStream,
    val::Val,
};

pub struct CodegenResult<'ctx> {
    pub instructions: Vec<Instruction<'ctx>>,
    pub output_slot: &'ctx Cell<TokenStream>,
}

pub fn codegen(input: TokenStream, ctx: &Context) -> Result<CodegenResult<'_>, ()> {
    let mut ctx = CodegenContext {
        ctx,
        bindings: Bindings::new(),
        instructions: Vec::new(),
    };

    let output = eval_quote(UnparsedQuote { stream: input }.parse(ctx.ctx), &mut ctx)?;
    let Val::TokenStream(output_slot) = output else {
        unreachable!("the result of `eval_quote` should be a token stream");
    };

    Ok(CodegenResult {
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
