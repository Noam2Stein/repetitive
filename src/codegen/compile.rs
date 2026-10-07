use std::cell::Cell;

use crate::{
    ast::{Expr, Meta, Pat, Quote, QuoteSegment},
    diagnostics::{Diagnostics, RecordedError},
    entrypoint::Context,
    instruction::Instruction,
    parse::parse_ast,
    proc_macro12::TokenStream,
    str_interner::StrInterner,
};

pub struct CompileResult<'storage> {
    pub instructions: Vec<Instruction<'storage>>,
    pub output_slot: &'storage Cell<TokenStream>,
}

pub fn compile<'ctx>(
    input: TokenStream,
    ctx: &'ctx Context<'ctx>,
) -> Result<CompileResult<'ctx>, RecordedError> {
    compile_quote(parse_ast(input), diagnostics, &str_interner)?;
    todo!("actually implement codegen")
}

fn compile_quote(
    quote: Quote,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<(), RecordedError> {
    for segment in quote.segments(diagnostics, str_interner) {
        match segment? {
            QuoteSegment::Group(segment) => {
                compile_quote(segment.stream, diagnostics, str_interner)?;
            }
            QuoteSegment::Meta(segment) => {
                compile_meta(segment, diagnostics, str_interner)?;
            }
            QuoteSegment::TokenStream(_) => {}
        }
    }

    Ok(())
}

fn compile_meta(
    meta: Meta,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<(), RecordedError> {
    match meta {
        Meta::For(meta) => {
            compile_pat(meta.pat, diagnostics, str_interner)?;
            compile_expr(meta.expr, diagnostics, str_interner)?;
            compile_quote(meta.body, diagnostics, str_interner)?;
        }
        Meta::Ident(_) => {}
        Meta::If(meta) => {
            for segment in meta.segments {
                if let Some(condition) = segment.condition {
                    compile_expr(condition, diagnostics, str_interner)?;
                }
                compile_quote(segment.body, diagnostics, str_interner)?;
            }
        }
        Meta::Let(meta) => {
            compile_pat(meta.pat, diagnostics, str_interner)?;
            compile_expr(meta.expr, diagnostics, str_interner)?;
        }
        Meta::Match(meta) => {
            compile_expr(meta.expr, diagnostics, str_interner)?;
            for arm in meta.body.arms(diagnostics, str_interner) {
                let arm = arm?;
                compile_pat(arm.pat, diagnostics, str_interner)?;
                compile_quote(arm.body, diagnostics, str_interner)?;
            }
        }
    }

    Ok(())
}

fn compile_expr(
    expr: Expr,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<(), RecordedError> {
    expr.kind(diagnostics, str_interner)?;
    Ok(())
}

fn compile_pat(
    pat: Pat,
    diagnostics: &Diagnostics,
    str_interner: &StrInterner,
) -> Result<(), RecordedError> {
    pat.kind(diagnostics, str_interner)?;
    Ok(())
}
