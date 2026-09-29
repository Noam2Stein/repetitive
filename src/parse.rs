use crate::{
    ast::{
        Expr, QuoteSegment, UnparsedExpr, UnparsedExprArray, UnparsedExprTuple, UnparsedPat,
        UnparsedQuote,
    },
    diagnostics::{Diagnostics, RecordedError},
    str_interner::StrInterner,
};

impl UnparsedQuote {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        interner: &StrInterner,
    ) -> impl Iterator<Item = Result<QuoteSegment, RecordedError>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
    }
}

impl UnparsedExpr {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        interner: &StrInterner,
    ) -> Result<Expr, RecordedError> {
        todo!()
    }
}

impl UnparsedExprArray {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        interner: &StrInterner,
    ) -> impl Iterator<Item = Result<Expr, RecordedError>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
    }
}

impl UnparsedExprTuple {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        interner: &StrInterner,
    ) -> impl Iterator<Item = Result<Expr, RecordedError>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
    }
}

impl UnparsedPat {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        interner: &StrInterner,
    ) -> Result<Expr, RecordedError> {
        todo!()
    }
}
