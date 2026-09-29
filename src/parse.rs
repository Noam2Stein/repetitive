use crate::{
    ast::{
        Expr, QuoteSegment, UnparsedExpr, UnparsedExprArray, UnparsedExprTuple, UnparsedPat,
        UnparsedQuote,
    },
    diagnostics::{Diagnostics, RecordedError},
    ident_interner::IdentInterner,
};

impl UnparsedQuote {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        ident_interner: &IdentInterner,
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
        ident_interner: &IdentInterner,
    ) -> Result<Expr, RecordedError> {
        todo!()
    }
}

impl UnparsedExprArray {
    pub fn parse(
        self,
        diagnostics: &Diagnostics,
        ident_interner: &IdentInterner,
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
        ident_interner: &IdentInterner,
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
        ident_interner: &IdentInterner,
    ) -> Result<Expr, RecordedError> {
        todo!()
    }
}
