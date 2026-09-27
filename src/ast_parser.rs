use crate::{
    ast::{
        Expr, QuoteSegment, UnparsedExpr, UnparsedExprArray, UnparsedExprTuple, UnparsedPat,
        UnparsedQuote,
    },
    context::Context,
};

impl UnparsedQuote {
    pub fn parse(self, ctx: &Context) -> impl Iterator<Item = Result<QuoteSegment, ()>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
    }
}

impl UnparsedExpr {
    pub fn parse(self, ctx: &Context) -> Result<Expr, ()> {
        todo!()
    }
}

impl UnparsedExprArray {
    pub fn parse(self, ctx: &Context) -> impl Iterator<Item = Result<Expr, ()>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
    }
}

impl UnparsedExprTuple {
    pub fn parse(self, ctx: &Context) -> impl Iterator<Item = Result<Expr, ()>> {
        todo!();
        #[expect(unreachable_code)]
        [].into_iter()
    }
}

impl UnparsedPat {
    pub fn parse(self, ctx: &Context) -> Result<Expr, ()> {
        todo!()
    }
}
