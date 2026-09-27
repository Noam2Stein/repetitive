use crate::{
    ast::{Expr, QuoteSegment, UnparsedExpr, UnparsedPat, UnparsedQuote},
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

impl UnparsedPat {
    pub fn parse(self, ctx: &Context) -> Result<Expr, ()> {
        todo!()
    }
}
