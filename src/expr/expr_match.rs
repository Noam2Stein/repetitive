use syn::Token;

use super::*;

#[derive(Debug, Clone)]
pub struct ExprMatch {
    #[expect(dead_code)]
    pub match_token: Token![match],
    pub expr: Box<Expr>,
    pub match_arms: Vec<ExprMatchArm>,
}

#[derive(Debug, Clone)]
pub struct ExprMatchArm {
    pub pat: Pattern,
    pub condition: Option<Expr>,
    pub body: Expr,
    pub unused_arm_warning: WarningHandle,
}
