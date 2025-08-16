use std::mem::replace;

use proc_macro2::{Delimiter, Group, Span, TokenStream};
use syn::{
    Error, Ident, Token,
    parse::ParseStream,
    token::{Bracket, Paren},
};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentExpr {
    pub span: Span,
    pub kind: FragmentExprKind,
}
#[derive(Debug, Clone)]
pub enum FragmentExprKind {
    Value(FragmentValueKind),
    Name(Name),
    Op(FragmentOp),
    List(Vec<FragmentExpr>),
    Match(FragmentMatch),
}

#[derive(Debug, Clone)]
pub struct FragmentOp {
    pub op: Op,
    pub args: Vec<FragmentExpr>,
}

#[derive(Debug, Clone)]
pub struct FragmentMatch {
    #[allow(dead_code)]
    pub match_span: Span,
    pub expr: Box<FragmentExpr>,
    pub match_arms: Vec<FragmentMatchArm>,
}

#[derive(Debug, Clone)]
pub struct FragmentMatchArm {
    pub pat: Pattern,
    pub condition: Option<FragmentExpr>,
    pub body: FragmentExpr,
    pub unused_arm_warning: WarningHandle,
}

impl FragmentExpr {
    pub fn peek(input: ParseStream) -> bool {
        FragmentValue::peek_lit(input)
            || input.peek(Paren)
            || input.peek(Bracket)
            || input.peek(Token![@])
            || input.peek(Token![if])
            || input.peek(Token![match])
    }

    pub fn temp() -> Self {
        Self {
            span: Span::call_site(),
            kind: FragmentExprKind::Value(FragmentValueKind::Int(0)),
        }
    }

    pub fn name(name: Name) -> Self {
        Self {
            span: name.span,
            kind: FragmentExprKind::Name(name),
        }
    }
}

impl ContextParse for FragmentExpr {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        #[derive(Debug, Clone, PartialEq, PartialOrd)]
        enum BinOpLvl {
            Or,
            And,
            Xor,
            BitwiseOr,
            BitwiseXor,
            BitwiseAnd,
            EqNe,
            Cmp,
            Range,
            AddSub,
            MulDivRem,
        }
        impl BinOpLvl {
            fn try_from_op(op: Op) -> Option<Self> {
                match op {
                    Op::Add(_) => Some(BinOpLvl::AddSub),
                    Op::Sub(_) => Some(BinOpLvl::AddSub),
                    Op::Mul(_) => Some(BinOpLvl::MulDivRem),
                    Op::Div(_) => Some(BinOpLvl::MulDivRem),
                    Op::Rem(_) => Some(BinOpLvl::MulDivRem),
                    Op::BitOr(_) => Some(BinOpLvl::BitwiseOr),
                    Op::BitXor(_) => Some(BinOpLvl::BitwiseXor),
                    Op::BitAnd(_) => Some(BinOpLvl::BitwiseAnd),
                    Op::Shl(_) => Some(BinOpLvl::BitwiseOr),
                    Op::Shr(_) => Some(BinOpLvl::BitwiseOr),

                    Op::Eq(_) => Some(BinOpLvl::EqNe),
                    Op::Ne(_) => Some(BinOpLvl::EqNe),
                    Op::Lt(_) => Some(BinOpLvl::Cmp),
                    Op::Gt(_) => Some(BinOpLvl::Cmp),
                    Op::Le(_) => Some(BinOpLvl::Cmp),
                    Op::Ge(_) => Some(BinOpLvl::Cmp),

                    Op::Or(_) => Some(BinOpLvl::Or),
                    Op::And(_) => Some(BinOpLvl::And),
                    Op::Xor(_) => Some(BinOpLvl::Xor),

                    Op::Range(_) => Some(BinOpLvl::Range),
                    Op::RangeInclusive(_) => Some(BinOpLvl::Range),

                    _ => None,
                }
            }
        }

        enum TempExpr {
            Bin {
                op: Op,
                op_lvl: BinOpLvl,
                left: Box<TempExpr>,
                right: Box<TempExpr>,
            },
            Single(FragmentExpr),
        }
        impl TempExpr {
            fn to_expr(self) -> FragmentExpr {
                match self {
                    TempExpr::Bin {
                        op,
                        op_lvl: _,
                        left,
                        right,
                    } => FragmentExpr {
                        span: op.span(),
                        kind: FragmentExprKind::Op(FragmentOp {
                            op,
                            args: vec![left.to_expr(), right.to_expr()],
                        }),
                    },

                    TempExpr::Single(expr) => expr,
                }
            }
        }

        let mut expr = TempExpr::Single(FragmentExpr::ctx_parse_single(input, ctx)?);

        while let Some(op) = Op::parse_option_bin(input) {
            let op_lvl = BinOpLvl::try_from_op(op).unwrap();

            let right = FragmentExpr::ctx_parse_single(input, ctx)?;

            match &mut expr {
                TempExpr::Bin {
                    op: _,
                    op_lvl: expr_lvl,
                    left: _,
                    right: expr_right,
                } => {
                    if op_lvl > *expr_lvl {
                        **expr_right = TempExpr::Bin {
                            op,
                            op_lvl,
                            left: Box::new(replace(
                                expr_right,
                                TempExpr::Single(FragmentExpr::temp()),
                            )),
                            right: Box::new(TempExpr::Single(right)),
                        };
                    } else {
                        expr = TempExpr::Bin {
                            op,
                            op_lvl,
                            left: Box::new(expr),
                            right: Box::new(TempExpr::Single(right)),
                        };
                    }
                }

                TempExpr::Single(left) => {
                    expr = TempExpr::Bin {
                        op,
                        op_lvl,
                        left: Box::new(TempExpr::Single(replace(left, FragmentExpr::temp()))),
                        right: Box::new(TempExpr::Single(right)),
                    };
                }
            }
        }

        let mut expr = expr.to_expr();
        expr.optimize(ctx)?;

        Ok(expr)
    }
}
impl FragmentExpr {
    pub fn ctx_parse_single(input: ParseStream, ctx: &mut Context) -> syn::Result<Self> {
        if let Some(op) = Op::parse_option_un(input) {
            let arg = FragmentExpr::ctx_parse_single(input, ctx)?;

            return Self::op(op, vec![arg], ctx);
        }

        let mut expr = FragmentExpr::ctx_parse_base(input, ctx)?;

        loop {
            if input.peek(Token![.]) && !input.peek(Token![..]) && !input.peek(Token![...]) {
                let method = Method::ctx_parse(input, ctx)?;
                let op = Op::from_method_ident(method);

                let group = input.parse::<Group>()?;
                if group.delimiter() != Delimiter::Parenthesis {
                    return Err(syn::Error::new(
                        group.span(),
                        "expected a parenthesized list",
                    ));
                }

                let args = ctx_parse_punctuated.ctx_parse2(group.stream(), ctx)?;

                expr = Self::op(op, [expr].into_iter().chain(args).collect(), ctx)?;
            } else if input.peek(Bracket) {
                let group = input.parse::<Group>()?;
                let idx = FragmentExpr::ctx_parse.ctx_parse2(group.stream(), ctx)?;

                expr = Self::op(Op::Index(group.span()), vec![expr, idx], ctx)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    pub fn op(op: Op, args: Vec<FragmentExpr>, ctx: &mut Context) -> syn::Result<Self> {
        if let Some(args) = args
            .iter()
            .map(|item| match &item.kind {
                FragmentExprKind::Value(val) => Some(FragmentValue {
                    span: item.span,
                    kind: val.clone(),
                }),
                _ => None,
            })
            .collect::<Option<Vec<_>>>()
        {
            let args = args.into_iter().collect::<Vec<_>>();

            return Ok(FragmentExpr {
                span: op.span(),
                kind: FragmentExprKind::Value(op.compute(&args, ctx)?.kind),
            });
        }

        Ok(Self {
            span: op.span(),
            kind: FragmentExprKind::Op(FragmentOp { op, args }),
        })
    }

    fn ctx_parse_base(input: ParseStream, ctx: &mut Context) -> syn::Result<Self> {
        if input.peek(Token![@]) {
            let at_span = input.parse::<Token![@]>()?.span;

            if input.peek(Paren) || Name::peek(input) {
                ctx.push_warning(syn::Error::new(at_span, "unnecessary `@`"));
            }

            let outer_kind = FragmentOuterKind::ctx_parse(input, ctx)?;
            let outer = FragmentOuter {
                at_span,
                kind: outer_kind,
            };

            return Ok(match outer.kind {
                FragmentOuterKind::Expr(expr) => expr,

                FragmentOuterKind::For(outer_for) => {
                    return Err(syn::Error::new(
                        outer_for.for_span,
                        "`@for` is not allowed here",
                    ));
                }
                FragmentOuterKind::Let(outer_let) => {
                    return Err(syn::Error::new(
                        outer_let.let_span,
                        "`@let` is not allowed here",
                    ));
                }
            });
        }

        if input.peek(Token![if]) {
            let if_span = input.parse::<Token![if]>()?.span;
            let cond = FragmentExpr::ctx_parse(input, ctx)?;

            let then_group = input.parse::<Group>()?;
            if then_group.delimiter() != Delimiter::Brace {
                return Err(syn::Error::new(
                    then_group.span(),
                    "expected a brace-delimited block",
                ));
            }

            let then = FragmentExpr::ctx_parse.ctx_parse2(then_group.stream(), ctx)?;

            input.parse::<Token![else]>()?;

            let else_group = input.parse::<Group>()?;
            if else_group.delimiter() != Delimiter::Brace {
                return Err(syn::Error::new(
                    else_group.span(),
                    "expected a brace-delimited block",
                ));
            }

            let else_ = FragmentExpr::ctx_parse.ctx_parse2(else_group.stream(), ctx)?;

            return Ok(Self::op(Op::IfElse(if_span), vec![cond, then, else_], ctx)?);
        }

        if input.peek(Token![match]) {
            let match_span = input.parse::<Token![match]>()?.span;
            let expr = Box::new(FragmentExpr::ctx_parse(input, ctx)?);

            let group = input.parse::<Group>()?;
            if group.delimiter() != Delimiter::Brace {
                return Err(syn::Error::new(
                    group.span(),
                    "expected a brace-delimited block",
                ));
            }

            let match_arms_parse_fn = |input: ParseStream, ctx: &mut Context| {
                let mut arms = Vec::new();

                while !input.is_empty() {
                    let pat_span = input.span();
                    let pat = Pattern::ctx_parse(input, ctx)?;

                    let condition = if input.peek(Token![if]) {
                        input.parse::<Token![if]>()?;

                        Some(FragmentExpr::ctx_parse(input, ctx)?)
                    } else {
                        None
                    };

                    input.parse::<Token![=>]>()?;

                    let body = FragmentExpr::ctx_parse(input, ctx)?;

                    arms.push(FragmentMatchArm {
                        pat,
                        condition,
                        body,
                        unused_arm_warning: ctx
                            .push_warning(Error::new(pat_span, "unused match arm")),
                    });

                    if !input.is_empty() {
                        input.parse::<Token![,]>()?;
                    }
                }

                Ok::<_, syn::Error>(arms)
            };

            let match_arms = match_arms_parse_fn.ctx_parse2(group.stream(), ctx)?;

            return Ok(FragmentExpr {
                span: match_span,
                kind: FragmentExprKind::Match(FragmentMatch {
                    match_span,
                    expr,
                    match_arms,
                }),
            });
        }

        if input.peek(Ident) {
            let name = Name::ctx_parse(input, ctx)?;

            return Ok(Self::name(name));
        }

        if let Some(lit) = FragmentValue::option_lit(input)? {
            return Ok(lit.into_expr());
        }

        if let Some(group) = input.parse::<Option<Group>>()? {
            match group.delimiter() {
                Delimiter::Parenthesis => {
                    let fragment_expr = FragmentExpr::ctx_parse.ctx_parse2(group.stream(), ctx)?;

                    return Ok(fragment_expr);
                }

                Delimiter::Brace => {
                    let tokens = Tokens::ctx_parse.ctx_parse2(group.stream(), ctx)?;

                    return Ok(FragmentExpr {
                        span: group.span(),
                        kind: FragmentExprKind::Value(FragmentValueKind::Tokens(tokens)),
                    });
                }

                Delimiter::Bracket => {
                    return Ok(FragmentExpr {
                        span: group.span(),
                        kind: FragmentExprKind::List(
                            ctx_parse_punctuated.ctx_parse2(group.stream(), ctx)?,
                        ),
                    });
                }

                Delimiter::None => {
                    return FragmentExpr::ctx_parse.ctx_parse2(group.stream(), ctx);
                }
            }
        }

        Err(syn::Error::new(input.span(), "expected a fragment expr"))
    }

    fn optimize(&mut self, ctx: &mut Context) -> syn::Result<()> {
        match &mut self.kind {
            FragmentExprKind::Value(_) => {}
            FragmentExprKind::Name(_) => {}

            FragmentExprKind::List(val) => {
                for item in val {
                    item.optimize(ctx)?;
                }
            }

            FragmentExprKind::Op(FragmentOp { op, args }) => {
                for item in args.iter_mut() {
                    item.optimize(ctx)?;
                }

                *self = Self::op(*op, args.clone(), ctx)?;
            }

            FragmentExprKind::Match(FragmentMatch {
                match_span: _,
                expr,
                match_arms,
            }) => {
                expr.optimize(ctx)?;

                for arm in match_arms.iter_mut() {
                    arm.body.optimize(ctx)?;

                    if let Some(condition) = &mut arm.condition {
                        condition.optimize(ctx)?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl FragmentExpr {
    pub fn eval(&self, ctx: &mut Context, namespace: &Namespace) -> syn::Result<FragmentValue> {
        let value = match &self.kind {
            FragmentExprKind::Value(val) => val.clone(),

            FragmentExprKind::Name(name) => namespace.try_get(*name)?.kind,

            FragmentExprKind::List(val) => FragmentValueKind::List(
                val.into_iter()
                    .map(|item| item.eval(ctx, namespace))
                    .collect::<syn::Result<_>>()?,
            ),

            FragmentExprKind::Op(FragmentOp { op, args }) => {
                let resolved_args = args
                    .into_iter()
                    .map(|item| item.eval(ctx, namespace))
                    .collect::<syn::Result<Vec<_>>>()?;

                op.compute(&resolved_args, ctx)?.kind
            }

            FragmentExprKind::Match(FragmentMatch {
                match_span: _,
                expr,
                match_arms,
            }) => {
                let expr_value = expr.eval(ctx, namespace)?;

                let mut matched_arm = None;
                for arm in match_arms {
                    let matches = arm.pat.matches(&expr_value, ctx)?.is_ok();

                    let passed_condition = arm.condition.as_ref().map_or(Ok(true), |cond| {
                        match cond.eval(ctx, namespace)?.kind {
                            FragmentValueKind::Bool(val) => Ok(val),
                            _ => Err(syn::Error::new(cond.span, "expected a boolean")),
                        }
                    })?;

                    if matches && passed_condition {
                        matched_arm = Some(arm);
                        break;
                    }
                }

                if let Some(matched_arm) = matched_arm {
                    matched_arm.unused_arm_warning.remove();

                    let mut arm_namespace = namespace.fork();
                    arm_namespace.flush();
                    matched_arm
                        .pat
                        .queue_insert(expr_value.clone(), &mut arm_namespace, ctx)?;

                    matched_arm.body.eval(ctx, &mut arm_namespace)?.kind
                } else {
                    return Err(syn::Error::new(
                        expr_value.span,
                        "none of the match arms matched",
                    ));
                }
            }
        };

        Ok(FragmentValue {
            span: self.span,
            kind: value,
        })
    }
}

impl Paste for FragmentExpr {
    fn paste(
        &self,
        output: &mut TokenStream,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> syn::Result<()> {
        self.eval(ctx, namespace)?.paste(output, ctx, namespace)
    }
}

fn ctx_parse_punctuated<T: ContextParse>(
    input: ParseStream,
    ctx: &mut Context,
) -> syn::Result<Vec<T>> {
    let mut items = Vec::new();

    while !input.is_empty() {
        let item = T::ctx_parse(input, ctx)?;
        items.push(item);

        if input.is_empty() {
            break;
        }

        input.parse::<Token![,]>()?;
    }

    Ok(items)
}
