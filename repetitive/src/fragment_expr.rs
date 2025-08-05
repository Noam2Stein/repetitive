use std::mem::replace;

use proc_macro2::{Delimiter, Group, Span, TokenStream};
use syn::{Ident, Token, parse::ParseStream};

use super::*;

#[derive(Debug, Clone)]
pub struct FragmentExpr {
    pub span: Span,
    pub kind: FragmentExprKind,
}
#[derive(Debug, Clone)]
pub enum FragmentExprKind {
    Value(FragmentValue),
    Name(Name),
    Op(FragmentOp),
    List(Vec<FragmentExpr>),
}

#[derive(Debug, Clone)]
pub struct FragmentOp {
    pub op: Op,
    pub args: Vec<FragmentExpr>,
}

impl FragmentExpr {
    pub fn temp() -> Self {
        Self {
            span: Span::call_site(),
            kind: FragmentExprKind::Value(FragmentValue::Int(0)),
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

        if input.peek(Token![.]) {
            input.parse::<Token![.]>()?;
            let ident = input.parse::<Ident>()?;

            let group = input.parse::<Group>()?;
            if group.delimiter() != Delimiter::Parenthesis {
                return Err(syn::Error::new(
                    group.span(),
                    "expected a parenthesized list",
                ));
            }

            let op = Op::parse_method(ident)?;
            let args = ctx_parse_punctuated.ctx_parse2(group.stream(), ctx)?;

            expr = Self::op(op, [expr].into_iter().chain(args).collect(), ctx)?;
        }

        Ok(expr)
    }

    pub fn op(op: Op, args: Vec<FragmentExpr>, ctx: &mut Context) -> syn::Result<Self> {
        if let Some(args) = args
            .iter()
            .map(|item| match &item.kind {
                FragmentExprKind::Value(val) => Some(val),
                _ => None,
            })
            .collect::<Option<Vec<_>>>()
        {
            let args = args.into_iter().cloned().collect::<Vec<_>>();

            return Ok(Self {
                span: op.span(),
                kind: FragmentExprKind::Value(op.compute(&args, ctx)?),
            });
        }

        Ok(Self {
            span: op.span(),
            kind: FragmentExprKind::Op(FragmentOp { op, args }),
        })
    }

    fn ctx_parse_base(input: ParseStream, ctx: &mut Context) -> syn::Result<Self> {
        if input.peek(Token![@]) {
            let outer = FragmentOuter::ctx_parse(input, ctx)?;

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

        if input.peek(Ident) {
            let name = Name::ctx_parse(input, ctx)?;

            return Ok(Self::name(name));
        }

        if let Some(lit) = FragmentValueExpr::option_lit(input) {
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
                        kind: FragmentExprKind::Value(FragmentValue::Tokens(tokens)),
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

        Err(syn::Error::new(input.span(), "expected a fragment"))
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
        }

        Ok(())
    }
}

impl FragmentExpr {
    pub fn eval(&self, ctx: &mut Context, namespace: &Namespace) -> syn::Result<FragmentValueExpr> {
        let value = match &self.kind {
            FragmentExprKind::Value(val) => val.clone(),

            FragmentExprKind::Name(name) => namespace.try_get(*name)?,

            FragmentExprKind::List(val) => FragmentValue::List(
                val.into_iter()
                    .map(|item| item.eval(ctx, namespace).map(|val_expr| val_expr.value))
                    .collect::<syn::Result<_>>()?,
            ),

            FragmentExprKind::Op(FragmentOp { op, args }) => {
                let resolved_args = args
                    .into_iter()
                    .map(|item| item.eval(ctx, namespace).map(|val_expr| val_expr.value))
                    .collect::<syn::Result<Vec<_>>>()?;

                op.compute(&resolved_args, ctx)?
            }
        };

        Ok(FragmentValueExpr {
            span: self.span,
            value,
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
