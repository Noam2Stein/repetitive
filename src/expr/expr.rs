use std::{fs::File, mem::replace, path::PathBuf, str::FromStr};

use proc_macro2::{Delimiter, Group, Span, TokenStream};
use syn::{
    Ident, LitStr, Token,
    parse::ParseStream,
    token::{Bracket, Paren},
};

use super::*;

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Value(Value),
    Name(Name),
    Op(FragmentOp),
    List(Vec<Expr>),
    Match(ExprMatch),
}

#[derive(Debug, Clone)]
pub struct FragmentOp {
    pub op: Op,
    pub args: Vec<Expr>,
}

impl Expr {
    pub fn peek(input: ParseStream) -> bool {
        Value::peek_lit(input)
            || input.peek(Paren)
            || input.peek(Bracket)
            || input.peek(Token![@])
            || input.peek(Token![if])
            || input.peek(Token![match])
    }

    pub fn temp() -> Self {
        Self {
            span: Span::call_site(),
            kind: ExprKind::Value(Value {
                span: Span::call_site(),
                kind: ValueKind::Int(0),
            }),
        }
    }

    pub fn name(name: Name) -> Self {
        Self {
            span: name.span,
            kind: ExprKind::Name(name),
        }
    }
}

impl ContextParse for Expr {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
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
            Single(Expr),
        }
        impl TempExpr {
            fn to_expr(self) -> Expr {
                match self {
                    TempExpr::Bin {
                        op,
                        op_lvl: _,
                        left,
                        right,
                    } => Expr {
                        span: op.span(),
                        kind: ExprKind::Op(FragmentOp {
                            op,
                            args: vec![left.to_expr(), right.to_expr()],
                        }),
                    },

                    TempExpr::Single(expr) => expr,
                }
            }
        }

        let mut expr = TempExpr::Single(Expr::ctx_parse_single(input, ctx)?);

        while let Some(op) = Op::parse_option_bin(input) {
            let op_lvl = BinOpLvl::try_from_op(op).unwrap();

            let right = Expr::ctx_parse_single(input, ctx)?;

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
                            left: Box::new(replace(expr_right, TempExpr::Single(Expr::temp()))),
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
                        left: Box::new(TempExpr::Single(replace(left, Expr::temp()))),
                        right: Box::new(TempExpr::Single(right)),
                    };
                }
            }
        }

        let mut expr = expr.to_expr();
        expr.optimize(ctx);

        Ok(expr)
    }
}
impl Expr {
    pub fn ctx_parse_single(input: ParseStream, ctx: &mut Context) -> Result<Self, Error> {
        if let Some(op) = Op::parse_option_un(input) {
            let arg = Expr::ctx_parse_single(input, ctx)?;

            return Ok(Self {
                span: op.span(),
                kind: ExprKind::Op(FragmentOp {
                    op,
                    args: vec![arg],
                }),
            });
        }

        let mut expr = Expr::ctx_parse_base(input, ctx)?;

        loop {
            if input.peek(Token![.]) && !input.peek(Token![..]) && !input.peek(Token![...]) {
                let method = Method::ctx_parse(input, ctx)?;
                let op = Op::from_method_ident(method);

                let group = Group::ctx_parse(input, ctx)?;
                if group.delimiter() != Delimiter::Parenthesis {
                    return Err(Error::ParseError(syn::Error::new(
                        group.span(),
                        "expected a parenthesized list",
                    )));
                }

                let args = ctx_parse_punctuated.ctx_parse2(group.stream(), ctx)?;

                expr = Self {
                    span: op.span(),
                    kind: ExprKind::Op(FragmentOp {
                        op,
                        args: [expr].into_iter().chain(args).collect(),
                    }),
                };
            } else if input.peek(Bracket) {
                let group = Group::ctx_parse(input, ctx)?;
                let idx = Expr::ctx_parse.ctx_parse2(group.stream(), ctx)?;

                expr = Self {
                    span: group.span(),
                    kind: ExprKind::Op(FragmentOp {
                        op: Op::Index(group.span()),
                        args: vec![expr, idx],
                    }),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn ctx_parse_base(input: ParseStream, ctx: &mut Context) -> Result<Self, Error> {
        if let Some(frag) = Fragment::ctx_parse_option(input, ctx)? {
            return Ok(match frag.kind {
                FragmentKind::If(expr) => expr.expr,
                FragmentKind::Match(expr) => expr.expr,
                FragmentKind::Concat(expr) => expr.expr,
                FragmentKind::Tokens(expr) => Value {
                    span: frag.at_token.span,
                    kind: ValueKind::Tokens(expr.tokens),
                }
                .into_expr(),

                FragmentKind::Expr(expr) => {
                    ctx.push_warning(Warning::UnnecessaryPunct {
                        span: frag.at_token.span,
                        punct: "@",
                    });

                    expr.expr
                }

                FragmentKind::For(outer_for) => {
                    return Err(Error::ParseError(syn::Error::new(
                        outer_for.for_token.span,
                        "`@for` is not allowed in expressions",
                    )));
                }
                FragmentKind::Let(outer_let) => {
                    return Err(Error::ParseError(syn::Error::new(
                        outer_let.let_token.span,
                        "`@let` is not allowed in expressions",
                    )));
                }
                FragmentKind::At(at) => {
                    return Err(Error::ParseError(syn::Error::new(
                        at.span,
                        "`@` is not allowed in expressions",
                    )));
                }
            });
        }

        if let Some(keyword) = Keyword::ctx_parse_option(input, ctx)? {
            match keyword {
                Keyword::Str(span) => {
                    return Err(Error::ParseError(syn::Error::new(
                        span,
                        "`str` can only be used in `@str[...]`",
                    )));
                }
                Keyword::Include(span) => {
                    let group = Group::ctx_parse(input, ctx)?;
                    if group.delimiter() != Delimiter::Parenthesis {
                        return Err(Error::ParseError(syn::Error::new(
                            group.span(),
                            "expected a parenthesized list",
                        )));
                    }

                    let path_lit = LitStr::ctx_parse.ctx_parse2(group.stream(), ctx)?;
                    let path = PathBuf::from(path_lit.value());

                    let root =
                        std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR is not set");
                    let path = PathBuf::from(root).join(path);

                    let mut file = File::open(path)
                        .map_err(|e| Error::ParseError(syn::Error::new(span, e.to_string())))?;

                    let mut file_str = String::new();
                    std::io::Read::read_to_string(&mut file, &mut file_str)
                        .map_err(|e| Error::ParseError(syn::Error::new(span, e.to_string())))?;

                    let tokens = TokenStream::from_str(&file_str)
                        .map_err(|e| Error::ParseError(syn::Error::new(span, e.to_string())))?;

                    let expr = Expr::ctx_parse.ctx_parse2(tokens, ctx)?;

                    return Ok(expr);
                }
            }
        }

        if input.peek(Token![if]) {
            let if_span = <Token![if]>::ctx_parse(input, ctx)?.span;
            let cond = Expr::ctx_parse(input, ctx)?;

            let then_group = Group::ctx_parse(input, ctx)?;
            if then_group.delimiter() != Delimiter::Brace {
                return Err(Error::ParseError(syn::Error::new(
                    then_group.span(),
                    "expected a brace-delimited block",
                )));
            }

            let then = Expr::ctx_parse.ctx_parse2(then_group.stream(), ctx)?;

            <Token![else]>::ctx_parse(input, ctx)?;

            let else_group = Group::ctx_parse(input, ctx)?;
            if else_group.delimiter() != Delimiter::Brace {
                return Err(Error::ParseError(syn::Error::new(
                    else_group.span(),
                    "expected a brace-delimited block",
                )));
            }

            let else_ = Expr::ctx_parse.ctx_parse2(else_group.stream(), ctx)?;

            return Ok(Self {
                span: if_span,
                kind: ExprKind::Op(FragmentOp {
                    op: Op::IfElse(if_span),
                    args: vec![cond, then, else_],
                }),
            });
        }

        if input.peek(Token![match]) {
            let match_token = <Token![match]>::ctx_parse(input, ctx)?;
            let expr = Box::new(Expr::ctx_parse(input, ctx)?);

            let group = Group::ctx_parse(input, ctx)?;
            if group.delimiter() != Delimiter::Brace {
                return Err(Error::ParseError(syn::Error::new(
                    group.span(),
                    "expected a brace-delimited block",
                )));
            }

            let match_arms_parse_fn = |input: ParseStream, ctx: &mut Context| {
                let mut arms = Vec::new();

                while !input.is_empty() {
                    let pat_span = input.span();
                    let pat = Pattern::ctx_parse(input, ctx)?;

                    let condition = if input.peek(Token![if]) {
                        <Token![if]>::ctx_parse(input, ctx)?;

                        Some(Expr::ctx_parse(input, ctx)?)
                    } else {
                        None
                    };

                    <Token![=>]>::ctx_parse(input, ctx)?;

                    let body = Expr::ctx_parse(input, ctx)?;

                    arms.push(ExprMatchArm {
                        pat,
                        condition,
                        body,
                        unused_arm_warning: ctx.push_warning(Warning::UnusedMatchArm(pat_span)),
                    });

                    if !input.is_empty() {
                        <Token![,]>::ctx_parse(input, ctx)?;
                    }
                }

                Ok::<_, Error>(arms)
            };

            let match_arms = match_arms_parse_fn.ctx_parse2(group.stream(), ctx)?;

            return Ok(Expr {
                span: match_token.span,
                kind: ExprKind::Match(ExprMatch {
                    match_token,
                    expr,
                    match_arms,
                }),
            });
        }

        if input.peek(Ident) {
            let name = Name::ctx_parse(input, ctx)?;

            return Ok(Self::name(name));
        }

        if let Some(lit) = Value::ctx_parse_option_lit(input, ctx)? {
            return Ok(lit.into_expr());
        }

        if let Some(group) = <Option<Group>>::ctx_parse(input, ctx)? {
            match group.delimiter() {
                Delimiter::Parenthesis => {
                    let fragment_expr = Expr::ctx_parse.ctx_parse2(group.stream(), ctx)?;

                    return Ok(fragment_expr);
                }

                Delimiter::Brace => {
                    return Err(Error::ParseError(syn::Error::new(
                        group.span(),
                        "block expressions are not supported",
                    )));
                }

                Delimiter::Bracket => {
                    return Ok(Expr {
                        span: group.span(),
                        kind: ExprKind::List(ctx_parse_punctuated.ctx_parse2(group.stream(), ctx)?),
                    });
                }

                Delimiter::None => {
                    return Expr::ctx_parse.ctx_parse2(group.stream(), ctx);
                }
            }
        }

        Err(Error::ParseError(syn::Error::new(
            input.span(),
            "expected a fragment expr",
        )))
    }
}

impl Optimize for Expr {
    fn optimize(&mut self, ctx: &mut Context) {
        match &mut self.kind {
            ExprKind::Value(_) => {}
            ExprKind::Name(_) => {}

            ExprKind::List(val) => {
                for item in val {
                    item.optimize(ctx);
                }
            }

            ExprKind::Op(FragmentOp { op: _, args }) => {
                for item in args.iter_mut() {
                    item.optimize(ctx);
                }
            }

            ExprKind::Match(ExprMatch {
                match_token: _,
                expr,
                match_arms,
            }) => {
                expr.optimize(ctx);

                for arm in match_arms.iter_mut() {
                    arm.body.optimize(ctx);

                    if let Some(condition) = &mut arm.condition {
                        condition.optimize(ctx);
                    }
                }
            }
        }
    }
}

impl Expr {
    pub fn eval(&self, ctx: &mut Context, namespace: &Namespace) -> Value {
        let value = match &self.kind {
            ExprKind::Value(val) => val.clone().kind,

            ExprKind::Name(name) => namespace.try_get(*name, ctx).kind,

            ExprKind::List(val) => ValueKind::List(
                val.into_iter()
                    .map(|item| item.eval(ctx, namespace))
                    .collect(),
            ),

            ExprKind::Op(FragmentOp { op, args }) => {
                let resolved_args = args
                    .into_iter()
                    .map(|item| item.eval(ctx, namespace))
                    .collect::<Vec<_>>();

                match op.compute(&resolved_args, ctx) {
                    Ok(val) => val.kind,
                    Err(err) => ValueKind::Unknown(ctx.push_error(err).unknown_guard()),
                }
            }

            ExprKind::Match(ExprMatch {
                match_token: _,
                expr,
                match_arms,
            }) => {
                let expr_value = expr.eval(ctx, namespace);

                let mut matched_arm = None;
                for arm in match_arms {
                    match arm.pat.matches(&expr_value, ctx) {
                        PatternMatches::Matches => {}
                        PatternMatches::Mismatched(_) => continue,
                        PatternMatches::Unknown(guard) => return Value::unknown(guard),
                    }

                    if let Some(condition) = &arm.condition {
                        let cond_value = condition.eval(ctx, namespace);
                        match cond_value.kind {
                            ValueKind::Unknown(guard) => {
                                return Value::unknown(guard);
                            }

                            ValueKind::Bool(val) => {
                                if !val {
                                    continue;
                                }
                            }
                            _ => {
                                return Value::unknown(
                                    ctx.push_error(Error::ExpectedFound {
                                        span: condition.span,
                                        expected: "bool",
                                        found: cond_value.kind.kind_str(),
                                    })
                                    .unknown_guard(),
                                );
                            }
                        }
                    }

                    matched_arm = Some(arm);
                    break;
                }

                if let Some(matched_arm) = matched_arm {
                    matched_arm.unused_arm_warning.remove();

                    let mut arm_namespace = namespace.fork();
                    arm_namespace.flush();
                    matched_arm
                        .pat
                        .queue_insert(expr_value.clone(), &mut arm_namespace, ctx);

                    matched_arm.body.eval(ctx, &mut arm_namespace).kind
                } else {
                    ValueKind::Unknown(
                        ctx.push_error(Error::NoMatches {
                            span: expr_value.span,
                            value: expr_value.to_string(),
                        })
                        .unknown_guard(),
                    )
                }
            }
        };

        Value {
            span: self.span,
            kind: value,
        }
    }
}

impl Expand for Expr {
    fn expand(&self, output: &mut TokenStream, ctx: &mut Context, namespace: &Namespace) {
        self.eval(ctx, namespace).expand(output, ctx, namespace)
    }
}

fn ctx_parse_punctuated<T: ContextParse>(
    input: ParseStream,
    ctx: &mut Context,
) -> Result<Vec<T>, Error> {
    let mut items = Vec::new();

    while !input.is_empty() {
        let item = T::ctx_parse(input, ctx)?;
        items.push(item);

        if input.is_empty() {
            break;
        }

        <Token![,]>::ctx_parse(input, ctx)?;
    }

    Ok(items)
}
