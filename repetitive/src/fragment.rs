use std::{iter::once, mem::replace};

use proc_macro2::{Delimiter, Group, Span, TokenStream};
use quote::ToTokens;
use syn::{
    Error, Ident, Lifetime, Lit, LitBool, LitChar, LitFloat, LitInt, LitStr, Token,
    parse::{Parse, ParseStream, Parser},
    token::Bracket,
};

use super::*;

#[derive(Debug, Clone)]
pub struct Fragment<T = FragmentBody> {
    pub span: Span,
    pub body: T,
}

#[derive(Debug, Clone)]
pub enum FragmentBody {
    Value(FragmentValue),
    Name(Name),
    Op(FragmentOp),
    List(Vec<FragmentBody>),
}

#[derive(Debug, Clone)]
pub enum FragmentValue {
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Ident(String),
    List(Vec<FragmentValue>),
    Tokens(Tokens),
}

#[derive(Debug, Clone)]
pub struct FragmentOp {
    op: Op,
    span: Span,
    args: Vec<FragmentBody>,
}

// Error

impl FragmentValue {
    pub fn error() -> Self {
        Self::Tokens(Tokens::default())
    }
}

// Parse

impl Fragment {
    pub fn ctx_parse_outer(
        input: syn::parse::ParseStream,
        ctx: &mut ParseContext,
    ) -> syn::Result<Self> {
        let at_token = input.parse::<Token![@]>()?;

        let inner = FragmentBody::ctx_parse_outer(input, ctx)?;

        Ok(Self {
            span: at_token.span,
            body: inner,
        })
    }

    pub fn ctx_parse_expr(
        input: syn::parse::ParseStream,
        ctx: &mut ParseContext,
    ) -> syn::Result<Self> {
        let span = input.span();
        let inner = FragmentBody::ctx_parse_expr(input, ctx)?;

        Ok(Self { span, body: inner })
    }

    pub fn ctx_parse_single_expr(
        input: syn::parse::ParseStream,
        ctx: &mut ParseContext,
    ) -> syn::Result<Self> {
        let span = input.span();
        let inner = FragmentBody::ctx_parse_single_expr(input, ctx)?;

        Ok(Self { span, body: inner })
    }

    pub fn end(&self, output: &mut TokenStream) {
        match &self.body {
            FragmentBody::Tokens(tokens) => tokens.end(output),
            FragmentBody::Lit(lit) => lit.to_tokens(self.span, output),
            FragmentBody::List(val) => val.iter().for_each(|item| {
                Fragment {
                    span: self.span,
                    body: item.clone(),
                }
                .end(output)
            }),

            _ => panic!("unresolved fragment"),
        }
    }
}

impl FragmentBody {
    fn ctx_parse_outer(
        input: syn::parse::ParseStream,
        ctx: &mut ParseContext,
    ) -> syn::Result<Self> {
        if Keyword::peek(input) {
            let keyword_span = input.span();
            let keyword = input.parse::<Keyword>()?;

            if input.peek(Bracket) {
                let group = input.parse::<Group>()?;

                let to_str = match keyword {
                    Keyword::Str => true,
                };

                return Ok(Self::parse_concat(group.stream(), to_str, ctx)?);
            }

            return Err(syn::Error::new(keyword_span, "unexpected keyword"));
        }

        if input.peek(Ident) {
            let name = Name::ctx_parse(input, ctx)?;

            return Ok(Self::Name(name));
        }

        if let Some(group) = input.parse::<Option<Group>>()? {
            match group.delimiter() {
                Delimiter::Parenthesis => {
                    let fragment_expr = ctx_parse2(Fragment::ctx_parse_expr, group.stream(), ctx)?;

                    return Ok(fragment_expr.body);
                }

                Delimiter::Brace => {
                    return Ok(Self::Tokens(ctx_parse2(
                        Tokens::ctx_parse,
                        group.stream(),
                        ctx,
                    )?));
                }

                Delimiter::Bracket => {
                    return Ok(Self::parse_concat(group.stream(), false, ctx)?);
                }

                Delimiter::None => {
                    return ctx_parse2(FragmentBody::ctx_parse_outer, group.stream(), ctx);
                }
            }
        }

        if input.peek(Lit) {
            return Ok(Self::Lit(FragmentValue::parse(input)?));
        }

        Err(syn::Error::new(input.span(), "expected a fragment"))
    }

    fn ctx_parse_expr(input: syn::parse::ParseStream, ctx: &mut ParseContext) -> syn::Result<Self> {
        #[derive(Debug, Clone, PartialEq, PartialOrd)]
        enum BinOpLvl {
            Or,
            And,
            BitwiseOr,
            BitwiseXor,
            BitwiseAnd,
            EqNe,
            Cmp,
            Range,
            AddSub,
            MulDivRem,
        }

        enum TempExpr {
            Bin(Box<TempExpr>, Span, Box<TempExpr>, Op, BinOpLvl),
            Single(FragmentBody),
        }

        fn resolve_bin_expr(expr: TempExpr) -> FragmentBody {
            match expr {
                TempExpr::Bin(left, op_span, right, op, _) => FragmentBody::Op(FragmentOp {
                    op,
                    span: op_span,
                    args: vec![resolve_bin_expr(*left), resolve_bin_expr(*right)],
                }),

                TempExpr::Single(expr) => expr,
            }
        }

        let mut expr = TempExpr::Single(FragmentBody::ctx_parse_single_expr(input, ctx)?);

        loop {
            let op_span = input.span();
            let (op, lvl) = {
                if input.peek(Token![+]) {
                    (Op::Add, BinOpLvl::AddSub)
                } else if input.peek(Token![-]) {
                    (Op::Sub, BinOpLvl::AddSub)
                } else if input.peek(Token![*]) {
                    (Op::Mul, BinOpLvl::MulDivRem)
                } else if input.peek(Token![/]) {
                    (Op::Div, BinOpLvl::MulDivRem)
                } else if input.peek(Token![%]) {
                    (Op::Rem, BinOpLvl::MulDivRem)
                } else if input.peek(Token![|]) {
                    (Op::BitOr, BinOpLvl::BitwiseOr)
                } else if input.peek(Token![^]) {
                    (Op::BitXor, BinOpLvl::BitwiseXor)
                } else if input.peek(Token![&]) {
                    (Op::BitAnd, BinOpLvl::BitwiseAnd)
                } else if input.peek(Token![==]) {
                    (Op::Eq, BinOpLvl::EqNe)
                } else if input.peek(Token![!=]) {
                    (Op::Ne, BinOpLvl::EqNe)
                } else if input.peek(Token![<]) {
                    (Op::Lt, BinOpLvl::Cmp)
                } else if input.peek(Token![>]) {
                    (Op::Gt, BinOpLvl::Cmp)
                } else if input.peek(Token![<=]) {
                    (Op::Le, BinOpLvl::Cmp)
                } else if input.peek(Token![>=]) {
                    (Op::Ge, BinOpLvl::Cmp)
                } else if input.peek(Token![..=]) {
                    (Op::RangeInclusive, BinOpLvl::Range)
                } else if input.peek(Token![..]) {
                    (Op::Range, BinOpLvl::Range)
                } else if input.peek(Token![||]) {
                    (Op::Or, BinOpLvl::Or)
                } else if input.peek(Token![&&]) {
                    (Op::And, BinOpLvl::And)
                } else {
                    break;
                }
            };

            let right = FragmentBody::ctx_parse_single_expr(input, ctx)?;

            match &mut expr {
                TempExpr::Bin(_, _, expr_right, _, expr_lvl) => {
                    if lvl > *expr_lvl {
                        **expr_right = TempExpr::Bin(
                            Box::new(replace(expr_right, TempExpr::Single(FragmentBody::error()))),
                            op_span,
                            Box::new(TempExpr::Single(right)),
                            op,
                            lvl,
                        );
                    } else {
                        expr = TempExpr::Bin(
                            Box::new(expr),
                            op_span,
                            Box::new(TempExpr::Single(right)),
                            op,
                            lvl,
                        );
                    }
                }

                TempExpr::Single(left) => {
                    expr = TempExpr::Bin(
                        Box::new(TempExpr::Single(replace(left, FragmentBody::error()))),
                        op_span,
                        Box::new(TempExpr::Single(right)),
                        op,
                        lvl,
                    );
                }
            }
        }

        let mut expr = resolve_bin_expr(expr);

        expr.try_resolve()?;

        Ok(expr)
    }

    fn ctx_parse_single_expr(
        input: syn::parse::ParseStream,
        ctx: &mut ParseContext,
    ) -> syn::Result<Self> {
        if input.peek(Token![-]) {
            let span = input.parse::<Token![-]>()?.span;

            let arg = FragmentBody::ctx_parse_single_expr(input, ctx)?;
            return Self::op(Op::Neg, span, vec![arg], &mut ctx.base);
        }

        if input.peek(Token![!]) {
            let span = input.parse::<Token![!]>()?.span;

            let arg = FragmentBody::ctx_parse_single_expr(input, ctx)?;
            return Self::op(Op::Not, span, vec![arg], &mut ctx.base);
        }

        let mut expr = FragmentBody::ctx_parse_base_expr(input, ctx)?;

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

            let args = Self::parse_list(group.stream(), ctx)?;

            let op = match ident.to_string().as_str() {
                "not" => Op::Not,
                "neg" => Op::Neg,

                "add" => Op::Add,
                "sub" => Op::Sub,
                "mul" => Op::Mul,
                "div" => Op::Div,
                "rem" => Op::Rem,
                "bitand" => Op::BitAnd,
                "bitor" => Op::BitOr,
                "bitxor" => Op::BitXor,
                "shl" => Op::Shl,
                "shr" => Op::Shr,

                "eq" => Op::Eq,
                "ne" => Op::Ne,
                "lt" => Op::Lt,
                "gt" => Op::Gt,
                "le" => Op::Le,
                "ge" => Op::Ge,

                "len" => Op::Len,
                "index" => Op::Index,
                "enumerate" => Op::Enumerate,
                "zip" => Op::Zip,
                "chain" => Op::Chain,

                _ => {
                    return Err(syn::Error::new(
                        ident.span(),
                        format!("method `{}` not found", ident.to_string()),
                    ));
                }
            };

            expr = Self::op(
                op,
                ident.span(),
                once(expr).chain(args).collect(),
                &mut ctx.base,
            )?;
        }

        Ok(expr)
    }

    fn ctx_parse_base_expr(
        input: syn::parse::ParseStream,
        ctx: &mut ParseContext,
    ) -> syn::Result<Self> {
        if input.peek(Token![@]) {
            input.parse::<Token![@]>()?;

            return Ok(Self::ctx_parse_outer(input, ctx)?);
        }

        if input.peek(Ident) {
            let name = Name::ctx_parse(input, ctx)?;

            return Ok(Self::Name(name));
        }

        if let Some(group) = input.parse::<Option<Group>>()? {
            match group.delimiter() {
                Delimiter::Parenthesis => {
                    let fragment_expr = ctx_parse2(Fragment::ctx_parse_expr, group.stream(), ctx)?;

                    return Ok(fragment_expr.body);
                }

                Delimiter::Brace => {
                    let tokens = ctx_parse2(Tokens::ctx_parse, group.stream(), ctx)?;

                    return Ok(Self::Tokens(tokens));
                }

                Delimiter::Bracket => {
                    return Ok(Self::List(Self::parse_list(group.stream(), ctx)?));
                }

                Delimiter::None => {
                    return ctx_parse2(FragmentBody::ctx_parse_base_expr, group.stream(), ctx);
                }
            }
        }

        if input.peek(Lit) {
            return Ok(Self::Lit(FragmentValue::parse(input)?));
        }

        Err(syn::Error::new(input.span(), "expected a fragment"))
    }

    fn parse_list(input: TokenStream, ctx: &mut ParseContext) -> syn::Result<Vec<Self>> {
        let parse_fn = |input: ParseStream| {
            let mut items = Vec::new();

            loop {
                if input.is_empty() {
                    break;
                }

                let item = FragmentBody::ctx_parse_expr(input, ctx)?;
                items.push(item);

                if input.is_empty() {
                    break;
                }

                input.parse::<Token![,]>()?;
            }

            Ok::<_, syn::Error>(items)
        };

        let items = parse_fn.parse2(input)?;

        Ok(items)
    }

    fn parse_concat(
        stream: TokenStream,
        to_str: bool,
        ctx: &mut ParseContext,
    ) -> syn::Result<Self> {
        let parse_fn = |input: ParseStream| {
            let mut parts = Vec::new();

            loop {
                if input.is_empty() {
                    break;
                }

                let item = Fragment::ctx_parse_single_expr(input, ctx)?;
                parts.push(item);
            }

            Ok::<_, syn::Error>(parts)
        };

        let parts = parse_fn.parse2(stream)?;

        let mut output = String::new();

        for mut part in parts {
            part.body.resolve(&mut ctx.base);

            match &part.body {
                FragmentBody::Lit(FragmentValue::String(val)) => output.push_str(val.as_str()),
                FragmentBody::Lit(FragmentValue::Int(val)) => {
                    output.push_str(val.to_string().as_str())
                }
                FragmentBody::Lit(FragmentValue::Float(val)) => {
                    output.push_str(val.to_string().as_str())
                }
                FragmentBody::Lit(FragmentValue::Bool(val)) => {
                    output.push_str(val.to_string().as_str())
                }
                FragmentBody::Lit(FragmentValue::Char(val)) => {
                    output.push_str(val.to_string().as_str())
                }
                FragmentBody::Lit(FragmentValue::Ident(val)) => {
                    output.push_str(val.to_string().as_str())
                }

                FragmentBody::Tokens(_) => {
                    return Err(syn::Error::new(part.span, "expected a fragment literal"));
                }
                FragmentBody::List(_) => {
                    return Err(syn::Error::new(part.span, "expected a fragment literal"));
                }

                FragmentBody::Name(_) => unreachable!(),
                FragmentBody::Op(_) => unreachable!(),
            }
        }

        if to_str {
            Ok(Self::Lit(FragmentValue::String(output)))
        } else {
            Ok(Self::Lit(FragmentValue::Ident(output)))
        }
    }

    fn op(op: Op, span: Span, args: Vec<FragmentBody>, ctx: &mut Context) -> syn::Result<Self> {
        if args.iter().all(Self::is_resolved) {
            return Ok(op.compute(span, &args, ctx)?);
        }

        Ok(Self::Op(FragmentOp { op, span, args }))
    }

    fn is_resolved(&self) -> bool {
        match self {
            Self::Lit(_) => true,
            Self::Tokens(_) => true,
            Self::List(val) => val.iter().all(|item| item.is_resolved()),
            Self::Name(_) => false,
            Self::Op(_) => false,
        }
    }

    fn try_resolve(&mut self) -> syn::Result<()> {
        match self {
            Self::Lit(_) => {}
            Self::Tokens(_) => {}
            Self::Name(_) => {}

            Self::List(val) => {
                for item in val {
                    item.try_resolve()?;
                }
            }

            Self::Op(FragmentOp { op, span, args }) => {
                for item in args.iter_mut() {
                    item.try_resolve()?;
                }

                if args.iter().all(Self::is_resolved) {
                    *self = op.compute(
                        *span,
                        args,
                        &mut Context {
                            errors: &mut Vec::new(),
                            namespace: &mut Namespace::new(),
                        },
                    )?;
                }
            }
        }

        Ok(())
    }

    pub fn error() -> Self {
        Self::Tokens(Tokens::default())
    }
}

// Resolve

impl FragmentBody {
    pub fn resolve(self, ctx: &mut Context) -> syn::Result<FragmentValue> {
        Ok(match self {
            Self::Value(val) => val,

            Self::List(val) => FragmentValue::List(
                val.into_iter()
                    .map(|item| item.resolve(ctx))
                    .collect::<syn::Result<_>>()?,
            ),

            Self::Name(name) => ctx.namespace.try_get(name, &mut ctx.errors),

            Self::Op(FragmentOp { op, span, args }) => {
                for item in args.iter_mut() {
                    item.resolve(ctx);
                }

                *self = match op.compute(*span, args, ctx) {
                    Ok(val) => val,
                    Err(err) => {
                        ctx.errors.push(err);

                        Self::error()
                    }
                };
            }
        })
    }
}

// Paste

impl Fragment<FragmentValue> {
    pub fn paste(&self, output: &mut TokenStream, ctx: &Context) {
        match &self.body {
            FragmentValue::Int(val) => {
                LitInt::new(val.to_string().as_str(), self.span).to_tokens(output)
            }
            FragmentValue::Float(val) => {
                LitFloat::new(val.to_string().as_str(), self.span).to_tokens(output)
            }
            FragmentValue::Bool(val) => LitBool::new(*val, self.span).to_tokens(output),
            FragmentValue::String(val) => LitStr::new(val, self.span).to_tokens(output),
            FragmentValue::Char(val) => LitChar::new(*val, self.span).to_tokens(output),
            FragmentValue::Ident(val) => Ident::new(val, self.span).to_tokens(output),

            FragmentValue::Tokens(val) => val.paste(output, ctx),

            FragmentValue::List(_) => ctx
                .errors
                .push(Error::new(self.span, "expected a fragment literal")),
        }
    }
}

impl Parse for FragmentValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Some(token) = input.parse::<Option<LitInt>>()? {
            return Ok(Self::Int(token.base10_digits().parse().unwrap()));
        }

        if let Some(token) = input.parse::<Option<LitFloat>>()? {
            return Ok(Self::Float(token.base10_digits().parse().unwrap()));
        }

        if let Some(token) = input.parse::<Option<LitBool>>()? {
            return Ok(Self::Bool(token.value));
        }

        if let Some(token) = input.parse::<Option<LitStr>>()? {
            return Ok(Self::String(token.value()));
        }

        if let Some(token) = input.parse::<Option<LitChar>>()? {
            return Ok(Self::Char(token.value()));
        }

        if let Some(lifetime) = input.parse::<Option<Lifetime>>()? {
            return Ok(Self::Ident(lifetime.ident.to_string()));
        }

        Err(syn::Error::new(input.span(), "expected a fragment literal"))
    }
}

impl FragmentValue {
    fn to_tokens(&self, at_token_span: Span, output: &mut TokenStream) {
        match self {
            Self::Int(val) => {
                LitInt::new(val.to_string().as_str(), at_token_span).to_tokens(output)
            }
            Self::Float(val) => {
                LitFloat::new(val.to_string().as_str(), at_token_span).to_tokens(output)
            }
            Self::Bool(val) => LitBool::new(*val, at_token_span).to_tokens(output),
            Self::String(val) => LitStr::new(val, at_token_span).to_tokens(output),
            Self::Char(val) => LitChar::new(*val, at_token_span).to_tokens(output),
            Self::Ident(val) => val.to_tokens(output),
        }
    }
}
