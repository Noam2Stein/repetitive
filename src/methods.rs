use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Ident, Token, parse::ParseStream};

use super::*;

/// Contains both the dot and the method ident.
#[derive(Debug, Clone, Copy)]
pub enum Method {
    // Unary Operators
    Neg(Span),
    Not(Span),

    // Binary Operators
    Add(Span),
    Sub(Span),
    Mul(Span),
    Div(Span),
    Rem(Span),

    BitAnd(Span),
    BitOr(Span),
    BitXor(Span),
    Shl(Span),
    Shr(Span),

    // Comparison Operators
    Eq(Span),
    Ne(Span),
    Lt(Span),
    Gt(Span),
    Le(Span),
    Ge(Span),

    Min(Span),
    Max(Span),
    Clamp(Span),

    // List
    Len(Span),
    Index(Span),
    Enumerate(Span),
    Zip(Span),
    Chain(Span),
    Contains(Span),
    ConcatIdent(Span),
    ConcatString(Span),
}

impl ContextParse for Method {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let dot = <Token![.]>::ctx_parse(input, ctx)?;

        let ident = match Ident::ctx_parse(input, ctx) {
            Ok(ident) => {
                ctx.push_method_call(dot, Some(ident.clone()));

                ident
            }
            Err(err) => {
                ctx.push_method_call(dot, None);

                return Err(err);
            }
        };

        Ok(match ident.to_string().as_str() {
            // Unary Operators
            "neg" => Self::Neg(ident.span()),
            "not" => Self::Not(ident.span()),

            // Binary Operators
            "add" => Self::Add(ident.span()),
            "sub" => Self::Sub(ident.span()),
            "mul" => Self::Mul(ident.span()),
            "div" => Self::Div(ident.span()),
            "rem" => Self::Rem(ident.span()),

            "bitand" => Self::BitAnd(ident.span()),
            "bitor" => Self::BitOr(ident.span()),
            "bitxor" => Self::BitXor(ident.span()),
            "shl" => Self::Shl(ident.span()),
            "shr" => Self::Shr(ident.span()),

            // Comparison Operators
            "eq" => Self::Eq(ident.span()),
            "ne" => Self::Ne(ident.span()),
            "lt" => Self::Lt(ident.span()),
            "gt" => Self::Gt(ident.span()),
            "le" => Self::Le(ident.span()),
            "ge" => Self::Ge(ident.span()),

            "min" => Self::Min(ident.span()),
            "max" => Self::Max(ident.span()),
            "clamp" => Self::Clamp(ident.span()),

            // List
            "len" => Self::Len(ident.span()),
            "index" => Self::Index(ident.span()),
            "enumerate" => Self::Enumerate(ident.span()),
            "zip" => Self::Zip(ident.span()),
            "chain" => Self::Chain(ident.span()),
            "contains" => Self::Contains(ident.span()),
            "concat_ident" => Self::ConcatIdent(ident.span()),
            "concat_string" => Self::ConcatString(ident.span()),

            _ => {
                return Err(Error::ParseError(syn::Error::new(
                    ident.span(),
                    "unknown method",
                )));
            }
        })
    }
}

#[cfg(feature = "doc")]
pub fn paste_method_doc(idents: &[(Token![.], Option<Ident>)]) -> TokenStream {
    let ident_calls = idents.iter().map(|(dot, ident)| {
        match ident
            .as_ref()
            .map(|ident| ident.to_string())
            .as_ref()
            .map(|ident| ident.as_str())
        {
            // Unary Operators
            Some("neg") => quote! { val #dot #ident() },
            Some("not") => quote! { val #dot #ident() },

            // Binary Operators
            Some("add") => quote! { val #dot #ident(other) },
            Some("sub") => quote! { val #dot #ident(other) },
            Some("mul") => quote! { val #dot #ident(other) },
            Some("div") => quote! { val #dot #ident(other) },
            Some("rem") => quote! { val #dot #ident(other) },

            Some("bitand") => quote! { val #dot #ident(other) },
            Some("bitor") => quote! { val #dot #ident(other) },
            Some("bitxor") => quote! { val #dot #ident(other) },
            Some("shl") => quote! { val #dot #ident(other) },
            Some("shr") => quote! { val #dot #ident(other) },

            // Comparison Operators
            Some("eq") => quote! { val #dot #ident(other) },
            Some("ne") => quote! { val #dot #ident(other) },
            Some("lt") => quote! { val #dot #ident(other) },
            Some("gt") => quote! { val #dot #ident(other) },
            Some("le") => quote! { val #dot #ident(other) },
            Some("ge") => quote! { val #dot #ident(other) },

            Some("min") => quote! { val #dot #ident(other) },
            Some("max") => quote! { val #dot #ident(other) },
            Some("clamp") => quote! { val #dot #ident(min, max) },

            // List
            Some("len") => quote! { val #dot #ident() },
            Some("index") => quote! { val #dot #ident(index) },
            Some("enumerate") => quote! { val #dot #ident() },
            Some("zip") => quote! { val #dot #ident(other) },
            Some("chain") => quote! { val #dot #ident(other) },
            Some("contains") => quote! { val #dot #ident(value) },
            Some("concat_ident") => quote! { val #dot #ident() },
            Some("concat_string") => quote! { val #dot #ident() },

            Some(_) => quote! { val #dot #ident },
            None => quote! { val #dot },
        }
    });

    let neg_doc = fn_doc("neg", NEG_DOC, &idents);
    let not_doc = fn_doc("not", NOT_DOC, &idents);

    let add_doc = fn_doc("add", ADD_DOC, &idents);
    let sub_doc = fn_doc("sub", SUB_DOC, &idents);
    let mul_doc = fn_doc("mul", MUL_DOC, &idents);
    let div_doc = fn_doc("div", DIV_DOC, &idents);
    let rem_doc = fn_doc("rem", REM_DOC, &idents);
    let bitand_doc = fn_doc("bitand", BITAND_DOC, &idents);
    let bitor_doc = fn_doc("bitor", BITOR_DOC, &idents);
    let bitxor_doc = fn_doc("bitxor", BITXOR_DOC, &idents);
    let shl_doc = fn_doc("shl", SHL_DOC, &idents);
    let shr_doc = fn_doc("shr", SHR_DOC, &idents);

    let eq_doc = fn_doc("eq", EQ_DOC, &idents);
    let ne_doc = fn_doc("ne", NE_DOC, &idents);
    let lt_doc = fn_doc("lt", LT_DOC, &idents);
    let gt_doc = fn_doc("gt", GT_DOC, &idents);
    let le_doc = fn_doc("le", LE_DOC, &idents);
    let ge_doc = fn_doc("ge", GE_DOC, &idents);

    let min_doc = fn_doc("min", MIN_DOC, &idents);
    let max_doc = fn_doc("max", MAX_DOC, &idents);
    let clamp_doc = fn_doc("clamp", CLAMP_DOC, &idents);

    let len_doc = fn_doc("len", LEN_DOC, &idents);
    let index_doc = fn_doc("index", INDEX_DOC, &idents);
    let enumerate_doc = fn_doc("enumerate", ENUMERATE_DOC, &idents);
    let zip_doc = fn_doc("zip", ZIP_DOC, &idents);
    let chain_doc = fn_doc("chain", CHAIN_DOC, &idents);
    let contains_doc = fn_doc("contains", CONTAINS_DOC, &idents);
    let concat_ident_doc = fn_doc("concat_ident", CONCAT_IDENT_DOC, &idents);
    let concat_string_doc = fn_doc("concat_string", CONCAT_STRING_DOC, &idents);

    quote! {
        const _: () = {
            #[allow(dead_code)]
            #[derive(Debug, Clone, Copy)]
            struct Fragment;

            impl Fragment {
                #[allow(dead_code)]
                #[doc = #neg_doc]
                const fn neg(self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #not_doc]
                const fn not(self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #add_doc]
                const fn add(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #sub_doc]
                const fn sub(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #mul_doc]
                const fn mul(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #div_doc]
                const fn div(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #rem_doc]
                const fn rem(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #bitand_doc]
                const fn bitand(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #bitor_doc]
                const fn bitor(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #bitxor_doc]
                const fn bitxor(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #shl_doc]
                const fn shl(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #shr_doc]
                const fn shr(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #eq_doc]
                const fn eq(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #ne_doc]
                const fn ne(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #lt_doc]
                const fn lt(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #gt_doc]
                const fn gt(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #le_doc]
                const fn le(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #ge_doc]
                const fn ge(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #min_doc]
                const fn min(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #max_doc]
                const fn max(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #clamp_doc]
                const fn clamp(self, min: Self, max: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #len_doc]
                const fn len(self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #index_doc]
                const fn index(self, index: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #enumerate_doc]
                const fn enumerate(self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #zip_doc]
                const fn zip(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #chain_doc]
                const fn chain(self, other: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #contains_doc]
                const fn contains(self, value: Self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #concat_ident_doc]
                const fn concat_ident(self) -> Self { Self }

                #[allow(dead_code)]
                #[doc = #concat_string_doc]
                const fn concat_string(self) -> Self { Self }
            }

            #[allow(dead_code)]
            let val = Fragment;
            #[allow(dead_code)]
            let value = Fragment;
            #[allow(dead_code)]
            let other = Fragment;
            #[allow(dead_code)]
            let index = Fragment;
            #[allow(dead_code)]
            let min = Fragment;
            #[allow(dead_code)]
            let max = Fragment;

            #(#ident_calls;)*
        };
    }
}

fn fn_doc(name: &str, doc: &'static str, idents: &[(Token![.], Option<Ident>)]) -> &'static str {
    if idents.iter().any(|(_, ident)| {
        ident
            .as_ref()
            .map(|ident| ident.to_string())
            .as_ref()
            .map(|ident| ident.as_str())
            == Some(name)
    }) {
        doc
    } else {
        ""
    }
}

const NEG_DOC: &str = "Negates a fragment. This works for `int` and `float` fragments.";

const NOT_DOC: &str = "Negates a fragment. This works for `bool` fragments.";

const ADD_DOC: &str = "
    Adds two fragments together.
    This works for `int`, `float`, `str` and `ident` fragments.

    For `str`/`ident` fragments, this concatenates the left with the right `str`/`ident`/`char`.
    ";

const SUB_DOC: &str = "Subtracts two fragments. This works for `int` and `float` fragments.";

const MUL_DOC: &str = "
    Multiplies two fragments.
    This works for `int`, `float`, `str`, `ident` and `char` fragments.

    For `str`/`ident`/`char` fragments, this repeats the left with the right `int`.
    ";

const DIV_DOC: &str = "Divides two fragments. This works for `int` and `float` fragments.";

const REM_DOC: &str = "Remains two fragments. This works for `int` and `float` fragments.";

const BITAND_DOC: &str = "Bitwise and of two fragments. This works for `int` and `bool` fragments.";

const BITOR_DOC: &str = "Bitwise or of two fragments. This works for `int` and `bool` fragments.";

const BITXOR_DOC: &str = "Bitwise xor of two fragments. This works for `int` and `bool` fragments.";

const SHL_DOC: &str = "Left shifts two fragments. This works for `int` fragments.";

const SHR_DOC: &str = "Right shifts two fragments. This works for `int` fragments.";

const EQ_DOC: &str =
    "Checks if two fragments are equal. This works for all fragments but `tokens`.";

const NE_DOC: &str =
    "Checks if two fragments are not equal. This works for all fragments but `tokens`.";

const LT_DOC: &str = "Checks if the first fragment is less than the second. This works for all fragments but `list` and `tokens`.";

const GT_DOC: &str = "Checks if the first fragment is greater than the second. This works for all fragments but `list` and `tokens`.";

const LE_DOC: &str = "Checks if the first fragment is less than or equal to the second. This works for all fragments but `list` and `tokens`.";

const GE_DOC: &str = "Checks if the first fragment is greater than or equal to the second. This works for all fragments but `list` and `tokens`.";

const MIN_DOC: &str = "
    Returns the minimum of two fragments.
    This uses the `<` / `>` operators, and thus works for more than just numbers.
    ";

const MAX_DOC: &str = "
    Returns the maximum of two fragments.
    This uses the `<` / `>` operators, and thus works for more than just numbers.
    ";

const CLAMP_DOC: &str = "
    Clamps a fragment between two other fragments.
    This uses the `<` / `>` operators, and thus works for more than just numbers.
    ";

const LEN_DOC: &str = "Returns the length of a fragment. This works for `str`, `ident`, `char`, `list` and `tokens` fragments.";

const INDEX_DOC: &str =
    "Returns the element at the given index. This only works for `list` fragments.";

const ENUMERATE_DOC: &str =
    "Returns a list of tuple-lists over the elements of the input list, as `[idx, val]`.";

const ZIP_DOC: &str =
    "Returns a list of tuple-lists over the elements of two fragments, as `[val1, val2]`.";

const CHAIN_DOC: &str =
    "Returns a list that has the elements of the first list, then of the second list.";

const CONTAINS_DOC: &str = "Checks if the specified value is contained in the list.";

const CONCAT_IDENT_DOC: &str =
    "Concatenates the items of a list into an `ident`, this works exactly like `@[...]`.";

const CONCAT_STRING_DOC: &str =
    "Concatenates the items of a list into a `str`, this works exactly like `@[...]`.";
