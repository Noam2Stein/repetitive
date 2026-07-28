use indoc::formatdoc;
use proc_macro2::{TokenStream, TokenTree};

use crate::repetitive2;

macro_rules! assert_tokenstream_eq {
    (repetitive! $input:tt, $expected_output:tt) => {
        crate::tests::utils::assert_tokenstream_eq_helper(
            quote::quote!$input,
            quote::quote!$expected_output,
        );
    };
}
pub(crate) use assert_tokenstream_eq;

#[doc(hidden)]
pub fn assert_tokenstream_eq_helper(input: TokenStream, expected_output: TokenStream) {
    let actual_output = repetitive2(input);

    if tokenstream_eq(actual_output.clone(), expected_output.clone()) {
        return;
    }

    let expected_output = match syn::parse2(expected_output) {
        Ok(file) => prettyplease::unparse(&file),
        Err(err) => format!("invalid rust syntax: {err}"),
    };
    let actual_output = match syn::parse2(actual_output) {
        Ok(file) => prettyplease::unparse(&file),
        Err(err) => format!("invalid rust syntax: {err}"),
    };

    panic!(
        "{}",
        formatdoc! {"
            actual output does not match expected output

            expected:

            ```
            {expected_output}
            ```

            actual:

            ```
            {actual_output}
            ```

        "}
    );
}

fn tokenstream_eq(a: TokenStream, b: TokenStream) -> bool {
    let a = a.into_iter().collect::<Vec<TokenTree>>();
    let b = b.into_iter().collect::<Vec<TokenTree>>();

    a.len() == b.len() && a.into_iter().zip(b).all(|(a, b)| tokentree_eq(a, b))
}

fn tokentree_eq(a: TokenTree, b: TokenTree) -> bool {
    match (a, b) {
        (TokenTree::Group(a), TokenTree::Group(b)) => {
            a.delimiter() == b.delimiter() && tokenstream_eq(a.stream(), b.stream())
        }
        (TokenTree::Group(_), _) | (_, TokenTree::Group(_)) => false,
        (TokenTree::Ident(a), TokenTree::Ident(b)) => a == b,
        (TokenTree::Ident(_), _) | (_, TokenTree::Ident(_)) => false,
        (TokenTree::Literal(a), TokenTree::Literal(b)) => a.to_string() == b.to_string(),
        (TokenTree::Literal(_), _) | (_, TokenTree::Literal(_)) => false,
        (TokenTree::Punct(a), TokenTree::Punct(b)) => {
            a.as_char() == b.as_char() && a.spacing() == b.spacing()
        }
    }
}
