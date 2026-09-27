use indoc::formatdoc;
use itertools::Itertools;
use proc_macro2::{TokenStream, TokenTree};

use crate::repetitive_impl;

macro_rules! assert_expansion_eq {
    (repetitive!$input:tt, quote!$expected_output:tt $(,)?) => {
        crate::tests::utils::assert_expansion_eq_quote_helper(
            quote::quote!$input,
            quote::quote!$expected_output,
        );
    };
    (repetitive!$input:tt, errors![$($error_message:literal),+ $(,)?] $(,)?) => {
        crate::tests::utils::assert_expansion_eq_errors_helper(
            quote::quote!$input,
            &[$($error_message),*],
        );
    };
}
pub(crate) use assert_expansion_eq;

#[doc(hidden)]
pub fn assert_expansion_eq_quote_helper(input: TokenStream, expected_output: TokenStream) {
    let (actual_output, errors) = repetitive_impl(input);

    if !errors.is_empty() {
        let errors = errors.into_iter().map(|error| error.message).join("\n");

        panic!(
            "{}",
            formatdoc! {"
                unexpected errors:

                {errors}

            "}
        );
    }

    if !tokenstream_eq(actual_output.clone(), expected_output.clone()) {
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
}

#[doc(hidden)]
pub fn assert_expansion_eq_errors_helper(input: TokenStream, expected_error_messages: &[&str]) {
    let (_, actual_errors) = repetitive_impl(input);

    if actual_errors.is_empty() {
        panic!("expected errors, found none");
    }

    let errors_match = actual_errors.len() == expected_error_messages.len()
        && actual_errors.iter().zip(expected_error_messages).all(
            |(actual_error, expected_error_message)| {
                actual_error.message == *expected_error_message
            },
        );

    if !errors_match {
        let expected_errors = expected_error_messages.join("\n");
        let actual_errors = actual_errors
            .into_iter()
            .map(|error| error.message)
            .join("\n");

        panic!(
            "{}",
            formatdoc! {"
                actual errors do not match expected errors

                expected:

                ```
                {expected_errors}
                ```

                actual:

                ```
                {actual_errors}
                ```

            "}
        );
    }
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
