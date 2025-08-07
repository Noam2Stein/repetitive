//! `repetitive!` macro crate.

#![deny(missing_docs)]

mod ctx;
mod ctx_parse;
mod fragment_expr;
mod fragment_outer;
mod fragment_value;
mod keyword;
mod name;
mod namespace;
mod ops;
mod paste;
mod pattern;
mod tokens;
use ctx::*;
use ctx_parse::*;
use fragment_expr::*;
use fragment_outer::*;
use fragment_value::*;
use keyword::*;
use name::*;
use namespace::*;
use ops::*;
use paste::*;
use pattern::*;
use tokens::*;

/// The macro!
///
/// This macro emits the exact same code it gets,
/// except for when you use fragments through the `@` prefix.
///
/// # Fragments
///
/// ### For Loop
///
/// Emits the body per value of the specified list.
///
/// Syntax:
/// `@for <pattern> in <list> { <code> }`
///
/// Also accepts multiple patterns separated by commas.
///
/// ```rust
/// @for a in [...], b in [...] {
///     <code>
/// }
/// ```
///
/// Acts like:
/// ```rust
/// @for a in [...] {
///     @for b in [...] {
///         <code>
///     }
/// }
/// ```
///
/// Example:
/// ```rust
/// @for Color in ['Red, 'Green, 'Blue] {
///     struct @Color;
/// }
/// ```
///
/// ### Concat
///
/// Concats multiple identifiers into one.
///
/// Syntax:
/// `@[<expr> <expr>...]`
///
/// Also accepts `@str[...]` which emits a string literal instead of an identifier.
///
/// Example:
/// ```rust
/// @for N in 2..=4 {
///     struct @['Vec N];
/// }
/// ```
///
/// ### Let Statement
///
/// Binds a value to a name.
///
/// Syntax:
/// `@let <pattern> = <expr>;`
///
/// Example:
/// ```rust
/// @for N in 2..=4 {
///     @let VecN = @['Vec N];
///
///     struct @VecN;
/// }
/// ```
///
/// ### If Statement
///
/// Emits the body if the condition is true.
///
/// Syntax:
/// `@if <expr> { <code> }`
///
/// Also accepts `@if <expr> { <code> } else { <code> }`.
///
/// Example:
/// ```rust
/// @for a in 0..5, b in 0..5 {
///     @if a != b {
///         println!("{} != {}", a, b);
///     }
/// }
/// ```
///
/// ### Expression
///
/// Emits the result of an expression.
///
/// Syntax:
/// `@(<expr>)`
///
/// Example:
/// ```rust
/// @for idx in 0..5 {
///     example::<@(idx + 1)>();
/// }
/// ```
///
/// ### Tokens
///
/// Stores tokens as a fragment value.
///
/// Syntax:
/// `@{ <token> }`
///
/// Example:
/// ```rust
/// @let tokens = @{ @frag + 1 };
///
/// @for frag in [1, 2, 3] {
///     println!("{}", @tokens);
/// }
/// ```
///
/// # Expressions
///
/// Expressions can evaluate to these types:
/// - bool lit,
/// - int lit,
/// - float lit,
/// - char lit,
/// - string lit,
/// - ident,
/// - list,
/// - tokenstream.
///
/// An expression can be:
/// - a literal: `1`, `1.0`, `'a'`, `"hello"`, `true`, `false`,
/// - an "ident literal": `'Ident`,
/// - a list: `[... <expr> ...]`,
/// - a name: `name`,
/// - a fragment: `@{...}`,
/// - if else: `@if <expr> { <expr> } else { <expr> }`,
/// - an operator: `<expr> + <expr>`, `!<expr>`,
/// - a method call: `<expr>.<method>(<expr>)`.
///
/// These operators are supported:
/// - add `+`, sub `-`, mul `*`, div `/`, rem `%`, neg `-`,
/// - bitand `&`, bitor `|`, bitxor `^`, shl `<<`, shr `>>`,
/// - eq `==`, ne `!=`, lt `<`, le `<=`, gt `>`, ge `>=`,
/// - and `&&`, or `||`, not `!`,
/// - range `..`, range_inclusive `..=`.
///
/// These methods are supported:
/// - operator methods,
/// - `.len()`,
/// - `.index(<idx>)`,
/// - `.enumerate()`,
/// - `.zip(<list>)`,
/// - `.chain(<list>)`,
/// - `.concat_ident()`,
/// - `.concat_string()`.
#[proc_macro]
pub fn repetitive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    main::repetitive(input)
}

mod main {
    use proc_macro2::TokenStream;
    use quote::quote;
    use string_interner::DefaultStringInterner;

    use super::*;

    pub fn repetitive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
        let mut ctx = Context {
            interner: DefaultStringInterner::new(),
            warnings: Vec::new(),
        };

        let tokens = Tokens::ctx_parse.ctx_parse2(input.into(), &mut ctx);
        let result = 'result: {
            Ok(match tokens {
                Ok(tokens) => {
                    let mut output = TokenStream::new();

                    match tokens.paste(&mut output, &mut ctx, &mut Namespace::new()) {
                        Ok(()) => output,
                        Err(err) => break 'result Err(err),
                    }
                }
                Err(err) => break 'result Err(err),
            })
        };

        let result = match result {
            Ok(output) => output,
            Err(err) => err.to_compile_error().into(),
        };

        let warnings = ctx
            .warnings
            .into_iter()
            .map(|warning| warning.into_compile_error());

        quote! {
            #result

            #(#warnings)*
        }
        .into()
    }
}
