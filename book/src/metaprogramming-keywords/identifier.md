# $&lt;identifier&gt;

Writing a variable name after `$` "pastes" that variable's value.

Pasting means taking an expression and turning it into Rust tokens. Here is the
default pasting behavior for common types:

- Integers turn into unsuffixed integer literals
- Strings turn into identifiers

To paste values into other kinds of tokens, use these special metaprogramming
keywords:

- [`$str`]: Paste a string value as a string literal (instead of an identifier)

[`$str`]: str.md
