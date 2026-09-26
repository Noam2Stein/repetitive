# `$<identifier>`

Writing a variable name after `$` "pastes" that variable's value.

Pasting means taking an expression and turning it into Rust tokens. Here is the
default pasting behavior for common types:

- Integers turn into unsuffixed integer literals
- Strings turn into identifiers (an error is emitted if the string is an invalid
  identifier)

The pasting behavior of remaining types is found in the documentation of each
type.

In future versions, it may be supported to paste values as alternative kinds of
tokens. For example, pasting a string value as a string literal instead of as an
identifier, or pasting an integer value as a suffixed integer literal.
