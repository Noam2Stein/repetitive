# Expressions

This section lists all `repetitive` expressions kinds. These fully follow
standard Rust [expression precedence].

- Integer literal

- String literal

- `true` or `false`

- Variable name `<identifier>`

- Macro call `some_macro!(...)` (see [supported macros])

- Array constructor `[a, b, ...]`

- Array repeat constructor `[value; N]`

- Tuple constructor `(a, b, ...)`

- Unary operators `-`, `!`

- Binary operators `&&`, `||`, `+`, `-`, `*`, `/`, `%`, `<<` `>>`, `&`, `|`, `^`

- Range operators `a..b`, `a..=b`, `a..`, `..b`,
`..=b`, `..`

- Parentheses `(...)` (used to control expression precedence)

- Field access `value.field`

- If else chain

- Match

[expression precedence]: https://doc.rust-lang.org/reference/expressions.html#expression-precedence
[supported macros]: macros.md
