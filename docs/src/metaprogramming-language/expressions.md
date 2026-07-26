# Expressions

This section lists all supported expressions in the `repetitive` interpreted
language.

## Literals

Supported literals are:

- Integer literal (evaluates to [integer])
- String literal (evaluates to [string])
- `true` and `false` (evaluates to [bool])

## Array Constructor

This uses standard Rust array syntax `[value_1, value_2, ...]` or `[value; N]`,
and evaluates to a fixed-lengthed [array].

## Tuple Constructor

This uses standard Rust tuple syntax `(value_1, value_2, ...)`,
and evaluates to a [tuple].

## Range Constructor

This uses standard Rust range syntax, and evaluates to a [range].

## Unary Operator

Standard Rust operators are supported: `-` and `!`.

## Binary Operator

Standard Rust operators are supported: `&&`, `||`, `+`, `-`, `*`, `/`, `%`,
`<<`, `>>`, `&`, `|` and `^`. These use stanard Rust operator precedence.

## Parentheses

The `(...)` expression is used to specify operator precedence.

## If else chain

These behave like in standard Rust. The `else` can technically be removed, but
that forces the type to be `()` (which is useless).

## Match

Tjos ne

[integer]: data-types/integers.md
[string]: data-types/strings.md
[bool]: data-types/booleans.md
[array]: data-types/arrays.md
[tuple]: data-types/tuples.md
[range]: data-types/ranges.md
