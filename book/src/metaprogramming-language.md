# Metaprogramming Language

When you write `$for <pattern> in <expression> { ... }`, that expression is not
standard Rust code. It is evaluated by the custom interpreter of the
`repetitive` macro.

`repetitive` implements a small, Rust-like language with simplified concepts
designed to be practical for metaprogramming. For example, while standard Rust
has distinct `&str` and `String` types, this language has a single unified
string type in order to minimize friction.

The subchapters in this section explore each supported feature of this language,
ordered from most to least useful.
