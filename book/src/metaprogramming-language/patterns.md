# Patterns

This section lists all `repetitive` pattern kinds. These mostly follow standard
Rust syntax.

One major difference from standard Rust is that here, because metaprogramming
code is immediately evaluated, patterns are not required to be exhaustive (e.g.,
in `$match` and `$let`). An error is only emitted if an actually encountered
value matches no pattern.

- Binding `<identifier>`. This matches everything and defines a variable in the
  current scope. It is allowed to shadow variable names.

- Discard `_` (match everything and ignore value)

- Integer literal (match specific value)

- String literal (match specific value)

- `true` or `false` (match specific value)

- Array `[<pattern>, <pattern>, <pattern>]` (matches array of specific size)

- Tuple `(<pattern>, <pattern>, <pattern>)` (matches specific tuple kind)

- `<pattern> | <pattern> | ...` (matches any of the given patterns)
