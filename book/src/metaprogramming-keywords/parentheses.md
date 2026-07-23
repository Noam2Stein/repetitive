# `$(...)`

Pastes the given expression. This is like a shortcut for writing
`$let var = ...;` then pasting with `$var`.

## Example

```rust
repetitive! {
    $for N in [2, 3, 4] {
        pub struct $(format!("Vec{N}"));
    }
}
```

Expands to:

```rust
pub struct Vec2;
pub struct Vec3;
pub struct Vec4;
```

## Syntax

```rust
$(<expression>)
```

See more about [expressions].

[expressions]: ../metaprogramming-language/expressions.md
