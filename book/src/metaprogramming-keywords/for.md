# `$for`

A control flow construct that repeats a block of tokens for each value in a
collection.

The collection must be an [iterator], otherwise an error is emitted.

## Example

```rust
repetitive! {
    $for Name in ["Foo", "Bar", "Goo"] {
        pub struct $Name;
    }
}
```

Expands to:

```rust
pub struct Foo;
pub struct Bar;
pub struct Goo;
```

## Syntax

```rust
$for <pattern> in <expression> {
    <tokens>
}
```

See more about [patterns] and [expressions].

`<tokens>` is the block of tokens repeated for each iteration. It can contain
both normal Rust code and nested metaprogramming keywords (e.g., `$for` inside
another `$for`).

[iterator]: ../metaprogramming-language/iterators.md
[patterns]: ../metaprogramming-language/patterns.md
[expressions]: ../metaprogramming-language/expressions.md
