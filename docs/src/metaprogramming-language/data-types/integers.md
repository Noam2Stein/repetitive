# Integers

Integers are useful for ranges, indexing, and performing math. They can be
created using standard integer literals.

## Representation

All integers are internally represented as [`i32`].

- Arithmetic operations panic on overflow.
- Operations whose behavior depends on specific integer widths are not
  supported.
- Indexing arrays accepts [`i32`] (unlike standard Rust, which requires
  [`usize`]).

These semantics are chosen for simplicity and are intended to be compatible
with standard Rust behavior, with the exception of array indexing.

## Supported Operations

- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Unary operators: `-`
- Binary operators: `+`, `-`, `*`, `/`, `%`
- [`Display`] formatting

## Pasting

By default, integers are pasted as unsuffixed integer literals. Pasting integers
with explicit type suffixes is currently not supported.

## Example

```rust
repetitive! {
    $let x = 9 + 10;
    
    // Pasted integers are unsuffixed, and fit into any integer type
    pub const A: i32 = $x;
    pub const B: i64 = $x;
}
```

[`i32`]: https://doc.rust-lang.org/std/primitive.i32.html
[`usize`]: https://doc.rust-lang.org/std/primitive.usize.html
