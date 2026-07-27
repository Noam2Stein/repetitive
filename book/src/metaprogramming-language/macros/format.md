# `format!`

Creates a string using interpolation of values.

This currently only supports a small subset of [Rust formatting syntax]
(unsupported features raise an error):

- No positional parameters
- No named parameters (outside the first string literal)
- No formatting parameters
- No escaping `{` and `}`
- Only [`Display`] formatting

Types that support formatting are strings, integers and booleans.

## Example

```rust
repetitive! {
    $for N in [2, 3, 4] {
        $let VecN = format!("Vec{N}");

        pub struct $VecN;
    }
}
```

Expands to:

```rust
pub struct Vec2;
pub struct Vec3;
pub struct Vec4;
```

[Rust formatting syntax]: https://doc.rust-lang.org/std/fmt/index.html
[`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html
