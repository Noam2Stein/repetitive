# `iproduct!`

Creates an iterator over the “cartesian product” of iterators. This behaves like
the macro from the [`itertools`] crate.

This is equivalent to putting a for loop inside another for loop.

## Example

```rust
repetitive! {
    $for (x, y) in iproduct!([1, 2, 3], ["A", "B", "C"]) {
        println!("{}, {}", $x, $str(y));
    }
}
```

Expands to:

```rust
println!("{}, {}", 1, "A");
println!("{}, {}", 1, "B");
println!("{}, {}", 1, "C");
println!("{}, {}", 2, "A");
println!("{}, {}", 2, "B");
println!("{}, {}", 2, "C");
println!("{}, {}", 3, "A");
println!("{}, {}", 3, "B");
println!("{}, {}", 3, "C");
```

And is equivalent to:

```rust
repetitive! {
    $for x in [1, 2, 3] {
        $for y in ["A", "B", "C"] {
            println!("{}, {}", $x, $str(y));
        }
    }
}
```

[`itertools`]: https://docs.rs/itertools/latest/itertools/macro.iproduct.html
