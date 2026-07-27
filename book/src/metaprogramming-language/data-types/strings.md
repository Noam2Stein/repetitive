# Strings

Strings are used to represent identifiers and string literals. They can be
created using string literals and the [`format!`] macro.

## Supported Operations

- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`

## Pasting

By default, strings are pasted as identifiers. String values can be pasted as
string literals via [`$str`].

## Example

```rust
repetitive! {
    $let string_1 = "ABCD";
    $let string_2 = format!("{string_1}, ABCD");
    
    pub const $string_1: &str = $str(string_2);
}
```

Expands to:

```rust
pub const ABCD: &str = "ABCD, ABCD";
```

[`format!`]: ../macros/format.md
[`$str`]: ../../metaprogramming-keywords/str.md
