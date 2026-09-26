# Strings

Strings are primarily used to represent identifiers. They can be created using
string literals and the [`format!`] macro.

## Supported Operations

- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Use in the [`format!`] macro

## Pasting

By default, strings are pasted as identifiers. String values currently cannot be
pasted as string literals.

## Example

```rust
repetitive! {
    $let foo = "FOO";
    
    pub const $foo: &str = "value";
}
```

Expands to:

```rust
pub const FOO: &str = "value";
```

[`format!`]: ../macros/format.md
