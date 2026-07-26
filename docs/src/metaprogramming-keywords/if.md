# $if

A control flow construct that emits a block of tokens if a condition is true.
This also supports if else chains via `$else` and `$else if`.

## Example

```rust
repetitive! {
    $for Name in ["Foo", "Bar", "Goo"] {
        $if Name != "Goo" {
            #[derive(Debug)]
        }
        pub struct $Name;
    }
}
```

Expands to:

```rust
#[derive(Debug)]
pub struct Foo;

#[derive(Debug)]
pub struct Bar;

pub struct Goo;
```

And an if else chain:

```rust
repetitive! {
    $for Name in ["Foo", "Bar", "Goo"] {
        $if Name == "Foo" {
            #[derive(Debug, Clone, Copy)]
        } $else if Name == "Bar" {
            #[derive(Debug, Clone)]
        } $else {
            #[derive(Debug)]
        }
        pub struct $Name;
    }
}
```

Expands to:

```rust
#[derive(Debug, Clone, Copy)]
pub struct Foo;

#[derive(Debug, Clone)]
pub struct Bar;

#[derive(Debug)]
pub struct Goo;
```

## Syntax

```rust
$if <expression> {
    <tokens>
}

... then zero or more ...
$else if <expression> {
    <tokens>
}

... then optionally
$else {
    <tokens>
}
```

See more about [expressions].

`<tokens>` is the block of tokens that is emitted if the condition is true. It
can contain both normal Rust code and nested metaprogramming keywords (e.g.,
`$for` inside `$if`).

[expressions]: ../metaprogramming-language/expressions.md
