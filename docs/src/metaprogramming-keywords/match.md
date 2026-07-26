# $match

A control flow construct that emits a selected block of tokens from a list of
predicates.

## Behavior

Note that unlike Rust `match` expressions, here it is allowed not to cover all
possible values of a type. An error only occurs if an actually encountered value
matches none of the predicates.

For example, the example below only covers three specific string values. Even
though a string could theoretically match none of the predicates, the code
compiles because all encountered values do match.

## Example

```rust
repetitive! {
    $for Name in ["Foo", "Bar", "Goo"] {
        $match Name {
            "Foo" => {
                /// This word is commonly the first one used in Rust examples.
            }
            "Bar" => {
                /// This word is commonly the second one used in Rust examples.
            }
            "Goo" => {
                /// This word is rarely used in Rust examples.
            }
        }
        pub struct $Name;
    }
}
```

Expands to:

```rust
/// This word is commonly the first one used in Rust examples.
pub struct Foo;

/// This word is commonly the second one used in Rust examples.
pub struct Bar;

/// This word is rarely used in Rust examples.
pub struct Goo;
```

## Syntax

```rust
$match <expression> {
    ... then zero or more ...
    <pattern> => {
        <tokens>
    }
}
```

See more about [expressions] and [patterns].

`<tokens>` is the block of tokens that is emitted if its predicate is chosen. It
can contain both normal Rust code and nested metaprogramming keywords (e.g.,
`$if` inside `$match`).

[expressions]: ../metaprogramming-language/expressions.md
[patterns]: ../metaprogramming-language/patterns.md
