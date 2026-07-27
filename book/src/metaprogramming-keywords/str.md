# `$str`

Pastes a string value as a string literal, instead of as an identifier.

## Example

```rust
repetitive! {
    // Define a string value
    $let value = "The Return of the Curse of the Creature's Ghost";

    // Paste it as a string literal. If instead we wrote `$value`, it would have
    // been pasted as an identifier.
    $str(value)
}
```

Expands to:

```rust
"The Return of the Curse of the Creature's Ghost"
```

## Syntax

```rust
$str(<expression>)
```

See more about [expressions].

[expressions]: ../metaprogramming-language/expressions.md
