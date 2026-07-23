# `$let`

A construct that sets bindings to an expression.

## Example

```rust
repetitive! {
    $for N in [2, 3, 4] {
        $let VecN = format!("Vec{N}");
        $let elements = ["x", "y", "z", "w"][..N];
        
        pub struct $VecN {
            $for element in elements {
                pub $element: f32,
            }
        }
    }
}
```

Expands to:

```rust
pub struct Vec2 {
    pub x: f32,
    pub y: f32,
}

pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

pub struct Vec4 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub w: f32,
}
```

## Syntax

```rust
$let <pattern> = <expression>;
```

See more about [patterns] and [expressions].

## Scope

Bindings defined via `$let` are scoped to the delimiters the `$let` statement is
in. Here are some examples:

```rust
repetitive! {
    $let <name> = <expression>;
    // Scope starts here

    // Scope ends here
}

repetitive! {
    $for <pattern> in <expression> {
        $let <name> = <expression>;
        // Scope starts here

        // Scope ends here
    }

    // Code here cannot access `<name>`
}

repetitive! {
    $for <pattern> in <expression> {
        // These braces are the scope of `<name>`, even though they are part of
        // quoted code
        {
            $let <name> = <expression>;
            // Scope starts here

            // Scope ends here
        }
        
        // Code here cannot access `<name>`
    }
}
```

[patterns]: ../metaprogramming-language/patterns.md
[expressions]: ../metaprogramming-language/expressions.md
