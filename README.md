# Repetitive

Rust macro for writing repetitive code in a simpler,
more readable and powerful way than declarative macros.

## Repetition

Repetition is done with the `@for` keyword.
The body of the for-loop is emitted for each value in the list.

```rust
repetitive! {
    @for name in ['StructA, 'StructB, 'StructC] {
        struct @name;
    }
}
```

Generates:

```rust
struct StructA;
struct StructB;
struct StructC;
```

## String/Identifier Concatenation

Concatenation is done with `@["string" "string"]` syntax.

```rust
repetitive! {
    @for letter in ['A, 'B, 'C] {
        #[doc = @str["Letter " letter "!"]]
        struct @['Struct letter];
    }
}
```

Generates:

```rust
/// Letter A!
struct StructA;
/// Letter B!
struct StructB;
/// Letter C!
struct StructC;
```

## Meta Variables

The `@let` keyword is used to store values in the macro context to make the code more readable.

Meta expressions support alot of useful operators and methods.

```rust
repetitive! {
    @for N in 2..=4 {
        @let VecN = @['Vec N];
        @let components = ['x, 'y, 'z, 'w][0..N];

        struct @VecN {
            @for c in @components {
                @c: f32,
            }
        }
    }
}
```

Generates:

```rust
struct Vec2 {
    x: f32,
    y: f32,
}
struct Vec3 {
    x: f32,
    y: f32,
    z: f32,
}
struct Vec4 {
    x: f32,
    y: f32,
    z: f32,
    w: f32,
}
```

## Conditions

Conditions are done with the `@if` keyword.

```rust
repetitive! {
    trait Even {}
    trait Odd {}

    @for i in 0..4 {
        @let NumberI = @['Number i];

        struct @NumberI;

        @if i % 2 == 0 {
            impl Even for @NumberI {}
        } else {
            impl Odd for @NumberI {}
        }
    }
}
```

Generates:

```rust
trait Even {}
trait Odd {}

struct Number0;
struct Number1;
struct Number2;
struct Number3;

impl Even for Number0 {}
impl Odd for Number1 {}
impl Even for Number2 {}
impl Odd for Number3 {}
```
