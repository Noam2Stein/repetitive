# Repetitive

Rust macro for writing repetitive code in a simpler,
more readable, and more powerful way than declarative macros.

Features:
- Repetition with `@for`
- String/Identifier Concatenation with `@["string" "string"]`
- Meta Variables with `@let`
- Conditions with `@if` and `@match`

## Repetition

Repetition is done with the `@for` keyword.
The body is repeated for each value in the given list.

```rust
repetitive! {
    @for name in ['StructA, 'StructB, 'StructC] {
        struct @name;
    }
}
```

Expands to:

```rust
struct StructA;
struct StructB;
struct StructC;
```

## String/Identifier Concatenation

Concatenation is done using `@["a" "b"]` syntax.

```rust
repetitive! {
    @for letter in ['A, 'B, 'C] {
        #[doc = @str["Letter " letter "!"]]
        struct @['Struct letter];
    }
}
```

Expands to:

```rust
/// Letter A!
struct StructA;
/// Letter B!
struct StructB;
/// Letter C!
struct StructC;
```

## Meta Variables

Use `@let` to define variables in the macro context to improve readability.

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

Expands to:

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

Conditions are done using `@if`.

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

## Operators And Methods

Macro context expressions support operators and methods.

```rust
repetitive! {
    @for N in [1, 2, 3] {
        @let crazy = (N * 2).min(5);

        const _: u32 = @crazy;
    }
}
```

Expands to:

```rust
const _: u32 = 2;
const _: u32 = 4;
const _: u32 = 5;
```
