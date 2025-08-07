# Repetitive

Rust macro for repetitive code generation which is easier to use than declarative macros,
or a combination of macro crates like `paste` and `seq_macro`.

## For Loop

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

## Concat

```rust
repetitive! {
    @for letter in ['A, 'B, 'C] {
        struct @['Struct letter];
    }
}
```

Generates:

```rust
struct StructA;
struct StructB;
struct StructC;
```

## Expressions

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

## If Statement

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