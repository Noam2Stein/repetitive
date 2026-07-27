# Vector Swizzle

This example starts with a set of vector types:

```rust
pub struct Vec2 {
    x: f32,
    y: f32,
}

pub struct Vec3 {
    x: f32,
    y: f32,
    z: f32,
}

pub struct Vec4 {
    x: f32,
    y: f32,
    z: f32,
    w: f32,
}
```

The goal is to define methods for all possible [vector swizzles].

Doing this using only declarative macros would require multiple helper macros
and recursion, that would result in unreadable code. Instead, use the
`repetitive` macro to handle repetition logic, and put the actual code in a
declarative macro:

```rust
macro_rules! define_swizzle_function {
    (
        $name:ident,
        $Output:ident,
        $OutputN:literal,
        $self_elements:literal,
        $($self_element:ident -> $output_element:ident),*
        $(,)?
    ) => {
        #[doc = concat!(
            "Returns a vector",
            $OutputN,
            " with the ",
            $self_elements,
            " elements of `self`.",
        )]
        pub fn $name(self) -> $Output {
            $Output { $($output_element: self.$self_element),* }
        }
    };
}

repetitive! {
    $for N in [2, 3, 4] {
        $let VecN = format!("Vec{N}");
        $let elements = ["x", "y", "z", "w"][..N];

        impl $VecN {
            $for (x, y) in iproduct!(elements, elements) {
                define_swizzle_function!(
                    $(format!("{x}{y}")),
                    Vec2,
                    2,
                    $str(format!("`{x}` and `{y}`")),
                    $x -> x,
                    $y -> y,
                );
            }

            $for (x, y, z) in iproduct!(elements, elements, elements) {
                define_swizzle_function!(
                    $(format!("{x}{y}{z}")),
                    Vec3,
                    3,
                    $str(format!("`{x}`, `{y}` and `{z}`")),
                    $x -> x,
                    $y -> y,
                    $z -> z,
                );
            }

            $for (x, y, z, w) in iproduct!(elements, elements, elements, elements) {
                define_swizzle_function!(
                    $(format!("{x}{y}{z}{w}")),
                    Vec4,
                    4,
                    $str(format!("`{x}`, `{y}`, `{z}` and `{w}`")),
                    $x -> x,
                    $y -> y,
                    $z -> z,
                    $w -> w,
                );
            }
        }
    }
}
```

A more complex goal is to add `set_{swizzle}` methods, which set specific
elements in the vector to the values of another vector.

This requires that each element only appears once. For example, `Vec4::set_zxy`
is valid, but `Vec4::set_zzy` is not.

This would be even harder using only declarative macros. Instead, use the same
approach as before:

```rust
macro_rules! define_set_swizzle_function {
    (
        $name:ident,
        $Other:ident,
        $other_elements:literal,
        $self_elements:literal,
        $($other_element:ident -> $self_element:ident),*
        $(,)?
    ) => {
        #[doc = concat!(
            "Sets the ",
            $self_elements,
            " elements of `self` to the ",
            $other_elements,
            " elements of `other`.",
        )]
        pub fn $name(&mut self) {
            $(self.$self_element = $other_element;)*
        }
    };
}

repetitive! {
    $for N in [2, 3, 4] {
        $let VecN = format!("Vec{N}");
        $let elements = ["x", "y", "z", "w"][..N];

        impl $VecN {
            $for (x, y) in iproduct!(elements, elements) {
                if x != y {
                    define_set_swizzle_function!(
                        $(format!("set_{x}{y}")),
                        Vec2,
                        "`x` and `y`",
                        $str(format!("`{x}` and `{y}`")),
                        x -> $x,
                        y -> $y,
                    );
                }
            }

            $for (x, y, z) in iproduct!(elements, elements, elements) {
                if x != y && x != z && y != z {
                    define_set_swizzle_function!(
                        $(format!("set_{x}{y}{z}")),
                        Vec3,
                        "`x`, `y` and `z`",
                        $str(format!("`{x}`, `{y}` and `{z}`")),
                        x -> $x,
                        y -> $y,
                        z -> $z,
                    );
                }
            }

            $for (x, y, z, w) in iproduct!(elements, elements, elements, elements) {
                if x != y && x != z && x != w && y != z && y != w && z != w {
                    define_set_swizzle_function!(
                        $(format!("set_{x}{y}{z}{w}")),
                        Vec4,
                        "`x`, `y`, `z` and `w`",
                        $str(format!("`{x}`, `{y}`, `{z}` and `{w}`")),
                        x -> $x,
                        y -> $y,
                        z -> $z,
                        w -> $w,
                    );
                }
            }
        }
    }
}
```

[vector swizzles]: https://en.wikipedia.org/wiki/Swizzling_(computer_graphics)
