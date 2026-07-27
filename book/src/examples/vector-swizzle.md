# Vector Swizzle

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
            " elements of `self`",
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
            $for x in elements {
                $for y in elements {
                    define_swizzle_function!(
                        $(format!("{x}{y}")),
                        Vec2,
                        2,
                        $str(format!("`{x}` and `{y}`")),
                        $x -> x,
                        $y -> y,
                    );

                    $for z in elements {
                        define_swizzle_function!(
                            $(format!("{x}{y}{z}")),
                            Vec3,
                            3,
                            $str(format!("`{x}`, `{y}` and `{z}`")),
                            $x -> x,
                            $y -> y,
                            $z -> z,
                        );

                        $for w in elements {
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
        }
    }
}
```
