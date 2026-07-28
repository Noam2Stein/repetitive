use crate::tests::utils::assert_tokenstream_eq;

mod utils;

#[test]
fn test_for() {
    assert_tokenstream_eq!(
        repetitive! {
            $for Name in ["Foo", "Bar", "Goo"] {
                pub struct $Name;
            }
        },
        {
            pub struct Foo;
            pub struct Bar;
            pub struct Goo;
        }
    );
}

#[test]
fn test_let() {
    assert_tokenstream_eq!(
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
        },
        {
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
        }
    );

    assert_tokenstream_eq!(
        repetitive! {
            $let x = "FirstX";
            pub struct $x;

            $let y = "FirstY";
            pub struct $y;

            $let x = "SecondX";
            pub struct $x;

            $let y = "SecondY";
            const _: () = {
                $let y = "ThirdY";
                pub struct $y;
            };

            pub struct $y;
        },
        {
            pub struct FirstX;
            pub struct FirstY;
            pub struct SecondX;
            pub struct ThirdY;
            pub struct SecondY;
        }
    );
}
