#![allow(unused)]

use repetitive::repetitive;

fn main() {
    let vec = Vec3 {
        x: 1.0,
        y: 2.0,
        z: 3.0,
    };

    println!("{:?}", vec.xz());
}

repetitive! {
    @for N in 2..=4 {
        @let VecN = @['Vec N];
        @let components = ['x, 'y, 'z, 'w][0..N];

        @let component_list = match N {
            2 => "`x` and `y`",
            3 => "`x`, `y` and `z`",
            4 => "`x`, `y`, `z` and `w`",
        };

        @let vector_fields = @{
            @for c in components {
                @c: f32,
            }
        };

        #[doc = @str["A vector with " N " components (" component_list ")"]]
        #[derive(Debug, Clone, Copy)]
        struct @VecN {
            @vector_fields
        }

        impl @VecN {
            @for x in components, y in components {
                @let component_list = @str["`" x "` and `" y "`"];

                #[doc = @str["Returns a vector with the " component_list " components of this vector."]]
                fn @[x y](self) -> Vec2 {
                    Vec2 {
                        x: self.@x,
                        y: self.@y,
                    }
                }
            }

            @for x in components, y in components, z in components {
                @let component_list = @str["`" x "`, `" y "` and `" z "`"];

                #[doc = @str["Returns a vector with the " component_list " components of this vector."]]
                fn @[x y z](self) -> Vec3 {
                    Vec3 {
                        x: self.@x,
                        y: self.@y,
                        z: self.@z,
                    }
                }
            }

            @for x in components, y in components, z in components, w in components {
                @let component_list = @str["`" x "`, `" y "`, `" z "` and `" w "`"];

                #[doc = @str["Returns a vector with the " component_list " components of this vector."]]
                fn @[x y z w](self) -> Vec4 {
                    Vec4 {
                        x: self.@x,
                        y: self.@y,
                        z: self.@z,
                        w: self.@w,
                    }
                }
            }
        }
    }
}
