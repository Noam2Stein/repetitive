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
    @for N in [2, 3, 4] {
        @let VecN = @['Vec N];
        @let components = ['x, 'y, 'z, 'w][0..N];

        #[derive(Debug, Clone, Copy)]
        struct @VecN {
            @for c in @components {
                @c: f32,
            }
        }

        impl @VecN {
            @for x in @components, y in @components {
                fn @[x y](self) -> Vec2 {
                    Vec2 {
                        x: self.@x,
                        y: self.@y,
                    }
                }
            }

            @for x in @components, y in @components, z in @components {
                fn @[x y z](self) -> Vec3 {
                    Vec3 {
                        x: self.@x,
                        y: self.@y,
                        z: self.@z,
                    }
                }
            }

            @for x in @components, y in @components, z in @components, w in @components {
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
