//! Check that the crate compiles without `cfg(test)`.

use repetitive::repetitive;

repetitive! {
    $for N in [2, 3, 4] {
        struct $(format!("Vec{N}"));
    }
}

const _: () = {
    let _ = Vec2;
    let _ = Vec3;
    let _ = Vec4;
};
