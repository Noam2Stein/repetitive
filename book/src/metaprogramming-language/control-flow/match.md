# Match

Match expressions are supported and use standard Rust syntax.

## Behavior

Note that unlike Rust `match` expressions, here it is allowed not to cover all
possible values of a type. An error only occurs if an actually encountered value
matches none of the predicates.

For example, the example below only covers three specific string values. Even
though a string could theoretically match none of the predicates, the code
compiles because all encountered values do match.

## Example

```rust
repetitive! {
    $for T in ["f32", "f64", "i32", "u32"] {
        $let Prefix = match T {
            "f32" => "F",
            "f64" => "D",
            "i32" => "I",
            "u32" => "U",
        };
        $let Vec3 = format!("{Prefix}Vec3");

        pub struct $Vec3 {
            pub x: $T,
            pub y: $T,
            pub z: $T,
        }
    }
}
```

Expands to:

```rust
pub struct FVec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

pub struct DVec3 {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

pub struct IVec3 {
    pub x: i32,
    pub y: i32,
    pub z: i32,
}

pub struct UVec3 {
    pub x: u32,
    pub y: u32,
    pub z: u32,
}
```
