# Tuples

A tuple is a fixed-size collection where each field may have a different type.
Tuples can be created using standard Rust tuple syntax `(a, b, ...)`.

## Supported Operations

- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Field access: `.0`, `.1`... `.<n-1>`

## Pasting

Tuples currently cannot be pasted.

## Example

```rust
repetitive! {
    $for (VecN, N) in [("Vec2", 2), ("Vec3", 3), ("Vec4", 4)] {
        $let elements = ["x", "y", "z", "w"][..N];
        
        pub struct $VecN {
            $for element in elements {
                pub $element: f32,
            }
        }
    }
}
```

Expands to:

```rust
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
```
