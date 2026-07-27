# Arrays

An array is a collection where all elements are of the same type. Arrays can be
created using standard Rust array syntax `[a, b, ...]` or `[value; N]`.

Unlike in actual Rust, where there are separate types for fixed-size and
dynamically-sized arrays, here all arrays are dynamically sized.

## Supported Operations

- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Use as iterator
- Indexing (by integers and ranges)

## Pasting

Arrays currently cannot be pasted.

## Example

```rust
repetitive! {
    // Create an array
    $let elements = ["x", "y", "z", "w"];

    $for (VecN, N) in [("Vec2", 2), ("Vec3", 3), ("Vec4", 4)] {
        // Get a slice of that array
        $let n_elements = elements[..N];
        
        pub struct $VecN {
            // Iterate over the slice
            $for element in n_elements {
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
