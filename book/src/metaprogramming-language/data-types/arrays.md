# Arrays

An array is a collection where all elements are of the same type. Arrays can be
created using standard Rust array syntax `[a, b, ...]` or `[value; N]`.

Like in standard Rust, there are two kinds of arrays:

- Fixed-size array (the result of array constructors)
- Dynamically-sized slice (the result of indexing by ranges)

## Supported Operations

- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Use as iterator
- Indexing (by integers and ranges)

## Pasting

Arrays currently cannot be pasted.

## Example

```rust
repetitive! {
    // This is a fixed-size array
    $let elements = ["x", "y", "z", "w"];

    $for N in [2, 3, 4] {
        $let VecN = format!("Vec{N}");

        // This is a dynamically-sized slice
        $let n_elements = elements[..N];
        
        pub struct $VecN {
            // You can iterate over slices
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
