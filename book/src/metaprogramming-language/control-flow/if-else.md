# If Else

If else chains are supported and use standard Rust syntax.

## Example

```rust
repetitive! {
    $for ROWS in [2, 3, 4] {
        $for COLUMNS in [2, 3, 4] {
            $let Mat = if ROWS == COLUMNS {
                format!("Mat{ROWS}")
            } else {
                format!("Mat{ROWS}x{COLUMNS}")
            };

            pub struct $Mat;
        }
    }
}
```

Expands to:

```rust
pub struct Mat2;
pub struct Mat2x3;
pub struct Mat2x4;
pub struct Mat3x2;
pub struct Mat3;
pub struct Mat3x4;
pub struct Mat4x2;
pub struct Mat4x3;
pub struct Mat4;
```
