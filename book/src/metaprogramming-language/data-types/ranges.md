# Ranges

Ranges can be created using standard Rust range syntax:

- `start..end`
- `start..=last`
- `start..`
- `..end`
- `..=last`
- `..`

## Supported Operations

- Comparison operators: `==`, `!=`
- Field access: `.start`, `.end`, `.last`

For ranges of integers:

- Use as iterator (for ranges with a start)
- Use as array index

## Pasting

Ranges currently cannot be pasted.
