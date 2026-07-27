# Compile Times

Internally, `repetitive` uses a small interpreter that verifies and expands the
code. Unlike [`crabtime`] and similar crates, no external tool is invoked.

Keeping this interpreter small and fast to compile is a high-priority goal.
Depending on the size of your crate, `repetitive` may be either insignificant
or too heavy in terms of compile times.

For example, the [`ggmath`] crate defines swizzle functions similar to the
example above, but avoids defining them using `repetitive` in order to keep
compile times as short as possible. Instead, it manually invokes all swizzle
combinations, then uses `repetitive` in tests to make sure the manual code
is correct.

[`crabtime`]: https://crates.io/crates/crabtime
[`ggmath`]: https://crates.io/crates/ggmath
