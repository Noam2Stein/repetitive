//! A metaprogramming macro with control flow syntax.
//!
//! Rust declarative macros are great for most metaprogramming, and result in clean
//! and readable code. However, in some rare cases, code requires repetition logic
//! that is too complicated for declarative macros.
//!
//! The `repetitive` macro solves this by introducing metaprogramming control flow
//! constructs, like [`$for`] for repetition, [`$if`] for conditions, and [`$let`]
//! for reusing values, as well as identifier concatenation and other useful
//! features.
//!
//! Think of `repetitive` as not trying to *replace* declarative macros, but as a
//! complementary tool handling repetition logic. This is basically a combination of
//! [`paste`] and [`seq-macro`] with improved ergonomics.
//!
//! For detailed documentation, see the [repetitive Documentation].
//!
//! # Example
//!
//! This example shows the intended usage of the macro. It is not recommended to use
//! this macro to fully replace declarative macros, because that would probably
//! result in messy unreadable code with ugly [deep nesting].
//!
//! Let's say we have a vector3 type:
//!
//! ```rust
//! #[derive(Debug, Clone, Copy)]
//! struct Vec3 {
//!     x: f32,
//!     y: f32,
//!     z: f32,
//! }
//! ```
//!
//! Our goal is to define swizzle functions (for example `vec3.zxy()` and
//! `vec3.xxy()`), and functions that set certain swizzle combinations (for example
//! `vec3.set_xzy(another_vec3)`, but not `vec3.set_xxy(another_vec3)` because that
//! does not make sense).
//!
//! Doing this using only declarative macros would result in unreadable code, as it
//! requires multiple layers of repetition and conditional logic (for `set_...`
//! functions).
//!
//! Instead, to make the code as readable as possible, we will keep the function
//! definitions in declarative macros, then invoke those macros from a
//! `repetitive` block that handles the repetition logic.
//!
//! ```rust
//! macro_rules! define_swizzle_function {
//!     ($f:ident, $x:ident, $y:ident, $z:ident) => {
//!         pub fn $f(self) -> Vec3 {
//!             Vec3 {
//!                 x: self.$x,
//!                 y: self.$y,
//!                 z: self.$z,
//!             }
//!         }
//!     };
//! }
//!
//! macro_rules! define_set_swizzle_function {
//!     ($f:ident, $x:ident, $y:ident, $z:ident) => {
//!         pub fn $f(&mut self, other: Vec3) {
//!             self.$x = other.x;
//!             self.$y = other.y;
//!             self.$z = other.z;
//!         }
//!     };
//! }
//!
//! repetitive! {
//!     $let elements = ["x", "y", "z"];
//!
//!     impl Vec3 {
//!         $for (x, y, z) in iproduct!(elements, elements, elements) {
//!             define_swizzle_function!(
//!                 $(format!("{x}{y}{z}")),
//!                 $x,
//!                 $y,
//!                 $z
//!             );
//!             $if x != y && x != z && y != z {
//!                 define_set_swizzle_function!(
//!                     $(format!("set_{x}{y}{z}")),
//!                     $x,
//!                     $y,
//!                     $z
//!                 );
//!             }
//!         }
//!     }
//! }
//! ```
//!
//! # Compile times and internal architecture
//!
//! Internally, `repetitive` uses a small interpreter that verifies and expands the
//! code. Unlike [`crabtime`] and similar crates, no external tool is invoked.
//!
//! Keeping this interpreter small and fast to compile is a high-priority goal.
//! Depending on the size of your crate, `repetitive` may be either insignificant
//! or too heavy in terms of compile times.
//!
//! For example, the [`ggmath`] crate defines swizzle functions similar to the
//! example above, but avoids defining them using `repetitive` in order to keep
//! compile times as short as possible. Instead, it manually invokes all swizzle
//! combinations, then uses `repetitive` in tests to make sure the manual code
//! is correct.
//!
//! [`$for`]: TODO
//! [`$if`]: TODO
//! [`$let`]: TODO
//! [`paste`]: https://crates.io/crates/paste
//! [`seq-macro`]: https://crates.io/crates/seq-macro
//! [repetitive Documentation]: TODO
//! [deep nesting]: https://www.youtube.com/watch?v=CFRhGnuXG-4
//! [`crabtime`]: https://crates.io/crates/crabtime
//! [`ggmath`]: https://crates.io/crates/ggmath

#![forbid(missing_docs)]

use proc_macro2::TokenStream;

mod chunked_vec;
mod error;
mod executor;
mod ident;
mod instruction;

#[cfg(test)]
mod tests;

/// A metaprogramming macro with control flow syntax.
///
/// For detailed documentation, see the [repetitive Documentation].
///
/// [repetitive Documentation]: TODO
#[proc_macro]
pub fn repetitive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    repetitive_proc_macro2(input.into()).into()
}

fn repetitive_proc_macro2(input: TokenStream) -> TokenStream {
    let _ = input;
    todo!()
}
