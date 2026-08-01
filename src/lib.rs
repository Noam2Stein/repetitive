//! A metaprogramming macro with control flow syntax.
//!
//! Rust declarative macros are great for most metaprogramming, and result in
//! clean and readable code. However, in some rare cases, code requires
//! repetition logic that is too complicated for declarative macros.
//!
//! The `repetitive` macro solves this by introducing metaprogramming control
//! flow constructs, like [`$for`] for repetition, [`$if`] for conditions, and
//! [`$let`] for reusing values, as well as identifier concatenation and other
//! useful features.
//!
//! Think of `repetitive` as not trying to *replace* declarative macros, but as
//! a complementary tool handling repetition logic. It is basically a
//! combination of [`paste`] and [`seq-macro`] with improved ergonomics.
//!
//! For detailed documentation, see [repetitive Documentation].
//!
//! # Example
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
//! Our goal is to define swizzle methods (for example `vec3.zxy()` and
//! `vec3.xxy()`), and methods that set swizzle combinations that have no
//! repeated elements (for example `vec3.set_xzy(another_vec3)`, but not
//! `vec3.set_xxy(another_vec3)`).
//!
//! Doing this using only declarative macros would result in unreadable code, as
//! it requires multiple layers of repetition and conditional logic (for
//! `set_...` methods).
//!
//! Instead, to make the code as readable as possible, we will keep the method
//! definitions in declarative macros, then invoke those macros from a
//! `repetitive` block that handles the repetition logic:
//!
//! ```rust
//! macro_rules! define_swizzle_method {
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
//! macro_rules! define_set_swizzle_method {
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
//!             $let f = format!("{x}{y}{z}");
//!             define_swizzle_method!($f, $x, $y, $z);
//!
//!             $if x != y && x != z && y != z {
//!                 $let f = format!("set_{x}{y}{z}");
//!                 define_set_swizzle_method!($f, $x, $y, $z);
//!             }
//!         }
//!     }
//! }
//! ```
//!
//! More examples can be found in the
//! [examples directory](https://github.com/noam2stein/repetitive/examples/).
//!
//! # Compile times and internal architecture
//!
//! Internally, `repetitive` uses a small interpreter that verifies and
//! expands the code. Unlike [`crabtime`] and similar crates, no external tool
//! is invoked.
//!
//! Keeping this interpreter small and fast to compile is a high-priority goal.
//! Depending on the size of your crate, `repetitive` may be either
//! insignificant or too heavy in terms of compile times.
//!
//! For example, the [`ggmath`] crate defines swizzle methods similar to the
//! example above, but avoids adding `repetitive` as a dependency in order to
//! keep compile times as short as possible. Instead, it manually invokes all
//! swizzle combinations, and uses `repetitive` in tests to make sure the
//! manual code is correct.
//!
//! [`$for`]: TODO
//! [`$if`]: TODO
//! [`$let`]: TODO
//! [`paste`]: https://crates.io/crates/paste
//! [`seq-macro`]: https://crates.io/crates/seq-macro
//! [repetitive Documentation]: TODO
//! [`crabtime`]: https://crates.io/crates/crabtime
//! [`ggmath`]: https://crates.io/crates/ggmath

#![forbid(missing_docs)]

use crate::proc_macro12::TokenStream;

/// Reexports the items from `proc_macro2` if `cfg(test)` is active, or from
/// `proc_macro` if not.
///
/// `proc_macro2` must be used when testing, since currently `proc_macro` only
/// supports invokations from actual proc-macros. When not testing,
/// `proc_macro2` is not used in order to minimize compile times.
mod proc_macro12 {
    #[cfg(not(test))]
    pub use proc_macro::*;
    #[cfg(test)]
    pub use proc_macro2::*;
}

mod error;
mod executor;
mod ident;
mod ident_interner;
mod instruction;
mod reserve;

#[cfg(test)]
mod tests;

/// A metaprogramming macro with control flow syntax.
///
/// For detailed documentation, see [repetitive Documentation].
///
/// [repetitive Documentation]: TODO
#[proc_macro]
pub fn repetitive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    cfg_select! {
        test => repetitive_impl(input.into()).into(),
        not(test) => repetitive_impl(input),
    }
}

fn repetitive_impl(input: TokenStream) -> TokenStream {
    let _ = input;
    todo!()
}
