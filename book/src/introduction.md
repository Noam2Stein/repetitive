# Introduction

`repetitive` is a metaprogramming macro with control flow syntax.

Rust declarative macros are great for most metaprogramming, and result in clean
and readable code. However, in some rare cases, code requires repetition logic
that is too complicated for declarative macros.

The `repetitive` macro solves this by introducing metaprogramming control flow
constructs, like [`$for`] for repetition, [`$if`] for conditions, and [`$let`]
for reusing values, as well as identifier concatenation and other useful
features.

This book provides complete documentation for every supported feature. Since
there are a lot of features, having a dedicated book is preferable to cramming
all features into the documentation of a single macro.

[`$for`]: metaprogramming-keywords/for.md
[`$if`]: metaprogramming-keywords/if.md
[`$let`]: metaprogramming-keywords/let.md
