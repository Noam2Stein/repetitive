# Metaprogramming Keywords

By default, the `repetitive` macro simply emits whatever input tokens it
receives. For example:

```rust
repetitive! {
    pub struct Foo;
}
```

Expands to:

```rust
pub struct Foo;
```

All of the macro's features are gated behind the `$` token. The subchapters in
this section explore each supported feature gated behind `$`, ordered from most
to least useful.
