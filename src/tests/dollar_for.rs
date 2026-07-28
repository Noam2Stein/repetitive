use crate::tests::utils::assert_expansion_eq;

#[test]
fn test_basic() {
    assert_expansion_eq!(
        repetitive! {
            $for Name in ["Foo", "Bar", "Goo"] {
                pub struct $Name;
            }
        },
        {
            pub struct Foo;
            pub struct Bar;
            pub struct Goo;
        }
    );
}

#[test]
fn test_tuple_pattern() {
    assert_expansion_eq!(
        repetitive! {
            $for (Name, N) in [("Foo", 1), ("Bar", 2), ("Goo", 3)] {
                pub struct $Name([i32; $N]);
            }
        },
        {
            pub struct Foo;
            pub struct Bar;
            pub struct Goo;
        }
    );
}

#[test]
fn test_non_binding_pattern() {
    assert_expansion_eq!(
        repetitive! {
            $for (Name, 1 | 2 | 3 | 5, _) in [("Foo", 1, 1), ("Bar", 2, 5), ("Goo", 3, 0)] {
                pub struct $Name([i32; $N]);
            }
        },
        {
            pub struct Foo;
            pub struct Bar;
            pub struct Goo;
        }
    );
}

#[test]
fn test_scope() {
    assert_expansion_eq!(
        repetitive! {
            $for X in ["Foo", "Bar", "Goo"] {
                pub struct $X;
            }

            const _: i32 = $X;
        },
        {
            compile_error!("cannot find value `X` in this scope");
        }
    );
}

#[test]
fn test_shadowing() {
    assert_expansion_eq!(
        repetitive! {
            $let X = 5;

            $for X in ["Foo", "Bar", "Goo"] {
                pub struct $X;
            }

            const _: i32 = $X;
        },
        {
            pub struct Foo;
            pub struct Bar;
            pub struct Goo;

            const _: i32 = 5;
        }
    );
}

#[test]
fn test_not_iterator() {
    assert_expansion_eq!(
        repetitive! {
            $for X in "Foo" {
                pub struct $X;
            }
        },
        {
            compile_error!("`str` is not an iterator");
        }
    );
}
