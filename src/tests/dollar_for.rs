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
