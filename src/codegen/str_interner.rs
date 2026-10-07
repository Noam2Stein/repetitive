use std::cell::UnsafeCell;

use crate::entrypoint::Context;

pub struct StrInterner<'storage>(UnsafeCell<Inner<'storage>>);

struct Inner<'storage> {
    interned_strs: Vec<InternedStr<'storage>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct StrId {
    index: u32,
}

struct InternedStr<'storage> {
    str: &'storage str,
    hash: u64,
}

impl<'storage> StrInterner<'storage> {
    pub fn new() -> Self {
        Self(UnsafeCell::new(Inner {
            interned_strs: Vec::new(),
        }))
    }

    #[must_use]
    pub fn intern(&self, str: &str, storage: &'storage Context) -> StrId {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let hash = hash(str);
        let matching_str_index = inner
            .interned_strs
            .iter()
            .position(|interned_str| interned_str.hash == hash && interned_str.str == str);

        if let Some(matching_str_index) = matching_str_index {
            StrId {
                index: matching_str_index as u32,
            }
        } else {
            let index = inner.interned_strs.len() as u32;

            inner.interned_strs.push(InternedStr {
                str: storage.mixed_arena.insert_str(str),
                hash,
            });

            StrId { index }
        }
    }

    #[must_use]
    pub fn restore(&self, str_id: StrId) -> &str {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        inner.interned_strs[str_id.index as usize].str
    }
}

fn hash(str: &str) -> u64 {
    // Based on `https://docs.rs/rustc-hash/latest/rustc_hash/index.html`.

    const K: u64 = 0xf1357aea2e62a9c5;

    let mut hash = 0u64;

    hash = hash.wrapping_add(str.len() as u64).wrapping_mul(K);
    for byte in str.bytes() {
        hash = hash.wrapping_add(byte as u64).wrapping_mul(K);
    }

    hash
}

#[cfg(test)]
mod tests {
    use crate::str_interner::StrInterner;

    #[test]
    fn test_correctness() {
        let input_strs = [
            "foo",
            "bar",
            "goo",
            "boo",
            "bar",
            "x",
            "y",
            "longlonglong",
            "y",
            "longlonglong",
            "foo",
            "goo",
            "boo",
            "bar",
            "x",
            "y",
            "longlonglong",
            "y",
            "long long long",
            "fooooooo baaaaaar",
        ];

        let str_interner = StrInterner::new();
        let mut ids = Vec::new();

        for str in input_strs {
            let str_id = str_interner.intern(str);
            ids.push(str_id);

            assert_eq!(str_interner.restore(str_id), str);

            for (other_str_id, other_str) in ids.iter().copied().zip(input_strs) {
                assert_eq!(str_id == other_str_id, str == other_str);
            }
        }
    }
}
