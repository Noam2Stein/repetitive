use std::cell::UnsafeCell;

use crate::arena::StrArena;

pub struct StrInterner {
    arena: StrArena,
    strs: UnsafeCell<Vec<StrMetadata>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct StrId {
    index: u32,
}

struct StrMetadata {
    /// # Safety
    ///
    /// This pointer must be valid as `&str`.
    ptr: *const str,
    hash: u64,
}

impl StrInterner {
    pub fn new() -> Self {
        Self {
            arena: StrArena::new(),
            strs: UnsafeCell::new(Vec::with_capacity(100)),
        }
    }

    #[must_use]
    pub fn intern(&self, str: &str) -> StrId {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let strs = unsafe { self.strs.get().as_mut_unchecked() };

        let hash = hash(str);
        let matching_str_index = strs.iter().position(|existing_str_metadata| {
            // SAFETY: The pointer is guaranteed to be valid as `&str`.
            let existing_str = unsafe { existing_str_metadata.ptr.as_ref_unchecked() };

            existing_str_metadata.hash == hash && existing_str == str
        });

        if let Some(matching_str_index) = matching_str_index {
            StrId {
                index: matching_str_index as u32,
            }
        } else {
            let index = strs.len();

            strs.push(StrMetadata {
                ptr: self.arena.insert(str) as *const str,
                hash,
            });

            StrId {
                index: index as u32,
            }
        }
    }

    #[must_use]
    pub fn restore(&self, str_id: StrId) -> &str {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let strs = unsafe { self.strs.get().as_ref_unchecked() };

        let ptr = strs[str_id.index as usize].ptr;

        // SAFETY: The pointer is guaranteed to be valid as `&str` and to live
        // as long as `self`.
        unsafe { ptr.as_ref_unchecked() }
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
