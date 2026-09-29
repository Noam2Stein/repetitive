use std::{cell::UnsafeCell, mem::MaybeUninit, ptr::copy_nonoverlapping};

pub struct Arena<T>(UnsafeCell<Inner<T>>);

pub struct StrArena(Arena<u8>);

struct Inner<T> {
    chunks: Vec<Chunk<T>>,
}

/// # Safety
///
/// - `ptr` must be the result of `Box::<[MaybeUninit<T>]>::into_inner`
///
/// - the first `elements` elements must not be used with mutable references,
///   because shared references may exist
///
/// - `taken_slots` must be less than or equal to `ptr.len()`
struct Chunk<T> {
    ptr: *mut [MaybeUninit<T>],
    elements: usize,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self(UnsafeCell::new(Inner { chunks: Vec::new() }))
    }

    #[expect(clippy::mut_from_ref)]
    pub fn insert(&self, value: T) -> &mut T {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let dst = inner.reserve_dst(1);

        // SAFETY: `dst` is guaranteed to be valid as a mutable reference of one
        // element, and its guaranteed to remain ours until `self` is dropped
        let dst = unsafe { dst.cast::<MaybeUninit<T>>().as_mut_unchecked() };

        dst.write(value)
    }

    #[expect(clippy::mut_from_ref)]
    pub fn insert_slice(&self, slice: &[T]) -> &mut [T]
    where
        T: Copy,
    {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let dst = inner.reserve_dst(slice.len());

        // SAFETY: `src` and `count` come from a valid slice. `dst` is
        // guaranteed to be valid as a mutable reference to `slice.len()`
        // elements. Since `dst` is valid as a mutable reference, it cannot
        // overlap with `slice`.
        unsafe { copy_nonoverlapping(slice.as_ptr(), dst, slice.len()) };

        // SAFETY: `dst` is guaranteed to be valid as a mutable reference to
        // `slice.len()`. It remains valid and untouched until `self` is
        // dropped.
        unsafe { std::slice::from_raw_parts_mut(dst, slice.len()) }
    }
}

impl StrArena {
    pub fn new() -> Self {
        Self(Arena::new())
    }

    #[expect(clippy::mut_from_ref)]
    pub fn insert(&self, str: &str) -> &mut str {
        // SAFETY: The output of `insert_slice` is the same as the input, which
        // is valid utf-8.
        unsafe { str::from_utf8_unchecked_mut(self.0.insert_slice(str.as_bytes())) }
    }
}

impl<T> Inner<T> {
    fn reserve_dst(&mut self, elements: usize) -> *mut T {
        let chunk = self.reserve_chunk(elements);

        // SAFETY: `chunk.elements` cannot overflow `isize` because it cannot go
        // outside of `chunk.ptr`.
        let result = unsafe { chunk.ptr.cast::<T>().add(chunk.elements) };
        chunk.elements = chunk.elements.strict_add(elements);
        result
    }

    fn reserve_chunk(&mut self, elements: usize) -> &mut Chunk<T> {
        // Note that this function is written quite strangely due to limitations
        // in the borrow checker.

        let existing_chunk_index = self
            .chunks
            .iter_mut()
            .position(|chunk| chunk.ptr.len() - chunk.elements >= elements);

        if let Some(existing_chunk_index) = existing_chunk_index {
            &mut self.chunks[existing_chunk_index]
        } else {
            loop {
                let new_chunk = self.append_chunk();
                if new_chunk.ptr.len() >= elements {
                    break;
                }
            }

            self.chunks.last_mut().unwrap()
        }
    }

    fn append_chunk(&mut self) -> &mut Chunk<T> {
        let boxed = Box::<[T]>::new_uninit_slice(self.next_chunk_capacity());
        let ptr = Box::<[MaybeUninit<T>]>::into_raw(boxed);

        self.chunks.push_mut(Chunk { ptr, elements: 0 })
    }

    fn next_chunk_capacity(&self) -> usize {
        const FIRST_CHUNK_CAP: usize = 128;

        assert!(
            self.chunks.len() <= FIRST_CHUNK_CAP.leading_zeros() as usize,
            "chunk capacity overflowed"
        );

        FIRST_CHUNK_CAP << self.chunks.len()
    }
}

impl<T> Drop for Chunk<T> {
    fn drop(&mut self) {
        // SAFETY: `self.ptr` originates from
        // `Box::<[MaybeUninit<T>]>::into_inner` and is only deallocated now.
        let mut boxed = unsafe { Box::<[MaybeUninit<T>]>::from_raw(self.ptr) };

        // SAFETY: The first `self.init_elements` elements are guaranteed to be
        // initialized.
        unsafe { boxed[..self.elements].assume_init_drop() };
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::arena::Arena;

    #[test]
    fn test_usage() {
        let arena = Arena::new();

        let values = (0..100).collect_vec();

        let results = values
            .iter()
            .map(|value| *arena.insert(*value))
            .collect_vec();

        assert_eq!(results, values);
    }
}
