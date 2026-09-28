use std::{cell::UnsafeCell, mem::MaybeUninit};

pub struct Constants<T>(UnsafeCell<Inner<T>>);

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

impl<T> Constants<T> {
    pub fn new() -> Self {
        Self(UnsafeCell::new(Inner { chunks: Vec::new() }))
    }

    pub fn insert(&self, value: T) -> &T {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let dst = inner.reserve_dst(1);

        // SAFETY: `dst` is guaranteed to be valid as a mutable reference of one
        // element, and its guaranteed to remain ours until `self` is dropped
        let dst = unsafe { dst.cast::<MaybeUninit<T>>().as_mut_unchecked() };

        dst.write(value)
    }
}

impl<T> Inner<T> {
    fn reserve_dst(&mut self, elements: usize) -> *mut T {
        let chunk = self.reserve_chunk(elements);

        // SAFETY: `chunk.elements` cannot overflow `isize` because it cannot go
        // outside of `chunk.ptr`.
        let result = unsafe { chunk.ptr.cast::<T>().add(chunk.elements) };
        chunk.elements += elements;
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

    use crate::constants::Constants;

    #[test]
    fn test_usage() {
        let constants = Constants::new();

        let values = (0..100).collect_vec();

        let results = values
            .iter()
            .map(|value| *constants.insert(*value))
            .collect_vec();

        assert_eq!(results, values);
    }
}
