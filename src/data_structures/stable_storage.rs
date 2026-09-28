use std::{
    cell::UnsafeCell,
    mem::{MaybeUninit, forget},
    ptr::{copy_nonoverlapping, slice_from_raw_parts_mut},
};

pub struct StableStorage<T>(UnsafeCell<Inner<T>>);

struct Inner<T> {
    /// # Safety
    ///
    /// The pointer of each chunk must be valid for
    /// `chunk_capacity(chunk_index)` uninitialized elements.
    chunks: Vec<Chunk<T>>,
}

struct Chunk<T> {
    /// # Safety
    ///
    /// This pointer must originate from an allocation of the global allocator,
    /// and be valid for `len` initialized elements. Those elements must not be
    /// mutated, since shared references can exist at any time.
    ptr: *mut MaybeUninit<T>,
    len: usize,
}

/// The capacity of a chunk based on its index.
///
/// This function is called whenever the capacity is needed, in order to avoid
/// storing the capacity permenantly.
fn chunk_capacity(chunk_index: usize) -> usize {
    const FIRST_CHUNK_CAPACITY: usize = 128;

    assert!(
        chunk_index <= FIRST_CHUNK_CAPACITY.leading_zeros() as usize,
        "chunk capacity overflowed"
    );

    FIRST_CHUNK_CAPACITY << chunk_index
}

impl<T> StableStorage<T> {
    pub fn new() -> Self {
        Self(UnsafeCell::new(Inner { chunks: Vec::new() }))
    }

    pub fn insert(&self, slice: &[T]) -> &[T]
    where
        T: Copy,
    {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        // Select either an existing or new chunk with enough leftover space for
        // the elements of `slice`.
        let chunk_index = if let Some(existing_chunk) = inner
            .chunks
            .iter()
            .enumerate()
            .map(|(chunk_index, chunk)| chunk_capacity(chunk_index) - chunk.len)
            .position(|chunk_leftover_space| chunk_leftover_space >= slice.len())
        {
            existing_chunk
        } else {
            loop {
                let new_chunk_index = inner.chunks.len();
                let new_chunk_capacity = chunk_capacity(new_chunk_index);
                let mut new_chunk_box = Box::<[T]>::new_uninit_slice(new_chunk_capacity);
                let new_chunk_ptr = new_chunk_box.as_mut_ptr();

                // SAFETY: The pointer originates from a global allocator box,
                // is valid for exactly `chunk_capacity(new_chunk_index)`
                // uninitialized elements, and does not need to initialize
                // anything yet because `len` is `0`. The pointer stays valid
                // since the box is immediately forgotten after this push.
                inner.chunks.push(Chunk {
                    ptr: new_chunk_ptr,
                    len: 0,
                });
                forget(new_chunk_box);

                if new_chunk_capacity >= slice.len() {
                    break new_chunk_index;
                }
            }
        };

        let chunk = &mut inner.chunks[chunk_index];

        // SAFETY: `chunk.ptr` is the start of an allocation, and `chunk.len` is
        // less than or equal to the size of that allocation. Because of that,
        // neither overflow or going out of the allocation can occur.
        let chunk_leftover_ptr = unsafe { chunk.ptr.add(chunk.len) };
        // SAFETY: `src` and `count` are the start and length of the same slice,
        // so they are valid for reads. `chunk_leftover_ptr` points to a chunk
        // that has been checked to have at least `slice.len()` leftover space
        // after `chunk.len`. `dst` is valid for writes since it does not point
        // to existing elements, which may have shared references to them. `dst`
        // and `src` cannot overlap.
        unsafe { copy_nonoverlapping(slice.as_ptr(), chunk_leftover_ptr.cast::<T>(), slice.len()) };
        chunk.len += slice.len();

        // SAFETY: `chunk_leftover_ptr` has been checked to be valid for
        // `slice.len()` elements. No mutable references are going to exist to
        // this slice, since `chunk.len` has been updated.
        let result_slice =
            unsafe { std::slice::from_raw_parts(chunk_leftover_ptr.cast_const(), slice.len()) };

        // SAFETY: We copied initialized elements into `result_slice` using
        // `copy_nonoverlapping`.
        unsafe { result_slice.assume_init_ref() }
    }
}

impl<T> Drop for StableStorage<T> {
    fn drop(&mut self) {
        let inner = self.0.get_mut();

        for (chunk_index, chunk) in inner.chunks.iter_mut().enumerate() {
            let chunk_slice_ptr = slice_from_raw_parts_mut(chunk.ptr, chunk_capacity(chunk_index));
            // SAFETY: This pointer corresponds to the exact pointer returned by
            // `Box::<[MaybeUninit<T>]>::into_inner`.
            let mut chunk_box = unsafe { Box::<[MaybeUninit<T>]>::from_raw(chunk_slice_ptr) };

            // SAFETY: The first `chunk.len` elements are guaranteed to be
            // initialized.
            unsafe { chunk_box[..chunk.len].assume_init_drop() };
        }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::data_structures::stable_storage::StableStorage;

    #[test]
    fn test_usage() {
        let storage = StableStorage::new();

        let values = (0..100)
            .map(|i| (0..i * 100 + i).collect_vec())
            .collect_vec();

        let stored_values = values
            .iter()
            .map(|slice| storage.insert(slice))
            .collect_vec();

        assert_eq!(stored_values, values);
    }
}
