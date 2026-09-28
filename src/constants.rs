use std::{
    alloc::{Layout, alloc, dealloc, handle_alloc_error},
    cell::UnsafeCell,
    marker::PhantomData,
    mem::MaybeUninit,
    ptr::copy_nonoverlapping,
};

#[expect(private_bounds)]
pub struct Constants<T: ?Sized + Supported>(
    /// # Safety
    ///
    /// - Chunks must not be dropped until `self` is dropped, since shared
    ///   references to a chunk's buffer may exist
    UnsafeCell<Inner<T>>,
);

struct Inner<T: ?Sized + Supported> {
    chunks: Vec<Chunk<T>>,
}

/// # Safety
///
/// - `ptr` must point to a global-allocator allocation of size `cap` and
///   alignment `T::ALIGNMENT`
///
/// - the first `len` bytes of memory must not be used with mutable references,
///   since shared references may exist
///
/// - `len` must be less than or equal to `cap`
struct Chunk<T: ?Sized + Supported> {
    ptr: *mut u8,
    cap: usize,
    len: usize,
    _marker: PhantomData<T>,
}

trait Supported {
    const ALIGNMENT: usize;

    /// # Safety
    ///
    /// Implementations may assume that `dst` is valid for writes of
    /// `size_of_val(self)` bytes, that it is aligned to `ALIGNMENT`, and that
    /// it stays valid as a mutable reference to `Self` for the lifetime `'dst`.
    unsafe fn ref_copy<'dst>(&self, dst: *mut u8) -> &'dst Self;
}

#[expect(private_bounds)]
impl<T: ?Sized + Supported> Constants<T> {
    pub fn new() -> Self {
        Self(UnsafeCell::new(Inner { chunks: Vec::new() }))
    }

    pub fn insert(&self, value: &T) -> &T {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let size_of_value = size_of_val(value);

        // Select either an existing or new chunk with enough leftover space for
        // `value`.
        let chunk = if let Some(existing_chunk) = inner
            .chunks
            .iter_mut()
            .find(|chunk| chunk.cap - chunk.len >= size_of_value)
        {
            existing_chunk
        } else {
            loop {
                let new_chunk_cap = inner.next_chunk_cap();
                let new_chunk_layout =
                    Layout::from_size_align(new_chunk_cap, T::ALIGNMENT).unwrap();

                // SAFETY: The layout is never zero-sized because
                // `next_chunk_capacity` is guaranteed to never return zero.
                let new_chunk_ptr = unsafe { alloc(new_chunk_layout) };
                if new_chunk_ptr.is_null() {
                    handle_alloc_error(new_chunk_layout);
                }

                // SAFETY: The pointer originates from the global allocator with
                // the corresponding capacity and alignment `T::ALIGNMENT`. The
                // pointer stays valid since it is only dropped when `self` is
                // dropped.
                let new_chunk = inner.chunks.push_mut(Chunk {
                    ptr: new_chunk_ptr,
                    cap: new_chunk_cap,
                    len: 0,
                    _marker: PhantomData,
                });

                if new_chunk_cap >= size_of_value {
                    break new_chunk;
                }
            }
        };

        // SAFETY: `chunk.ptr` is the start of an allocation, and `chunk.len` is
        // less than or equal to the size of that allocation. Because of that,
        // neither overflow or going out of the allocation can occur.
        let result_dst = unsafe { chunk.ptr.add(chunk.len) };

        // SAFETY: `chunk.ptr` has been specifically selected so that
        // `result_dst` is valid for at least `size_of_value` bytes. It is
        // aligned to `T::ALIGNMENT`. If the function does not panic,
        // `chunk.len` is updated so that the written to memory is not written
        // to again. If the function panics, the reference immediately dies.
        let result = unsafe { value.ref_copy(result_dst) };
        chunk.len += size_of_value;

        result
    }
}

impl<T: ?Sized + Supported> Inner<T> {
    /// The recommended capacity for the chunk.
    ///
    /// This is guaranteed to never return zero.
    fn next_chunk_cap(&self) -> usize {
        const FIRST_CHUNK_CAP: usize = 128;

        assert!(
            self.chunks.len() <= FIRST_CHUNK_CAP.leading_zeros() as usize,
            "chunk capacity overflowed"
        );

        FIRST_CHUNK_CAP << self.chunks.len()
    }
}

impl<T: ?Sized + Supported> Drop for Chunk<T> {
    fn drop(&mut self) {
        let layout = Layout::from_size_align(self.cap, T::ALIGNMENT).unwrap();

        // SAFETY: `self.ptr` has been allocated using the global allocator.
        // `layout` is the same layout used when allocating.
        unsafe { dealloc(self.ptr.cast::<u8>(), layout) };
    }
}

impl<T: Copy> Supported for T {
    const ALIGNMENT: usize = align_of::<T>();

    unsafe fn ref_copy<'dst>(&self, dst: *mut u8) -> &'dst Self {
        // SAFETY: `dst` is guaranteed to be valid as a mutable reference of `T`
        // for the lifetime `'dst`.
        let dst = unsafe { dst.cast::<MaybeUninit<T>>().as_mut_unchecked() };

        dst.write(*self)
    }
}

impl<T: Copy> Supported for [T] {
    const ALIGNMENT: usize = align_of::<T>();

    unsafe fn ref_copy<'src, 'dst>(&'src self, dst: *mut u8) -> &'dst Self {
        // SAFETY: `src` is valid for `count` since they correspond to the same
        // slice. `dst` is guaranteed to be valid for writes of
        // `size_of_val(self)` and thus for `count`. `src` and `dst` cannot
        // overlap because `dst` is guaranteed to be valid as a mutable
        // reference. `src` and `dst` are aligned to `align_of::<T>()` through
        // `ALIGNMENT`.
        unsafe {
            copy_nonoverlapping(self.as_ptr(), dst.cast::<T>(), self.len());
        }

        // SAFETY: `dst` is guaranteed to be valid for `self.len()` as a mutable
        // reference for the lifetime `'dst`.
        unsafe { std::slice::from_raw_parts::<'dst, T>(dst.cast::<T>(), self.len()) }
    }
}

impl Supported for str {
    const ALIGNMENT: usize = 1;

    unsafe fn ref_copy<'dst>(&self, dst: *mut u8) -> &'dst Self {
        // SAFETY: `dst` is valid for `&[u8]` just like it is valid for `&str`.
        // The result is valid utf8 because it is the same value as the input.
        unsafe { Self::from_utf8_unchecked(self.as_bytes().ref_copy(dst)) }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::constants::Constants;

    #[test]
    fn test_sized() {
        let storage = Constants::<i32>::new();

        let values = (0..1000).collect_vec();

        let stored_values = values
            .iter()
            .map(|slice| *storage.insert(slice))
            .collect_vec();

        assert_eq!(stored_values, values);
    }

    #[test]
    fn test_slice() {
        let storage = Constants::<[i32]>::new();

        let values = (0..100)
            .map(|i| (0..i * 100 + i).collect_vec())
            .collect_vec();

        let stored_values = values
            .iter()
            .map(|slice| storage.insert(slice))
            .collect_vec();

        assert_eq!(stored_values, values);
    }

    #[test]
    fn test_str() {
        let storage = Constants::<str>::new();

        let values = (0..100)
            .map(|i| i.to_string().repeat(i * 63 % 24))
            .collect_vec();

        let stored_values = values
            .iter()
            .map(|slice| storage.insert(slice))
            .collect_vec();

        assert_eq!(stored_values, values);
    }
}
