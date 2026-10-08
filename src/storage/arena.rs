use std::{
    cell::UnsafeCell,
    mem::MaybeUninit,
    ptr::{NonNull, copy_nonoverlapping},
};

/// A type-specific arena data-structure.
///
/// Insert methods take a shared reference to `self` and return references that
/// live for the entire lifetime of the arena. This means references can be
/// retained while inserting more values.
///
/// This type stores values in chunks in order to avoid reallocation. The size
/// of each chunk is twice the size of the previous chunk in order to avoid
/// making too many separate allocations.
///
/// This arena is type-specific so that when it is dropped, the drop glue of `T`
/// can be run. For types that implement [`Copy`], it is more efficient to use
/// one shared [`MixedArena`].
pub struct Arena<T>(UnsafeCell<Inner<T>>);

/// A mixed-type arena data-structure that supports all types implementing
/// [`Copy`].
///
/// The only reason [`Arena<T>`] is type-specific is so that when it is dropped,
/// the drop glue of `T` can be run. Types that implement [`Copy`] have no drop
/// glue, and thus can be stored in one shared arena. This also supports slices
/// and [`str`].
///
/// Even though this is just a type alias, dedicated mixed-type functionality is
/// implemented for it.
pub type MixedArena = Arena<MaybeUninit<u8>>;

struct Inner<T> {
    chunks: Vec<Chunk<T>>,
}

/// # Safety
///
/// - `ptr` must be the result of `Box::<[MaybeUninit<T>]>::into_non_null`
///
/// - The element range `..used_slots` must not be referenced, since the caller
///   may retain references to it for the entire lifetime of the arena
///
/// - The element range `..used_slots` must only contain initialized values of
///   `T`, which may be accessed when dropping the arena
///
/// - `used_slots` must be less than or equal to `ptr.len()`
struct Chunk<T> {
    ptr: NonNull<[MaybeUninit<T>]>,
    used_slots: usize,
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

        let mut dst = inner.reserve_dst(1);

        // SAFETY: `dst` is guaranteed to be valid as a mutable reference of one
        // element, and its guaranteed to remain ours until the arena is dropped
        let dst = unsafe { dst.as_mut() };

        dst.write(value)
    }
}

impl MixedArena {
    #[expect(clippy::mut_from_ref)]
    pub fn insert_ref<T>(&self, value: &T) -> &mut T
    where
        T: Copy,
    {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let mut dst = inner.reserve_dst(size_of::<T>()).cast::<MaybeUninit<T>>();

        // SAFETY: `dst` is guaranteed to be valid as a mutable reference of one
        // element, and its guaranteed to remain ours until the arena is dropped
        let dst = unsafe { dst.as_mut() };

        dst.write(*value)
    }

    #[expect(clippy::mut_from_ref)]
    pub fn insert_slice<T>(&self, slice: &[T]) -> &mut [T]
    where
        T: Copy,
    {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let src = slice.as_ptr();
        let count = slice.len();

        let bytes = count.strict_mul(size_of::<T>());
        let dst = inner.reserve_dst(bytes).cast::<T>().as_ptr();

        // SAFETY: `src` and `count` come from a valid slice. `dst` is
        // guaranteed to be valid as a mutable reference to `slice.len()`
        // elements. Since `dst` is valid as a mutable reference, it cannot
        // overlap with `slice`.
        unsafe { copy_nonoverlapping(src, dst, count) };

        // SAFETY: `dst` is guaranteed to be valid as a mutable reference to
        // `slice.len()`. It remains valid and untouched until `self` is
        // dropped.
        unsafe { std::slice::from_raw_parts_mut(dst, count) }
    }

    #[expect(clippy::mut_from_ref)]
    pub fn insert_str(&self, str: &str) -> &mut str {
        // SAFETY: The output of `insert_slice` is the same as the input, which
        // is valid utf-8.
        unsafe { str::from_utf8_unchecked_mut(self.insert_slice(str.as_bytes())) }
    }
}

impl<T> Inner<T> {
    fn reserve_dst(&mut self, elements: usize) -> NonNull<MaybeUninit<T>> {
        let chunk = self.reserve_chunk(elements);

        // SAFETY: `chunk.elements` cannot overflow `isize` because it cannot go
        // outside of `chunk.ptr`.
        let result = unsafe { chunk.ptr.cast::<MaybeUninit<T>>().add(chunk.used_slots) };
        chunk.used_slots = chunk.used_slots.strict_add(elements);
        result
    }

    fn reserve_chunk(&mut self, elements: usize) -> &mut Chunk<T> {
        // Note that this function is written quite strangely due to limitations
        // in the borrow checker.

        let existing_chunk_index = self
            .chunks
            .iter_mut()
            .position(|chunk| chunk.ptr.len() - chunk.used_slots >= elements);

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
        let ptr = Box::<[MaybeUninit<T>]>::into_non_null(boxed);

        self.chunks.push_mut(Chunk { ptr, used_slots: 0 })
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
        // `Box::<[MaybeUninit<T>]>::into_non_null` and is only deallocated now.
        let mut boxed = unsafe { Box::<[MaybeUninit<T>]>::from_non_null(self.ptr) };

        // SAFETY: The first `self.used_slots` elements are guaranteed to be
        // initialized.
        unsafe { boxed[..self.used_slots].assume_init_drop() };
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::storage::arena::{Arena, MixedArena};

    #[test]
    fn test_arena() {
        let arena = Arena::<String>::new();

        let values = (0..100).map(|n| n.to_string()).collect_vec();

        let results = values
            .iter()
            .cloned()
            .map(|value| arena.insert(value).as_str())
            .collect_vec();

        assert_eq!(results, values);
    }

    #[test]
    fn test_mixed_arena() {
        let arena = MixedArena::new();

        let values = (0..100).map(|n| n.to_string()).collect_vec();

        let results = values
            .iter()
            .map(|value| &*arena.insert_str(value))
            .collect_vec();

        assert_eq!(results, values);
    }
}
