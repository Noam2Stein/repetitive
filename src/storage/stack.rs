use std::{cell::UnsafeCell, ptr::NonNull, range::Range};

// TODO: Improve this naming its really bad

pub struct Stack<T>(UnsafeCell<Inner<T>>);

struct Inner<T> {
    chunks: Vec<Chunk<T>>,
}

struct Chunk<T> {
    /// # Safety
    ///
    /// During the lifetime of `Stack<T>`, it must be sound to convert this
    /// pointer to a shared reference. This means that it must point at valid
    /// data, and that no mutable references are created.
    ptr: NonNull<[T]>,
    indices: Range<usize>,
}

impl<T> Drop for Chunk<T> {
    fn drop(&mut self) {
        // SAFETY: The pointer was created from a Box via `Box::into_raw`
        // and remains valid until this drop. All shared references created
        // from this pointer must have already expired, since `self` has.
        drop(unsafe { Box::<[T]>::from_non_null(self.ptr) });
    }
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self(UnsafeCell::new(Inner { chunks: Vec::new() }))
    }

    pub fn get_or_grow(&self, index: usize) -> &T
    where
        T: Default,
    {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let chunk = if let Some(existing_chunk) =
            inner.chunks.iter().find(|chunk| index < chunk.indices.end)
        {
            existing_chunk
        } else {
            loop {
                let new_chunk = inner.push_chunk();
                if index < new_chunk.indices.end {
                    break new_chunk;
                }
            }
        };

        let index_in_chunk = index - chunk.indices.start;
        let result_ptr = unsafe { chunk.ptr.cast::<T>().add(index_in_chunk) };

        unsafe { result_ptr.as_ref() }
    }
}

impl<T> Inner<T> {
    fn push_chunk(&mut self) -> &Chunk<T>
    where
        T: Default,
    {
        let (len, indices) = if let Some(last_chunk) = self.chunks.last() {
            let len = last_chunk.ptr.len() * 2;
            let indices = last_chunk.indices.end..last_chunk.indices.end + len;
            (len, indices)
        } else {
            (64, 0..64)
        };
        // Convert `std::ops::Range` to `std::range::Range`
        let indices = Range::from(indices);

        let boxed = (0..len).map(|_| T::default()).collect();
        let ptr = Box::<[T]>::into_non_null(boxed);

        // SAFETY: The chunk pointer remains valid until `Stack<T>` is
        // dropped.
        self.chunks.push_mut(Chunk { ptr, indices })
    }
}
