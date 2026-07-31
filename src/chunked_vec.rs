use std::cell::UnsafeCell;

/// Holds a collection of fixed-size chunks of type `T`. Chunks can be appended,
/// and once added, they remain valid and immutable for the lifetime of the
/// container.
///
/// This is used to store variables durating code execution. In order for this
/// container to be actually useful, `T` needs to be a cell.
pub struct ChunkedVec<T> {
    /// # Safety
    ///
    /// All pointers must be convertable to shared references during the
    /// lifetime of `self`. This means they must point at valid data and mutable
    /// references must not be created.
    chunks: UnsafeCell<Vec<*mut [T]>>,
}

impl<T> Drop for ChunkedVec<T> {
    fn drop(&mut self) {
        for chunk in self.chunks.get_mut().iter().copied() {
            // SAFETY: It is guaranteed that all pointers point to valid data,
            // and it is valid to dangle this pointer here because after this
            // function, the lifetime of `self` is over.
            drop(unsafe { Box::<[T]>::from_raw(chunk) });
        }
    }
}

impl<T> ChunkedVec<T> {
    pub fn new() -> Self {
        Self {
            chunks: UnsafeCell::default(),
        }
    }

    pub fn push_chunk(&self, len: usize)
    where
        T: Default,
    {
        // SAFETY: This reference only exists for the duration of this function,
        // during which no other references are created.
        let chunks = unsafe { self.chunks.get().as_mut_unchecked() };

        // SAFETY: The added reference always remains valid, as the box is
        // not dropped.
        chunks.push(Box::<[T]>::into_raw(
            (0..len).map(|_| T::default()).collect(),
        ));
    }

    pub fn get_chunk(&self, index: usize) -> Option<&[T]> {
        // SAFETY: This reference only exists for the duration of this function,
        // during which no other references are created.
        let chunks = unsafe { self.chunks.get().as_ref_unchecked() };

        chunks.get(index).map(|chunk| {
            // SAFETY: It is guaranteed that all pointers are valid shared
            // references during the lifetime of `self`.
            unsafe { chunk.as_ref_unchecked() }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::chunked_vec::ChunkedVec;

    #[test]
    fn test_usage() {
        let vec = ChunkedVec::new();

        vec.push_chunk(5);
        assert_eq!(vec.get_chunk(0), Some([0; 5].as_slice()));

        vec.push_chunk(3);
        assert_eq!(vec.get_chunk(0), Some([0; 5].as_slice()));
        assert_eq!(vec.get_chunk(1), Some([0; 3].as_slice()));
    }
}
