use std::cell::UnsafeCell;

pub struct Stack<T> {
    /// # Safety
    ///
    /// All chunk pointers must be convertable to shared references during the
    /// lifetime of `self`. This means they must point at valid data and mutable
    /// references must not be created.
    chunks: UnsafeCell<Vec<Chunk<T>>>,
    allocations: UnsafeCell<Vec<Allocation>>,
}

struct Chunk<T> {
    ptr: *mut [T],
    taken: usize,
}

struct Allocation {
    chunk_index: usize,
}

impl<T> Drop for Stack<T> {
    fn drop(&mut self) {
        for chunk in self.chunks.get_mut() {
            // SAFETY: It is guaranteed that all pointers point to valid data,
            // and it is valid to dangle this pointer here because after this
            // function, the lifetime of `self` is over.
            drop(unsafe { Box::<[T]>::from_raw(chunk.ptr) });
        }
    }
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self {
            chunks: UnsafeCell::default(),
            allocations: UnsafeCell::default(),
        }
    }

    pub fn alloc(&self) -> &T
    where
        T: Default,
    {
        // SAFETY: The reference does not escape this function, and during this
        // function no other references are created.
        let chunks = unsafe { self.chunks.get().as_mut_unchecked() };

        // SAFETY: The reference does not escape this function, and during this
        // function no other references are created.
        let allocations = unsafe { self.allocations.get().as_mut_unchecked() };

        if let Some((chunk_index, chunk)) = chunks
            .iter_mut()
            .enumerate()
            .find(|(_, chunk)| chunk.taken < chunk.ptr.len())
        {
            // SAFETY: It is guaranteed that all pointers are valid shared
            // references during the lifetime of `self`.
            let chunk_slice = unsafe { chunk.ptr.as_ref_unchecked() };

            let result = &chunk_slice[chunk.taken];
            chunk.taken += 1;

            allocations.push(Allocation { chunk_index });

            result
        } else {
            allocations.push(Allocation {
                chunk_index: chunks.len(),
            });

            let len = chunks.last().map_or(32, |chunk| chunk.ptr.len() * 2);

            let ptr = Box::<[T]>::into_raw((0..len).map(|_| T::default()).collect());

            // SAFETY: The added reference always remains valid, as the box is
            // not dropped.
            chunks.push(Chunk { ptr, taken: 1 });

            // SAFETY: It is guaranteed that all pointers are valid shared
            // references during the lifetime of `self`.
            let chunk_slice = unsafe { ptr.as_ref_unchecked() };

            &chunk_slice[0]
        }
    }

    pub fn free(&self) {
        // SAFETY: The reference does not escape this function, and during this
        // function no other references are created.
        let allocations = unsafe { self.allocations.get().as_mut_unchecked() };

        let allocation = allocations
            .pop()
            .expect("attempt to call `free` without any allocations");

        // SAFETY: The reference does not escape this function, and during this
        // function no other references are created.
        let chunks = unsafe { self.chunks.get().as_mut_unchecked() };

        chunks[allocation.chunk_index].taken -= 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::stack::Stack;

    #[test]
    fn test_usage() {
        let vec = Stack::<i32>::new();

        let e0 = vec.alloc();
        let e1 = vec.alloc();
        vec.free();
        let e2 = vec.alloc();
        vec.free();
        vec.free();

        assert_eq!([e0, e1, e2], [&0; 3]);
    }
}
