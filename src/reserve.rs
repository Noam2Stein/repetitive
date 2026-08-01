use std::cell::UnsafeCell;

pub struct Reserve<T>(UnsafeCell<Inner<T>>);

struct Inner<T> {
    chunks: Vec<Chunk<T>>,
    reservations: Vec<Reservation>,
}

struct Chunk<T> {
    /// # Safety
    ///
    /// During the lifetime of `ReservedStack<T>`, it must be sound to convert
    /// this pointer to a shared reference. This means that it must point at
    /// valid data, and that no mutable references are created.
    ptr: *mut [T],
    reservation_count: u8,
}

struct Reservation {
    chunk_index: usize,
}

impl<T> Drop for Reserve<T> {
    fn drop(&mut self) {
        let inner = self.0.get_mut();

        for chunk in &mut inner.chunks {
            // SAFETY: The pointer was created from a Box via `Box::into_raw`
            // and remains valid until this drop. All shared references created
            // from this pointer must have already expired, since `self` has.
            drop(unsafe { Box::<[T]>::from_raw(chunk.ptr) });
        }
    }
}

impl<T> Reserve<T> {
    pub fn new() -> Self {
        Self(UnsafeCell::new(Inner {
            chunks: Vec::new(),
            reservations: Vec::new(),
        }))
    }

    pub fn reserve(&self) -> &T
    where
        T: Default,
    {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let chunk_index = if let Some(existing_chunk) = inner
            .chunks
            .iter()
            .position(|chunk| (chunk.reservation_count as usize) < chunk.ptr.len())
        {
            existing_chunk
        } else {
            let new_chunk_index = inner.chunks.len();

            let new_chunk_len = if let Some(last_chunk) = inner.chunks.last() {
                last_chunk.ptr.len() * 2
            } else {
                32
            };
            inner
                .reservations
                .reserve_exact(new_chunk_len.saturating_sub(inner.reservations.len()));

            let new_chunk = (0..new_chunk_len).map(|_| T::default()).collect();

            // SAFETY: The chunk pointer remains valid until `ReservedStack<T>`
            // is dropped.
            inner.chunks.push(Chunk {
                ptr: Box::<[T]>::into_raw(new_chunk),
                reservation_count: 0,
            });

            new_chunk_index
        };

        inner.reservations.push(Reservation { chunk_index });

        let chunk = &mut inner.chunks[chunk_index];

        // SAFETY: All chunk pointers can be converted to shared references
        // that last until `ReservedStack<T>` is dropped.
        let chunk_slice = unsafe { chunk.ptr.as_ref_unchecked() };

        let result = &chunk_slice[chunk.reservation_count as usize];
        chunk.reservation_count += 1;

        result
    }

    pub fn release(&self) {
        // SAFETY: This reference does not escape the function, and during this
        // function no other references are created.
        let inner = unsafe { self.0.get().as_mut_unchecked() };

        let reservation = inner
            .reservations
            .pop()
            .expect("attempt to call `release` with no active reservations");

        inner.chunks[reservation.chunk_index].reservation_count -= 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::reserve::Reserve;

    #[test]
    fn test_usage() {
        let vec = Reserve::<i32>::new();

        let e0 = vec.reserve();
        let e1 = vec.reserve();
        vec.release();
        let e2 = vec.reserve();
        vec.release();
        vec.release();

        assert_eq!([e0, e1, e2], [&0; 3]);
    }
}
