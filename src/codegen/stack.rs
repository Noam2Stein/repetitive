use std::cell::Cell;

use crate::storage::Storage;

#[derive(Clone, Copy)]
pub struct Stack {
    bools: usize,
    ints: usize,
}

impl Stack {
    pub fn alloc_bool<'storage>(&mut self, storage: &'storage Storage) -> &'storage Cell<bool> {
        let result = storage.bool_stack.get_or_grow(self.bools);
        self.bools = self.bools.strict_add(1);
        result
    }

    pub fn alloc_int<'storage>(&mut self, storage: &'storage Storage) -> &'storage Cell<i32> {
        let result = storage.int_stack.get_or_grow(self.bools);
        self.ints = self.ints.strict_add(1);
        result
    }
}
