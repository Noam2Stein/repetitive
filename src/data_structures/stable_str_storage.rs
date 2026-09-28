use crate::data_structures::stable_storage::StableStorage;

pub struct StableStrStorage {
    inner: StableStorage<u8>,
}

impl StableStrStorage {
    pub fn new() -> Self {
        Self {
            inner: StableStorage::new(),
        }
    }

    pub fn insert(&self, s: &str) -> &str {
        let bytes = self.inner.insert(s.as_bytes());

        // SAFETY: The inserted value is a valid utf8 string converted to bytes.
        unsafe { str::from_utf8_unchecked(bytes) }
    }
}
