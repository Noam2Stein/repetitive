use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnknownGuard {
    private: (),
}

impl ErrorHandle {
    pub fn unknown_guard(&self) -> UnknownGuard {
        UnknownGuard { private: () }
    }
}
