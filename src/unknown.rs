use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnknownGuard {
    private: (),
}

impl UnknownGuard {
    pub fn new(_: &ErrorHandle) -> Self {
        Self { private: () }
    }
}
