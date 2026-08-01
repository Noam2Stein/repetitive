use std::{
    fmt::{Display, Write},
    range::Range,
};

pub struct IdentInterner {
    buffer: String,
    idents: Vec<IdentMetadata>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct IdentId {
    index: u32,
}

struct IdentMetadata {
    buffer_range: Range<usize>,
    hash: u64,
}

impl IdentInterner {
    pub fn new() -> Self {
        Self {
            buffer: String::with_capacity(200),
            idents: Vec::with_capacity(100),
        }
    }

    #[must_use]
    pub fn intern(&mut self, ident: &impl Display) -> IdentId {
        let buffer_start = self.buffer.len();
        write!(&mut self.buffer, "{ident}").expect("failed to write identifier into string");
        let buffer_range = Range::from(buffer_start..self.buffer.len());
        let str = &self.buffer[buffer_range];

        let hash = hash(str);
        let existing = self.idents.iter().position(|existing| {
            existing.hash == hash && &self.buffer[existing.buffer_range] == str
        });

        if let Some(existing) = existing {
            self.buffer.truncate(buffer_start);

            IdentId {
                index: existing as u32,
            }
        } else {
            let id = IdentId {
                index: self.idents.len() as u32,
            };

            self.idents.push(IdentMetadata { buffer_range, hash });

            id
        }
    }

    pub fn resolve(&self, id: IdentId) -> &str {
        let metadata = &self.idents[id.index as usize];
        &self.buffer[metadata.buffer_range]
    }
}

fn hash(str: &str) -> u64 {
    // Based on `https://docs.rs/rustc-hash/latest/rustc_hash/index.html`.

    const K: u64 = 0xf1357aea2e62a9c5;

    let mut hash = 0u64;

    hash = hash.wrapping_add(str.len() as u64).wrapping_mul(K);
    for byte in str.bytes() {
        hash = hash.wrapping_add(byte as u64).wrapping_mul(K);
    }

    hash
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::ident_interner::IdentInterner;

    #[test]
    fn test_correctness() {
        let idents = [
            "foo",
            "bar",
            "goo",
            "boo",
            "bar",
            "x",
            "y",
            "longlonglong",
            "y",
            "longlonglong",
            "foo",
            "goo",
            "boo",
            "bar",
            "x",
            "y",
            "longlonglong",
            "y",
            "longlonglong",
            "foo",
        ];

        let mut interner = IdentInterner::new();
        let mut ids = Vec::new();

        for ident in idents {
            let id = interner.intern(&ident);
            ids.push(id);

            assert_eq!(interner.resolve(id), ident);

            for (other_id, other_ident) in ids.iter().copied().zip(idents) {
                assert_eq!(id == other_id, ident == other_ident);
            }
        }

        assert_eq!(
            interner.buffer,
            idents.into_iter().unique().collect::<String>()
        );
        assert_eq!(interner.idents.len(), idents.into_iter().unique().count());
    }
}
