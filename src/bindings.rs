use std::fmt::Debug;

use crate::{ident_interner::IdentId, val::Val};

pub struct Bindings<'ctx> {
    binding_idents: Vec<IdentId>,
    binding_values: Vec<Val<'ctx>>,
    scopes: Vec<Scope>,
}

struct Scope {
    binding_count_when_opened: usize,
}

pub enum CloseScopeError {
    NoOpenScope,
}

impl<'ctx> Bindings<'ctx> {
    pub fn open_scope(&mut self) {
        self.scopes.push(Scope {
            binding_count_when_opened: self.binding_idents.len(),
        });
    }

    pub fn push_binding(&mut self, ident: IdentId, value: Val<'ctx>) {
        self.binding_idents.push(ident);
        self.binding_values.push(value);
    }

    pub fn get_binding(&self, ident: IdentId) -> Option<&Val<'ctx>> {
        let index = self.binding_idents.iter().rev().position(|b| ident == *b)?;

        Some(&self.binding_values[index])
    }

    pub fn close_scope(&mut self) -> Result<(), CloseScopeError> {
        let Some(scope) = self.scopes.pop() else {
            return Err(CloseScopeError::NoOpenScope);
        };

        self.binding_idents
            .truncate(scope.binding_count_when_opened);
        self.binding_values
            .truncate(scope.binding_count_when_opened);
        Ok(())
    }
}

impl Debug for CloseScopeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CloseScopeError::NoOpenScope => {
                write!(f, "attempt to call `close_scope` with no open scope")
            }
        }
    }
}
