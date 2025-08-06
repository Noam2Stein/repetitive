use std::collections::HashMap;

use super::*;

#[derive(Debug, Clone)]
pub struct Namespace<'p> {
    parent: Option<&'p Namespace<'p>>,
    names: HashMap<NameId, FragmentValue>,
    new_names: HashMap<NameId, FragmentValue>,
}

impl<'p> Namespace<'p> {
    pub fn new() -> Self {
        Self {
            parent: None,
            names: HashMap::new(),
            new_names: HashMap::new(),
        }
    }

    pub fn fork<'s>(&'s self) -> Namespace<'s> {
        Namespace {
            parent: Some(self),
            names: HashMap::new(),
            new_names: HashMap::new(),
        }
    }

    pub fn queue_insert(&mut self, name: Name, fragment: FragmentValue, ctx: &mut Context) {
        if self.new_names.contains_key(&name.id) {
            ctx.errors
                .push(syn::Error::new(name.span, "Name already exists"));
        }

        self.new_names.insert(name.id, fragment);
    }
    pub fn flush(&mut self) {
        self.names.extend(self.new_names.drain());
    }

    pub fn get(&self, name: NameId) -> Option<&FragmentValue> {
        if let Some(fragment) = self.names.get(&name) {
            Some(fragment)
        } else if let Some(parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn try_get(&self, name: Name) -> syn::Result<FragmentValue> {
        match self.get(name.id) {
            Some(fragment) => Ok(fragment.clone()),
            None => Err(syn::Error::new(name.span, "Name not found")),
        }
    }
}
