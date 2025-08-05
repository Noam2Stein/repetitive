use std::collections::HashMap;

use proc_macro2::Span;
use string_interner::DefaultSymbol;
use syn::{Ident, parse::ParseStream};

use super::*;

#[derive(Debug, Clone)]
pub struct Namespace<'p> {
    parent: Option<&'p Namespace<'p>>,
    names: HashMap<NameId, FragmentValue>,
    new_names: HashMap<NameId, FragmentValue>,
}

#[derive(Debug, Clone, Copy)]
pub struct Name {
    pub id: NameId,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameId {
    pub inner: DefaultSymbol,
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

impl ContextParse for Name {
    fn ctx_parse(input: ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
        if Keyword::peek(input) {
            return Err(syn::Error::new(
                input.span(),
                "expected name, found keyword",
            ));
        }

        let ident = input.parse::<Ident>()?;
        let id = NameId {
            inner: ctx.interner.get_or_intern(ident.to_string()),
        };

        Ok(Self {
            id,
            span: ident.span(),
        })
    }
}
