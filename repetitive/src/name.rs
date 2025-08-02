use std::collections::HashMap;

use proc_macro2::Span;
use string_interner::DefaultSymbol;
use syn::Ident;

use super::*;

#[derive(Debug, Clone)]
pub struct Namespace<'p> {
    parent: Option<&'p Namespace<'p>>,
    names: HashMap<NameId, Fragment>,
}

#[derive(Debug, Clone, Copy)]
pub struct Name {
    pub id: NameId,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameId {
    inner: DefaultSymbol,
}

impl<'p> Namespace<'p> {
    pub fn new() -> Self {
        Self {
            parent: None,
            names: HashMap::new(),
        }
    }

    pub fn fork<'s>(&'s self) -> Namespace<'s> {
        Namespace {
            parent: Some(self),
            names: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: Name, fragment: Fragment, ctx: &mut Context) {
        if self.names.contains_key(&name.id) {
            ctx.errors
                .push(syn::Error::new(name.span, "Name already exists"));
        }

        self.names.insert(name.id, fragment);
    }

    pub fn get(&self, name: NameId) -> Option<&Fragment> {
        if let Some(fragment) = self.names.get(&name) {
            Some(fragment)
        } else if let Some(parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }
}

impl ContextParse for Name {
    fn ctx_parse(input: syn::parse::ParseStream, ctx: &mut Context) -> syn::Result<Self>
    where
        Self: Sized,
    {
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
