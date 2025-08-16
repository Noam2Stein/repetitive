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

    pub fn queue_insert(&mut self, name: Name, fragment: FragmentValue, _ctx: &mut Context) {
        if self.new_names.contains_key(&name.id) {
            _ctx.push_error(Error::NameAlreadyExists(name));
            return;
        }

        self.new_names.insert(name.id, fragment);
    }
    pub fn flush(&mut self) {
        self.names.extend(self.new_names.drain());
    }

    pub fn get(&self, name: Name) -> Option<FragmentValue> {
        if let Some(fragment) = self.names.get(&name.id) {
            Some(FragmentValue {
                span: name.span,
                kind: match &fragment.kind {
                    FragmentValueKind::List(val) => FragmentValueKind::List(
                        val.iter()
                            .map(|item| FragmentValue {
                                span: name.span,
                                kind: item.kind.clone(),
                            })
                            .collect(),
                    ),

                    FragmentValueKind::Int(val) => FragmentValueKind::Int(*val),
                    FragmentValueKind::Float(val) => FragmentValueKind::Float(*val),
                    FragmentValueKind::Bool(val) => FragmentValueKind::Bool(*val),
                    FragmentValueKind::String(val) => FragmentValueKind::String(val.clone()),
                    FragmentValueKind::Char(val) => FragmentValueKind::Char(*val),
                    FragmentValueKind::Ident(val) => FragmentValueKind::Ident(val.clone()),
                    FragmentValueKind::Tokens(val) => FragmentValueKind::Tokens(val.clone()),

                    FragmentValueKind::Unknown(guard) => FragmentValueKind::Unknown(*guard),
                },
            })
        } else if let Some(parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn try_get(&self, name: Name, ctx: &mut Context) -> FragmentValue {
        match self.get(name) {
            Some(fragment) => fragment,
            None => FragmentValue::unknown(UnknownGuard::new(
                &ctx.push_error(Error::NameNotFound(name)),
            )),
        }
    }
}
