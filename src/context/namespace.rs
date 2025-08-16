use std::collections::HashMap;

use super::*;

#[derive(Debug, Clone)]
pub struct Namespace<'p> {
    parent: Option<&'p Namespace<'p>>,
    names: HashMap<NameId, Value>,
    new_names: HashMap<NameId, Value>,
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

    pub fn queue_insert(&mut self, name: Name, fragment: Value, _ctx: &mut Context) {
        if self.new_names.contains_key(&name.id) {
            _ctx.push_error(Error::NameAlreadyExists(name));
            return;
        }

        self.new_names.insert(name.id, fragment);
    }
    pub fn flush(&mut self) {
        self.names.extend(self.new_names.drain());
    }

    pub fn get(&self, name: Name) -> Option<Value> {
        if let Some(fragment) = self.names.get(&name.id) {
            Some(Value {
                span: name.span,
                kind: match &fragment.kind {
                    ValueKind::List(val) => ValueKind::List(
                        val.iter()
                            .map(|item| Value {
                                span: name.span,
                                kind: item.kind.clone(),
                            })
                            .collect(),
                    ),

                    ValueKind::Int(val) => ValueKind::Int(*val),
                    ValueKind::Float(val) => ValueKind::Float(*val),
                    ValueKind::Bool(val) => ValueKind::Bool(*val),
                    ValueKind::String(val) => ValueKind::String(val.clone()),
                    ValueKind::Char(val) => ValueKind::Char(*val),
                    ValueKind::Ident(val) => ValueKind::Ident(val.clone()),
                    ValueKind::Tokens(val) => ValueKind::Tokens(val.clone()),

                    ValueKind::Unknown(guard) => ValueKind::Unknown(*guard),
                },
            })
        } else if let Some(parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn try_get(&self, name: Name, ctx: &mut Context) -> Value {
        match self.get(name) {
            Some(fragment) => fragment,
            None => Value::unknown(ctx.push_error(Error::NameNotFound(name)).unknown_guard()),
        }
    }
}
