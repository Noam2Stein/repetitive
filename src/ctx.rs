use std::sync::{Arc, Mutex};

use string_interner::{DefaultStringInterner, DefaultSymbol};
use syn::{Ident, Token};

use super::*;

pub struct Context {
    interner: DefaultStringInterner,
    method_calls: Vec<(Token![.], Option<Ident>)>,
    warning_arcs: Vec<Arc<Mutex<Option<Warning>>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StrId {
    inner: DefaultSymbol,
}

#[derive(Debug, Clone)]
pub struct WarningHandle {
    option_arc: Arc<Mutex<Option<Warning>>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            interner: DefaultStringInterner::new(),
            method_calls: Vec::new(),
            warning_arcs: Vec::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> StrId {
        StrId {
            inner: self.interner.get_or_intern(s.to_string()),
        }
    }
    pub fn unintern(&self, id: StrId) -> &str {
        self.interner.resolve(id.inner).unwrap()
    }

    pub fn push_method_call(&mut self, dot: Token![.], method: Option<Ident>) {
        self.method_calls.push((dot, method));
    }
    pub fn get_method_calls(&self) -> &[(Token![.], Option<Ident>)] {
        &self.method_calls
    }

    pub fn push_warning(&mut self, warning: Warning) -> WarningHandle {
        let option_arc = Arc::new(Mutex::new(Some(warning)));
        self.warning_arcs.push(option_arc.clone());

        WarningHandle { option_arc }
    }
    pub fn take_warnings(&self) -> impl Iterator<Item = Warning> {
        self.warning_arcs
            .iter()
            .filter_map(|arc| arc.lock().unwrap().take())
    }
}

impl WarningHandle {
    pub fn remove(&self) {
        *self.option_arc.lock().unwrap() = None;
    }
}
