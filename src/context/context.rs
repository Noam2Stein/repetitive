use std::sync::{Arc, Mutex};

use string_interner::{DefaultStringInterner, DefaultSymbol};
use syn::{Ident, Token};

use super::*;

pub struct Context {
    interner: DefaultStringInterner,
    method_calls: Vec<(Token![.], Option<Ident>)>,
    error_arcs: Vec<Arc<Mutex<Option<Error>>>>,
    warning_arcs: Vec<Arc<Mutex<Option<Warning>>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StrId {
    inner: DefaultSymbol,
}

#[derive(Debug, Clone)]
pub struct ErrorHandle {
    option_arc: Arc<Mutex<Option<Error>>>,
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
            error_arcs: Vec::new(),
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

    pub fn push_error(&mut self, error: Error) -> ErrorHandle {
        let option_arc = Arc::new(Mutex::new(Some(error)));
        self.error_arcs.push(option_arc.clone());

        println!("pushng error {}", self.error_arcs.len());

        ErrorHandle { option_arc }
    }
    pub fn has_errors(&self) -> bool {
        self.error_arcs
            .iter()
            .any(|arc| arc.lock().unwrap().is_some())
    }
    pub fn take_errors(&self) -> impl Iterator<Item = Error> {
        self.error_arcs
            .iter()
            .filter_map(|arc| arc.lock().unwrap().take())
    }

    pub fn push_warning(&mut self, warning: Warning) -> WarningHandle {
        let option_arc = Arc::new(Mutex::new(Some(warning)));
        self.warning_arcs.push(option_arc.clone());

        WarningHandle { option_arc }
    }
    #[expect(dead_code)]
    pub fn has_warnings(&self) -> bool {
        self.warning_arcs
            .iter()
            .any(|arc| arc.lock().unwrap().is_some())
    }
    pub fn take_warnings(&self) -> impl Iterator<Item = Warning> {
        self.warning_arcs
            .iter()
            .filter_map(|arc| arc.lock().unwrap().take())
    }
}

impl ErrorHandle {
    #[expect(dead_code)]
    pub fn remove(&self) {
        *self.option_arc.lock().unwrap() = None;
    }
}

impl WarningHandle {
    pub fn remove(&self) {
        *self.option_arc.lock().unwrap() = None;
    }
}
