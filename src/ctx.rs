use std::sync::{Arc, Mutex};

use string_interner::DefaultStringInterner;
use syn::{Error, Ident, Token};

pub struct Context {
    pub interner: DefaultStringInterner,
    pub method_idents: Vec<(Token![.], Option<Ident>)>,
    pub warnings: Vec<syn::Error>,
    pub arc_warnings: Vec<Arc<Mutex<Option<syn::Error>>>>,
}

impl Context {
    pub fn push_arc_warning(&mut self, warning: Error) -> Arc<Mutex<Option<Error>>> {
        let arc_warning = Arc::new(Mutex::new(Some(warning)));
        self.arc_warnings.push(arc_warning.clone());

        arc_warning
    }
}
