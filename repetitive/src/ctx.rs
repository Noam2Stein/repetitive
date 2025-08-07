use string_interner::DefaultStringInterner;
use syn::{Ident, Token};

pub struct Context {
    pub interner: DefaultStringInterner,
    pub method_idents: Vec<(Token![.], Option<Ident>)>,
    pub warnings: Vec<syn::Error>,
}
