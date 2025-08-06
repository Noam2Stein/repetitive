use string_interner::DefaultStringInterner;

#[derive(Debug)]
pub struct Context {
    pub interner: DefaultStringInterner,
}
