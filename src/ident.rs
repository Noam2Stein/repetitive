/// Checks if a string is a valid identifier.
///
/// Note that this returns true for keywords.
///
/// This is currently limited to ascii characters only, because supporting
/// arbitrary utf-8 requires handling identifer equality exactly like Rust does.
pub fn is_valid_ident(str: &str) -> bool {
    str.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}
