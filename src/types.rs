use std::collections::HashMap;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Keyword {
    Break,
    Case,
    Chan,
    Const,
    Continue,
    Default,
    Defer,
    Else,
    Fallthrough,
    For,
    Func,
    Go,
    Goto,
    If,
    Import,
    Interface,
    Map,
    Package,
    Range,
    Return,
    Select,
    Struct,
    Switch,
    Type,
    Var,
}

impl Keyword {
    pub fn extract(ident: &str) -> Option<Keyword> {
        match ident {
            // TODO может переписать это дерьмо на макрос?
            "break" => Some(Keyword::Break),
            "package" => Some(Keyword::Package),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Token {
    /// Keyword - cannot be used as an identifier
    Kw(Keyword),
    /// Identifier - name of a variable, structure, function, etc
    Ident(usize),
}

#[derive(Clone, Debug)]
pub struct IdentifierStorage {
    identifiers: Vec<String>,
    lookup_table: HashMap<String, usize>,
}

impl IdentifierStorage {
    pub fn new() -> Self {
        IdentifierStorage {
            identifiers: vec![],
            lookup_table: HashMap::new(),
        }
    }

    pub fn create_identifier(&mut self, ident: &str) -> usize {
        return match self.lookup_table.get(ident) {
            Some(&id) => id,
            None => {
                self.identifiers.push(ident.to_string());
                let id = self.identifiers.len() - 1;
                self.lookup_table.insert(ident.to_string(), id);
                id
            }
        };
    }
}

#[derive(Clone, Debug)]
pub enum Error {
    TokenizingError,
}
