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
    pub fn extract(ident: String) -> Result<Keyword, String> {
        match ident.as_str() {
            // TODO может переписать это дерьмо на макрос?
            "break" => Ok(Keyword::Break),
            "package" => Ok(Keyword::Package),
            _ => Err(ident),
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

    pub fn create_identifier(&mut self, ident: String) -> usize {
        return match self.lookup_table.get(&ident) {
            Some(&id) => id,
            None => {
                self.identifiers.push(ident);
                self.identifiers.len() - 1
            }
        }
    }
}

pub enum Error {
    TokenizingError,
}
