use types::{Token, IdentifierStorage, Error};

#[derive(Clone, Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
    idents: IdentifierStorage,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            tokens: vec![],
            idents: IdentifierStorage::new(),
        }
    }

    pub fn tokenize(self, input: &str) -> Result<Self, Error> {
        for c in input.chars() {
            // TODO lexer
        }
        Ok(self)
    }

    pub fn collect(self) -> (Vec<Token>, IdentifierStorage) {
        (self.tokens, self.idents)
    }
}

// Helper predicates

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic()
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric()
}