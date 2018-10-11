use std::str::CharIndices;

use types::{Error, IdentifierStorage, Keyword, Token};

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

    pub fn tokenize(mut self, input: &str) -> Result<Self, Error> {
        {
            let mut inner = LexerInner::new(input, &mut self.idents);
            while let Some(token) = inner.next_token()? {
                self.tokens.push(token);
            }
        }
        Ok(self)
    }

    pub fn collect(self) -> (Vec<Token>, IdentifierStorage) {
        (self.tokens, self.idents)
    }
}

struct LexerInner<'a> {
    input: &'a str,
    idents: &'a mut IdentifierStorage,
    iter: CharIndices<'a>,
    cur: Option<(usize, char)>,
}

impl<'a> LexerInner<'a> {
    fn new(input: &'a str, idents: &'a mut IdentifierStorage) -> Self {
        let mut iter = input.char_indices();
        let cur = iter.next();
        Self {
            input,
            idents,
            iter,
            cur,
        }
    }

    fn wrap_ident(&mut self, ident: &str) -> Token {
        match Keyword::extract(ident) {
            Some(kw) => Token::Kw(kw),
            None => Token::Ident(self.idents.create_identifier(ident)),
        }
    }

    fn next_token(&mut self) -> Result<Option<Token>, Error> {
        self.skip_whitespace();

        let (idx_start, cur_char) = match self.cur {
            None => return Ok(None), // end of input
            Some(i) => i,
        };

        // # Identifiers and keywords
        if is_ident_start(cur_char) {
            self.adv();
            while let Some((idx, cur)) = self.cur {
                if is_whitespace(cur) {
                    return Ok(Some(self.wrap_ident(&self.input[idx_start..idx])));
                } else if !is_ident_char(cur) {
                    return Err(Error::TokenizingError);
                }
                self.adv();
            }
            return Ok(Some(self.wrap_ident(&self.input[idx_start..])));
        }

        Err(Error::TokenizingError)
    }

    fn adv(&mut self) {
        self.cur = self.iter.next();
    }

    fn skip_whitespace(&mut self) {
        while let Some((_, c)) = self.cur {
            if !is_whitespace(c) {
                break;
            }
            self.adv();
        }
    }
}

// Helper predicates

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_whitespace(c: char) -> bool {
    c.is_whitespace()
}
