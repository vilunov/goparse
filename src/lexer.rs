use std::str::CharIndices;

use types::BinaryOp::*;
use types::PairedToken::*;
use types::Punctuation::*;
use types::Token::*;
use types::*;

/// Lexical analyser of Go source code.
/// Stores information about scanned tokens
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

type Cur = Option<(usize, char)>;

struct LexerInner<'a> {
    input: &'a str,
    idents: &'a mut IdentifierStorage,
    iter: CharIndices<'a>,
    cur: Cur,
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
            Some(kw) => Kw(kw),
            None => Ident(self.idents.create_identifier(ident)),
        }
    }

    fn next_token(&mut self) -> Result<Option<Token>, Error> {
        self.skip_whitespace();

        let (idx_start, cur_char) = match self.cur {
            None => return Ok(None), // end of input
            Some(i) => i,
        };

        macro_rules! consume {
            ($token: expr) => {{
                self.adv();
                return Ok(Some($token));
            }};
        }

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
        // # Operators and punctuation
        match cur_char {
            ';' => consume!(Punc(Semicolon)),
            ':' => match self.next() {
                Some((_, '=')) => consume!(Punc(ColonAssign)),
                _ => return Ok(Some(Punc(Colon))),
            },
            '.' => match self.next() {
                Some((_, '.')) => match self.next() {
                    Some((_, '.')) => consume!(Punc(DotDotDot)),
                    _ => return Err(Error::TokenizingError),
                },
                _ => return Ok(Some(Punc(Dot))),
            },
            ',' => consume!(Punc(Comma)),
            '(' => consume!(Punc(Left(Parenthesis))),
            ')' => consume!(Punc(Right(Parenthesis))),
            '[' => consume!(Punc(Left(Bracket))),
            ']' => consume!(Punc(Right(Bracket))),
            '{' => consume!(Punc(Left(Brace))),
            '}' => consume!(Punc(Right(Brace))),
            '&' => match self.next() {
                Some((_, '&')) => consume!(Punc(DoubleAnd)),
                Some((_, '=')) => consume!(BinOpAssign(And)),
                Some((_, '^')) => match self.next() {
                    Some((_, '=')) => consume!(BinOpAssign(AndHat)),
                    _ => return Ok(Some(BinOp(AndHat))),
                },
                _ => return Ok(Some(BinOp(And))),
            },
            '|' => match self.next() {
                Some((_, '&')) => consume!(Punc(DoubleOr)),
                Some((_, '=')) => consume!(BinOpAssign(Or)),
                _ => return Ok(Some(BinOp(Or))),
            },
            '=' => match self.next() {
                Some((_, '=')) => consume!(Punc(Equals)),
                _ => return Ok(Some(Punc(Assign))),
            },
            '!' => match self.next() {
                Some((_, '=')) => consume!(Punc(Ne)),
                _ => return Ok(Some(Punc(Bang))),
            },
            '<' => match self.next() {
                Some((_, '=')) => consume!(Punc(Le)),
                Some((_, '-')) => consume!(Punc(LeftArrow)),
                Some((_, '<')) => match self.next() {
                    Some((_, '=')) => consume!(BinOpAssign(LeftShift)),
                    _ => return Ok(Some(BinOp(LeftShift))),
                },
                _ => return Ok(Some(Punc(Lt))),
            },
            '>' => match self.next() {
                Some((_, '=')) => consume!(Punc(Ge)),
                Some((_, '>')) => match self.next() {
                    Some((_, '=')) => consume!(BinOpAssign(RightShift)),
                    _ => return Ok(Some(BinOp(RightShift))),
                },
                _ => return Ok(Some(Punc(Gt))),
            },
            '+' => match self.next() {
                Some((_, '+')) => consume!(Punc(Increment)),
                Some((_, '=')) => consume!(BinOpAssign(Plus)),
                _ => return Ok(Some(BinOp(Plus))),
            },
            '-' => match self.next() {
                Some((_, '-')) => consume!(Punc(Decrement)),
                Some((_, '=')) => consume!(BinOpAssign(Minus)),
                _ => return Ok(Some(BinOp(Minus))),
            },
            '*' => match self.next() {
                Some((_, '=')) => consume!(BinOpAssign(Multiply)),
                _ => return Ok(Some(BinOp(Multiply))),
            },
            '/' => match self.next() {
                Some((_, '=')) => consume!(BinOpAssign(Divide)),
                _ => return Ok(Some(BinOp(Divide))),
            },
            '%' => match self.next() {
                Some((_, '=')) => consume!(BinOpAssign(Modulus)),
                _ => return Ok(Some(BinOp(Modulus))),
            },
            '^' => match self.next() {
                Some((_, '=')) => consume!(BinOpAssign(Hat)),
                _ => return Ok(Some(BinOp(Hat))),
            },
            _ => (),
        }

        Err(Error::TokenizingError)
    }

    fn adv(&mut self) {
        self.cur = self.iter.next();
    }

    fn next(&mut self) -> Cur {
        self.adv();
        self.cur
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
