use std::str::CharIndices;
use std::str::FromStr;
use std::char::from_u32;
use types::*;
use types::BinaryOp::*;
use types::PairedToken::*;
use types::Punctuation::*;
use types::Token::*;

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
                if token == LineBreak {
                    if self.tokens
                        .last()
                        .filter(|&i| implicit_semicolon(i))
                        .is_some()
                    {
                        self.tokens.push(Punc(Semicolon))
                    }
                } else {
                    self.tokens.push(token);
                }
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

    // # Rune Literal Help routines
    fn process_end_rune(&mut self, x: char) -> Result<Option<Token>, Error> {
        match self.next() {
            Some((_, '\'')) => {
                self.adv();
                Ok(Some(Rune(x)))
            }
            _ => Err(Error::TokenizingError),
        }
    }

    fn process_octal_rune(&mut self, a: char) -> Result<Option<Token>, Error> {
        match self.next() {
            Some((_, b)) if is_octal_digit(b) => match self.next() {
                Some((_, c)) if is_octal_digit(c) => match convert_octal(a, b, c) {
                    Some(res) => self.process_end_rune(res),
                    _ => Err(Error::TokenizingError)
                },
                _ => Err(Error::TokenizingError),
            },
            _ => Err(Error::TokenizingError),
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
            self.skip_chars(is_ident_char);
            if let Some((idx, cur)) = self.cur {
                return Ok(Some(self.wrap_ident(&self.input[idx_start..idx])));
            }
            return Ok(Some(self.wrap_ident(&self.input[idx_start..])));
        }

        // # Rune Literal
        match cur_char {
            '\'' => match self.next() {
                Some((_, '\\')) => match self.next() {
                    Some((_, x)) if is_rune_escaped(x) => return self.process_end_rune(x),
                    Some((_, x)) if is_octal_digit(x) => return self.process_octal_rune(x),
                    Some((idx_u, 'u')) => {
                        let idx = self.get_next_chars(4, &is_hex_digit)?;
                        return match char::from_str(&("\\u{".to_string() + &self.input[idx_u + 1..idx] + "}")) {
                            Ok(res) => self.process_end_rune(res),
                            _ => Err(Error::TokenizingError)
                        }
                    },
                    Some((idx_u_big, 'U')) => {
                        let idx = self.get_next_chars(8, &is_hex_digit)?;
                        if &self.input[idx_u_big + 1..=idx_u_big + 2] != "00" {
                            return Err(Error::TokenizingError)
                        }
                        return match char::from_str(&("\\u{".to_string() + &self.input[idx_u_big + 3..idx] + "}")) {
                            Ok(res) => self.process_end_rune(res),
                            _ => Err(Error::TokenizingError)
                        }
                    },
                    _ => return Err(Error::TokenizingError),
                },
                Some((_, '\0')) => return self.process_octal_rune('0'),
                Some((_, x)) => return self.process_end_rune(x),
                _ => return Err(Error::TokenizingError)
            },
            _ => ()
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
            '\n' => consume!(LineBreak),
            _ => {}
        }

        // # Integer literals
        if let '1'..='9' = cur_char {
            self.skip_chars(|i| i >= '0' && i <= '9');
            if let Some((idx, _)) = self.cur {
                return Ok(Some(Decimal(self.input[idx_start..idx].to_string())));
            }
            return Ok(Some(Decimal(self.input[idx_start..].to_string())));
        }
        if '0' == cur_char {
            self.adv();
            match self.cur {
                Some((_, 'x')) | Some((_, 'X')) => {
                    self.adv();
                    self.skip_chars(is_hex_digit);
                    if let Some((idx, _)) = self.cur {
                        return Ok(Some(Hex(self.input[idx_start + 2..idx].to_string())));
                    }
                    return Ok(Some(Hex(self.input[idx_start + 2..].to_string())));
                }
                _ => {
                    self.skip_chars(is_octal_digit);
                    if let Some((idx, _)) = self.cur {
                        return Ok(Some(Octal(self.input[idx_start + 1..idx].to_string())));
                    }
                    return Ok(Some(Octal(self.input[idx_start + 1..].to_string())));
                }
            }
        }

        Err(Error::TokenizingError) // unknown character
    }

    fn adv(&mut self) {
        self.cur = self.iter.next();
    }

    fn next(&mut self) -> Cur {
        self.adv();
        self.cur
    }

    fn skip_whitespace(&mut self) {
        self.skip_chars(is_whitespace);
    }

    fn get_char<F>(&mut self, predicate: &F) -> Result<usize, Error>
    where
        F: Fn(char) -> bool,
    {
        match self.next() {
            Some((idx, x)) if predicate(x) => Ok(idx),
            _ => Err(Error::TokenizingError),
        }
    }

    fn get_next_chars<F>(&mut self, n: usize, predicate: &F) -> Result<usize, Error>
    where
        F: Fn(char) -> bool,
    {
        for _ in 0..n-1 {
            if self.get_char(predicate).is_err() {
                return Err(Error::TokenizingError)
            }
        }
        self.get_char(predicate)
    }

    fn skip_chars<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool,
    {
        while let Some((_, c)) = self.cur {
            if !predicate(c) {
                break;
            }
            self.adv();
        }
    }
}

fn convert_octal(a: char, b: char, c: char) -> Option<char> {
    match (a.to_digit(8), b.to_digit(8), c.to_digit(8)) {
        (Some(x), Some(y), Some(z)) => from_u32(x + y + z),
        _ => None
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
    c.is_whitespace() && c != '\n'
}

fn is_hex_digit(c: char) -> bool {
    (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')
}

fn is_octal_digit(c: char) -> bool {
    (c >= '0' && c <= '7')
}

fn is_escaped(c: char) -> bool {
    (c == 'a') || (c == 'b') || (c == 'f') || (c == 'n') || (c == 'r') || (c == 't') || (c == 't')
        || (c == 'v') || (c == '\\')
}

fn is_rune_escaped(c: char) -> bool {
    is_escaped(c) || (c == '\'')
}

fn is_string_escaped(c: char) -> bool {
    is_escaped(c) || (c == '\"')
}

/// Whether the token is automatically followed by an implicit `;`
/// if it's the last token on the line
fn implicit_semicolon(t: &Token) -> bool {
    match *t {
        Kw(kw) => {
            kw == Keyword::Break || kw == Keyword::Continue || kw == Keyword::Fallthrough
                || kw == Keyword::Return
        }
        Ident(_) | Punc(Right(_)) | Punc(Increment) | Punc(Decrement) => true,
        _ => false,
    }
}
