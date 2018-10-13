use std::char::from_u32;
use std::str::CharIndices;
use std::u32;
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
    strings: StringLiteralsStorage,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            tokens: vec![],
            idents: IdentifierStorage::new(),
            strings: StringLiteralsStorage::new(),
        }
    }

    pub fn tokenize(mut self, input: &str) -> Result<Self, Error> {
        {
            let mut inner = LexerInner::new(input, &mut self.idents, &mut self.strings);
            while let Some(token) = inner.next_token()? {
                if token == LineBreak {
                    if self
                        .tokens
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

    pub fn collect(self) -> (Vec<Token>, IdentifierStorage, StringLiteralsStorage) {
        (self.tokens, self.idents, self.strings)
    }
}

type Cur = Option<(usize, char)>;

struct LexerInner<'a> {
    input: &'a str,
    idents: &'a mut IdentifierStorage,
    strings: &'a mut StringLiteralsStorage,
    iter: CharIndices<'a>,
    cur: Cur,
}

impl<'a> LexerInner<'a> {
    fn new(
        input: &'a str,
        idents: &'a mut IdentifierStorage,
        strings: &'a mut StringLiteralsStorage,
    ) -> Self {
        let mut iter = input.char_indices();
        let cur = iter.next();
        Self {
            input,
            idents,
            strings,
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

    fn process_char_bytes(&mut self, x: &str, base: u32) -> Result<char, Error> {
        match u32::from_str_radix(x, base) {
            Ok(res) => match from_u32(res) {
                Some(c) => Ok(c),
                _ => Err(Error::TokenizingError),
            },
            _ => Err(Error::TokenizingError),
        }
    }

    fn process_unicode_value<F>(&mut self, predicate: &F) -> Result<char, Error>
    where
        F: Fn(char) -> bool,
    {
        match self.next() {
            Some((_, '\\')) => match self.next() {
                Some((_, x)) if is_escaped(x) || predicate(x) => map_escaped(x),
                Some((idx_oct, x)) if is_octal_digit(x) => {
                    let idx = self.get_next_chars(2, &is_octal_digit)?;
                    self.process_char_bytes(&self.input[idx_oct..=idx], 8)
                }
                Some((idx_hex, 'x')) => {
                    let idx = self.get_next_chars(2, &is_hex_digit)?;
                    self.process_char_bytes(&self.input[idx_hex + 1..=idx], 16)
                }
                Some((idx_u, 'u')) => {
                    let idx = self.get_next_chars(4, &is_hex_digit)?;
                    self.process_char_bytes(&self.input[idx_u + 1..=idx], 16)
                }
                Some((idx_u_big, 'U')) => {
                    let idx = self.get_next_chars(8, &is_hex_digit)?;
                    if &self.input[idx_u_big + 1..=idx_u_big + 2] != "00" {
                        return Err(Error::TokenizingError);
                    }
                    self.process_char_bytes(&self.input[idx_u_big + 3..=idx], 16)
                }
                _ => return Err(Error::TokenizingError),
            },
            Some((_, x)) if !predicate(x) => Ok(x),
            Some((_, x)) if predicate(x) => Err(Error::LiteralEnd),
            _ => Err(Error::TokenizingError),
        }
    }

    // # Integer Literals

    fn process_decimals(&mut self) -> usize {
        self.skip_chars(is_decimal_digit);
        match self.cur {
            Some((idx, _)) => idx,
            None => self.input.len(),
        }
    }

    fn process_exponent(&mut self) -> Result<usize, Error> {
        match self.next() {
            Some((_, '+')) | Some((_, '-')) => match self.next() {
                Some((_, x)) if is_decimal_digit(x) => Ok(self.process_decimals()),
                _ => Err(Error::TokenizingError),
            },
            Some((_, x)) if is_decimal_digit(x) => Ok(self.process_decimals()),
            _ => Err(Error::TokenizingError),
        }
    }

    fn process_after_dot(&mut self, idx_start: usize) -> Result<Option<Token>, Error> {
        match self.next() {
            Some((_, x)) if is_decimal_digit(x) => {
                let idx = self.process_decimals();
                match self.cur {
                    Some((_, 'e')) | Some((_, 'E')) => {
                        let idx = self.process_exponent()?;
                        Ok(Some(Float(self.input[idx_start..idx].to_string())))
                    },
                    _ => Ok(Some(Float(self.input[idx_start..idx].to_string())))
                }
            },
            Some((_, 'e')) | Some((_, 'E')) => {
                let idx = self.process_exponent()?;
                Ok(Some(Float(self.input[idx_start..idx].to_string())))
            },
            Some((idx, _)) => Ok(Some(Float(self.input[idx_start..idx].to_string()))),
            _ => Ok(Some(Float(self.input[idx_start..].to_string()))),
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
            '\'' => {
                return match self.process_unicode_value(&is_rune_escaped) {
                    Ok(c) => self.process_end_rune(c),
                    _ => Err(Error::TokenizingError),
                }
            }
            _ => (),
        }

        // # String Literal

        match cur_char {
            '\"' => {
                let mut string = String::new();
                loop {
                    match self.process_unicode_value(&is_string_escaped) {
                        Ok(c) => string.push(c),
                        Err(Error::LiteralEnd) => {
                            consume!(InterpretedString(
                                self.strings.create_interpreted_string(&string)
                            ))
                        }
                        _ => return Err(Error::TokenizingError),
                    }
                }
            }
            '`' => {
                let mut string = String::new();
                loop {
                    match self.next() {
                        Some((_, '`')) => {
                            consume!(RawString(
                                self.strings.create_interpreted_string(&string)
                            ))
                        }
                        Some((_, x)) => string.push(x),
                        _ => return Err(Error::TokenizingError),
                    }
                }
            }
            _ => (),
        }

        // # Number Literals

        match cur_char {
            '1'..='9' => {
                self.adv();
                let idx = self.process_decimals();
                match self.cur {
                    Some((_, '.')) => return self.process_after_dot(idx_start),
                    Some((_, 'e')) | Some((_, 'E')) => {
                        let idx = self.process_exponent()?;
                        return Ok(Some(Float(self.input[idx_start..idx].to_string())))
                    }
                    _ => return Ok(Some(Decimal(self.input[idx_start..idx].to_string())))
                }
            }
            '0' => {
                match self.next() {
                    Some((_, 'x')) | Some((_, 'X')) => {
                        self.adv();
                        self.skip_chars(is_hex_digit);
                        if let Some((idx, _)) = self.cur {
                            return Ok(Some(Hex(self.input[idx_start + 2..idx].to_string())));
                        }
                        return Ok(Some(Hex(self.input[idx_start + 2..].to_string())));
                    },
                    Some((_, '.')) => return self.process_after_dot(idx_start),
                    _ => {
                        self.skip_chars(is_octal_digit);
                        match self.cur {
                            Some((_, '.')) => return self.process_after_dot(idx_start),
                            Some((idx, _)) => return Ok(Some(Octal(self.input[idx_start + 1..idx].to_string()))),
                            _ => return Ok(Some(Octal(self.input[idx_start + 1..].to_string()))),
                        }
                    }
                }
            }
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
                Some((_, x)) if is_decimal_digit(x) => {
                    let idx = self.process_decimals();
                    match self.cur {
                        Some((_, 'e')) | Some((_, 'E')) => {
                            let idx = self.process_exponent()?;
                            return Ok(Some(Float(self.input[idx_start..idx].to_string())))
                        },
                        _ => return Ok(Some(Float(self.input[idx_start..idx].to_string())))
                    }
                },
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
        for _ in 0..n - 1 {
            if self.get_char(predicate).is_err() {
                return Err(Error::TokenizingError);
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

fn is_decimal_digit(c: char) -> bool {
    (c >= '0' && c <= '9')
}

fn is_hex_digit(c: char) -> bool {
    (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')
}

fn is_octal_digit(c: char) -> bool {
    (c >= '0' && c <= '7')
}

fn is_escaped(c: char) -> bool {
    (c == 'a')
        || (c == 'b')
        || (c == 'f')
        || (c == 'n')
        || (c == 'r')
        || (c == 't')
        || (c == 'v')
        || (c == '\\')
}

fn is_rune_escaped(c: char) -> bool {
    c == '\''
}

fn is_string_escaped(c: char) -> bool {
    c == '\"'
}

fn map_escaped(c: char) -> Result<char, Error> {
    match c {
        'a' => Ok('\u{0007}'),
        'b' => Ok('\u{0008}'),
        'f' => Ok('\u{000C}'),
        'n' => Ok('\u{000A}'),
        'r' => Ok('\u{000D}'),
        't' => Ok('\u{0009}'),
        'v' => Ok('\u{000B}'),
        '\\' => Ok('\u{005C}'),
        '\'' => Ok('\u{0027}'),
        '\"' => Ok('\u{0022}'),
        _ => Err(Error::TokenizingError),
    }
}

/// Whether the token is automatically followed by an implicit `;`
/// if it's the last token on the line
fn implicit_semicolon(t: &Token) -> bool {
    match *t {
        Kw(kw) => {
            kw == Keyword::Break
                || kw == Keyword::Continue
                || kw == Keyword::Fallthrough
                || kw == Keyword::Return
        }
        Ident(_) | Punc(Right(_)) | Punc(Increment) | Punc(Decrement) => true,
        InterpretedString(_) => true,
        _ => false,
    }
}
