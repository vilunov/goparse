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
pub enum BinaryOp {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `%`
    Modulus,
    /// `&`
    And,
    /// `|`
    Or,
    /// `^`
    Hat,
    /// `<<`
    LeftShift,
    /// `>>`
    RightShift,
    /// `&^`
    AndHat,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum PairedToken {
    /// `(` or `)`
    Parenthesis,
    /// `[` or `]`
    Bracket,
    /// `{` or `}`
    Brace,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Punctuation {
    /// `&&`
    DoubleAnd,
    /// `||`
    DoubleOr,
    /// `<-`
    LeftArrow,
    /// `++`
    Increment,
    /// `--`
    Decrement,
    /// `==`
    Equals,
    /// `<=`
    Le,
    /// `<`
    Lt,
    /// `>=`
    Ge,
    /// `>`
    Gt,
    /// `!=`
    Ne,
    /// `!`
    Bang,
    /// `=`
    Assign,
    /// `:=`
    ColonAssign,
    /// One of these: `(`, `{`, `[`
    Left(PairedToken),
    /// One of these: `)`, `}`, `]`
    Right(PairedToken),
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `...`
    DotDotDot,
    /// `:`
    Colon,
    /// `;`
    Semicolon,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Token {
    /// Keyword - cannot be used as an identifier
    Kw(Keyword),
    /// Identifier - name of a variable, structure, function, etc
    Ident(usize),
    /// Binary operator that could be placed between two values, e.g.: `1 + 2`.
    /// Does not include `&&` and `||`.
    BinOp(BinaryOp),
    /// Binary operator combined with assignment, e.g. `i += 5`
    BinOpAssign(BinaryOp),
    /// Other operators, symbols and punctuation
    Punc(Punctuation),
    /// Decimal integer literal, i.e. `1972`
    Decimal(String),
    /// Octal integer literal, i.e. `031337`
    Octal(String),
    /// Hexadecimal integer literal, i.e. `0xDEADBEEF`
    Hex(String),
    /// Rune (Character) literal
    Rune(char),
    #[doc(hidden)] LineBreak,
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
