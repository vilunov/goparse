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
            "chan" => Some(Keyword::Chan),
            "const" => Some(Keyword::Const),
            "import" => Some(Keyword::Import),
            "interface" => Some(Keyword::Interface),
            "package" => Some(Keyword::Package),
            "func" => Some(Keyword::Func),
            "map" => Some(Keyword::Map),
            "struct" => Some(Keyword::Struct),
            "type" => Some(Keyword::Type),
            "var" => Some(Keyword::Var),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Serialize)]
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

#[derive(Clone, Eq, PartialEq, Debug, Serialize)]
pub enum Literal {
    /// Decimal integer literal, i.e. `1972`
    Decimal(String),
    /// Octal integer literal, i.e. `031337`
    Octal(String),
    /// Hexadecimal integer literal, i.e. `0xDEADBEEF`
    Hex(String),
    /// Float literal, i.e. '0.5'
    Float(String),
    /// Imaginary literal, i.e. '0.5i'
    Imaginary(String),
    /// Rune (Character) literal, i.e. 'k'
    Rune(char),
    /// Interpreted String literal, i.e. "mp4 footage"
    InterpretedString(usize),
    /// RawString literal, i.e. `RAW footage`
    RawString(usize),
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
    /// Literal - constant numbers, strings
    Lit(Literal),
    #[doc(hidden)]
    LineBreak,
}

#[derive(Clone, Debug, Serialize)]
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

    pub fn identifiers(self) -> Vec<String> {
        self.identifiers
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct StringLiteralsStorage {
    pub literals: Vec<String>,
}

impl StringLiteralsStorage {
    pub fn new() -> Self {
        StringLiteralsStorage { literals: vec![] }
    }

    pub fn create_interpreted_string(&mut self, string: &str) -> usize {
        self.literals.push(string.to_string());
        self.literals.len() - 1
    }

    pub fn literals(self) -> Vec<String> {
        self.literals
    }
}

#[derive(Clone, Debug)]
pub enum Error {
    TokenizingError,
    LiteralEnd,
}
