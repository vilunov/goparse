#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Literal {
    Decimal(String),
    Float(String),
    Imaginary(String),
    Character(String),
    String(String),
}
