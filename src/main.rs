#[macro_use]
extern crate nom;
extern crate regex;

pub mod ast;
pub mod lexer;
pub mod rules;
pub mod types;

fn main() {
    let (res, idents) = lexer::Lexer::new()
        .tokenize("privet _kak de_1la break privet 12 0x12F+=0")
        .unwrap()
        .collect();
    println!("{:?}\n{:?}", res, idents);
    println!("{}", std::mem::size_of::<String>());
}

#[cfg(test)]
mod tests;
