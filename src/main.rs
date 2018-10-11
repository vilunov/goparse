#[macro_use]
extern crate nom;
extern crate regex;

pub mod ast;
pub mod lexer;
pub mod rules;
pub mod types;

fn main() {
    let (res, idents) = lexer::Lexer::new()
        .tokenize("privet _kak de_1la break privet")
        .unwrap()
        .collect();
    println!("{:?}\n{:?}", res, idents);
}

#[cfg(test)]
mod tests;
