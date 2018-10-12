#[macro_use]
extern crate nom;
extern crate regex;

pub mod ast;
pub mod lexer;
pub mod types;
pub mod syntax;

fn main() {
    let (res, idents) = lexer::Lexer::new()
        .tokenize(
            r"package kekistan
            import . priv
            ;")
        .unwrap()
        .collect();
    println!("{:?}\n{:?}", res, idents);
    let program = syntax::program(&res[..]).unwrap();
    println!("{:?}", program);
}

#[cfg(test)]
mod tests;
