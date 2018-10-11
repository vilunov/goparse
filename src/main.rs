#[macro_use]
extern crate nom;
extern crate regex;

mod rules;
mod ast;
mod types;
mod lexer;

fn main() {
    let literal = "\\00";

    println!("{:?}", rules::literals::literal_parse(literal));
}

#[cfg(test)]
mod tests;
