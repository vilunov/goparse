#[macro_use]
extern crate nom;
extern crate regex;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use std::fs::{read_to_string, write};

pub mod ast;
pub mod lexer;
pub mod syntax;
pub mod types;

const OUTPUT: &'static str = "out.txt";

#[derive(Serialize)]
struct CompleteOutput {
    ast: ast::Program,
    idents: Vec<String>,
    string_literals: Vec<String>,
}

fn main() {
    let (res, idents, string_literals) = lexer::Lexer::new()
        .tokenize(
            r#"package kekistan
            import . "fmt"
            import (
                "fmt2"
                kek "std/fmt3"
                . "shrek"
            )
            "#,
        ).unwrap()
        .collect();
    let idents = idents.identifiers();
    let string_literals = string_literals.literals();
    let (_, ast) = syntax::program(&res[..]).unwrap();
    let j = CompleteOutput { ast, idents, string_literals };
    let i = serde_json::to_string_pretty(&j).unwrap();
    write(OUTPUT, i).unwrap();
}

#[cfg(test)]
mod tests;
