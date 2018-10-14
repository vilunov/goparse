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

            const shrek, kek = ^+1[:] + !1 && 2, 1[:1:2], i(i)(i), i(i, i)
            const shrek, kek int = int(32).kek, aye.smert[1[1]]
            const shrek; //dsadsaads

            const (
                autism, sdada chan<- int = 32
                is
                good <-chan chan int = 1
                for chan <- chan int= 4;
                your;
                health;
                d
            )

            type (
                ebal = int
                kek = string
                net = [1]string
                kek = [][1]string
                shrek = map[string]int
                intrfc = interface {
                    kek
                    shrek (i int) bool
                }
                some = struct {
                    intrfc
                    *net
                    kek "tag2";
                    a, b int "tag"
                }
            )

            var kek = 10

            func blea(a int) bool
            func (p *Point) blea(a int) bool
            "#,
        ).unwrap()
        .collect();
    let idents = idents.identifiers();
    let string_literals = string_literals.literals();
    println!("{:?}\n{:?}", &idents, &string_literals);
    let (remainder, ast) = syntax::program(&res[..]).unwrap();
    assert_eq!(remainder, &[]);
    let j = CompleteOutput {
        ast,
        idents,
        string_literals,
    };
    let i = serde_json::to_string_pretty(&j).unwrap();
    write(OUTPUT, i).unwrap();
}

#[cfg(test)]
mod tests;
