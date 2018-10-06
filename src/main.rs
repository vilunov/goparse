#[macro_use]
extern crate nom;
extern crate regex;

mod rules;
mod ast;

fn main() {
    let literal = "\\00";

    println!("{:?}", rules::literals::literal_parse(literal));
}

#[cfg(test)]
mod tests;
