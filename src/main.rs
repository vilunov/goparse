#[macro_use]
extern crate nom;
extern crate regex;

mod rules;
mod ast;

fn main() {
    let literal = b"1488 test";

    println!("{:?}", rules::literals::literal_decimal(literal));
}

#[cfg(test)]
mod tests;
