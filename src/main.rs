#[macro_use]
extern crate nom;
extern crate regex;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use std::path::{Path, PathBuf};
use std::fs::{read_to_string, write, read_dir, create_dir};

pub mod ast;
pub mod lexer;
pub mod syntax;
pub mod types;

const INPUT_FOLDER: &'static str = "input";
const OUTPUT_FOLDER: &'static str = "output";

#[derive(Serialize)]
struct CompleteOutput {
    ast: ast::Program,
    idents: Vec<String>,
    string_literals: Vec<String>,
}

fn file_paths() -> impl Iterator<Item=PathBuf> {
    read_dir(INPUT_FOLDER).unwrap()
        .flatten()
        .filter(|i| i.path()
            .extension()
            .and_then(|i| i.to_str()) == Some("go"))
        .map(|i| i.path())
}

fn read_files(paths: impl Iterator<Item=PathBuf>) -> impl Iterator<Item=(PathBuf, String)> {
    paths.flat_map(|path| match read_to_string(&path) {
        Ok(v) => Some((path, v)),
        Err(e) => {
            eprintln!("Read error: {}", e);
            None
        }
    })
}

fn write_file(path: &Path, input: &str) -> Result<(), Box<std::error::Error>> {
    let (tokens, idents, strings): (Vec<types::Token>, _, _) = lexer::Lexer::new().tokenize(input)?.collect();
    let ast = {
        match syntax::program(tokens.as_slice())
            .map_err(|_| types::Error::SyntaxParsingError)? {
            (&[], v) => v,
            _ => return Err(Box::new(types::Error::SyntaxParsingError)),
        }
    };
    let path: &Path = path.strip_prefix(INPUT_FOLDER)?;
    let path = Path::new(OUTPUT_FOLDER).join(path);
    let s = CompleteOutput {
        ast,
        idents: idents.identifiers(),
        string_literals: strings.literals(),
    };
    let output = serde_json::to_string_pretty(&s)?;
    Ok(write(path, output)?)
}

fn main() {
    let _ = create_dir(OUTPUT_FOLDER);
    for (path, input) in read_files(file_paths()) {
        println!("{:?} {:?}", path, write_file(&path, &input));
    }
}

#[cfg(test)]
mod tests;
