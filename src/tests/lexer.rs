use types::Token::*;
use lexer;

macro_rules! test {
    ($name: ident, $input: expr, $expected: expr) => {
        #[test]
        fn $name() {
            let (parsed, _) = lexer::Lexer::new().tokenize($input).unwrap().collect();
            assert_eq!($expected, parsed)
        }
    };
}

test!(rune_simple, "\'l\'", vec![Rune('l')]);
test!(rune_big_u, "\'\\U00101234\'", vec![Rune('\u{101234}')]);
test!(rune_octal, "\'\\000\'", vec![Rune('\0')]);
