use lexer;
use types::Token::*;

macro_rules! test {
    ($name: ident, $input: expr, $expected: expr) => {
        #[test]
        fn $name() {
            let (parsed, _) = lexer::Lexer::new().tokenize($input).unwrap().collect();
            assert_eq!($expected, parsed)
        }
    };
}

macro_rules! test_ne {
    ($name: ident, $input: expr) => {
        #[test]
        fn $name() {
            let unparsed = lexer::Lexer::new().tokenize($input);
            assert!(unparsed.is_err())
        }
    };
}

test!(rune_simple, r"'l'", vec![Rune('l')]);
test!(rune_big_u, r"'\U00101234'", vec![Rune('\u{101234}')]);
test!(rune_octal, r"'\000'", vec![Rune('\u{0}')]);
test!(rune_hex, r"'\x07'", vec![Rune('\u{7}')]);
test!(
    rune_multiple,
    r"'\x07''\000'",
    vec![Rune('\u{7}'), Rune('\u{0}')]
);

test_ne!(rune_not_valid_big_u, r"'\U00110000'");
test_ne!(rune_not_valid_u, r"'\uDFFF'");
test_ne!(rune_not_valid_octal, r"'\0'");
test_ne!(rune_not_valid_char, r"'aa'");
test_ne!(rune_not_valid_hex, r"'\xa'");
