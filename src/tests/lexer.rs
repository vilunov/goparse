use lexer;
use types::Token::*;

macro_rules! test_simple {
    ($name: ident, $input: expr, $expected: expr) => {
        #[test]
        fn $name() {
            let (parsed, _, _) = lexer::Lexer::new().tokenize($input).unwrap().collect();
            assert_eq!($expected, parsed)
        }
    };
}

macro_rules! test_literal {
    ($name: ident, $input: expr, $expected: expr) => {
        #[test]
        fn $name() {
            let (parsed, _, literals) = lexer::Lexer::new().tokenize($input).unwrap().collect();
            match parsed[0] {
                InterpretedString(idx) => {
                    let result = &literals.interpreted_strings[idx];
                    assert_eq!(result.clone(), $expected);
                },
                _ => panic!("Failed")
            };

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

// # Runes

// # Positive Tests
test_simple!(rune_simple, r"'l'", vec![Rune('l')]);
test_simple!(rune_big_u, r"'\U00101234'", vec![Rune('\u{101234}')]);
test_simple!(rune_octal, r"'\000'", vec![Rune('\u{0}')]);
test_simple!(rune_hex, r"'\x07'", vec![Rune('\u{7}')]);
test_simple!(rune_escaped_1, r"'\t'", vec![Rune('\t')]);
test_simple!(rune_escaped_2, r"'\a'", vec![Rune('\u{7}')]);
test_simple!(rune_single_quote, r"'\''", vec![Rune('\'')]);
test_simple!(
    rune_multiple,
    r"'\x07''\000'",
    vec![Rune('\u{7}'), Rune('\u{0}')]
);

// # Negative Tests
test_ne!(rune_not_valid_big_u, r"'\U00110000'");
test_ne!(rune_not_valid_u, r"'\uDFFF'");
test_ne!(rune_not_valid_octal, r"'\0'");
test_ne!(rune_not_valid_char, r"'aa'");
test_ne!(rune_not_valid_hex, r"'\xa'");
test_ne!(rune_not_valid_single_quote, r"'''");

// # String Literals
test_literal!(
    string_interpreted_simple_1,
    r####""kek""####,
    vec!['k', 'e', 'k']
);
test_literal!(
    string_interpreted_simple_2,
    r####""privet kek""####,
    vec!['p', 'r', 'i', 'v', 'e', 't', ' ', 'k', 'e', 'k']
);
