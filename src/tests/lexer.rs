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
                InterpretedString(idx) | RawString(idx) => {
                    let result = &literals.interpreted_strings[idx];
                    assert_eq!(result.clone(), $expected);
                }
                _ => panic!("Failed"),
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

// # Positive Tests
test_literal!(string_interpreted_simple_1, r####""kek""####, "kek");
test_literal!(
    string_interpreted_simple_2,
    r####""privet kek""####,
    "privet kek"
);
test_literal!(
    string_interpreted_escaped_quotes_1,
    r####""privet \"kek\"""####,
    "privet \"kek\""
);
test_literal!(
    string_interpreted_escaped_quotes_2,
    r####""privet \\\"kek\\\"""####,
    "privet \\\"kek\\\""
);
test_literal!(string_raw_simple, r####"`kek`"####, "kek");
test_literal!(string_raw_newline, "`kek\n`", "kek\n");

// # Negative Tests
test_ne!(string_interpreted_unescaped, r####""privet " kek""####);
test_ne!(string_interpreted_unclosed, r####""privet"####);

// # Integer Literals
test_simple!(decimal_simple, r"1337", vec![Decimal("1337".to_string())]);
test_simple!(octal_simple, r"037", vec![Octal("37".to_string())]);
test_simple!(hex_simple, r"0x37", vec![Hex("37".to_string())]);
test_simple!(float_simple, r"0.5", vec![Float("0.5".to_string())]);

test_simple!(float_zero_dot, r"0.", vec![Float("0.".to_string())]);
test_simple!(float_simple_2, r"72.40", vec![Float("72.40".to_string())]);
test_simple!(float_simple_zero, r"072.40", vec![Float("072.40".to_string())]);
test_simple!(float_exp_1, r"1.e+0", vec![Float("1.e+0".to_string())]);
test_simple!(float_exp_2, r"6.67428e-11", vec![Float("6.67428e-11".to_string())]);
test_simple!(float_exp_3, r"1E6", vec![Float("1E6".to_string())]);
test_simple!(float_no_zero_1, r".25", vec![Float(".25".to_string())]);
test_simple!(float_no_zero_2, r".12345E+5", vec![Float(".12345E+5".to_string())]);









