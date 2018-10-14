use lexer;
use types::Literal::*;
use types::Token::*;
use types::Token;

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
                Lit(InterpretedString(idx)) | Lit(RawString(idx)) => {
                    let result = &literals.literals[idx];
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
test_simple!(rune_simple, r"'l'", vec![Lit(Rune('l'))]);
test_simple!(rune_big_u, r"'\U00101234'", vec![Lit(Rune('\u{101234}'))]);
test_simple!(rune_octal, r"'\000'", vec![Lit(Rune('\u{0}'))]);
test_simple!(rune_hex, r"'\x07'", vec![Lit(Rune('\u{7}'))]);
test_simple!(rune_escaped_1, r"'\t'", vec![Lit(Rune('\t'))]);
test_simple!(rune_escaped_2, r"'\a'", vec![Lit(Rune('\u{7}'))]);
test_simple!(rune_single_quote, r"'\''", vec![Lit(Rune('\''))]);
test_simple!(
    rune_multiple,
    r"'\x07''\000'",
    vec![Lit(Rune('\u{7}')), Lit(Rune('\u{0}'))]
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
test_simple!(
    decimal_simple,
    r"1337",
    vec![Lit(Decimal("1337".to_string()))]
);
test_simple!(octal_simple, r"037", vec![Lit(Octal("37".to_string()))]);
test_simple!(hex_simple, r"0x37", vec![Lit(Hex("37".to_string()))]);
test_simple!(float_simple, r"0.5", vec![Lit(Float("0.5".to_string()))]);

test_simple!(float_zero_dot, r"0.", vec![Lit(Float("0.".to_string()))]);
test_simple!(
    float_simple_2,
    r"72.40",
    vec![Lit(Float("72.40".to_string()))]
);
test_simple!(
    float_simple_zero_1,
    r"072.40",
    vec![Lit(Float("072.40".to_string()))]
);
test_simple!(
    float_simple_zero_2,
    r"0729.40",
    vec![Lit(Float("0729.40".to_string()))]
);
test_simple!(
    float_simple_zero_3,
    r"0729.",
    vec![Lit(Float("0729.".to_string()))]
);
test_simple!(float_exp_1, r"1.e+0", vec![Lit(Float("1.e+0".to_string()))]);
test_simple!(
    float_exp_2,
    r"6.67428e-11",
    vec![Lit(Float("6.67428e-11".to_string()))]
);
test_simple!(float_exp_3, r"1E6", vec![Lit(Float("1E6".to_string()))]);
test_simple!(float_no_zero_1, r".25", vec![Lit(Float(".25".to_string()))]);
test_simple!(
    float_no_zero_2,
    r".12345E+5",
    vec![Lit(Float(".12345E+5".to_string()))]
);
test_simple!(
    imaginary_simple_1,
    r"1i",
    vec![Lit(Imaginary("1i".to_string()))]
);
test_simple!(
    imaginary_simple_2,
    r"072.40i",
    vec![Lit(Imaginary("072.40i".to_string()))]
);
test_simple!(
    imaginary_simple_3,
    r".40i",
    vec![Lit(Imaginary(".40i".to_string()))]
);
test_simple!(
    imaginary_simple_4,
    r"0.i",
    vec![Lit(Imaginary("0.i".to_string()))]
);
test_simple!(
    imaginary_simple_5,
    r"1.e+0i",
    vec![Lit(Imaginary("1.e+0i".to_string()))]
);

test_ne!(not_valid_decimal, r"0729");

test_simple!(
    comment_single_line,
    r"//Drums, the never ending drums\n",
    Vec::<Token>::new()
);

test_simple!(
    comment_multi_line_1,
    r"/*Drums, the never ending drums\n*/",
    Vec::<Token>::new()
);

test_simple!(
    comment_multi_line_2,
    r"/*Drums, the never ending drums\n*/20",
    vec![Lit(Decimal("20".to_string()))]
);

test_simple!(
    comment_multi_line_3,
    r"/*Drums, the never ending drums*/20",
    vec![Lit(Decimal("20".to_string()))]
);

test_simple!(
    comment_multi_line_4,
    r"/*Drums, the never ****** ending drums*/20",
    vec![Lit(Decimal("20".to_string()))]
);
