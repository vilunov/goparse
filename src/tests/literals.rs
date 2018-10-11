use ast::literals::Literal::*;
use rules::literals::*;

macro_rules! test {
    ($name: ident, $input: expr, $expect: ident) => {
        #[test]
        fn $name() {
            let to_parse = $input;
            let parsed = literal_parse(to_parse).unwrap().1;
            assert_eq!($expect(to_parse.to_string()), parsed)
        }
    };
}

macro_rules! test_err {
    ($name: ident, $input: expr) => {
        #[test]
        fn $name() {
            let to_parse = $input;
            let parsed = literal_parse(to_parse);
            println!("{:?}", parsed);
            assert!(parsed.is_err())
        }
    };
}

// Decimal Literals
test!(decimal_simple, "1488", Decimal);
test!(decimal_long, "10000000000000000000000000000020", Decimal);
test!(decimal_octal, "0600", Decimal);
test!(decimal_hex, "0xBadFace", Decimal);

// Float Literals
test!(float_without_1, "0.", Float);
test!(float_without_2, ".25", Float);
test!(float_without_3, ".12345E+5", Float);
test!(float_simple, "72.40", Float);
test!(float_simple_2, "2.71828", Float);
test!(float_with_zero, "072.40", Float);
test!(float_exp, "1.e+0", Float);
test!(float_exp_2, "6.67428e-11", Float);
test!(float_exp_3, "1E6", Float);

// Imaginary Literals
test!(imaginary_decimal, "0i", Imaginary);
test!(imaginary_decimal_2, "011i", Imaginary);
test!(imaginary_float, "2.71828i", Imaginary);
test!(imaginary_float_2, "6.67428e-11i", Imaginary);
test!(imaginary_float_3, ".12345E+5i", Imaginary);

// Char Literals
test!(char, "'a'", Character);
test!(char_escaped, "'\\t'", Character);
test!(char_octal, "'\\000'", Character);
test!(char_hex, "'\\x07'", Character);
test!(char_little_u, "'\\u12e4'", Character);
test!(char_big_u, "'\\U00101234'", Character);
test!(char_escaped_quote, "'\\\''", Character);

test_err!(char_single_quote, "\'");
test_err!(char_double, "'aa'");
test_err!(char_few_hex, "'\\xa'");
test_err!(char_few_octal, "'\\0'");

// TODO: нормальный фикс Unicode символов
//test_err!(char_surrogate_half, "'\\uDFFF'", literal_char);
//test_err!(char_invalid_unicode, "'\\U00110000'", literal_char);

// String Literals
test!(string_raw, "`Eto vanya is proshlogo. Nikita ebalnusa`", String);
test!(string_interpreted, "\"Nikita ne bei za test snizu\"", String);
test!(string_interpreted_inner, "\"\\\"\"", String);

test_err!(string_interpreted_newline, "\"\n\"");
#[test]
fn string_raw_newlines() {
    let to_parse = "`Here \n \r \r\n`";
    let parsed = match literal_parse(to_parse).unwrap().1 {
        String(x) => x,
        _ => panic!("Not String"),
    };
    assert_eq!(to_parse.replace("\r", ""), parsed)
}
