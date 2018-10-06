use rules::literals::*;

macro_rules! test {
    ($name: ident, $input: expr, $parser: ident) => {
        #[test]
        fn $name() {
            let to_parse = $input;
            let parsed = $parser(to_parse).unwrap().1;
            assert_eq!(to_parse, parsed)
        }
    }
}

// Decimal Literals
test!(decimal_simple, b"1488",                             literal_decimal);
test!(decimal_long,   b"10000000000000000000000000000020", literal_decimal);
test!(decimal_octal,  b"0600",                             literal_decimal);
test!(decimal_hex,    b"0xBadFace",                        literal_decimal);

// Float Literals
test!(float_without_1, b"0.",          literal_float);
test!(float_without_2, b".25",         literal_float);
test!(float_without_3, b".12345E+5",   literal_float);
test!(float_simple,    b"72.40",       literal_float);
test!(float_simple_2,  b"2.71828",     literal_float);
test!(float_with_zero, b"072.40",      literal_float);
test!(float_exp,       b"1.e+0",       literal_float);
test!(float_exp_2,     b"6.67428e-11", literal_float);
test!(float_exp_3,     b"1E6",         literal_float);

// Imaginary Literals
test!(imaginary_decimal,    b"0i",           literal_imaginary);
test!(imaginary_decimal_2,  b"011i",         literal_imaginary);
test!(imaginary_float,      b"2.71828i",     literal_imaginary);
test!(imaginary_float_2,    b"6.67428e-11i", literal_imaginary);
test!(imaginary_float_3,    b".12345E+5i",   literal_imaginary);
