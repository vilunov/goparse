const DECIMAL_LITERAL: &str = "^[1-9][0-9]*";
const OCTAL_LITERAL: &str = "^0[0-7]*";
const HEX_LITERAL: &str = "^0(x|X)[0-9A-Fa-f]+";

macro_rules! decimal_regexp {
    () => (format!("{decimal}|{hex}|{octal}",
                        decimal = DECIMAL_LITERAL,
                        octal = OCTAL_LITERAL,
                        hex = HEX_LITERAL).as_str())
}

named!(pub literal_decimal<&[u8], &[u8]>,
    re_bytes_find!(decimal_regexp!())
);

const EXPONENT: &str = "(e|E)(\\+|\\-)?[0-9]+";
const DECIMALS: &str = "[0-9]+";

macro_rules! float_regexp {
    () => (format!("{decs}.({decs})?({exp})?|{decs}{exp}|.{decs}({exp})?",
                        decs = DECIMALS,
                        exp = EXPONENT).as_str())
}

named!(pub literal_float<&[u8], &[u8]>,
    re_bytes_find!(float_regexp!())
);

macro_rules! imaginary_regexp {
    () => (format!("({decimals}|{floats})i",
                        decimals = decimal_regexp!(),
                        floats = float_regexp!()).as_str())
}

named!(pub literal_imaginary<&[u8], &[u8]>,
    re_bytes_find!(imaginary_regexp!())
);
