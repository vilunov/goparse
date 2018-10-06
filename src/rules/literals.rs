use ast::literals::Literal;
// Decimal Literals

const DECIMAL_LITERAL: &'static str = "[1-9][0-9]*";
const OCTAL_LITERAL: &'static str = "0[0-7]*";
const HEX_LITERAL: &'static str = "0(x|X)[0-9A-Fa-f]+";

macro_rules! decimal_regexp {
    () => {{
        let regexp = format!(r"^({decimal}|{hex}|{octal})",
                                decimal = DECIMAL_LITERAL,
                                octal = OCTAL_LITERAL,
                                hex = HEX_LITERAL);
        println!("Decimal REG: {}", &regexp);
        regexp
    }};
}

// Float Literals

const EXPONENT: &'static str = "(e|E)(\\+|\\-)?[0-9]+";
const DECIMALS: &'static str = "[0-9]+";

macro_rules! float_regexp {
    () => {{
        let regexp = format!(r"^{decs}\.[0-9]*({exp})?|{decs}{exp}|\.{decs}({exp})?",
                                decs = DECIMALS,
                                exp = EXPONENT);
        println!("Float REG: {}", &regexp);
        regexp
    }};
}

// Imaginary Literals

macro_rules! imaginary_regexp {
    () => {{
        let regexp = format!(r"({decimals}|{floats})i",
                        decimals = decimal_regexp!(),
                        floats = float_regexp!());
        println!("Imaginary REG: {}", &regexp);
        regexp
    }};
}

// Char Literals

const ESCAPED_CHARS: &'static str = "\\\\(a|b|f|n|r|t|v|\\\\|\"\"|\')";
const OCTAL_BYTE_VALUE: &'static str = "\\\\[0-7][0-7][0-7]";
const HEX_BYTE_VALUE: &'static str = "\\\\x[0-9A-Fa-f][0-9A-Fa-f]";
const LITTLE_U_VALUE: &'static str = "\\\\u[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]";
const BIG_U_VALUE: &'static str = "\\\\U[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\
                           [0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]";

macro_rules! rune_regexp {
    () => {{
        let unicode_value = format!(r".|{little_u}|{big_u}|{escaped}",
                                        little_u = LITTLE_U_VALUE,
                                        big_u = BIG_U_VALUE,
                                        escaped = ESCAPED_CHARS);
        let byte_value = format!(r"{octal}|{hex}",
                                    octal = OCTAL_BYTE_VALUE,
                                    hex = HEX_BYTE_VALUE);
        let regexp = format!("^\'({unicode_value}|{byte_value})\'",
                                    unicode_value = unicode_value,
                                    byte_value = byte_value);
        println!("Character REG: {}", regexp);
        regexp
    }};
}

const RAW_STRING: &'static str = "`(.|\\n|\\r|\\r\\n)*`";

macro_rules! string_regexp {
    () => {{
        let unicode_value = format!(r".|{little_u}|{big_u}|{escaped}",
                                        little_u = LITTLE_U_VALUE,
                                        big_u = BIG_U_VALUE,
                                        escaped = ESCAPED_CHARS);
        let byte_value = format!(r"{octal}|{hex}",
                                    octal = OCTAL_BYTE_VALUE,
                                    hex = HEX_BYTE_VALUE);
        let interpreted_string = format!("\"({unicode_value}|{byte_value})*\"",
                                    unicode_value = unicode_value,
                                    byte_value = byte_value);
        let regexp = format!(r"^({raw}|{interpreted})", raw = RAW_STRING, interpreted = interpreted_string);

        println!("String REG: {}", regexp);
        regexp
    }};
}

named!(pub literal_parse<&str, Literal>, alt!(
    re_find!(rune_regexp!().as_str())        => {|x: &str| Literal::Character(x.to_string()) } |
    re_find!(string_regexp!().as_str())      => {|x: &str| Literal::String(x.replace("\r", "as")) } |
    re_find!(imaginary_regexp!().as_str())   => {|x: &str| Literal::Imaginary(x.to_string()) } |
    re_find!(float_regexp!().as_str())       => {|x: &str| Literal::Float(x.to_string()) } |
    re_find!(decimal_regexp!().as_str())     => {|x: &str| Literal::Decimal(x.to_string()) }
));
