enum Literal {
    IntegerDecimal(String),
}

named!(pub literal_decimal<&[u8], &[u8]>,
    re_bytes_find!(r"^[1-9][0-9]*")
);
