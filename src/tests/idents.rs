use std::str::from_utf8;

use ast::Identifier;
use rules::ident;

const EMPTY: &'static str = "";


macro_rules! test {
    (+$name: ident, $input: expr) => {
        #[test]
        fn $name() {
            let expect = Identifier($input.to_string());
            assert_eq!(Ok((EMPTY, expect)), ident($input));
        }
    };
    (-$name: ident, $input: expr) => {
        #[test]
        fn $name() {
            let expect = Identifier($input.to_string());
            assert!(ident($input).is_err())
        }
    }
}

test!(+pos1, "пр1ивет");
test!(+pos2, "_1");
test!(-neg1, "1пр1ивет");
test!(-neg2, "1");
test!(-neg3, "");
test!(-neg4, " 1");
test!(-neg5, ";");
