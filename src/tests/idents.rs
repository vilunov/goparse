use std::str::from_utf8;

use ast::Identifier;
use rules::ident;

const SPACE: &'static str = " ";


macro_rules! test_pos {
    ($name: ident, $input: expr) => {
        #[test]
        fn $name() {
            let input = format!("{} ", $input);
            let result = ident(&input);
            let expect = Identifier($input.to_string());
            assert_eq!(Ok((SPACE, expect)), result);
        }
    }
}

test_pos!(test1, "1test");
