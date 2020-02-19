//use crate::{
//generation::{RenderReturn, RenderType},
//interpretable::{InputType::Filename, Interpretable},
//};
//use weresocool_ast::Term;
//use weresocool_error::Error;

///// Function for testing a transformation .socool files are equivilant.
///// ```
///// # use weresocool::testing::expect::{expect};
///// fn test_expect_eq() {
/////     expect("mocks/input.socool");
///// }
///// ```

//fn expect_eq_internal(input: &str) -> Result<(), Error> {
//let input_render_return = Filename(input).make(RenderType::NfBasisAndTable)?;

//let (nf, _basis, defs) = match input_render_return {
//RenderReturn::NfBasisAndTable(nf, basis, defs) => (nf, basis, defs),
//_ => panic!(),
//};

//let expect_term = defs
//.terms
//.get("expect")
//.unwrap_or_else(|| panic!("\n\n  No expect in: \n  {}\n\n", input));

//let expected = match expect_term {
//Term::Nf(nf) => nf,
//_ => unimplemented!(),
//};

//println!("\n\n\t{}\n\n", input);
//assert_eq!(nf, *expected);
//Ok(())
//}

//pub fn expect(input: &str) {
//let result = expect_eq_internal(input);
//match result {
//Ok(_) => {}
//_ => panic!(),
//}
//}

//#[cfg(test)]
//mod expect_eq_tests {
//use super::*;

//#[test]
//fn test_expect() {
//expect("src/testing/mocks/simple.socool");
//}

//#[test]
//#[should_panic]
//fn test_expect_fail() {
//expect("./mocks/simple_fail.socool");
//}
//}
