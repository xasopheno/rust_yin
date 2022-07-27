#![warn(
    clippy::nursery,
    //clippy::restriction,
    // clippy::pedantic,
    //clippy::cargo,
)]

pub use weresocool_ast as ast;
pub use weresocool_core::*;
pub use weresocool_error as error;
pub use weresocool_instrument as instrument;
pub use weresocool_parser as parser;
pub use weresocool_shared as shared;
pub mod testing;
