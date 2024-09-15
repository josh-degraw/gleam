mod expression;
mod pattern;
#[cfg(test)]
mod tests;

use crate::build::Target;
use crate::type_::PRELUDE_MODULE_NAME;
use crate::{ast::*, Error};
use crate::{docvec, pretty::*};
use camino::Utf8Path;
use ecow::EcoString;

const INDENT: isize = 4;

pub type Output<'a> = Result<Document<'a>, Error>;

pub struct Generator {
    // Add necessary fields
}

// impl<'a> Generator<'a> {
//     pub fn new(// Add necessary parameters
//     ) -> Self {
//         // Initialize the generator
//         todo!()
//     }

//     pub fn compile(&mut self) -> Output<'a> {
//         // Implement compilation logic
//     }
// }
