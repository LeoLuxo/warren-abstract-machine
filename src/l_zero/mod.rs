pub mod first_order_term;
pub mod machine;

use first_order_term::FirstOrderTerm;

use crate::Language;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub struct L0;

impl Language for L0 {
	type Program = FirstOrderTerm;
	type Query = FirstOrderTerm;
}
