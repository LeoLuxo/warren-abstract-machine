pub mod machine;
pub mod terms;

use terms::FirstOrderTerm;

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
