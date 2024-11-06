use crate::{Cell, Language, Machine, Substitution};

use super::L0;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub struct M0 {
	heap: Vec<Cell>,
}

impl Machine<L0> for M0 {
	fn new(program: <L0 as Language>::Program) -> Self {
		todo!()
	}

	fn submit_query(query: <L0 as Language>::Query) -> Substitution {
		todo!()
	}
}
