use crate::{
	ast::{Functor, Identifier},
	Language, Machine, Substitution,
};

use super::L0;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

type Address = usize;

pub struct M0 {
	heap: Vec<Cell>,
	s: Address,
}

pub enum Cell {
	STR(Address),
	REF(Address),
	Functor(Functor),
}

impl Machine<L0> for M0 {
	fn from_program(program: <L0 as Language>::Program) -> Self {
		todo!()
	}

	fn submit_query(query: <L0 as Language>::Query) -> Substitution {
		todo!()
	}
}
