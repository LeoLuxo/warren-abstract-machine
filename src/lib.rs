use std::marker::PhantomData;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait Language {
	type Program;
	type Query;
}

pub struct Machine<L: Language> {
	_language: PhantomData<L>,
}

type Substitution = ();

impl<L: Language> Machine<L> {
	pub fn new(program: L::Program) -> Machine<L> {
		todo!();
	}

	pub fn submit_query(query: L::Query) -> Substitution {
		todo!();
	}
}
