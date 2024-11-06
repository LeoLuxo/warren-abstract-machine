/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod ast;
pub mod l_zero;
pub mod parser;

pub trait Language {
	type Program;
	type Query;
}

type Substitution = ();

type Cell = ();

pub trait Machine<L: Language>: Sized {
	fn new(program: L::Program) -> Self;
	fn submit_query(query: L::Query) -> Substitution;
}
