/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

use anyhow::Result;
use parser::Parsable;

pub mod ast;
pub mod l_zero;
pub mod parser;

pub trait Language {
	type Program: Parsable;
	type Query: Parsable;
}

type Substitution = ();

pub trait Machine<L: Language>: Sized {
	fn from_program(program: L::Program) -> Self;
	fn submit_query(query: L::Query) -> Substitution;

	fn from_program_source(source: &str) -> Result<Self> {
		let program = L::Program::parse_from(source)?;
		Ok(Self::from_program(program))
	}

	fn submit_query_source(source: &str) -> Result<Substitution> {
		let query = L::Query::parse_from(source)?;
		Self::submit_query(query);
  Ok(())
	}
}
