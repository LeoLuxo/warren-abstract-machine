use std::collections::HashMap;

use anyhow::Result;
use ast::{Term, Variable};
use parser::Parsable;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod ast;
pub mod l_zero;
pub mod parser;

// type Substitution = HashMap<Variable, Term>;
type Substitution = ();

pub trait WAMLanguage: Sized {
	type Program;
	type Query;

	fn from_program(program: Self::Program) -> Self;
	fn submit_query(&mut self, query: Self::Query) -> Substitution;

	fn from_program_source(source: &str) -> Result<Self>
	where
		Self::Program: Parsable,
	{
		let program = Self::Program::parse_from(source)?;
		Ok(Self::from_program(program))
	}

	fn submit_query_source(&mut self, source: &str) -> Result<Substitution>
	where
		Self::Query: Parsable,
	{
		let query = Self::Query::parse_from(source)?;
		Ok(self.submit_query(query))
	}
}

pub trait CompilableProgram {
	type Target;
	fn compile(self) -> Self::Target;
}

pub trait CompilableQuery {
	type Target;
	fn compile(self) -> Self::Target;
}
