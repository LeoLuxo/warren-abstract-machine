use std::collections::HashMap;

use anyhow::Result;
use ast::Variable;
use parser::Parsable;
use util::Successor;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod ast;
pub mod l_zero;
pub mod parser;
pub mod util;

// type Substitution = HashMap<Variable, Term>;
type Substitution = ();

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarRegister(usize);
newtype!(VarRegister { usize });

impl Default for VarRegister {
	fn default() -> Self {
		Self(1)
	}
}

impl Successor for VarRegister {
	fn next(&self) -> Self {
		Self(self.0 + 1)
	}
}

pub type VarMapping = HashMap<Variable, VarRegister>;

pub trait Language: Sized {
	type Program: CompilableProgram<Self>;
	type Query: CompilableQuery<Self>;
	type InstructionSet;
	type Interpreter: Interpreter;
}

pub trait Interpreter: Sized {
	type Lang: Language;

	fn submit_query(&mut self, query: <Self::Lang as Language>::Query) -> Result<Substitution>;
	fn from_program(program: <Self::Lang as Language>::Program) -> Self;

	fn from_program_source(source: &str) -> Result<Self>
	where
		<Self::Lang as Language>::Program: Parsable,
	{
		let program = <Self::Lang as Language>::Program::parse_from(source)?;
		Ok(Self::from_program(program))
	}

	fn submit_query_source(&mut self, source: &str) -> Result<Substitution>
	where
		<Self::Lang as Language>::Query: Parsable,
	{
		let query = <Self::Lang as Language>::Query::parse_from(source)?;
		self.submit_query(query)?;
		Ok(())
	}
}

pub trait CompilableProgram<L: Language> {
	fn compile_as_program(self) -> Vec<L::InstructionSet>;
}

pub trait CompilableQuery<L: Language> {
	fn compile_as_query(self) -> (Vec<L::InstructionSet>, VarMapping);
}
