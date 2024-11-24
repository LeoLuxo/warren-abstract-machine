use anyhow::Result;
use subst::Substitution;
use universal_compiler::{CompilableProgram, CompilableQuery};
use util::Successor;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod anonymous;
pub mod ast;
pub mod l_one;
pub mod l_zero;
pub mod machine_types;
pub mod parser;
pub mod subst;
pub mod universal_compiler;
pub mod util;

pub fn solve<L: Language>(program: L::Program, query: L::Query) -> Result<Substitution> {
	L::Interpreter::solve(program, query)
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait Language: Sized {
	type Program: CompilableProgram<Self>;
	type Query: CompilableQuery<Self>;
	type InstructionSet;
	type Interpreter: Interpreter<Self>;
}

pub trait Interpreter<L: Language>: Sized {
	fn from_program(program: L::Program) -> Result<Self>;
	fn submit_query(&mut self, query: L::Query) -> Result<Substitution>;

	fn solve(program: L::Program, query: L::Query) -> Result<Substitution> {
		Self::from_program(program)?.submit_query(query)
	}
}
