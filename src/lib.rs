use std::{
	fmt::Display,
};

use anyhow::Result;
use derive_more::derive::{Display, From};
use l_zero::compiler::VarToRegMapping;
use subst::Substitution;
use util::Successor;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod ast;
pub mod l_zero;
pub mod parser;
pub mod subst;
pub mod util;

pub fn solve<L: Language>(program: L::Program, queries: Vec<L::Query>) -> Vec<Result<Substitution>> {
	L::Interpreter::solve(program, queries)
}

pub fn solve_single<L: Language>(program: L::Program, query: L::Query) -> Result<Substitution> {
	L::Interpreter::solve_single(program, query)
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
	fn from_program(program: L::Program) -> Self;
	fn submit_query(&mut self, query: L::Query) -> Result<Substitution>;

	fn solve_single(program: L::Program, query: L::Query) -> Result<Substitution> {
		Self::from_program(program).submit_query(query)
	}

	fn solve(program: L::Program, queries: Vec<L::Query>) -> Vec<Result<Substitution>> {
		let mut interpreter = Self::from_program(program);
		queries.into_iter().map(|q| interpreter.submit_query(q)).collect()
	}
}

pub trait CompilableProgram<L: Language> {
	fn compile_as_program(self) -> Compiled<L>;
}

pub trait CompilableQuery<L: Language> {
	fn compile_as_query(self) -> Compiled<L>;
}

#[derive(Clone, Debug, PartialEq, Eq, From, Display)]
#[display("{}\n{}", display_iter!(instructions, "\n"), var_mapping)]
#[display(bounds(L::InstructionSet: Display))]
pub struct Compiled<L: Language> {
	pub instructions: Vec<L::InstructionSet>,
	pub var_mapping: VarToRegMapping,
}

impl<L: Language> Default for Compiled<L> {
	fn default() -> Self {
		Self {
			instructions: Default::default(),
			var_mapping: Default::default(),
		}
	}
}
