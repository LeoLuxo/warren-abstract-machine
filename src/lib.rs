use std::{fmt::Display, marker::PhantomData};

use anyhow::Result;
use derive_more::derive::{Display, From};
use machine_types::VarToRegMapping;
use subst::Substitution;
use util::Successor;
use velcro::vec;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod ast;
pub mod l_one;
pub mod l_zero;
pub mod machine_types;
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
	fn compile_as_program(self) -> Compiled<L, false>;
}

pub trait CompilableQuery<L: Language> {
	fn compile_as_query(self) -> Compiled<L, true>;
}

#[derive(Clone, Debug, PartialEq, Eq, From, Display)]
#[display("{}\n{}", display_iter!(instructions, "\n"), var_reg_mapping.as_ref().map_or("(without mapping)".to_string(), |m| format!("(where {})", m)))]
#[display(bounds(L::InstructionSet: Display))]
pub struct Compiled<L: Language, const HasValidMapping: bool = false> {
	pub instructions: Vec<L::InstructionSet>,
	var_reg_mapping: Option<VarToRegMapping>,
}

impl<L: Language> Compiled<L, true> {
	fn new(instructions: Vec<L::InstructionSet>, var_reg_mapping: VarToRegMapping) -> Self {
		Self {
			instructions,
			var_reg_mapping: Some(var_reg_mapping),
		}
	}

	pub fn get_var_reg_mapping(&self) -> VarToRegMapping {
		self.var_reg_mapping
			.clone()
			.expect("Compiled<true> with invalid mapping encountered")
	}

	pub fn set_var_reg_mapping(&mut self, var_reg_mapping: VarToRegMapping) {
		self.var_reg_mapping = Some(var_reg_mapping)
	}

	pub fn invalidate_mapping(self) -> Compiled<L, false> {
		Compiled {
			instructions: self.instructions,
			var_reg_mapping: None,
		}
	}
}

impl<L: Language> Compiled<L, false> {
	fn new(instructions: Vec<L::InstructionSet>) -> Self {
		Self {
			instructions,
			var_reg_mapping: None,
		}
	}

	pub fn give_mapping(self, var_reg_mapping: VarToRegMapping) -> Compiled<L, true> {
		Compiled {
			instructions: self.instructions,
			var_reg_mapping: Some(var_reg_mapping),
		}
	}
}

impl<L: Language, const V: bool> Default for Compiled<L, V> {
	fn default() -> Self {
		Self {
			instructions: Default::default(),
			var_reg_mapping: Default::default(),
		}
	}
}

pub trait CombinableWith<Other> {
	type Output;

	fn combine(self, other: Other) -> Self::Output;
}

impl<L: Language> CombinableWith<Compiled<L, false>> for Compiled<L, false> {
	type Output = Compiled<L, false>;

	fn combine(self, other: Compiled<L, false>) -> Self::Output {
		Self::Output {
			instructions: vec![..self.instructions, ..other.instructions],
			var_reg_mapping: None,
		}
	}
}

impl<L: Language> CombinableWith<Compiled<L, true>> for Compiled<L, false> {
	type Output = Compiled<L, true>;

	fn combine(self, other: Compiled<L, true>) -> Self::Output {
		Self::Output {
			instructions: vec![..self.instructions, ..other.instructions],
			var_reg_mapping: other.var_reg_mapping,
		}
	}
}

impl<L: Language> CombinableWith<Compiled<L, false>> for Compiled<L, true> {
	type Output = Compiled<L, true>;

	fn combine(self, other: Compiled<L, false>) -> Self::Output {
		Self::Output {
			instructions: vec![..self.instructions, ..other.instructions],
			var_reg_mapping: self.var_reg_mapping,
		}
	}
}
