use std::{
	fmt::Display,
	mem,
	ops::{Add, AddAssign},
};

use anyhow::{Context, Result};
use derive_more::derive::{Display, From};
use subst::{StaticMapping, Substitution, VarToHeapMapping, VarToRegMapping};
use util::Successor;
use velcro::vec;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod ast;
// pub mod l_one;
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
	fn compile_as_program(self) -> Compiled<L>;
}

pub trait CompilableQuery<L: Language> {
	fn compile_as_query(self) -> Compiled<L>;
}

#[derive(Clone, Debug, PartialEq, Eq, From, Display)]
#[display("{}\n{}", display_iter!(instructions, "\n"), var_reg_mapping.as_ref().map_or("(without mapping)".to_string(), |m| format!("(where {})", m)))]
#[display(bounds(L::InstructionSet: Display))]
pub struct Compiled<L: Language> {
	pub instructions: Vec<L::InstructionSet>,
	pub var_reg_mapping: Option<VarToRegMapping>,
}

impl<L: Language> Default for Compiled<L> {
	fn default() -> Self {
		Self {
			instructions: Default::default(),
			var_reg_mapping: Default::default(),
		}
	}
}

impl<L: Language> Compiled<L> {
	pub fn strip_mapping(&mut self) {
		self.var_reg_mapping = None;
	}

	pub fn with_stripped_mapping(self) -> Self {
		Self {
			instructions: self.instructions,
			var_reg_mapping: None,
		}
	}

	pub fn combined(self, other: Self) -> Self {
		// self's instructions then other's instructions, in order
		let instructions = vec![..self.instructions, ..other.instructions];
		// self's mapping takes priority over other's mapping (overwritten where needed)
		let var_reg_mapping = match (self.var_reg_mapping, other.var_reg_mapping) {
			(Some(m1), Some(m2)) => Some(m2.into_iter().chain(m1).collect()),
			(_, Some(m)) | (Some(m), _) => Some(m),
			_ => None,
		};

		Self {
			instructions,
			var_reg_mapping,
		}
	}

	pub fn combine(&mut self, other: Self) {
		*self = mem::take(self).combined(other)
	}

	pub fn compute_var_heap_mapping(&self) -> Result<VarToHeapMapping>
	where
		<L as Language>::InstructionSet: StaticMapping,
	{
		subst::compute_var_heap_mapping(
			self.var_reg_mapping
				.as_ref()
				.context("Cannot compute var-heap mapping of compiled without a valid mapping")?,
			&self.instructions,
		)
	}
}

impl<L: Language> Add for Compiled<L> {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		self.combined(rhs)
	}
}

impl<L: Language> AddAssign for Compiled<L> {
	fn add_assign(&mut self, rhs: Self) {
		self.combine(rhs);
	}
}
