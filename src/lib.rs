use std::{
	collections::HashMap,
	fmt::Display,
	ops::{Add, AddAssign, Sub, SubAssign},
};

use anyhow::Result;
use ast::{Term, Variable};
use derive_more::derive::{Add, Deref, DerefMut, Display, From, Index, IndexMut, IntoIterator, Sub};
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

type Substitution = HashMap<Variable, Option<Term>>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Display, From, Deref, DerefMut, Add, Sub)]
#[from(forward)]
#[display("X{_0}")]
pub struct VarRegister(usize);

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

#[rustfmt::skip] impl Add<usize> for VarRegister { type Output = Self; fn add(self, rhs: usize) -> Self::Output { Self(self.0 + rhs) } }
#[rustfmt::skip] impl Sub<usize> for VarRegister { type Output = Self; fn sub(self, rhs: usize) -> Self::Output { Self(self.0 - rhs) } }
#[rustfmt::skip] impl AddAssign<usize> for VarRegister { fn add_assign(&mut self, rhs: usize) { self.0 += rhs } }
#[rustfmt::skip] impl SubAssign<usize> for VarRegister { fn sub_assign(&mut self, rhs: usize) { self.0 += rhs } }

pub type VarMapping = HashMap<Variable, VarRegister>;

pub trait Language: Sized {
	type Program: CompilableProgram<Self>;
	type Query: CompilableQuery<Self>;
	type InstructionSet;
	type Interpreter: Interpreter<Self>;

	fn solve(program: Self::Program, queries: Vec<Self::Query>) -> Vec<Result<Substitution>> {
		Self::Interpreter::solve(program, queries)
	}
}

pub trait CompilableProgram<L: Language> {
	fn compile_as_program(self) -> Instructions<L>;
}

pub trait CompilableQuery<L: Language> {
	fn compile_as_query(self) -> (Instructions<L>, VarMapping);
}

pub trait Interpreter<L: Language>: Sized {
	fn from_program(program: L::Program) -> Self;
	fn submit_query(&mut self, query: L::Query) -> Result<Substitution>;

	fn solve(program: L::Program, queries: Vec<L::Query>) -> Vec<Result<Substitution>> {
		let mut interpreter = Self::from_program(program);

		queries.into_iter().map(|q| interpreter.submit_query(q)).collect()
	}
}

pub struct WAMInterpreter<L: Language>(<L as Language>::Interpreter);

impl<L: Language> Interpreter<L> for WAMInterpreter<L> {
	fn from_program(program: L::Program) -> Self {
		Self(L::Interpreter::from_program(program))
	}

	fn submit_query(&mut self, query: L::Query) -> Result<Substitution> {
		self.0.submit_query(query)
	}
}

#[derive(Clone, Debug, PartialEq, Eq, From, IntoIterator, Deref, DerefMut, Index, IndexMut, Display)]
#[display(bound(L::InstructionSet: Display))]
#[display("Instructions:\n{}", indent!(2, display_iter!(_0, "\n")))]
pub struct Instructions<L: Language>(Vec<L::InstructionSet>);

impl<L: Language> Default for Instructions<L> {
	fn default() -> Self {
		Self(Default::default())
	}
}

pub trait ExtractSubstitution {
	fn extract_reg(&self, reg: VarRegister) -> Option<Term>;

	fn extract_mapping(&self, mapping: VarMapping) -> Substitution {
		let mut substitution = HashMap::new();
		for (var, register) in mapping.into_iter() {
			substitution.insert(var, self.extract_reg(register));
		}

		substitution
	}
}
