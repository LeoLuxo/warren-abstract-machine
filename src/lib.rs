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

pub mod ast;
pub mod l_zero;
pub mod parser;
pub mod util;

#[derive(Clone, Debug, Default, PartialEq, Eq, From, Display)]
#[display("{{ {} }}", display_map!(_0))]
pub struct Substitution(HashMap<Variable, SubstitutionEntry>);

#[derive(Clone, Debug, Default, PartialEq, Eq, From, Display)]
#[display("{}", _0.as_ref().map_or("(unbound)".to_string(), |t| format!("{}", t)))]
pub struct SubstitutionEntry(Option<Term>);

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

	fn solve_single(program: L::Program, query: L::Query) -> Result<Substitution> {
		Self::from_program(program).submit_query(query)
	}

	fn solve(program: L::Program, queries: Vec<L::Query>) -> Vec<Result<Substitution>> {
		let mut interpreter = Self::from_program(program);
		queries.into_iter().map(|q| interpreter.submit_query(q)).collect()
	}
}

#[derive(Clone, Debug, PartialEq, Eq, From, IntoIterator, Deref, DerefMut, Index, IndexMut, Display)]
#[display("Instructions:\n{}", indent!(2, display_iter!(_0, "\n")))]
#[display(bounds(L::InstructionSet: Display))]
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
			let entry = self.extract_reg(register).into();
			substitution.insert(var, entry);
		}

		substitution.into()
	}
}
