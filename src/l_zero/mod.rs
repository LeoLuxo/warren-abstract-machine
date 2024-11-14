use std::collections::HashMap;

use anyhow::{bail, Result};
use machine::M0;

use crate::{
	ast::{Constant, Functor, Structure, Term},
	parser::Parsable,
	CompilableProgram, CompilableQuery, Interpreter, Language, Substitution, VarRegister,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

mod compiler;
mod machine;

pub struct L0;

impl Language for L0 {
	type Program = FirstOrderTerm;
	type Query = FirstOrderTerm;
	type InstructionSet = L0Instruction;
	type Interpreter = L0Interpreter;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum L0Instruction {
	PutStructure(Functor, VarRegister),
	SetVariable(VarRegister),
	SetValue(VarRegister),
	GetStructure(Functor, VarRegister),
	UnifyVariable(VarRegister),
	UnifyValue(VarRegister),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct L0Interpreter {
	program: Vec<L0Instruction>,
}

impl L0Interpreter {
	fn new(program: Vec<L0Instruction>) -> Self {
		Self {
			program,
			..Default::default()
		}
	}
}

impl Interpreter for L0Interpreter {
	type Lang = L0;

	fn from_program(program: FirstOrderTerm) -> Self {
		Self::new(program.compile_as_program(&mut ()))
	}

	fn submit_query(&mut self, query: FirstOrderTerm) -> Result<Substitution> {
		let mut var_mapping = HashMap::new();
		let query = query.compile_as_query(&mut var_mapping);

		let mut machine = M0::new();

		machine.execute(&query)?;
		machine.execute(&self.program)?;

		// let substitution = HashMap::new();
		for (var, register) in var_mapping.into_iter() {
			// substitution[&var] = self.read_register(register).clone()
			println!("{:?}: {:?}", var, register);
		}
		// substitution

		Ok(())
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub enum FirstOrderTerm {
	Constant(Constant),
	Structure(Structure),
}

impl TryInto<FirstOrderTerm> for Term {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<FirstOrderTerm, Self::Error> {
		Ok(match self {
			Term::Constant(constant) => FirstOrderTerm::Constant(constant),
			Term::Structure(structure) => FirstOrderTerm::Structure(structure),
			_ => bail!("Cannot convert term to first order term"),
		})
	}
}

impl From<FirstOrderTerm> for Term {
	fn from(val: FirstOrderTerm) -> Self {
		match val {
			FirstOrderTerm::Constant(constant) => Term::Constant(constant),
			FirstOrderTerm::Structure(structure) => Term::Structure(structure),
		}
	}
}

impl Parsable for FirstOrderTerm {
	fn parse_from(source: &str) -> Result<Self> {
		Term::parse_from(source)?.try_into()
	}
}
