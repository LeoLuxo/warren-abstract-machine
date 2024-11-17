use std::{fmt, str::FromStr};

use anyhow::{bail, Result};
use derive_more::derive::Display;
use machine::{VarRegister, M0};

use crate::{
	ast::{Constant, Functor, Structure, Term},
	CompilableProgram, CompilableQuery, Compiled, Interpreter, Language, Substitution,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

// TODO: make private again
pub mod compiler;
pub mod machine;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
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

impl fmt::Display for L0Instruction {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.pad(&match self {
			L0Instruction::PutStructure(functor, var_register) => format!("put_structure {functor}, {var_register}"),
			L0Instruction::SetVariable(var_register) => format!("set_variable {var_register}"),
			L0Instruction::SetValue(var_register) => format!("set_value {var_register}"),
			L0Instruction::GetStructure(functor, var_register) => format!("get_structure {functor}, {var_register}"),
			L0Instruction::UnifyVariable(var_register) => format!("unify_variable {var_register}"),
			L0Instruction::UnifyValue(var_register) => format!("unify_value {var_register}"),
		})
	}
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct L0Interpreter {
	compiled_program: Compiled<L0>,
}

impl L0Interpreter {
	fn new(compiled_program: Compiled<L0>) -> Self {
		Self {
			compiled_program,
			..Default::default()
		}
	}
}

impl Interpreter<L0> for L0Interpreter {
	fn from_program(program: FirstOrderTerm) -> Self {
		Self::new(program.compile_as_program())
	}

	fn submit_query(&mut self, query: FirstOrderTerm) -> Result<Substitution> {
		let compiled_query = query.compile_as_query();

		let mut machine = M0::new();

		machine.execute(&compiled_query.instructions)?;

		// let pre_substitution = machine.pre_extract_targets(compiled_query.var_mapping)?;

		machine.execute(&self.compiled_program.instructions)?;

		// let substitution = machine.extract_substitution(pre_substitution)?;
		let substitution = todo!();

		println!("{}", machine);
		println!("{}", substitution);

		Ok(substitution)
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, PartialEq, Eq, Display)]
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

impl FromStr for FirstOrderTerm {
	type Err = anyhow::Error;

	fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
		s.parse::<Term>()?.try_into()
	}
}
