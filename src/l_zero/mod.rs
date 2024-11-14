use std::fmt::{self, Display, Formatter};

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

impl Display for L0Instruction {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
	compiled_program: Vec<L0Instruction>,
}

impl L0Interpreter {
	fn new(compiled_program: Vec<L0Instruction>) -> Self {
		Self {
			compiled_program,
			..Default::default()
		}
	}
}

impl Interpreter for L0Interpreter {
	type Lang = L0;

	fn from_program(program: FirstOrderTerm) -> Self {
		Self::new(program.compile_as_program())
	}

	fn submit_query(&mut self, query: FirstOrderTerm) -> Result<Substitution> {
		let (compiled_query, var_mapping) = query.compile_as_query();

		let mut machine = M0::new();

		machine.execute(&compiled_query)?;
		machine.execute(&self.compiled_program)?;

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
