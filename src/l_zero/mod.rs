use std::{fmt, str::FromStr};

use anyhow::{bail, Result};
use derive_more::derive::Display;
use machine::M0;

use crate::{
	ast::{Constant, Functor, Structure, Term},
	machine_types::{HeapAddress, VarRegister},
	subst::{ExtractSubstitution, StaticMapping},
	universal_compiler::{CompilableProgram, CompilableQuery, Compiled},
	Interpreter, Language, Substitution,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

mod compiler;
mod machine;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct L0;

impl Language for L0 {
	type Program = NonVariableTerm;
	type Query = NonVariableTerm;

	type InstructionSet = L0Instruction;
	type Interpreter = L0Interpreter;
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
	fn from_program(program: NonVariableTerm) -> Result<Self> {
		Ok(Self::new(program.compile_as_program()?))
	}

	fn submit_query(&mut self, query: NonVariableTerm) -> Result<Substitution> {
		let compiled_query = query.compile_as_query()?;

		let mut machine = M0::new();

		let problem = compiled_query + self.compiled_program.clone();

		machine.execute(&problem.instructions)?;

		let solution = machine.extract_substitution(problem.compute_var_heap_mapping()?)?;

		println!("{}", problem);
		println!("{}", solution);

		Ok(solution)
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

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
	#[rustfmt::skip]
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.pad(&match self {
			L0Instruction::PutStructure(functor, reg) => format!("put_structure {functor}, {reg}"),
			L0Instruction::SetVariable(reg)           => format!("set_variable {reg}"),
			L0Instruction::SetValue(reg)              => format!("set_value {reg}"),
			L0Instruction::GetStructure(functor, reg) => format!("get_structure {functor}, {reg}"),
			L0Instruction::UnifyVariable(reg)         => format!("unify_variable {reg}"),
			L0Instruction::UnifyValue(reg)            => format!("unify_value {reg}"),
		})
	}
}

impl StaticMapping for L0Instruction {
	fn static_heap_size(&self) -> Option<HeapAddress> {
		match self {
			L0Instruction::PutStructure(_, _) => Some(2),
			L0Instruction::SetVariable(_) => Some(1),
			L0Instruction::SetValue(_) => Some(1),

			_ => None,
		}
		.map(Into::into)
	}

	fn static_variable_entry_point(&self, register: &VarRegister, pre_heap_top: HeapAddress) -> Option<HeapAddress> {
		match self {
			L0Instruction::SetVariable(reg) if reg == register => Some(pre_heap_top),

			_ => None,
		}
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, PartialEq, Eq, Display)]
pub enum NonVariableTerm {
	Constant(Constant),
	Structure(Structure),
}

impl TryInto<NonVariableTerm> for Term {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<NonVariableTerm, Self::Error> {
		Ok(match self {
			Term::Constant(constant) => NonVariableTerm::Constant(constant),
			Term::Structure(structure) => NonVariableTerm::Structure(structure),
			_ => bail!("Cannot convert term to non-variable term"),
		})
	}
}

impl From<NonVariableTerm> for Term {
	fn from(val: NonVariableTerm) -> Self {
		match val {
			NonVariableTerm::Constant(constant) => Term::Constant(constant),
			NonVariableTerm::Structure(structure) => Term::Structure(structure),
		}
	}
}

impl FromStr for NonVariableTerm {
	type Err = anyhow::Error;

	fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
		s.parse::<Term>()?.try_into()
	}
}
