use std::fmt;

use anyhow::{bail, Result};
use derive_more::derive::Display;
use machine::M0;

use crate::{
	ast::{Constant, Functor, Structure, Term},
	machine_types::{CodeAddress, VarRegister},
	parser::{Parsable, Parser},
	substitution::{ExtractSubstitution, VarEntryPoint},
	universal_compiler::{CompilableProgram, CompilableQuery, Compiled, OffsetJumpLabels},
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

		let code = compiled_query + self.compiled_program.clone();
		println!("Code:\n{}\n", code);

		let solution = machine.execute_and_extract_substitution(code)?;
		println!("Solution:\n{}\n", solution);

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

impl VarEntryPoint for L0Instruction {
	fn is_variable_entry_point(&self) -> Option<(VarRegister, Self)> {
		match self {
			L0Instruction::SetVariable(reg) => Some((*reg, L0Instruction::SetValue(*reg))),

			_ => None,
		}
	}
}

impl OffsetJumpLabels for L0Instruction {
	// L0 doesn't have any instruction with labels, so nothing happens
	fn offset_jump_labels(self, _offset: CodeAddress) -> Self {
		self
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

impl Parsable for NonVariableTerm {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.match_type::<Term>()?.try_into()
	}
}
