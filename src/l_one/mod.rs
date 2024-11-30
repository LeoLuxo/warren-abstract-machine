use std::fmt;

use anyhow::Result;
use derive_more::derive::{Deref, DerefMut, Display, From, Index, IndexMut, IntoIterator};

use crate::{
	ast::{Fact, Functor, Identifier},
	display_iter,
	machine_types::VarRegister,
	parser::{parser_sequence::Separator, Parsable, Parser},
	substitution::VarEntryPoint,
	universal_compiler::Compiled,
	CompilableProgram, Interpreter, Language, Substitution,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod compiler;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct L1;

impl Language for L1 {
	type Program = Facts;
	type Query = Fact;

	type InstructionSet = L1Instruction;
	type Interpreter = L1Interpreter;
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct L1Interpreter {
	compiled_program: Compiled<L1>,
}

impl L1Interpreter {
	fn new(compiled_program: Compiled<L1>) -> Self {
		Self {
			compiled_program,
			..Default::default()
		}
	}
}

impl Interpreter<L1> for L1Interpreter {
	fn from_program(program: Facts) -> Result<Self> {
		Ok(Self::new(program.compile_as_program()?))
	}

	fn submit_query(&mut self, _query: Fact) -> Result<Substitution> {
		todo!()
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum L1Instruction {
	Call(Identifier),
	Proceed,

	PutStructure(Functor, VarRegister),
	SetVariable(VarRegister),
	SetValue(VarRegister),
	PutVariable(VarRegister, VarRegister),
	PutValue(VarRegister, VarRegister),

	GetStructure(Functor, VarRegister),
	UnifyVariable(VarRegister),
	UnifyValue(VarRegister),
	GetVariable(VarRegister, VarRegister),
	GetValue(VarRegister, VarRegister),
}

impl fmt::Display for L1Instruction {
	#[rustfmt::skip]
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.pad(&match self {
			L1Instruction::Call(identifier)           => format!("call {identifier}"),
			L1Instruction::Proceed                    => "proceed".to_string(),
			L1Instruction::PutStructure(functor, reg) => format!("put_structure {functor}, {reg}"),
			L1Instruction::SetVariable(reg)           => format!("set_variable {reg}"),
			L1Instruction::SetValue(reg)              => format!("set_value {reg}"),
			L1Instruction::PutVariable(reg1, reg2)    => format!("put_variable {reg1}, {reg2}"),
			L1Instruction::PutValue(reg1, reg2)       => format!("put_value {reg1}, {reg2}"),
			L1Instruction::GetStructure(functor, reg) => format!("get_structure {functor}, {reg}"),
			L1Instruction::UnifyVariable(reg)         => format!("unify_variable {reg}"),
			L1Instruction::UnifyValue(reg)            => format!("unify_value {reg}"),
			L1Instruction::GetVariable(reg1, reg2)    => format!("get_variable {reg1}, {reg2}"),
			L1Instruction::GetValue(reg1, reg2)       => format!("get_value {reg1}, {reg2}"),
		})
	}
}

impl VarEntryPoint for L1Instruction {
	fn is_variable_entry_point(&self) -> Option<(VarRegister, Self)> {
		match self {
			L1Instruction::SetVariable(reg) => Some((*reg, L1Instruction::SetValue(*reg))),

			L1Instruction::PutVariable(xn, ai) => Some((*xn, L1Instruction::PutValue(*xn, *ai))),

			_ => None,
		}
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, Default, PartialEq, Eq, Deref, DerefMut, Display, From, IntoIterator, Index, IndexMut)]
#[display("{}", display_iter!(_0, ", "))]
pub struct Facts(Vec<Fact>);

impl Facts {
	pub fn new() -> Self {
		Self(Vec::new())
	}
}

impl Parsable for Facts {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let facts = parser.match_sequence_by_type::<Fact>(Separator::None, None)?;

		Ok(Self(facts))
	}
}
