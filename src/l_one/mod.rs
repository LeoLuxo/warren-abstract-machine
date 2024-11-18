use std::{fmt, str::FromStr};

use anyhow::{bail, Result};
use derive_more::derive::{Deref, DerefMut, Display, From, Index, IndexMut, IntoIterator};

use crate::{
	ast::{Constant, Fact, Functor, Structure, Term},
	display_iter,
	machine_types::{HeapAddress, VarRegister},
	subst::{ExtractSubstitution, StaticMapping},
	CompilableProgram, CompilableQuery, Compiled, Interpreter, Language, Substitution,
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
	fn from_program(program: Fact) -> Self {
		Self::new(program.compile_as_program())
	}

	fn submit_query(&mut self, query: Fact) -> Result<Substitution> {
		let compiled_query = query.compile_as_query();

		let mut machine = M0::new();

		machine.execute(&compiled_query.instructions)?;
		machine.execute(&self.compiled_program.instructions)?;

		let substitution = machine.extract_substitution(&compiled_query)?;

		println!("{}", compiled_query);
		println!("{}", self.compiled_program);
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

#[derive(Clone, Debug, PartialEq, Eq)]
enum L1Instruction {
	PutStructure(Functor, VarRegister),
	SetVariable(VarRegister),
	SetValue(VarRegister),

	GetStructure(Functor, VarRegister),
	UnifyVariable(VarRegister),
	UnifyValue(VarRegister),
}

impl fmt::Display for L1Instruction {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.pad(&match self {
			L1Instruction::PutStructure(functor, var_register) => format!("put_structure {functor}, {var_register}"),
			L1Instruction::SetVariable(var_register) => format!("set_variable {var_register}"),
			L1Instruction::SetValue(var_register) => format!("set_value {var_register}"),
			L1Instruction::GetStructure(functor, var_register) => format!("get_structure {functor}, {var_register}"),
			L1Instruction::UnifyVariable(var_register) => format!("unify_variable {var_register}"),
			L1Instruction::UnifyValue(var_register) => format!("unify_value {var_register}"),
		})
	}
}

impl StaticMapping for L1Instruction {
	fn static_heap_size(&self) -> Option<HeapAddress> {
		match self {
			L1Instruction::PutStructure(_, _) => Some(2),
			L1Instruction::SetVariable(_) => Some(1),
			L1Instruction::SetValue(_) => Some(1),
			_ => None,
		}
		.map(Into::into)
	}

	fn static_variable_entry_point(&self, register: VarRegister) -> bool {
		match self {
			L1Instruction::SetVariable(reg) if *reg == register => true,
			_ => false,
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
struct Facts(Vec<Fact>);

impl Facts {
	pub fn new() -> Self {
		Self(Vec::new())
	}
}
