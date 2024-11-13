use crate::{
	ast::{Functor, GetFunctor, Term, Variable},
	Machine, Substitution, WAMLanguage,
};
use std::collections::{hash_map::Entry, HashMap, HashSet};
use velcro::vec;

use super::M0;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub type Address = usize;
pub type VarRegister = usize;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Machine {
	s: Address,
	var_registers: Vec<Cell>,

	heap: Vec<Cell>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum Cell {
	STR(Address),
	REF(Address),
	Functor(Functor),

	#[default]
	Empty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
	PutStructure(Functor, VarRegister),
	SetVariable(VarRegister),
	SetValue(VarRegister),
	GetStructure(Functor, VarRegister),
	UnifyVariable(VarRegister),
	UnifyValue(VarRegister),
}

impl Machine {
	fn new(program: Vec<Instruction>) -> Self {
		Machine {
			instructions: program,
			..Default::default()
		}
	}

	fn write_register(&mut self, register: VarRegister, cell: Cell) {
		self.var_registers.resize(register, Default::default());
		self.var_registers[register] = cell;
	}

	fn read_register(&mut self, register: VarRegister) -> &Cell {
		self.var_registers.resize(register, Default::default());
		&self.var_registers[register]
	}

	fn prepend_instructions(&mut self, instuctions: Vec<Instruction>) {
		let post = std::mem::replace(&mut self.instructions, instuctions);
		self.instructions.extend(post);
	}

	fn execute(&mut self) {
		for i in self.instructions {
			self.execute_instruction(i.clone());
		}
	}

	fn execute_instruction(&mut self, instruction: Instruction) {
		match instruction {}
	}
}
