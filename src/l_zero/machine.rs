use std::default;

use anyhow::Result;

use crate::ast::Functor;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub type Address = usize;
pub type VarRegister = usize;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct M0 {
	s: Address,
	var_registers: Vec<Cell>,
	read_write_mode: ReadWrite,

	heap: Vec<Cell>,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
enum ReadWrite {
	#[default]
	Read,
	Write,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
enum Cell {
	STR(Address),
	REF(Address),
	Functor(Functor),

	#[default]
	Uninitialized,
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

impl M0 {
	pub fn new() -> Self {
		Default::default()
	}

	pub fn execute(&mut self, instructions: &[Instruction]) -> Result<()> {
		for i in instructions {
			self.execute_instruction(i)?;
		}

		Ok(())
	}

	fn write_register(&mut self, register: VarRegister, cell: Cell) {
		self.var_registers.resize(register, Default::default());
		self.var_registers[register] = cell;
	}

	fn read_register(&mut self, register: VarRegister) -> &Cell {
		let value = self
			.var_registers
			.get(register)
			.expect("Attempted reading an uninitialized register");

		if let Cell::Uninitialized = *value {
			panic!("Attempted reading an uninitialized register");
		}

		value
	}

	fn h(&self) -> Address {
		self.heap.len()
	}

	fn execute_instruction(&mut self, instruction: &Instruction) -> Result<()> {
		match instruction {
			Instruction::PutStructure(functor, reg) => {
				let pointer = Cell::STR(self.h() + 1);

				self.write_register(*reg, pointer.clone());
				self.heap.push(pointer);
				self.heap.push(Cell::Functor(functor.clone()));
			}

			Instruction::SetVariable(reg) => {
				let pointer = Cell::REF(self.h());

				self.write_register(*reg, pointer.clone());
				self.heap.push(pointer);
			}

			Instruction::SetValue(reg) => {
				let value = self.read_register(*reg).clone();

				self.heap.push(value);
			}

			Instruction::GetStructure(functor, reg) => {}

			Instruction::UnifyVariable(reg) => {}

			Instruction::UnifyValue(reg) => {}
		}

		Ok(())
	}
}
