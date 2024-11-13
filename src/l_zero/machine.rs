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

	heap: Vec<Cell>,
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
pub enum I0 {
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

	pub fn execute(&mut self, instructions: &[I0]) -> Result<()> {
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

	fn execute_instruction(&mut self, instruction: &I0) -> Result<()> {
		match instruction {
			I0::PutStructure(functor, reg) => {
				let pointer = Cell::STR(self.h() + 1);

				self.write_register(*reg, pointer.clone());
				self.heap.push(pointer);
				self.heap.push(Cell::Functor(functor.clone()));
			}

			I0::SetVariable(reg) => {
				let pointer = Cell::REF(self.h());

				self.write_register(*reg, pointer.clone());
				self.heap.push(pointer);
			}

			I0::SetValue(reg) => {
				let value = self.read_register(*reg).clone();

				self.heap.push(value);
			}

			I0::GetStructure(functor, reg) => todo!(),

			I0::UnifyVariable(reg) => todo!(),

			I0::UnifyValue(reg) => todo!(),
		}

		Ok(())
	}
}
