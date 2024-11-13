use std::default;

use anyhow::{bail, Result};

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
	var_registers: Vec<Option<Cell>>,
	readwrite_mode: Option<ReadWrite>,

	heap: Vec<Cell>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ReadWrite {
	Read,
	Write,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Cell {
	STR(Address),
	REF(Address),
	Functor(Functor),
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
		self.var_registers[register] = Some(cell);
	}

	fn read_register(&self, register: VarRegister) -> &Cell {
		if let Some(Some(value)) = self.var_registers.get(register) {
			value
		} else {
			panic!("Attempted to read an uninitialized register");
		}
	}

	fn set_readwrite(&mut self, mode: ReadWrite) {
		self.readwrite_mode = Some(mode);
	}

	fn get_readwrite(&self) -> ReadWrite {
		self.readwrite_mode
			.expect("Attempted to get uninitialized readwrite_mode")
	}

	fn h(&self) -> Address {
		self.heap.len()
	}

	fn deref(&self, address: Address) -> Address {
		let Some(cell) = self.heap.get(address) else {
			panic!("Attempted to deref invalid heap address")
		};

		match cell {
			Cell::REF(a) if *a != address => self.deref(*a),
			_ => address,
		}
	}

	fn bind(&mut self, address1: Address, address2: Address) -> Result<()> {
		let (Some(cell1), Some(cell2)) = (self.heap.get(address1), self.heap.get(address1)) else {
			panic!("Attempted to deref invalid heap address")
		};

		match (cell1, cell2) {
			(Cell::REF(a), c2) if *a == address1 => self.heap[address1] = c2.clone(),
			(c1, Cell::REF(a)) if *a == address2 => self.heap[address2] = c1.clone(),

			_ => bail!("Unification error"),
		}

		Ok(())
	}

	fn unify(&mut self, address1: Address, address2: Address) -> Result<()> {
		let d1 = self.deref(address1);
		let d2 = self.deref(address2);

		if d1 == d2 {
			return Ok(());
		}

		// deref already took care of checking the bounds
		let c1 = &self.heap[d1];
		let c2 = &self.heap[d2];

		match (c1, c2) {
			(Cell::REF(_), _) | (_, Cell::REF(_)) => {
				self.bind(d1, d2)?;
			}

			(Cell::STR(str1), Cell::STR(str2)) => {
				self.unify(*str1, *str2)?;
			}

			(Cell::Functor(Functor { name: f1, arity: n1 }), Cell::Functor(Functor { name: f2, arity: n2 }))
				if f1 == f2 && n1 == n2 =>
			{
				for i in 1..=*n1 {
					self.unify(d1 + i, d2 + i)?;
				}
			}

			_ => bail!("Unification error"),
		}

		Ok(())
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
