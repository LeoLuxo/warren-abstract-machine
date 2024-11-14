use std::{
	collections::HashMap,
	ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign},
};

use anyhow::{bail, Result};

use crate::{ast::Functor, newtype, VarRegister};

use super::L0Instruction;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Address {
	Register(VarRegister),
	Heap(HeapAddress),
}

#[rustfmt::skip] impl PartialEq<VarRegister> for Address { fn eq(&self, other: &VarRegister) -> bool { match self  { Address::Register(var_register) => var_register == other, _ => false, } } }
#[rustfmt::skip] impl PartialEq<Address> for VarRegister { fn eq(&self, other: &Address)     -> bool { match other { Address::Register(var_register) => var_register == self,  _ => false, } } }
#[rustfmt::skip] impl PartialEq<HeapAddress> for Address { fn eq(&self, other: &HeapAddress) -> bool { match self  { Address::Heap(heap_address)     => heap_address == other, _ => false, } } }
#[rustfmt::skip] impl PartialEq<Address> for HeapAddress { fn eq(&self, other: &Address)     -> bool { match other { Address::Heap(heap_address)     => heap_address == self,  _ => false, } } }

#[rustfmt::skip] impl Add<usize> for Address { type Output = Self; fn add(self, rhs: usize) -> Self::Output { match self { Address::Register(var_register) => Address::Register(var_register + rhs), Address::Heap(heap_address) => Address::Heap(heap_address + rhs), } } }
#[rustfmt::skip] impl Sub<usize> for Address { type Output = Self; fn sub(self, rhs: usize) -> Self::Output { match self { Address::Register(var_register) => Address::Register(var_register - rhs), Address::Heap(heap_address) => Address::Heap(heap_address - rhs), } } }
#[rustfmt::skip] impl AddAssign<usize> for Address { fn add_assign(&mut self, rhs: usize)  {  match self { Address::Register(var_register) => *var_register += rhs, Address::Heap(heap_address) => *heap_address += rhs, } } }
#[rustfmt::skip] impl SubAssign<usize> for Address { fn sub_assign(&mut self, rhs: usize)  {  match self { Address::Register(var_register) => *var_register -= rhs, Address::Heap(heap_address) => *heap_address -= rhs, } } }

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
struct HeapAddress(usize);
newtype!(HeapAddress { usize });

#[derive(Clone, Debug, PartialEq, Eq)]
enum Cell {
	Str(HeapAddress),
	Ref(HeapAddress),
	Functor(Functor),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct VarRegisters(HashMap<VarRegister, Cell>);

impl Index<VarRegister> for VarRegisters {
	type Output = Cell;

	fn index(&self, index: VarRegister) -> &Self::Output {
		self.0.get(&index).expect("Attempted reading an uninitialized register")
	}
}

impl IndexMut<VarRegister> for VarRegisters {
	fn index_mut(&mut self, index: VarRegister) -> &mut Self::Output {
		self.0
			.get_mut(&index)
			.expect("Attempted accessing an uninitialized register")
	}
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Heap(Vec<Cell>);

impl Heap {
	pub fn push(&mut self, value: Cell) {
		self.0.push(value);
	}

	pub fn top(&self) -> HeapAddress {
		HeapAddress(self.0.len())
	}
}

impl Index<HeapAddress> for Heap {
	type Output = Cell;

	fn index(&self, index: HeapAddress) -> &Self::Output {
		self.0.get(index.0).expect("Attempted reading an invalid heap address")
	}
}

impl IndexMut<HeapAddress> for Heap {
	fn index_mut(&mut self, index: HeapAddress) -> &mut Self::Output {
		self.0
			.get_mut(index.0)
			.expect("Attempted accessing an invalid heap address")
	}
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
enum ReadWrite {
	#[default]
	Read,
	Write,
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct M0 {
	s: HeapAddress,
	mode: ReadWrite,

	var_registers: VarRegisters,
	heap: Heap,
}

impl M0 {
	pub fn new() -> Self {
		Default::default()
	}

	pub fn execute(&mut self, instructions: &[L0Instruction]) -> Result<()> {
		for i in instructions {
			self.execute_instruction(i)?;
		}

		Ok(())
	}

	fn read_store(&self, address: Address) -> &Cell {
		match address {
			Address::Register(var_register) => &self.var_registers[var_register],
			Address::Heap(heap_address) => &self.heap[heap_address],
		}
	}

	fn deref(&self, address: Address) -> Address {
		match self.read_store(address) {
			Cell::Ref(a) if address != *a => self.deref(Address::Heap(*a)),
			_ => address,
		}
	}

	fn bind(&mut self, address1: Address, address2: Address) -> Result<()> {
		let (dest, src) = match (self.read_store(address1), self.read_store(address2)) {
			(Cell::Ref(a), c) if *a == address1 => (*a, c),
			(c, Cell::Ref(a)) if *a == address2 => (*a, c),
			_ => bail!("Unification error"),
		};

		self.heap[dest] = src.clone();

		Ok(())
	}

	fn unify(&mut self, address1: Address, address2: Address) -> Result<()> {
		let d1 = self.deref(address1);
		let d2 = self.deref(address2);

		if d1 == d2 {
			return Ok(());
		}

		let c1 = self.read_store(d1);
		let c2 = self.read_store(d2);

		match (c1, c2) {
			(Cell::Ref(_), _) | (_, Cell::Ref(_)) => {
				self.bind(d1, d2)?;
			}

			(Cell::Str(str1), Cell::Str(str2)) => {
				self.unify(Address::Heap(*str1), Address::Heap(*str2))?;
			}

			(Cell::Functor(Functor { name: f1, arity: n1 }), Cell::Functor(Functor { name: f2, arity: n2 }))
				if f1 == f2 && n1 == n2 =>
			{
				for i in 1..=(*n1).into() {
					self.unify(d1 + i, d2 + i)?;
				}
			}

			_ => bail!("Unification error"),
		}

		Ok(())
	}

	fn execute_instruction(&mut self, instruction: &L0Instruction) -> Result<()> {
		match instruction {
			L0Instruction::PutStructure(functor, reg) => {
				let pointer = Cell::Str(self.heap.top() + 1);
				self.var_registers[*reg] = pointer.clone();
				self.heap.push(pointer);
				self.heap.push(Cell::Functor(functor.clone()));
			}

			L0Instruction::SetVariable(reg) => {
				let pointer = Cell::Ref(self.heap.top());
				self.var_registers[*reg] = pointer.clone();
				self.heap.push(pointer);
			}

			L0Instruction::SetValue(reg) => {
				self.heap.push(self.var_registers[*reg].clone());
			}

			L0Instruction::GetStructure(functor, reg) => {
				let addr = self.deref(Address::Register(*reg));

				match self.read_store(addr) {
					Cell::Ref(_) => {
						self.heap.push(Cell::Str(self.heap.top() + 1));
						self.heap.push(Cell::Functor(functor.clone()));
						self.bind(addr, Address::Heap(self.heap.top() - 2))?;
						self.mode = ReadWrite::Write;
					}

					Cell::Str(a) if self.heap[*a] == Cell::Functor(functor.clone()) => {
						self.s = *a + 1;
						self.mode = ReadWrite::Read;
					}

					_ => bail!("Unification error"),
				}
			}

			L0Instruction::UnifyVariable(reg) => {
				match self.mode {
					ReadWrite::Read => {
						self.var_registers[*reg] = self.heap[self.s].clone();
					}

					ReadWrite::Write => {
						let pointer = Cell::Ref(self.heap.top());
						self.var_registers[*reg] = pointer.clone();
						self.heap.push(pointer);
					}
				};

				self.s += 1;
			}

			L0Instruction::UnifyValue(reg) => {
				match self.mode {
					ReadWrite::Read => {
						self.unify(Address::Register(*reg), Address::Heap(self.s))?;
					}

					ReadWrite::Write => {
						self.heap.push(self.var_registers[*reg].clone());
					}
				};

				self.s += 1;
			}
		}

		Ok(())
	}
}
