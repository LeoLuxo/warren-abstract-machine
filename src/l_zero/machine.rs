use std::ops::{Add, AddAssign, Sub, SubAssign};

use anyhow::{bail, Result};
use derive_more::derive::Display;

use crate::{
	ast::{Constant, Functor, Structure},
	enumerate, indent,
	machine_types::{Heap, HeapAddress, VarRegister, VarRegisters},
	subst::{AnonymousIdGenerator, ExtractSubstitution, SubstTerm},
};

use super::{L0Instruction, L0};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Copy, Clone, Debug, PartialEq, Eq, Display)]
enum MachineAddress {
	Register(VarRegister),
	Heap(HeapAddress),
}

#[rustfmt::skip] impl PartialEq<VarRegister> for MachineAddress { fn eq(&self, other: &VarRegister) -> bool { match self  { MachineAddress::Register(var_register) => var_register == other, _ => false, } } }
#[rustfmt::skip] impl PartialEq<MachineAddress> for VarRegister { fn eq(&self, other: &MachineAddress)     -> bool { match other { MachineAddress::Register(var_register) => var_register == self,  _ => false, } } }
#[rustfmt::skip] impl PartialEq<HeapAddress> for MachineAddress { fn eq(&self, other: &HeapAddress) -> bool { match self  { MachineAddress::Heap(heap_address)     => heap_address == other, _ => false, } } }
#[rustfmt::skip] impl PartialEq<MachineAddress> for HeapAddress { fn eq(&self, other: &MachineAddress)     -> bool { match other { MachineAddress::Heap(heap_address)     => heap_address == self,  _ => false, } } }

#[rustfmt::skip] impl Add<usize> for MachineAddress { type Output = Self; fn add(self, rhs: usize) -> Self::Output { match self { MachineAddress::Register(var_register) => MachineAddress::Register(var_register + rhs), MachineAddress::Heap(heap_address) => MachineAddress::Heap(heap_address + rhs), } } }
#[rustfmt::skip] impl Sub<usize> for MachineAddress { type Output = Self; fn sub(self, rhs: usize) -> Self::Output { match self { MachineAddress::Register(var_register) => MachineAddress::Register(var_register - rhs), MachineAddress::Heap(heap_address) => MachineAddress::Heap(heap_address - rhs), } } }
#[rustfmt::skip] impl AddAssign<usize> for MachineAddress { fn add_assign(&mut self, rhs: usize)  {  match self { MachineAddress::Register(var_register) => *var_register += rhs, MachineAddress::Heap(heap_address) => *heap_address += rhs, } } }
#[rustfmt::skip] impl SubAssign<usize> for MachineAddress { fn sub_assign(&mut self, rhs: usize)  {  match self { MachineAddress::Register(var_register) => *var_register -= rhs, MachineAddress::Heap(heap_address) => *heap_address -= rhs, } } }

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Display)]
enum Cell {
	#[display("REF {}", _0)]
	REF(HeapAddress),

	#[display("STR {}", _0)]
	STR(HeapAddress),

	Functor(Functor),
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Display)]
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

#[derive(Clone, Debug, Default, PartialEq, Eq, Display)]
#[display("M0:\n{}", indent!(format!("mode = {mode}\nS = {s}\n{var_registers}\n\nHEAP:\n{}", indent!(enumerate!(format!("{}", heap))))))]
pub struct M0 {
	mode: ReadWrite,

	s: HeapAddress,
	var_registers: VarRegisters<Cell>,

	heap: Heap<Cell>,
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

	fn read_store(&self, address: MachineAddress) -> &Cell {
		match address {
			MachineAddress::Register(var_register) => &self.var_registers[var_register],
			MachineAddress::Heap(heap_address) => &self.heap[heap_address],
		}
	}

	fn deref(&self, address: MachineAddress) -> MachineAddress {
		match self.read_store(address) {
			Cell::REF(a) if address != *a => self.deref(MachineAddress::Heap(*a)),
			_ => address,
		}
	}

	fn bind(&mut self, address1: MachineAddress, address2: MachineAddress) -> Result<()> {
		let (dest, src) = match (self.read_store(address1), self.read_store(address2)) {
			(Cell::REF(a), c) if *a == address1 => (*a, c),
			(c, Cell::REF(a)) if *a == address2 => (*a, c),
			_ => bail!("Unification error"),
		};

		self.heap[dest] = src.clone();

		Ok(())
	}

	fn unify(&mut self, address1: MachineAddress, address2: MachineAddress) -> Result<()> {
		let d1 = self.deref(address1);
		let d2 = self.deref(address2);

		if d1 == d2 {
			return Ok(());
		}

		match (self.read_store(d1), self.read_store(d2)) {
			(Cell::REF(_), _) | (_, Cell::REF(_)) => {
				self.bind(d1, d2)?;
			}

			(Cell::STR(str1), Cell::STR(str2)) => {
				self.unify(MachineAddress::Heap(*str1), MachineAddress::Heap(*str2))?;
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

	fn execute_instruction(&mut self, instruction: &L0Instruction) -> Result<()> {
		match instruction {
			L0Instruction::PutStructure(functor, reg) => {
				let pointer = Cell::STR(self.heap.top() + 1);
				self.var_registers.set(*reg, pointer.clone());
				self.heap.push(pointer);
				self.heap.push(Cell::Functor(functor.clone()));
			}

			L0Instruction::SetVariable(reg) => {
				let pointer = Cell::REF(self.heap.top());
				self.var_registers.set(*reg, pointer.clone());
				self.heap.push(pointer);
			}

			L0Instruction::SetValue(reg) => {
				self.heap.push(self.var_registers[*reg].clone());
			}

			L0Instruction::GetStructure(functor, reg) => {
				let addr = self.deref(MachineAddress::Register(*reg));

				match self.read_store(addr) {
					Cell::REF(_) => {
						self.heap.push(Cell::STR(self.heap.top() + 1));
						self.heap.push(Cell::Functor(functor.clone()));
						self.bind(addr, MachineAddress::Heap(self.heap.top() - 2))?;
						self.mode = ReadWrite::Write;
					}

					Cell::STR(a) if self.heap[*a] == Cell::Functor(functor.clone()) => {
						self.s = *a + 1;
						self.mode = ReadWrite::Read;
					}

					_ => bail!("Unification error"),
				}
			}

			L0Instruction::UnifyVariable(reg) => {
				match self.mode {
					ReadWrite::Read => {
						self.var_registers.set(*reg, self.heap[self.s].clone());
					}

					ReadWrite::Write => {
						let pointer = Cell::REF(self.heap.top());
						self.var_registers.set(*reg, pointer.clone());
						self.heap.push(pointer);
					}
				};

				self.s += 1;
			}

			L0Instruction::UnifyValue(reg) => {
				match self.mode {
					ReadWrite::Read => {
						self.unify(MachineAddress::Register(*reg), MachineAddress::Heap(self.s))?;
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

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

fn extract_heap(
	heap: &Heap<Cell>,
	address: HeapAddress,
	anon_gen: &mut AnonymousIdGenerator<HeapAddress>,
	iterations: u32,
) -> Result<SubstTerm> {
	match &heap[address] {
		_ if iterations > heap.len() as u32 => {
			bail!("Non-terminating substitution encountered. The solution doesn't satisfy the occurs-check.")
		}

		Cell::REF(a) if address != *a => extract_heap(heap, *a, anon_gen, iterations + 1),
		Cell::STR(a) if address != *a => extract_heap(heap, *a, anon_gen, iterations + 1),

		Cell::REF(a) => Ok(SubstTerm::Unbound(anon_gen.get_identifier(*a))),

		Cell::Functor(Functor { name, arity }) if *arity == 0 => Ok(SubstTerm::Constant(Constant(name.clone()))),

		Cell::Functor(Functor { name, arity }) => Ok(SubstTerm::Structure(Structure {
			name: name.clone(),
			arguments: (1..=*arity)
				.map(|i| extract_heap(heap, address + i, anon_gen, iterations + 1))
				.collect::<Result<Vec<_>>>()?
				.into(),
		})),

		_ => bail!("Machine yielded an invalid substitution. This might be a bug."),
	}
}

impl ExtractSubstitution<L0> for M0 {
	fn extract_heap(
		&self,
		address: HeapAddress,
		anon_gen: &mut AnonymousIdGenerator<HeapAddress>,
	) -> Result<SubstTerm> {
		extract_heap(&self.heap, address, anon_gen, 0)
	}
}
