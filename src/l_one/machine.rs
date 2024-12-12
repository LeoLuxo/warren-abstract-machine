
use anyhow::{bail, Result};
use derive_more::derive::Display;

use crate::{
	anonymous::AnonymousIdGenerator,
	ast::Functor,
	enumerate, indent,
	machine_types::{Cell, Code, CodeAddress, Heap, HeapAddress, ReadWrite, StoreAddress, VarRegisters},
	substitution::{self, ExtractSubstitution, SubstTerm},
	universal_compiler::{Combinable, Compiled, Labels},
};

use super::{L1Instruction, L1};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, Default, PartialEq, Eq, Display)]
#[display("M0:\n{}", indent!(format!("mode = {mode}\nS = {s}\n{var_registers}\n\nHEAP:\n{}", indent!(enumerate!(format!("{}", heap))))))]
pub struct M1 {
	mode: ReadWrite,

	s: HeapAddress,
	p: CodeAddress,
	var_registers: VarRegisters<Cell>,

	heap: Heap<Cell>,
	code: Code<L1Instruction>,
}

impl M1 {
	pub fn new() -> Self {
		Default::default()
	}

	pub fn add_code(&mut self, instructions: Vec<L1Instruction>, labels: Labels) {
		self.code.combine(Code { instructions, labels });
	}

	pub fn execute(&mut self) -> Result<()> {
		self.p = 0.into();

		loop {
			let Some(next_p) = self.execute_step()? else {
				// Execute_step has not provided a new PC, which indicates we should halt the machine
				break;
			};

			self.p = next_p;
		}

		Ok(())
	}

	fn read_store(&self, address: StoreAddress) -> &Cell {
		match address {
			StoreAddress::Register(var_register) => &self.var_registers[var_register],
			StoreAddress::Heap(heap_address) => &self.heap[heap_address],
		}
	}

	fn deref(&self, address: StoreAddress) -> StoreAddress {
		match self.read_store(address) {
			Cell::REF(a) if address != *a => self.deref(StoreAddress::Heap(*a)),
			_ => address,
		}
	}

	fn bind(&mut self, address1: StoreAddress, address2: StoreAddress) -> Result<()> {
		let (dest, src) = match (self.read_store(address1), self.read_store(address2)) {
			(Cell::REF(a), c) if *a == address1 => (*a, c),
			(c, Cell::REF(a)) if *a == address2 => (*a, c),
			_ => bail!("Unification error"),
		};

		self.heap[dest] = src.clone();

		Ok(())
	}

	fn unify(&mut self, address1: StoreAddress, address2: StoreAddress) -> Result<()> {
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
				self.unify(StoreAddress::Heap(*str1), StoreAddress::Heap(*str2))?;
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

	fn execute_step(&mut self) -> Result<Option<CodeAddress>> {
		let instruction = &self.code[self.p];

		match instruction {
			// New M1 instructions
			L1Instruction::Call(identifier) => {
				let address = self.code[identifier];
				return Ok(Some(address));
			}

			L1Instruction::Proceed => {
				return Ok(None);
			}

			L1Instruction::PutVariable(xn, ai) => {
				let pointer = Cell::REF(self.heap.top());
				self.var_registers.set(*xn, pointer.clone());
				self.var_registers.set(*ai, pointer.clone());
				self.heap.push(pointer);
			}

			L1Instruction::PutValue(xn, ai) => {
				self.var_registers.set(*ai, self.var_registers[*xn].clone());
			}

			L1Instruction::GetVariable(xn, ai) => {
				self.var_registers.set(*xn, self.var_registers[*ai].clone());
			}

			L1Instruction::GetValue(xn, ai) => {
				self.unify(StoreAddress::Register(*xn), StoreAddress::Register(*ai))?;
			}

			// Same as in M0
			L1Instruction::PutStructure(functor, reg) => {
				let pointer = Cell::STR(self.heap.top() + 1);
				self.var_registers.set(*reg, pointer.clone());
				self.heap.push(pointer);
				self.heap.push(Cell::Functor(functor.clone()));
			}

			L1Instruction::SetVariable(reg) => {
				let pointer = Cell::REF(self.heap.top());
				self.var_registers.set(*reg, pointer.clone());
				self.heap.push(pointer);
			}

			L1Instruction::SetValue(reg) => {
				self.heap.push(self.var_registers[*reg].clone());
			}

			L1Instruction::GetStructure(functor, reg) => {
				let addr = self.deref(StoreAddress::Register(*reg));

				match self.read_store(addr) {
					Cell::REF(_) => {
						self.heap.push(Cell::STR(self.heap.top() + 1));
						self.heap.push(Cell::Functor(functor.clone()));
						self.bind(addr, StoreAddress::Heap(self.heap.top() - 2))?;
						self.mode = ReadWrite::Write;
					}

					Cell::STR(a) if self.heap[*a] == Cell::Functor(functor.clone()) => {
						self.s = *a + 1;
						self.mode = ReadWrite::Read;
					}

					_ => bail!("Unification error"),
				}
			}

			L1Instruction::UnifyVariable(reg) => {
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

			L1Instruction::UnifyValue(reg) => {
				match self.mode {
					ReadWrite::Read => {
						self.unify(StoreAddress::Register(*reg), StoreAddress::Heap(self.s))?;
					}

					ReadWrite::Write => {
						self.heap.push(self.var_registers[*reg].clone());
					}
				};

				self.s += 1;
			}
		}

		Ok(Some(self.p + 1))
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

impl ExtractSubstitution<L1> for M1 {
	fn execute_static_code(&mut self, code: &Compiled<L1>) -> Result<()> {
		self.add_code(code.instructions.clone(), code.labels.clone());
		self.execute()
	}

	fn extract_heap(
		&self,
		address: HeapAddress,
		anon_gen: &mut AnonymousIdGenerator<HeapAddress>,
	) -> Result<SubstTerm> {
		substitution::extract_heap(&self.heap, address, anon_gen, 0)
	}
}
