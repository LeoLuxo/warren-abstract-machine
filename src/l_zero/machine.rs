use crate::ast::Functor;

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
	pub fn new(program: Vec<Instruction>) -> Self {
		Machine { ..Default::default() }
	}

	pub fn execute(&mut self, instructions: &[Instruction]) {
		for i in instructions {
			self.execute_instruction(i);
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

	fn execute_instruction(&mut self, instruction: &Instruction) {
		match instruction {
			Instruction::PutStructure(functor, _) => todo!(),
			Instruction::SetVariable(_) => todo!(),
			Instruction::SetValue(_) => todo!(),
			Instruction::GetStructure(functor, _) => todo!(),
			Instruction::UnifyVariable(_) => todo!(),
			Instruction::UnifyValue(_) => todo!(),
		}
	}
}
