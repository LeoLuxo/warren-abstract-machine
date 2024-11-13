use anyhow::{bail, Result};
use machine::{Instruction, Machine};

use crate::{
	ast::{Constant, Structure, Term},
	parser::Parsable,
	CompilableProgram, CompilableQuery, Substitution, WAMLanguage,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

mod compile;
mod machine;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct M0 {
	program: Vec<Instruction>,
	machine: Machine,
}

impl M0 {
	fn new(program: Vec<Instruction>) -> Self {
		Self {
			program,
			..Default::default()
		}
	}
}

impl WAMLanguage for M0 {
	type Program = FirstOrderTerm;
	type Query = FirstOrderTerm;

	fn from_program(program: Self::Program) -> Self {
		Self::new(program.compile_as_program())
	}

	fn submit_query(&mut self, query: Self::Query) -> Substitution {
		let (query, var_mapping) = query.compile_as_query();

		self.machine.execute(&query);
		self.machine.execute(&self.program);

		// let substitution = HashMap::new();
		for (var, register) in var_mapping.into_iter() {
			// substitution[&var] = self.read_register(register).clone()
			println!("{:?}: {:?}", var, register);
		}
		// substitution
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub enum FirstOrderTerm {
	Constant(Constant),
	Structure(Structure),
}

impl TryInto<FirstOrderTerm> for Term {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<FirstOrderTerm, Self::Error> {
		Ok(match self {
			Term::Constant(constant) => FirstOrderTerm::Constant(constant),
			Term::Structure(structure) => FirstOrderTerm::Structure(structure),
			_ => bail!("Cannot convert term to first order term"),
		})
	}
}

impl From<FirstOrderTerm> for Term {
	fn from(val: FirstOrderTerm) -> Self {
		match val {
			FirstOrderTerm::Constant(constant) => Term::Constant(constant),
			FirstOrderTerm::Structure(structure) => Term::Structure(structure),
		}
	}
}

impl Parsable for FirstOrderTerm {
	fn parse_from(source: &str) -> Result<Self> {
		Term::parse_from(source)?.try_into()
	}
}
