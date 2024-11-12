use crate::{
	ast::{Functor, GetFunctor, Term, Variable},
	Language, Machine, Substitution,
};
use std::collections::{hash_map::Entry, HashMap};
use velcro::vec;

use super::L0;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

type Address = usize;
type VarRegister = usize;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct M0 {
	heap: Vec<Cell>,
	s: Address,
	var_registers: Vec<Cell>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum Cell {
	#[default]
	Empty,
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

impl M0 {}

impl Machine<L0> for M0 {
	fn from_program(program: <L0 as Language>::Program) -> Self {
		todo!()
	}

	fn submit_query(query: <L0 as Language>::Query) -> Substitution {
		todo!()
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum MappingToken {
	Functor(VarRegister, Functor),
	VarRegister(VarRegister),
}

fn flatten_query_term(
	term: Term,
	variable_mapping: &mut HashMap<Variable, VarRegister>,
	next_id: &mut VarRegister,
) -> (VarRegister, Vec<MappingToken>) {
	let mut incr_id = || {
		let id = *next_id;
		*next_id += 1;
		id
	};

	match term {
		Term::Constant(constant) => {
			let id = incr_id();

			let tokens = vec![MappingToken::Functor(id, constant.get_functor())];

			(id, tokens)
		}

		Term::Variable(variable) => {
			let id = match variable_mapping.entry(variable) {
				Entry::Occupied(e) => *e.get(),
				Entry::Vacant(e) => *e.insert(incr_id()),
			};

			(id, vec![])
		}

		Term::Structure(structure) => {
			let functor = structure.get_functor();
			let id = incr_id();

			let mut tokens = vec![MappingToken::Functor(id, functor)];

			for subterm in structure.arguments {
				let (subid, subtokens) = flatten_query_term(subterm, variable_mapping, next_id);
				tokens = vec![..subtokens, ..tokens, MappingToken::VarRegister(subid)];
			}

			(id, tokens)
		}
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[cfg(test)]
mod tests {
	use crate::parser::Parsable;

	use super::*;
	use anyhow::Result;
	use velcro::vec;

	#[test]
	fn test_flatten_query_term() -> Result<()> {
		assert_eq!(
			flatten_query_term(Term::parse_from("c")?, &mut Default::default(), &mut 0),
			(
				0,
				vec![MappingToken::Functor(
					0,
					Functor {
						name: "c".to_string(),
						arity: 0
					}
				)]
			)
		);

		assert_eq!(
			flatten_query_term(
				Term::parse_from("p(Z, h(Z,W), f(h(c, W)))")?,
				&mut Default::default(),
				&mut 0
			),
			(
				0,
				vec![
					MappingToken::Functor(
						6,
						Functor {
							name: "c".to_string(),
							arity: 0
						}
					),
					MappingToken::Functor(
						5,
						Functor {
							name: "h".to_string(),
							arity: 2
						}
					),
					MappingToken::VarRegister(6),
					MappingToken::VarRegister(3),
					MappingToken::Functor(
						4,
						Functor {
							name: "f".to_string(),
							arity: 1
						}
					),
					MappingToken::VarRegister(5),
					MappingToken::Functor(
						2,
						Functor {
							name: "h".to_string(),
							arity: 2
						}
					),
					MappingToken::VarRegister(1),
					MappingToken::VarRegister(3),
					MappingToken::Functor(
						0,
						Functor {
							name: "p".to_string(),
							arity: 3
						}
					),
					MappingToken::VarRegister(1),
					MappingToken::VarRegister(2),
					MappingToken::VarRegister(4)
				]
			)
		);

		Ok(())
	}
}
