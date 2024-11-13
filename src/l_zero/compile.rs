use std::collections::{hash_map::Entry, HashMap, HashSet};

use velcro::vec;

use crate::{
	ast::{Functor, GetFunctor, Term, Variable},
	CompilableProgram, CompilableQuery, WAMLanguage,
};

use super::{
	machine::{Instruction, VarRegister},
	M0,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub type VarMapping = HashMap<Variable, VarRegister>;

impl CompilableProgram for <M0 as WAMLanguage>::Program {
	type Target = Vec<Instruction>;

	fn compile(self) -> Self::Target {
		let (_, tokens) = flatten_term(self.into(), &mut HashMap::new(), &mut 0, FlatteningOrder::TopDown);
		let instructions = compile_program_tokens(tokens);

		instructions
	}
}

impl CompilableQuery for <M0 as WAMLanguage>::Query {
	type Target = (Vec<Instruction>, VarMapping);

	fn compile(self) -> Self::Target {
		let mut variable_mapping = HashMap::new();
		let (_, tokens) = flatten_term(self.into(), &mut variable_mapping, &mut 0, FlatteningOrder::BottomUp);
		let instructions = compile_query_tokens(tokens);

		(instructions, variable_mapping)
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum FlatteningOrder {
	BottomUp,
	TopDown,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum MappingToken {
	Functor(VarRegister, Functor),
	VarRegister(VarRegister),
}

pub fn flatten_term(
	term: Term,
	variable_mapping: &mut VarMapping,
	next_id: &mut VarRegister,
	order: FlatteningOrder,
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
				let (subid, subtokens) = flatten_term(subterm, variable_mapping, next_id, order);

				match order {
					FlatteningOrder::BottomUp => tokens = vec![..subtokens, ..tokens, MappingToken::VarRegister(subid)],
					FlatteningOrder::TopDown => tokens = vec![..tokens, MappingToken::VarRegister(subid), ..subtokens],
				};
			}

			(id, tokens)
		}
	}
}

fn compile_query_tokens(tokens: Vec<MappingToken>) -> Vec<Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for (inst, token) in instructions.iter_mut().zip(tokens) {
		*inst = match token {
			MappingToken::Functor(var, functor) => Instruction::PutStructure(functor, var),

			MappingToken::VarRegister(var) if !encountered.contains(&var) => {
				encountered.insert(var);
				Instruction::SetVariable(var)
			}

			MappingToken::VarRegister(var) => Instruction::SetValue(var),
		}
	}

	instructions
}

pub fn compile_program_tokens(tokens: Vec<MappingToken>) -> Vec<Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for (inst, token) in instructions.iter_mut().zip(tokens) {
		*inst = match token {
			MappingToken::Functor(var, functor) => Instruction::GetStructure(functor, var),

			MappingToken::VarRegister(var) if !encountered.contains(&var) => {
				encountered.insert(var);
				Instruction::UnifyVariable(var)
			}

			MappingToken::VarRegister(var) => Instruction::UnifyValue(var),
		}
	}

	instructions
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
	fn test_flatten_term_bottomup() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			flatten_term(Term::parse_from("c")?, &mut Default::default(), &mut 0, FlatteningOrder::BottomUp),
			(0, vec![
				MappingToken::Functor(0, Functor { name: "c".to_string(), arity: 0 })
			])
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(Term::parse_from("p(Z, h(Z,W), f(h(c, W)))")?, &mut Default::default(), &mut 0, FlatteningOrder::BottomUp),
			(0, vec![
				MappingToken::Functor(6, Functor { name: "c".to_string(), arity: 0 }),
				MappingToken::Functor(5, Functor { name: "h".to_string(), arity: 2 }),
				MappingToken::VarRegister(6),
				MappingToken::VarRegister(3),
				MappingToken::Functor(4, Functor { name: "f".to_string(), arity: 1 }),
				MappingToken::VarRegister(5),
				MappingToken::Functor(2, Functor { name: "h".to_string(), arity: 2 }),
				MappingToken::VarRegister(1),
				MappingToken::VarRegister(3),
				MappingToken::Functor(0, Functor { name: "p".to_string(), arity: 3 }),
				MappingToken::VarRegister(1),
				MappingToken::VarRegister(2),
				MappingToken::VarRegister(4)
			])
		);

		Ok(())
	}

	#[test]
	fn test_flatten_term_topdown() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			flatten_term(Term::parse_from("c")?, &mut Default::default(), &mut 0, FlatteningOrder::TopDown),
			(0, vec![
				MappingToken::Functor(0, Functor { name: "c".to_string(), arity: 0 })
			])
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(Term::parse_from("p(Z, h(Z,W), f(h(c, W)))")?, &mut Default::default(), &mut 0, FlatteningOrder::TopDown),
			(0, vec![
				MappingToken::Functor(0, Functor { name: "p".to_string(), arity: 3 }),
				MappingToken::VarRegister(1),
				MappingToken::VarRegister(2),
				MappingToken::Functor(2, Functor { name: "h".to_string(), arity: 2 }),
				MappingToken::VarRegister(1),
				MappingToken::VarRegister(3),
				MappingToken::VarRegister(4),
				MappingToken::Functor(4, Functor { name: "f".to_string(), arity: 1 }),
				MappingToken::VarRegister(5),
				MappingToken::Functor(5, Functor { name: "h".to_string(), arity: 2 }),
				MappingToken::VarRegister(6),
				MappingToken::Functor(6, Functor { name: "c".to_string(), arity: 0 }),
				MappingToken::VarRegister(3)
			])
		);

		Ok(())
	}
}
