use std::collections::{hash_map::Entry, HashMap, HashSet};

use velcro::vec;

use crate::{
	ast::{Functor, GetFunctor, Term, Variable},
	CompilableProgram, CompilableQuery, Language, Successor, VarRegister,
};

use super::{FirstOrderTerm, L0Instruction, L0};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub type VarMapping = HashMap<Variable, VarRegister>;

impl CompilableProgram<L0> for FirstOrderTerm {
	type SideEffects = ();

	fn compile_as_program(self, _: &mut ()) -> Vec<<L0 as Language>::InstructionSet> {
		let (_, tokens) = flatten_term(
			self.into(),
			&mut HashMap::new(),
			&mut VarRegister::default(),
			FlatteningOrder::TopDown,
		);

		compile_program_tokens(tokens)
	}
}

impl CompilableQuery<L0> for FirstOrderTerm {
	type SideEffects = VarMapping;

	fn compile_as_query(self, var_mapping: &mut VarMapping) -> Vec<<L0 as Language>::InstructionSet> {
		let (_, tokens) = flatten_term(
			self.into(),
			var_mapping,
			&mut VarRegister::default(),
			FlatteningOrder::BottomUp,
		);

		compile_query_tokens(tokens)
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

fn flatten_term(
	term: Term,
	variable_mapping: &mut VarMapping,
	next_id: &mut VarRegister,
	order: FlatteningOrder,
) -> (VarRegister, Vec<MappingToken>) {
	match term {
		Term::Constant(constant) => {
			let id = next_id.incr();

			let tokens = vec![MappingToken::Functor(id, constant.get_functor())];

			(id, tokens)
		}

		Term::Variable(variable) => {
			let id = match variable_mapping.entry(variable) {
				Entry::Occupied(e) => *e.get(),
				Entry::Vacant(e) => *e.insert(next_id.incr()),
			};

			(id, vec![])
		}

		Term::Structure(structure) => {
			let functor = structure.get_functor();
			let id = next_id.incr();

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

fn compile_query_tokens(tokens: Vec<MappingToken>) -> Vec<L0Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for (inst, token) in instructions.iter_mut().zip(tokens) {
		*inst = match token {
			MappingToken::Functor(var, functor) => L0Instruction::PutStructure(functor, var),

			MappingToken::VarRegister(var) if !encountered.contains(&var) => {
				encountered.insert(var);
				L0Instruction::SetVariable(var)
			}

			MappingToken::VarRegister(var) => L0Instruction::SetValue(var),
		}
	}

	instructions
}

fn compile_program_tokens(tokens: Vec<MappingToken>) -> Vec<L0Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for (inst, token) in instructions.iter_mut().zip(tokens) {
		*inst = match token {
			MappingToken::Functor(var, functor) => L0Instruction::GetStructure(functor, var),

			MappingToken::VarRegister(var) if !encountered.contains(&var) => {
				encountered.insert(var);
				L0Instruction::UnifyVariable(var)
			}

			MappingToken::VarRegister(var) => L0Instruction::UnifyValue(var),
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
			flatten_term(Term::parse_from("c")?, &mut Default::default(), &mut VarRegister(0), FlatteningOrder::BottomUp),
			(VarRegister(0), vec![
				MappingToken::Functor(VarRegister(0), Functor { name: "c".to_string(), arity: 0 })
			])
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(Term::parse_from("p(Z, h(Z,W), f(h(c, W)))")?, &mut Default::default(), &mut VarRegister(0), FlatteningOrder::BottomUp),
			(VarRegister(0), vec![
				MappingToken::Functor(VarRegister(6), Functor { name: "c".to_string(), arity: 0 }),
				MappingToken::Functor(VarRegister(5), Functor { name: "h".to_string(), arity: 2 }),
				MappingToken::VarRegister(VarRegister(6)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::Functor(VarRegister(4), Functor { name: "f".to_string(), arity: 1 }),
				MappingToken::VarRegister(VarRegister(5)),
				MappingToken::Functor(VarRegister(2), Functor { name: "h".to_string(), arity: 2 }),
				MappingToken::VarRegister(VarRegister(1)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::Functor(VarRegister(0), Functor { name: "p".to_string(), arity: 3 }),
				MappingToken::VarRegister(VarRegister(1)),
				MappingToken::VarRegister(VarRegister(2)),
				MappingToken::VarRegister(VarRegister(4))
			])
		);

		Ok(())
	}

	#[test]
	fn test_flatten_term_topdown() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			flatten_term(Term::parse_from("c")?, &mut Default::default(), &mut VarRegister(0), FlatteningOrder::TopDown),
			(VarRegister(0), vec![
				MappingToken::Functor(VarRegister(0), Functor { name: "c".to_string(), arity: 0 })
			])
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(Term::parse_from("p(Z, h(Z,W), f(h(c, W)))")?, &mut Default::default(), &mut VarRegister(0), FlatteningOrder::TopDown),
			(VarRegister(0), vec![
				MappingToken::Functor(VarRegister(0), Functor { name: "p".to_string(), arity: 3 }),
				MappingToken::VarRegister(VarRegister(1)),
				MappingToken::VarRegister(VarRegister(2)),
				MappingToken::Functor(VarRegister(2), Functor { name: "h".to_string(), arity: 2 }),
				MappingToken::VarRegister(VarRegister(1)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::VarRegister(VarRegister(4)),
				MappingToken::Functor(VarRegister(4), Functor { name: "f".to_string(), arity: 1 }),
				MappingToken::VarRegister(VarRegister(5)),
				MappingToken::Functor(VarRegister(5), Functor { name: "h".to_string(), arity: 2 }),
				MappingToken::VarRegister(VarRegister(6)),
				MappingToken::Functor(VarRegister(6), Functor { name: "c".to_string(), arity: 0 }),
				MappingToken::VarRegister(VarRegister(3))
			])
		);

		Ok(())
	}
}
