use std::collections::{hash_map::Entry, HashMap, HashSet};

use velcro::vec;

use crate::{
	ast::{Functor, GetFunctor, Term},
	CompilableProgram, CompilableQuery, Instructions, RegisterMapping, Successor, VarRegister,
};

use super::{FirstOrderTerm, L0Instruction, L0};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

impl CompilableProgram<L0> for FirstOrderTerm {
	fn compile_as_program(self) -> Instructions<L0> {
		let (_, tokens) = flatten_term(
			self.into(),
			&mut RegisterMapping::default(),
			&mut VarRegister::default(),
			FlatteningOrder::TopDown,
		);
		let instructions = compile_program_tokens(tokens);

		instructions.into()
	}
}

impl CompilableQuery<L0> for FirstOrderTerm {
	fn compile_as_query(self) -> (Instructions<L0>, RegisterMapping) {
		let mut var_mapping = RegisterMapping::default();

		let (_, tokens) = flatten_term(
			self.into(),
			&mut var_mapping,
			&mut VarRegister::default(),
			FlatteningOrder::BottomUp,
		);
		let instructions = compile_query_tokens(tokens);

		(instructions.into(), var_mapping)
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
	variable_mapping: &mut RegisterMapping,
	free_id: &mut VarRegister,
	order: FlatteningOrder,
) -> (VarRegister, Vec<MappingToken>) {
	// Turns out it's not important that the register ids follow the exact order they have in the WAMbook
	// The important part is that they respect two rules:
	// - If a same variable already had an idea, then it must be assigned that same id
	// - An id as a term argument must match the id of the subterm
	// Anything else doesn't matter in the end

	match term {
		Term::Constant(constant) => {
			let id = free_id.post_incr();
			(id, vec![MappingToken::Functor(id, constant.get_functor())])
		}

		Term::Variable(variable) => {
			let id = match variable_mapping.entry(variable) {
				Entry::Occupied(e) => *e.get(),
				Entry::Vacant(e) => *e.insert(free_id.post_incr()),
			};

			(id, vec![])
		}

		Term::Structure(structure) => {
			let term_id = free_id.post_incr();
			let functor = structure.get_functor();

			let mut term_tokens = vec![MappingToken::Functor(term_id, functor)];
			let mut subterm_tokens = vec![];

			for subterm in structure.arguments {
				let (subid, subtokens) = flatten_term(subterm, variable_mapping, free_id, order);

				term_tokens.push(MappingToken::VarRegister(subid));
				subterm_tokens.extend(subtokens);
			}

			let tokens = match order {
				FlatteningOrder::BottomUp => {
					vec![..subterm_tokens, ..term_tokens]
				}
				FlatteningOrder::TopDown => {
					vec![..term_tokens, ..subterm_tokens]
				}
			};

			(term_id, tokens)
		}
	}
}

fn compile_query_tokens(tokens: Vec<MappingToken>) -> Vec<L0Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for token in tokens {
		let inst = match token {
			MappingToken::Functor(var, functor) => L0Instruction::PutStructure(functor, var),

			MappingToken::VarRegister(var) if !encountered.contains(&var) => {
				encountered.insert(var);
				L0Instruction::SetVariable(var)
			}

			MappingToken::VarRegister(var) => L0Instruction::SetValue(var),
		};

		instructions.push(inst);
	}

	instructions
}

fn compile_program_tokens(tokens: Vec<MappingToken>) -> Vec<L0Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for token in tokens {
		let inst = match token {
			MappingToken::Functor(var, functor) => L0Instruction::GetStructure(functor, var),

			MappingToken::VarRegister(var) if !encountered.contains(&var) => {
				encountered.insert(var);
				L0Instruction::UnifyVariable(var)
			}

			MappingToken::VarRegister(var) => L0Instruction::UnifyValue(var),
		};

		instructions.push(inst);
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

	use super::*;
	use anyhow::Result;
	use velcro::vec;

	#[test]
	fn test_flatten_term_bottomup() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			flatten_term("c".parse()?, &mut Default::default(), &mut VarRegister(0), FlatteningOrder::BottomUp),
			(VarRegister(0), vec![
				MappingToken::Functor(VarRegister(0), Functor { name: "c".into(), arity: 0 })
			])
		);

		panic!(
			"{:#?}",
			flatten_term(
				"p(A,B(c),C)".parse()?,
				&mut Default::default(),
				&mut VarRegister(1),
				FlatteningOrder::TopDown
			)
		);

		// #[rustfmt::skip]
		// assert_eq!(
		// 	flatten_term("p(Z, h(Z,W), f(h(c, W)))".parse()?, &mut Default::default(), &mut VarRegister(0), FlatteningOrder::BottomUp),
		// 	(VarRegister(0), vec![
		// 		MappingToken::Functor(VarRegister(6), Functor { name: "c".into(), arity: 0 }),
		// 		MappingToken::Functor(VarRegister(5), Functor { name: "h".into(), arity: 2 }),
		// 		MappingToken::VarRegister(VarRegister(6)),
		// 		MappingToken::VarRegister(VarRegister(3)),
		// 		MappingToken::Functor(VarRegister(4), Functor { name: "f".into(), arity: 1 }),
		// 		MappingToken::VarRegister(VarRegister(5)),
		// 		MappingToken::Functor(VarRegister(2), Functor { name: "h".into(), arity: 2 }),
		// 		MappingToken::VarRegister(VarRegister(1)),
		// 		MappingToken::VarRegister(VarRegister(3)),
		// 		MappingToken::Functor(VarRegister(0), Functor { name: "p".into(), arity: 3 }),
		// 		MappingToken::VarRegister(VarRegister(1)),
		// 		MappingToken::VarRegister(VarRegister(2)),
		// 		MappingToken::VarRegister(VarRegister(4))
		// 	])
		// );

		// #[rustfmt::skip]
		// assert_eq!(
		// 	flatten_term("p(Z, h(Z,W), f(W))".parse()?, &mut Default::default(), &mut VarRegister(1), FlatteningOrder::BottomUp),
		// 	(VarRegister(1), vec![
		// 		MappingToken::Functor(VarRegister(3), Functor { name: "h".into(), arity: 2 }),
		// 		MappingToken::VarRegister(VarRegister(2)),
		// 		MappingToken::VarRegister(VarRegister(5)),
		// 		MappingToken::Functor(VarRegister(4), Functor { name: "f".into(), arity: 1 }),
		// 		MappingToken::VarRegister(VarRegister(5)),
		// 		MappingToken::Functor(VarRegister(1), Functor { name: "p".into(), arity: 3 }),
		// 		MappingToken::VarRegister(VarRegister(2)),
		// 		MappingToken::VarRegister(VarRegister(3)),
		// 		MappingToken::VarRegister(VarRegister(4))
		// 	])
		// );

		Ok(())
	}

	#[test]
	fn test_flatten_term_topdown() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			flatten_term("c".parse()?, &mut Default::default(), &mut VarRegister(0), FlatteningOrder::TopDown),
			(VarRegister(0), vec![
				MappingToken::Functor(VarRegister(0), Functor { name: "c".into(), arity: 0 })
			])
		);

		// #[rustfmt::skip]
		// assert_eq!(
		// 	flatten_term("p(Z, h(Z,W), f(h(c, W)))".parse()?, &mut Default::default(), &mut VarRegister(0), FlatteningOrder::TopDown),
		// 	(VarRegister(0), vec![
		// 		MappingToken::Functor(VarRegister(0), Functor { name: "p".into(), arity: 3 }),
		// 		MappingToken::VarRegister(VarRegister(1)),
		// 		MappingToken::VarRegister(VarRegister(2)),
		// 		MappingToken::Functor(VarRegister(2), Functor { name: "h".into(), arity: 2 }),
		// 		MappingToken::VarRegister(VarRegister(1)),
		// 		MappingToken::VarRegister(VarRegister(3)),
		// 		MappingToken::VarRegister(VarRegister(4)),
		// 		MappingToken::Functor(VarRegister(4), Functor { name: "f".into(), arity: 1 }),
		// 		MappingToken::VarRegister(VarRegister(5)),
		// 		MappingToken::Functor(VarRegister(5), Functor { name: "h".into(), arity: 2 }),
		// 		MappingToken::VarRegister(VarRegister(6)),
		// 		MappingToken::Functor(VarRegister(6), Functor { name: "c".into(), arity: 0 }),
		// 		MappingToken::VarRegister(VarRegister(3))
		// 	])
		// );

		#[rustfmt::skip]
		assert_eq!(
			flatten_term("p(f(X), h(Y, f(a)), Y)".parse()?, &mut Default::default(), &mut VarRegister(1), FlatteningOrder::TopDown),
			(VarRegister(1), vec![
				MappingToken::Functor(VarRegister(1), Functor { name: "p".into(), arity: 3 }),
				MappingToken::VarRegister(VarRegister(2)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::VarRegister(VarRegister(4)),
				MappingToken::Functor(VarRegister(2), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(VarRegister(5)),
				MappingToken::Functor(VarRegister(3), Functor { name: "h".into(), arity: 2 }),
				MappingToken::VarRegister(VarRegister(4)),
				MappingToken::VarRegister(VarRegister(6)),
				MappingToken::Functor(VarRegister(6), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(VarRegister(7)),
				MappingToken::Functor(VarRegister(7), Functor { name: "a".into(), arity: 0 }),
			])
		);

		Ok(())
	}

	#[test]
	fn test_compile_program() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			"p(f(X), h(Y, f(a)), Y)"
				.parse::<FirstOrderTerm>()?
				.compile_as_program(),
			vec![
				L0Instruction::GetStructure(Functor { name: "p".into(), arity: 3 }, VarRegister(1) ),
				L0Instruction::UnifyVariable(VarRegister(2)),
				L0Instruction::UnifyVariable(VarRegister(3)),
				L0Instruction::UnifyVariable(VarRegister(4)),
				L0Instruction::GetStructure(Functor { name: "f".into(), arity: 1 }, VarRegister(2) ),
				L0Instruction::UnifyVariable(VarRegister(5)),
				L0Instruction::GetStructure(Functor { name: "h".into(), arity: 2 }, VarRegister(3) ),
				L0Instruction::UnifyValue(VarRegister(4)),
				L0Instruction::UnifyVariable(VarRegister(6)),
				L0Instruction::GetStructure(Functor { name: "f".into(), arity: 1 }, VarRegister(6) ),
				L0Instruction::UnifyVariable(VarRegister(7)),
				L0Instruction::GetStructure(Functor { name: "a".into(), arity: 0 }, VarRegister(7) ),
			]
			.into()
		);

		Ok(())
	}
}
