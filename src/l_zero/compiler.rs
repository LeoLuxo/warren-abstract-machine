use std::collections::{btree_map, HashSet, VecDeque};

use crate::{
	ast::{Functor, GetFunctor, Term},
	CompilableProgram, CompilableQuery, Instructions, Successor, VarRegister, VarToRegMapping,
};

use super::{FirstOrderTerm, L0Instruction, L0};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

impl CompilableProgram<L0> for FirstOrderTerm {
	fn compile_as_program(self) -> Instructions<L0> {
		let tokens = flatten_program_term(self);
		let instructions = compile_program_tokens(tokens);

		instructions.into()
	}
}

impl CompilableQuery<L0> for FirstOrderTerm {
	fn compile_as_query(self) -> (Instructions<L0>, VarToRegMapping) {
		let (tokens, var_mapping) = flatten_query_term(self);
		let instructions = compile_query_tokens(tokens);

		(instructions.into(), var_mapping)
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum FlatteningOrder {
	TopDown,
	BottomUp,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum MappingToken {
	Functor(VarRegister, Functor),
	VarRegister(VarRegister),
}

fn allocate_register_id(
	term: &Term,
	variable_mapping: &mut VarToRegMapping,
	reserved_ids: &mut VarRegister,
) -> VarRegister {
	match term {
		Term::Constant(_) => reserved_ids.pre_incr(),
		Term::Structure(_) => reserved_ids.pre_incr(),

		Term::Variable(variable) => match variable_mapping.entry(variable.clone()) {
			btree_map::Entry::Occupied(e) => *e.get(),
			btree_map::Entry::Vacant(e) => *e.insert(reserved_ids.pre_incr()),
		},
	}
}

fn flatten_term(
	outer_id: VarRegister,
	outer_term: Term,
	variable_mapping: &mut VarToRegMapping,
	reserved_ids: &mut VarRegister,
	order: FlatteningOrder,
) -> Vec<MappingToken> {
	// Turns out it's not important that the register ids follow the exact order they have in the WAMbook
	// The important part is that they respect two rules:
	// - If a same variable already had an id, then it must be assigned that same id
	// - An id as a term argument must match the id of the subterm
	// Anything else doesn't matter in the end

	// But this algorithm preserves it anyway

	// TODO: Make sure an outer-level variable returns a variable token

	let mut tokens = VecDeque::new();

	let mut term_queue = VecDeque::new();
	term_queue.push_back((outer_id, outer_term));

	while let Some((id, term)) = term_queue.pop_front() {
		match (term, order) {
			(Term::Variable(_), _) => {}

			(Term::Constant(constant), FlatteningOrder::TopDown) => {
				tokens.push_back(MappingToken::Functor(id, constant.get_functor()));
			}

			(Term::Constant(constant), FlatteningOrder::BottomUp) => {
				tokens.push_front(MappingToken::Functor(id, constant.get_functor()));
			}

			(Term::Structure(structure), FlatteningOrder::TopDown) => {
				let functor = structure.get_functor();

				tokens.push_back(MappingToken::Functor(id, functor));

				for subterm in structure.arguments {
					let sub_id = allocate_register_id(&subterm, variable_mapping, reserved_ids);
					term_queue.push_back((sub_id, subterm));

					tokens.push_back(MappingToken::VarRegister(sub_id));
				}
			}

			(Term::Structure(structure), FlatteningOrder::BottomUp) => {
				let functor = structure.get_functor();

				for subterm in structure.arguments {
					let sub_id = allocate_register_id(&subterm, variable_mapping, reserved_ids);
					term_queue.push_front((sub_id, subterm));

					// Pushing the argument tokens in front and then reversing their order in-place avoids allocating a temp vec
					tokens.push_front(MappingToken::VarRegister(sub_id));
				}

				// Reverse the order of the structure argument tokens in-place
				// It does require us to make the vecdequeue contiguous but hopefully that should be worth it compared to adding a vec
				tokens.make_contiguous()[0..functor.arity].reverse();

				tokens.push_front(MappingToken::Functor(id, functor));
			}
		}
	}

	tokens.into()
}

fn flatten_query_term(term: FirstOrderTerm) -> (Vec<MappingToken>, VarToRegMapping) {
	let mut var_mapping = VarToRegMapping::default();
	let tokens = flatten_term(
		VarRegister::default(),
		term.into(),
		&mut var_mapping,
		&mut VarRegister::default(),
		FlatteningOrder::BottomUp,
	);

	(tokens, var_mapping)
}

fn flatten_program_term(term: FirstOrderTerm) -> Vec<MappingToken> {
	flatten_term(
		VarRegister::default(),
		term.into(),
		&mut VarToRegMapping::default(),
		&mut VarRegister::default(),
		FlatteningOrder::TopDown,
	)
}

fn compile_query_tokens(tokens: Vec<MappingToken>) -> Vec<L0Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for token in tokens {
		#[rustfmt::skip]
		let (reg, inst) = match token {
			MappingToken::Functor(reg, functor)                          => (reg, L0Instruction::PutStructure(functor, reg)),
			MappingToken::VarRegister(reg) if encountered.contains(&reg) => (reg, L0Instruction::SetValue(reg)),
			MappingToken::VarRegister(reg)                               => (reg, L0Instruction::SetVariable(reg)),
		};

		encountered.insert(reg);
		instructions.push(inst);
	}

	instructions
}

fn compile_program_tokens(tokens: Vec<MappingToken>) -> Vec<L0Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for token in tokens {
		#[rustfmt::skip]
		let (reg, inst) = match token {
			MappingToken::Functor(reg, functor)                          => (reg, L0Instruction::GetStructure(functor, reg)),
			MappingToken::VarRegister(reg) if encountered.contains(&reg) => (reg, L0Instruction::UnifyValue(reg)),
			MappingToken::VarRegister(reg)                               => (reg, L0Instruction::UnifyVariable(reg)),
		};

		encountered.insert(reg);
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
	fn test_flatten_query_term() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			flatten_query_term("c".parse()?).0,
			vec![
				MappingToken::Functor(VarRegister(1), Functor { name: "c".into(), arity: 0 })
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_query_term("p(X,Y,Z,Y,X)".parse()?).0,
			vec![
				MappingToken::Functor(VarRegister(1), Functor { name: "p".into(), arity: 5 }),
				MappingToken::VarRegister(VarRegister(2)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::VarRegister(VarRegister(4)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::VarRegister(VarRegister(2)),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_query_term("p(f(X), h(Y, f(a)), Y)".parse()?).0,
			vec![
				MappingToken::Functor(VarRegister(2), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(VarRegister(7)),
				MappingToken::Functor(VarRegister(6), Functor { name: "a".into(), arity: 0 }),
				MappingToken::Functor(VarRegister(5), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(VarRegister(6)),
				MappingToken::Functor(VarRegister(3), Functor { name: "h".into(), arity: 2 }),
				MappingToken::VarRegister(VarRegister(4)),
				MappingToken::VarRegister(VarRegister(5)),
				MappingToken::Functor(VarRegister(1), Functor { name: "p".into(), arity: 3 }),
				MappingToken::VarRegister(VarRegister(2)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::VarRegister(VarRegister(4)),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_query_term("p(Z, h(Z,W), f(W))".parse()?).0,
			vec![
				MappingToken::Functor(VarRegister(3), Functor { name: "h".into(), arity: 2 }),
				MappingToken::VarRegister(VarRegister(2)),
				MappingToken::VarRegister(VarRegister(5)),
				MappingToken::Functor(VarRegister(4), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(VarRegister(5)),
				MappingToken::Functor(VarRegister(1), Functor { name: "p".into(), arity: 3 }),
				MappingToken::VarRegister(VarRegister(2)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::VarRegister(VarRegister(4)),
			]
		);

		Ok(())
	}

	#[test]
	fn test_flatten_program_term() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			flatten_program_term("c".parse()?),
			vec![
				MappingToken::Functor(VarRegister(1), Functor { name: "c".into(), arity: 0 })
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_program_term("p(X,Y,Z,Y,X)".parse()?),
			vec![
				MappingToken::Functor(VarRegister(1), Functor { name: "p".into(), arity: 5 }),
				MappingToken::VarRegister(VarRegister(2)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::VarRegister(VarRegister(4)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::VarRegister(VarRegister(2)),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_program_term("p(f(X), h(Y, f(a)), Y)".parse()?),
			vec![
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
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_program_term("p(Z, h(Z,W), f(W))".parse()?),
			vec![
				MappingToken::Functor(VarRegister(1), Functor { name: "p".into(), arity: 3 }),
				MappingToken::VarRegister(VarRegister(2)),
				MappingToken::VarRegister(VarRegister(3)),
				MappingToken::VarRegister(VarRegister(4)),
				MappingToken::Functor(VarRegister(3), Functor { name: "h".into(), arity: 2 }),
				MappingToken::VarRegister(VarRegister(2)),
				MappingToken::VarRegister(VarRegister(5)),
				MappingToken::Functor(VarRegister(4), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(VarRegister(5)),
			]
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

	#[test]
	fn test_compile_query() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			"p(Z, h(Z,W), f(W))"
				.parse::<FirstOrderTerm>()?
				.compile_as_query().0,
			vec![
				L0Instruction::PutStructure(Functor { name: "h".into(), arity: 2 }, VarRegister(3) ),
				L0Instruction::SetVariable(VarRegister(2)),
				L0Instruction::SetVariable(VarRegister(5)),
				L0Instruction::PutStructure(Functor { name: "f".into(), arity: 1 }, VarRegister(4) ),
				L0Instruction::SetValue(VarRegister(5)),
				L0Instruction::PutStructure(Functor { name: "p".into(), arity: 3 }, VarRegister(1) ),
				L0Instruction::SetValue(VarRegister(2)),
				L0Instruction::SetValue(VarRegister(3)),
				L0Instruction::SetValue(VarRegister(4)),
			]
			.into()
		);

		Ok(())
	}
}
