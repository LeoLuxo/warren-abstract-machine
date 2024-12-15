//! Provides types and implementations common to all language compilers (or at least L0 and L1).
//! Also provides the interface/traits that unify the compiler interfaces.

use std::{
	collections::{hash_map, HashMap, VecDeque},
	fmt::Display,
	mem,
};

use anyhow::Result;
use derive_more::derive::{Display, From};
use velcro::vec;

use crate::{
	ast::{Functor, GetFunctor, Identifier, Term, Variable},
	display_iter, display_map,
	machine_types::{CodeAddress, VarRegister},
	substitution::VarToRegMapping,
	util::Successor,
	Language,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait CompilableProgram<L: Language> {
	fn compile_as_program(self) -> Result<Compiled<L>>;
}

pub trait CompilableQuery<L: Language> {
	fn compile_as_query(self) -> Result<Compiled<L>>;
}

pub type Labels = HashMap<Identifier, CodeAddress>;

/// Represents a compiled piece of code (regardless whether program or query) for a given Language.
#[derive(Clone, Debug, PartialEq, Eq, From, Display)]
#[display("{}\n{}\n{}", display_iter!(instructions, "\n"), display_map!(labels), var_reg_mapping.as_ref().map_or("(without mapping)".to_string(), |m| format!("(where {})", m)))]
#[display(bounds(L::InstructionSet: Display))]
pub struct Compiled<L: Language> {
	/// The sequence of instructions of the code
	pub instructions: Vec<L::InstructionSet>,

	/// The symbolic labels of the code
	pub labels: Labels,

	/// The variable -> register mapping computed during compilation, is important to extract the substitution set
	pub var_reg_mapping: Option<VarToRegMapping>,
}

impl<L: Language> Default for Compiled<L> {
	fn default() -> Self {
		Self {
			instructions: Default::default(),
			var_reg_mapping: Default::default(),
			labels: Default::default(),
		}
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait Combinable {
	fn combined(self, other: Self) -> Self;

	fn combine(&mut self, other: Self)
	where
		Self: Default,
	{
		*self = mem::take(self).combined(other)
	}
}

impl<I> Combinable for (Vec<I>, HashMap<Identifier, CodeAddress>) {
	fn combined(self, other: Self) -> Self {
		let label_offset = self.0.len();

		// self's instructions then other's instructions, in order
		let instructions = vec![..self.0, ..other.0];

		let labels = self
			.1
			.into_iter()
			.chain(other.1.into_iter().map(|(ident, addr)| (ident, addr + label_offset)))
			.collect();

		(instructions, labels)
	}
}

impl<L: Language> Combinable for Compiled<L> {
	fn combined(self, other: Self) -> Self {
		// self's mapping takes priority over other's mapping (overwritten where needed)
		let var_reg_mapping = match (self.var_reg_mapping, other.var_reg_mapping) {
			(Some(m1), Some(m2)) => Some(m2.into_iter().chain(m1).collect()),
			(_, Some(m)) | (Some(m), _) => Some(m),
			_ => None,
		};

		let (instructions, labels) = (self.instructions, self.labels).combined((other.instructions, other.labels));

		Self {
			instructions,
			var_reg_mapping,
			labels,
		}
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FlatteningOrder {
	TopDown,
	BottomUp,
}

impl FlatteningOrder {
	pub fn for_program() -> Self {
		FlatteningOrder::TopDown
	}

	pub fn for_query() -> Self {
		FlatteningOrder::BottomUp
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MappingToken {
	Functor(VarRegister, Functor),
	VarRegister(VarRegister),
	ArgumentRegister(VarRegister, VarRegister),
}

fn allocate_new_register_id(
	term: &Term,
	variable_mapping: &mut HashMap<Variable, VarRegister>,
	reserved_ids: &mut VarRegister,
) -> VarRegister {
	match term {
		Term::Constant(_) => reserved_ids.pre_incr(),
		Term::Structure(_) => reserved_ids.pre_incr(),

		Term::Variable(variable) => match variable_mapping.entry(variable.clone()) {
			hash_map::Entry::Occupied(e) => *e.get(),
			hash_map::Entry::Vacant(e) => *e.insert(reserved_ids.pre_incr()),
		},
	}
}

pub fn flatten_term(
	outer_id: VarRegister,
	outer_term: Term,
	variable_mapping: &mut HashMap<Variable, VarRegister>,
	reserved_ids: &mut VarRegister,
	order: FlatteningOrder,
) -> Vec<MappingToken> {
	// Turns out it's not important that the register ids follow the exact order they have in the WAMbook
	// The important part is that they respect two rules:
	// - If a same variable already had an id, then it must be assigned that same id
	// - An id as a term argument must match the id of the subterm
	// Anything else doesn't matter in the end.
	// I had the realization above while I was reworking the recursive version of this algorithm.
	// But in the end this imperative version preserves the wambook order anyway so it doesn't matter

	// Handle separately the case where the root term is a variable
	if let Term::Variable(_) = &outer_term {
		let xn = allocate_new_register_id(&outer_term, variable_mapping, reserved_ids);
		let ai = outer_id;

		let token = MappingToken::ArgumentRegister(xn, ai);
		return vec![token];
	}

	// The other cases still work fine:
	// If we are in argument position, then outer_id should be Ai, which we indeed want to explicitly pass as the reg/id for a structure or a constant

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
					let sub_id = allocate_new_register_id(&subterm, variable_mapping, reserved_ids);
					term_queue.push_back((sub_id, subterm));

					tokens.push_back(MappingToken::VarRegister(sub_id));
				}
			}

			(Term::Structure(structure), FlatteningOrder::BottomUp) => {
				let functor = structure.get_functor();

				for subterm in structure.arguments {
					let sub_id = allocate_new_register_id(&subterm, variable_mapping, reserved_ids);
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

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[cfg(test)]
mod tests {
	use crate::parser::ParseAs;

	use super::*;
	use anyhow::Result;
	use velcro::vec;

	#[test]
	fn test_flatten_term_bottomup() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "c".parse_as()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::BottomUp),
			vec![
				MappingToken::Functor("X1".parse_as()?, "c/0".parse_as()?)
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(X,Y,Z,Y,X)".parse_as()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::BottomUp),
			vec![
				MappingToken::Functor("X1".parse_as()?, "p/5".parse_as()?),
				MappingToken::VarRegister("X2".parse_as()?),
				MappingToken::VarRegister("X3".parse_as()?),
				MappingToken::VarRegister("X4".parse_as()?),
				MappingToken::VarRegister("X3".parse_as()?),
				MappingToken::VarRegister("X2".parse_as()?),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(f(X), h(Y, f(a)), Y)".parse_as()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::BottomUp),
			vec![
				MappingToken::Functor("X2".parse_as()?, "f/1".parse_as()?),
				MappingToken::VarRegister("X7".parse_as()?),
				MappingToken::Functor("X6".parse_as()?, "a/0".parse_as()?),
				MappingToken::Functor("X5".parse_as()?, "f/1".parse_as()?),
				MappingToken::VarRegister("X6".parse_as()?),
				MappingToken::Functor("X3".parse_as()?, "h/2".parse_as()?),
				MappingToken::VarRegister("X4".parse_as()?),
				MappingToken::VarRegister("X5".parse_as()?),
				MappingToken::Functor("X1".parse_as()?, "p/3".parse_as()?),
				MappingToken::VarRegister("X2".parse_as()?),
				MappingToken::VarRegister("X3".parse_as()?),
				MappingToken::VarRegister("X4".parse_as()?),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(Z, h(Z,W), f(W))".parse_as()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::BottomUp),
			vec![
				MappingToken::Functor("X3".parse_as()?, "h/2".parse_as()?),
				MappingToken::VarRegister("X2".parse_as()?),
				MappingToken::VarRegister("X5".parse_as()?),
				MappingToken::Functor("X4".parse_as()?, "f/1".parse_as()?),
				MappingToken::VarRegister("X5".parse_as()?),
				MappingToken::Functor("X1".parse_as()?, "p/3".parse_as()?),
				MappingToken::VarRegister("X2".parse_as()?),
				MappingToken::VarRegister("X3".parse_as()?),
				MappingToken::VarRegister("X4".parse_as()?),
			]
		);

		Ok(())
	}

	#[test]
	fn test_flatten_term_topdown() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "c".parse_as()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::TopDown),
			vec![
				MappingToken::Functor("X1".parse_as()?, "c/0".parse_as()?)
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(X,Y,Z,Y,X)".parse_as()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::TopDown),
			vec![
				MappingToken::Functor("X1".parse_as()?, "p/5".parse_as()?),
				MappingToken::VarRegister("X2".parse_as()?),
				MappingToken::VarRegister("X3".parse_as()?),
				MappingToken::VarRegister("X4".parse_as()?),
				MappingToken::VarRegister("X3".parse_as()?),
				MappingToken::VarRegister("X2".parse_as()?),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(f(X), h(Y, f(a)), Y)".parse_as()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::TopDown),
			vec![
				MappingToken::Functor("X1".parse_as()?, "p/3".parse_as()?),
				MappingToken::VarRegister("X2".parse_as()?),
				MappingToken::VarRegister("X3".parse_as()?),
				MappingToken::VarRegister("X4".parse_as()?),
				MappingToken::Functor("X2".parse_as()?, "f/1".parse_as()?),
				MappingToken::VarRegister("X5".parse_as()?),
				MappingToken::Functor("X3".parse_as()?, "h/2".parse_as()?),
				MappingToken::VarRegister("X4".parse_as()?),
				MappingToken::VarRegister("X6".parse_as()?),
				MappingToken::Functor("X6".parse_as()?, "f/1".parse_as()?),
				MappingToken::VarRegister("X7".parse_as()?),
				MappingToken::Functor("X7".parse_as()?, "a/0".parse_as()?),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(Z, h(Z,W), f(W))".parse_as()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::TopDown),
			vec![
				MappingToken::Functor("X1".parse_as()?, "p/3".parse_as()?),
				MappingToken::VarRegister("X2".parse_as()?),
				MappingToken::VarRegister("X3".parse_as()?),
				MappingToken::VarRegister("X4".parse_as()?),
				MappingToken::Functor("X3".parse_as()?, "h/2".parse_as()?),
				MappingToken::VarRegister("X2".parse_as()?),
				MappingToken::VarRegister("X5".parse_as()?),
				MappingToken::Functor("X4".parse_as()?, "f/1".parse_as()?),
				MappingToken::VarRegister("X5".parse_as()?),
			]
		);

		Ok(())
	}
}
