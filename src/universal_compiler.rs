use std::{
	collections::{hash_map, HashMap, VecDeque},
	fmt::Display,
	mem,
	ops::{Add, AddAssign},
};

use anyhow::{Context, Result};
use derive_more::derive::{Display, From};
use velcro::vec;

use crate::{
	ast::{Functor, GetFunctor, Identifier, Term, Variable},
	display_iter,
	machine_types::{CodeAddress, VarRegister},
	subst::{self, StaticMapping, VarToHeapMapping, VarToRegMapping},
	util::Successor,
	Language,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait CompilableProgram<L: Language> {
	fn compile_as_program(self) -> Compiled<L>;
}

pub trait CompilableQuery<L: Language> {
	fn compile_as_query(self) -> Compiled<L>;
}

#[derive(Clone, Debug, PartialEq, Eq, From, Display)]
#[display("{}\n{}", display_iter!(instructions, "\n"), var_reg_mapping.as_ref().map_or("(without mapping)".to_string(), |m| format!("(where {})", m)))]
#[display(bounds(L::InstructionSet: Display))]
pub struct Compiled<L: Language> {
	pub instructions: Vec<L::InstructionSet>,
	pub var_reg_mapping: Option<VarToRegMapping>,
	pub labels: HashMap<Identifier, CodeAddress>,
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

impl<L: Language> Compiled<L> {
	pub fn combined(self, other: Self) -> Self {
		let label_offset = self.instructions.len().into();

		// self's instructions then other's instructions, in order
		let instructions = vec![..self.instructions, ..other.instructions];

		// self's mapping takes priority over other's mapping (overwritten where needed)
		let var_reg_mapping = match (self.var_reg_mapping, other.var_reg_mapping) {
			(Some(m1), Some(m2)) => Some(m2.into_iter().chain(m1).collect()),
			(_, Some(m)) | (Some(m), _) => Some(m),
			_ => None,
		};

		let labels = self
			.labels
			.into_iter()
			.chain(
				other
					.labels
					.into_iter()
					.map(|(ident, addr)| (ident, addr + label_offset)),
			)
			.collect();

		Self {
			instructions,
			var_reg_mapping,
			labels,
		}
	}

	pub fn combine(&mut self, other: Self) {
		*self = mem::take(self).combined(other)
	}

	pub fn compute_var_heap_mapping(&self) -> Result<VarToHeapMapping>
	where
		<L as Language>::InstructionSet: StaticMapping,
	{
		subst::compute_var_heap_mapping(
			self.var_reg_mapping
				.as_ref()
				.context("Cannot compute var-heap mapping of compiled without a valid mapping")?,
			&self.instructions,
		)
	}
}

impl<L: Language> Add for Compiled<L> {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		self.combined(rhs)
	}
}

impl<L: Language> AddAssign for Compiled<L> {
	fn add_assign(&mut self, rhs: Self) {
		self.combine(rhs);
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
}

fn allocate_register_id(
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
			flatten_term(VarRegister::default(), "c".parse()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::BottomUp),
			vec![
				MappingToken::Functor(1_usize.into(), Functor { name: "c".into(), arity: 0 })
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(X,Y,Z,Y,X)".parse()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::BottomUp),
			vec![
				MappingToken::Functor(1_usize.into(), Functor { name: "p".into(), arity: 5 }),
				MappingToken::VarRegister(2_usize.into()),
				MappingToken::VarRegister(3_usize.into()),
				MappingToken::VarRegister(4_usize.into()),
				MappingToken::VarRegister(3_usize.into()),
				MappingToken::VarRegister(2_usize.into()),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(f(X), h(Y, f(a)), Y)".parse()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::BottomUp),
			vec![
				MappingToken::Functor(2_usize.into(), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(7_usize.into()),
				MappingToken::Functor(6_usize.into(), Functor { name: "a".into(), arity: 0 }),
				MappingToken::Functor(5_usize.into(), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(6_usize.into()),
				MappingToken::Functor(3_usize.into(), Functor { name: "h".into(), arity: 2 }),
				MappingToken::VarRegister(4_usize.into()),
				MappingToken::VarRegister(5_usize.into()),
				MappingToken::Functor(1_usize.into(), Functor { name: "p".into(), arity: 3 }),
				MappingToken::VarRegister(2_usize.into()),
				MappingToken::VarRegister(3_usize.into()),
				MappingToken::VarRegister(4_usize.into()),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(Z, h(Z,W), f(W))".parse()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::BottomUp),
			vec![
				MappingToken::Functor(3_usize.into(), Functor { name: "h".into(), arity: 2 }),
				MappingToken::VarRegister(2_usize.into()),
				MappingToken::VarRegister(5_usize.into()),
				MappingToken::Functor(4_usize.into(), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(5_usize.into()),
				MappingToken::Functor(1_usize.into(), Functor { name: "p".into(), arity: 3 }),
				MappingToken::VarRegister(2_usize.into()),
				MappingToken::VarRegister(3_usize.into()),
				MappingToken::VarRegister(4_usize.into()),
			]
		);

		Ok(())
	}

	#[test]
	fn test_flatten_term_topdown() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "c".parse()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::TopDown),
			vec![
				MappingToken::Functor(1_usize.into(), Functor { name: "c".into(), arity: 0 })
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(X,Y,Z,Y,X)".parse()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::TopDown),
			vec![
				MappingToken::Functor(1_usize.into(), Functor { name: "p".into(), arity: 5 }),
				MappingToken::VarRegister(2_usize.into()),
				MappingToken::VarRegister(3_usize.into()),
				MappingToken::VarRegister(4_usize.into()),
				MappingToken::VarRegister(3_usize.into()),
				MappingToken::VarRegister(2_usize.into()),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(f(X), h(Y, f(a)), Y)".parse()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::TopDown),
			vec![
				MappingToken::Functor(1_usize.into(), Functor { name: "p".into(), arity: 3 }),
				MappingToken::VarRegister(2_usize.into()),
				MappingToken::VarRegister(3_usize.into()),
				MappingToken::VarRegister(4_usize.into()),
				MappingToken::Functor(2_usize.into(), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(5_usize.into()),
				MappingToken::Functor(3_usize.into(), Functor { name: "h".into(), arity: 2 }),
				MappingToken::VarRegister(4_usize.into()),
				MappingToken::VarRegister(6_usize.into()),
				MappingToken::Functor(6_usize.into(), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(7_usize.into()),
				MappingToken::Functor(7_usize.into(), Functor { name: "a".into(), arity: 0 }),
			]
		);

		#[rustfmt::skip]
		assert_eq!(
			flatten_term(VarRegister::default(), "p(Z, h(Z,W), f(W))".parse()?, &mut HashMap::default(), &mut VarRegister::default(), FlatteningOrder::TopDown),
			vec![
				MappingToken::Functor(1_usize.into(), Functor { name: "p".into(), arity: 3 }),
				MappingToken::VarRegister(2_usize.into()),
				MappingToken::VarRegister(3_usize.into()),
				MappingToken::VarRegister(4_usize.into()),
				MappingToken::Functor(3_usize.into(), Functor { name: "h".into(), arity: 2 }),
				MappingToken::VarRegister(2_usize.into()),
				MappingToken::VarRegister(5_usize.into()),
				MappingToken::Functor(4_usize.into(), Functor { name: "f".into(), arity: 1 }),
				MappingToken::VarRegister(5_usize.into()),
			]
		);

		Ok(())
	}
}
