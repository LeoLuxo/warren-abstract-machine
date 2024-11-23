use crate::{
	ast::{Constant, Identifier, Structure, Variable},
	display_map,
	machine_types::{HeapAddress, VarRegister},
	util::Successor,
	Language,
};
use anyhow::Result;
use derive_more::derive::{Deref, DerefMut, Display, From, Index, IndexMut, IntoIterator};
use itertools::Itertools;
use std::{
	collections::{hash_map::Entry, BTreeMap, HashMap},
	hash::Hash,
	mem,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, From, Display)]
#[display("?{}", _0)]
#[from(forward)]
pub struct AnonymousIdentifier(u32);

impl Successor for AnonymousIdentifier {
	fn next(&self) -> Self {
		Self(self.0 + 1)
	}
}

impl Default for AnonymousIdentifier {
	fn default() -> Self {
		Self(1)
	}
}

#[derive(Clone, Debug, Default, PartialEq, Eq, From)]
pub struct AnonymousIdGenerator<T: Hash + Eq> {
	map: HashMap<T, AnonymousIdentifier>,
	next_identifier: AnonymousIdentifier,
}

impl<T: Hash + Eq> AnonymousIdGenerator<T> {
	pub fn get_identifier(&mut self, unique_value: T) -> AnonymousIdentifier {
		match self.map.entry(unique_value) {
			Entry::Occupied(occupied_entry) => *occupied_entry.get(),
			Entry::Vacant(vacant_entry) => *vacant_entry.insert(self.next_identifier.post_incr()),
		}
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum VariableContext {
	#[default]
	Query,
	Local(Identifier),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[display("{}", match context { 
	VariableContext::Query             => format!("{}", variable),
	VariableContext::Local(identifier) => format!("{}<{}>", variable, identifier)
})]
pub struct ScopedVariable {
	context: VariableContext,
	variable: Variable,
}

impl ScopedVariable {
	pub fn set_context(&mut self, context: VariableContext) {
		self.context = context;
	}

	pub fn drop_context(self) -> Variable {
		self.variable
	}

	pub fn matches_context(&self, context: &VariableContext) -> bool {
		self.context == *context
	}
}

impl From<Variable> for ScopedVariable {
	fn from(value: Variable) -> Self {
		Self {
			variable: value,
			context: Default::default(),
		}
	}
}

impl From<ScopedVariable> for Variable {
	fn from(value: ScopedVariable) -> Self {
		value.drop_context()
	}
}

#[derive(Clone, Debug, Default, PartialEq, Eq, From, IntoIterator, Deref, DerefMut, Index, IndexMut, Display)]
#[display("{}", display_map!(_0))]
pub struct VarToRegMapping(HashMap<ScopedVariable, VarRegister>);

impl VarToRegMapping {
	pub fn filter_by_context(&mut self, context: VariableContext) {
		self.0 = mem::take(&mut self.0)
			.into_iter()
			.filter(|(var, _)| var.matches_context(&context))
			.collect()
	}

	pub fn from_hashmap_with_context(hashmap: HashMap<Variable, VarRegister>, context: VariableContext) -> Self {
		hashmap
			.into_iter()
			.map(|(variable, reg)| {
				(
					ScopedVariable {
						variable,
						context: context.clone(),
					},
					reg,
				)
			})
			.collect()
	}
}

impl FromIterator<(ScopedVariable, VarRegister)> for VarToRegMapping {
	fn from_iter<T: IntoIterator<Item = (ScopedVariable, VarRegister)>>(iter: T) -> Self {
		HashMap::from_iter(iter).into()
	}
}

impl FromIterator<(Variable, VarRegister)> for VarToRegMapping {
	fn from_iter<T: IntoIterator<Item = (Variable, VarRegister)>>(iter: T) -> Self {
		HashMap::from_iter(iter.into_iter().map(|(var, reg)| (var.into(), reg))).into()
	}
}

#[derive(Clone, Debug, Default, PartialEq, Eq, From, IntoIterator, Deref, DerefMut, Index, IndexMut, Display)]
#[display("{}", display_map!(_0))]
pub struct VarToHeapMapping(HashMap<ScopedVariable, HeapAddress>);

impl VarToHeapMapping {
	pub fn filter_by_context(&mut self, context: VariableContext) {
		self.0 = mem::take(&mut self.0)
			.into_iter()
			.filter(|(var, _)| var.matches_context(&context))
			.collect()
	}
}

impl FromIterator<(ScopedVariable, HeapAddress)> for VarToHeapMapping {
	fn from_iter<T: IntoIterator<Item = (ScopedVariable, HeapAddress)>>(iter: T) -> Self {
		HashMap::from_iter(iter).into()
	}
}

impl FromIterator<(Variable, HeapAddress)> for VarToHeapMapping {
	fn from_iter<T: IntoIterator<Item = (Variable, HeapAddress)>>(iter: T) -> Self {
		HashMap::from_iter(iter.into_iter().map(|(var, addr)| (var.into(), addr))).into()
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait StaticMapping {
	fn static_heap_size(&self) -> Option<HeapAddress>;
	fn static_variable_entry_point(&self, register: &VarRegister, pre_heap_top: HeapAddress) -> Option<HeapAddress>;
}

#[derive(Clone, Debug, Default, PartialEq, Eq, From, Display, Deref, DerefMut)]
#[display("{{ {} }}", display_map!(_0))]
pub struct Substitution(HashMap<ScopedVariable, SubstTerm>);

impl FromIterator<(ScopedVariable, SubstTerm)> for Substitution {
	fn from_iter<T: IntoIterator<Item = (ScopedVariable, SubstTerm)>>(iter: T) -> Self {
		Self(HashMap::from_iter(iter))
	}
}

#[derive(Clone, Debug, Eq, Display, From)]
#[from(forward)]
pub enum SubstTerm {
	Constant(Constant),
	Unbound(AnonymousIdentifier),
	Variable(Variable),
	Structure(Structure<SubstTerm>),
}

impl PartialEq for SubstTerm {
	fn eq(&self, other: &Self) -> bool {
		let mut term_stack = vec![self, other];
		let mut anon_mapping_left = AnonymousIdGenerator::default();
		let mut anon_mapping_right = AnonymousIdGenerator::default();

		while !term_stack.is_empty() {
			let result = match (term_stack.pop().unwrap(), term_stack.pop().unwrap()) {
				(Self::Constant(l), Self::Constant(r)) => l == r,
				(Self::Variable(l), Self::Variable(r)) => l == r,

				(Self::Structure(l), Self::Structure(r)) => {
					term_stack.extend(l.arguments.iter().interleave(r.arguments.iter()));

					l.name == r.name && l.arguments.len() == r.arguments.len()
				}

				(Self::Unbound(l), Self::Unbound(r)) => {
					let id_l = anon_mapping_left.get_identifier(*l);
					let id_r = anon_mapping_right.get_identifier(*r);

					id_l == id_r
				}

				_ => false,
			};

			if !result {
				return false;
			}
		}

		true
	}
}

fn compute_var_heap_address<V, I>(register: &VarRegister, instructions: &V) -> Option<HeapAddress>
where
	for<'a> &'a V: IntoIterator<Item = &'a I>,
	I: StaticMapping,
{
	let mut heap_top = HeapAddress::default();

	for instruction in instructions {
		if let Some(address) = instruction.static_variable_entry_point(register, heap_top) {
			return Some(address);
		}

		if let Some(ep) = instruction.static_heap_size() {
			heap_top += ep;
		} else {
			break;
		}
	}

	None
}

pub fn compute_var_heap_mapping<V, I>(var_reg_mapping: &VarToRegMapping, instructions: &V) -> Result<VarToHeapMapping>
where
	for<'a> &'a V: IntoIterator<Item = &'a I>,
	I: StaticMapping,
{
	Ok(var_reg_mapping
		.iter()
		.filter_map(|(var, reg)| compute_var_heap_address(reg, instructions).map(|addr| (var.clone(), addr)))
		.collect())
}

pub trait ExtractSubstitution<L: Language>
where
	L::InstructionSet: StaticMapping,
{
	fn extract_heap(&self, address: HeapAddress, anon_gen: &mut AnonymousIdGenerator<HeapAddress>)
		-> Result<SubstTerm>;

	fn extract_substitution(&self, mut var_heap_mapping: VarToHeapMapping) -> Result<Substitution> {
		var_heap_mapping.filter_by_context(VariableContext::Query);

		let mut substitution = Substitution::default();
		let mut anon_map = AnonymousIdGenerator::default();

		for (var, address) in var_heap_mapping.into_iter().sorted() {
			let entry = self.extract_heap(address, &mut anon_map)?;
			substitution.insert(var, entry);
		}

		Ok(substitution)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_subst_term_eq() {
		assert_eq!(SubstTerm::Constant("a".into()), SubstTerm::Constant("a".into()));
		assert_eq!(SubstTerm::Variable("X".into()), SubstTerm::Variable("X".into()));

		assert_eq!(
			SubstTerm::Unbound(AnonymousIdentifier(1)),
			SubstTerm::Unbound(AnonymousIdentifier(1))
		);

		assert_ne!(SubstTerm::Constant("a".into()), SubstTerm::Constant("b".into()));
		assert_ne!(SubstTerm::Variable("X".into()), SubstTerm::Variable("Y".into()));

		assert_eq!(
			SubstTerm::Unbound(AnonymousIdentifier(1)),
			SubstTerm::Unbound(AnonymousIdentifier(2))
		);

		assert_eq!(
			SubstTerm::Structure(Structure {
				name: "f".into(),
				arguments: vec![
					SubstTerm::Unbound(AnonymousIdentifier(1)),
					SubstTerm::Unbound(AnonymousIdentifier(1))
				]
				.into()
			}),
			SubstTerm::Structure(Structure {
				name: "f".into(),
				arguments: vec![
					SubstTerm::Unbound(AnonymousIdentifier(2)),
					SubstTerm::Unbound(AnonymousIdentifier(2))
				]
				.into()
			})
		);

		assert_ne!(
			SubstTerm::Structure(Structure {
				name: "f".into(),
				arguments: vec![
					SubstTerm::Unbound(AnonymousIdentifier(1)),
					SubstTerm::Unbound(AnonymousIdentifier(2))
				]
				.into()
			}),
			SubstTerm::Structure(Structure {
				name: "f".into(),
				arguments: vec![
					SubstTerm::Unbound(AnonymousIdentifier(2)),
					SubstTerm::Unbound(AnonymousIdentifier(2))
				]
				.into()
			})
		);
	}
}
