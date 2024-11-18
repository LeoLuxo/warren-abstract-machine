use std::collections::{hash_map::Entry, BTreeMap, HashMap};

use anyhow::Result;
use derive_more::derive::{Deref, DerefMut, Display, From};

use crate::{
	ast::{Constant, Structure, Variable},
	display_map,
	machine_types::{HeapAddress, VarRegister, VarToHeapMapping},
	util::Successor,
	Compiled, Language,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait StaticMapping {
	fn static_heap_size(&self) -> Option<HeapAddress>;
	fn static_variable_entry_point(&self, register: VarRegister) -> bool;
}

#[derive(Clone, Debug, Default, PartialEq, Eq, From, Display, Deref, DerefMut)]
#[display("{{ {} }}", display_map!(_0))]
pub struct Substitution(BTreeMap<Variable, SubstTerm>);

// #[derive(Clone, Debug, Default, PartialEq, Eq, From, Display)]
// #[display("{}", _0.as_ref().map_or("(unbound)".to_string(), |t| format!("{}", t)))]
// pub struct SubstitutionEntry(Option<Term>);

#[derive(Clone, Debug, PartialEq, Eq, Display, From)]
#[from(forward)]
pub enum SubstTerm {
	Constant(Constant),
	Unbound(UnboundIdentifier),
	Variable(Variable),
	Structure(Structure<SubstTerm>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, From, Display)]
#[display("?{}", _0)]
#[from(forward)]
pub struct UnboundIdentifier(u32);

impl Successor for UnboundIdentifier {
	fn next(&self) -> Self {
		Self(self.0 + 1)
	}
}

impl Default for UnboundIdentifier {
	fn default() -> Self {
		Self(1)
	}
}

#[derive(Clone, Debug, Default, PartialEq, Eq, From)]
pub struct UnboundMapping {
	map: HashMap<usize, UnboundIdentifier>,
	next_id: UnboundIdentifier,
}

impl UnboundMapping {
	pub fn get(&mut self, id: usize) -> UnboundIdentifier {
		match self.map.entry(id) {
			Entry::Occupied(occupied_entry) => *occupied_entry.get(),
			Entry::Vacant(vacant_entry) => *vacant_entry.insert(self.next_id.post_incr()),
		}
	}
}

// pub type SubstTargetMapping<Target> = BTreeMap<Variable, Target>;

impl<L: Language> Compiled<L> {
	fn compute_var_heap_address(&self, register: VarRegister) -> Option<HeapAddress>
	where
		L::InstructionSet: StaticMapping,
	{
		let mut heap_top = HeapAddress::default();

		for instruction in &self.instructions {
			if let Some(ep) = instruction.static_heap_size() {
				heap_top += ep;
			} else {
				break;
			}

			if instruction.static_variable_entry_point(register) {
				if *heap_top > 0 {
					return Some(heap_top - 1);
				} else {
					break;
				}
			}
		}

		None
	}

	pub fn compute_var_heap_mapping(&self) -> VarToHeapMapping
	where
		L::InstructionSet: StaticMapping,
	{
		self.var_reg_mapping
			.iter()
			.filter_map(|(var, reg)| self.compute_var_heap_address(*reg).map(|addr| (var.clone(), addr)))
			.collect()
	}
}

pub trait ExtractSubstitution<L: Language>
where
	L::InstructionSet: StaticMapping,
{
	// fn find_target(&self, reg: VarRegister) -> Result<Self::Target>;
	fn extract_heap(&self, address: HeapAddress, unbound_map: &mut UnboundMapping) -> Result<SubstTerm>;

	// fn pre_extract_targets(&self, mapping: VarToRegMapping) -> Result<SubstTargetMapping<Self::Target>> {
	// 	let mut target_mapping = BTreeMap::new();

	// 	for (var, register) in mapping.into_iter() {
	// 		let target = self.find_target(register)?;
	// 		target_mapping.insert(var, target);
	// 	}

	// 	Ok(target_mapping)
	// }

	fn extract_substitution(&self, compiled: &Compiled<L>) -> Result<Substitution> {
		let var_heap_mapping = compiled.compute_var_heap_mapping();
		let mut substitution = Substitution::default();
		let mut unbound_map = UnboundMapping::default();

		for (var, address) in var_heap_mapping.into_iter() {
			let entry = self.extract_heap(address, &mut unbound_map)?;
			substitution.insert(var, entry);
		}

		Ok(substitution)
	}
}
