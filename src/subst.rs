use crate::{
	ast::{Constant, Identifier, Structure, Variable},
	display_map,
	machine_types::{HeapAddress, VarRegister},
	util::Successor,
	Language,
};
use anyhow::Result;
use derive_more::derive::{Deref, DerefMut, Display, From, Index, IndexMut, IntoIterator};
use std::{
	cmp::Ordering,
	collections::{hash_map::Entry, BTreeMap, HashMap},
	mem,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, PartialEq, Eq, Ord, Hash, Default)]
pub enum VariableContext {
	#[default]
	Global,
	Local(Identifier),
}

impl PartialOrd for VariableContext {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		match (self, other) {
			(Self::Global, Self::Local(_)) => Some(Ordering::Less),
			(Self::Local(_), Self::Global) => Some(Ordering::Greater),
			(Self::Local(a), Self::Local(b)) => a.partial_cmp(b),
			(a, b) if a == b => Some(Ordering::Equal),
			_ => None,
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, Hash, Display)]
#[display("{}", match context { 
	VariableContext::Global =>            format!("{}", variable),
	VariableContext::Local(identifier) => format!("{}<{}>", variable, identifier)
})]
pub struct ScopedVariable {
	variable: Variable,
	context: VariableContext,
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

impl PartialOrd for ScopedVariable {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(
			self.context
				.partial_cmp(&other.context)?
				.then(self.variable.partial_cmp(&other.variable)?),
		)
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
pub struct VarToRegMapping(BTreeMap<ScopedVariable, VarRegister>);

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
		BTreeMap::from_iter(iter).into()
	}
}

impl FromIterator<(Variable, VarRegister)> for VarToRegMapping {
	fn from_iter<T: IntoIterator<Item = (Variable, VarRegister)>>(iter: T) -> Self {
		BTreeMap::from_iter(iter.into_iter().map(|(var, reg)| (var.into(), reg))).into()
	}
}

#[derive(Clone, Debug, Default, PartialEq, Eq, From, IntoIterator, Deref, DerefMut, Index, IndexMut, Display)]
#[display("{}", display_map!(_0))]
pub struct VarToHeapMapping(BTreeMap<ScopedVariable, HeapAddress>);

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
		BTreeMap::from_iter(iter).into()
	}
}

impl FromIterator<(Variable, HeapAddress)> for VarToHeapMapping {
	fn from_iter<T: IntoIterator<Item = (Variable, HeapAddress)>>(iter: T) -> Self {
		BTreeMap::from_iter(iter.into_iter().map(|(var, addr)| (var.into(), addr))).into()
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait StaticMapping {
	fn static_heap_size(&self) -> Option<HeapAddress>;
	fn static_variable_entry_point(&self, register: &VarRegister) -> bool;
}

#[derive(Clone, Debug, Default, PartialEq, Eq, From, Display, Deref, DerefMut)]
#[display("{{ {} }}", display_map!(_0))]
pub struct Substitution(BTreeMap<ScopedVariable, SubstTerm>);

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

fn compute_var_heap_address<V, I>(register: &VarRegister, instructions: &V) -> Option<HeapAddress>
where
	for<'a> &'a V: IntoIterator<Item = &'a I>,
	I: StaticMapping,
{
	let mut heap_top = HeapAddress::default();

	for instruction in instructions {
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

pub fn compute_var_heap_mapping<V, I>(var_reg_mapping: &VarToRegMapping, instructions: &V) -> Result<VarToHeapMapping>
where
	for<'a> &'a V: IntoIterator<Item = &'a I>,
	I: StaticMapping,
{
	// let asd = instructions.into_iter();
	Ok(var_reg_mapping
		.iter()
		.filter_map(|(var, reg)| compute_var_heap_address(reg, instructions).map(|addr| (var.clone(), addr)))
		.collect())
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

	fn extract_substitution(&self, mut var_heap_mapping: VarToHeapMapping) -> Result<Substitution> {
		var_heap_mapping.filter_by_context(VariableContext::Global);

		let mut substitution = Substitution::default();
		let mut unbound_map = UnboundMapping::default();

		for (var, address) in var_heap_mapping.into_iter() {
			let entry = self.extract_heap(address, &mut unbound_map)?;
			substitution.insert(var, entry);
		}

		Ok(substitution)
	}
}
