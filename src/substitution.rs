use crate::{
	anonymous::{AnonymousEq, AnonymousIdGenerator, AnonymousIdentifier},
	ast::{Constant, Functor, Identifier, Structure, Variable},
	display_map,
	machine_types::{Cell, Heap, HeapAddress, VarRegister},
	universal_compiler::Compiled,
	Language,
};
use anyhow::{bail, Result};
use bimap::BiHashMap;
use derive_more::derive::{Deref, DerefMut, Display, From, Index, IndexMut, IntoIterator};
use itertools::Itertools;
use std::{collections::HashMap, fmt::Display, hash::Hash, mem};
use velcro::vec;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum VariableContext {
	#[default]
	Query,
	Program(Identifier),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[display("{}", match context { 
	VariableContext::Query               => format!("{}", variable),
	VariableContext::Program(identifier) => format!("{}<{}>", variable, identifier)
})]
pub struct ScopedVariable {
	context: VariableContext,
	variable: Variable,
}

impl ScopedVariable {
	pub fn new(variable: Variable, context: VariableContext) -> Self {
		Self { context, variable }
	}

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
pub struct VarToRegMapping(BiHashMap<ScopedVariable, VarRegister>);

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
		BiHashMap::from_iter(iter).into()
	}
}

impl FromIterator<(Variable, VarRegister)> for VarToRegMapping {
	fn from_iter<T: IntoIterator<Item = (Variable, VarRegister)>>(iter: T) -> Self {
		BiHashMap::from_iter(iter.into_iter().map(|(var, reg)| (var.into(), reg))).into()
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

pub trait VarEntryPoint
where
	Self: Sized,
{
	fn is_variable_entry_point(&self) -> Option<(VarRegister, Self)>;
}

pub trait StaticallyAnalysable
where
	Self: Sized,
{
	fn to_statically_analysable(self) -> (Self, VarToHeapMapping);
}

impl<L> StaticallyAnalysable for Compiled<L>
where
	L: Language,
	L::InstructionSet: VarEntryPoint + Clone,
	Self: Clone,
{
	fn to_statically_analysable(self) -> (Compiled<L>, VarToHeapMapping) {
		let mut var_heap_mapping = VarToHeapMapping::default();
		let mut new_instructions_front = Vec::new();
		let mut new_instructions_back = Vec::new();

		let Some(var_reg_mapping) = self.var_reg_mapping else {
			return (self, var_heap_mapping);
		};

		let Compiled {
			instructions,
			var_reg_mapping: _,
			labels,
		} = self;

		for instruction in instructions {
			if let Some((reg, non_entry_point_inst)) = instruction.is_variable_entry_point() {
				if let Some(ScopedVariable {
					context: VariableContext::Query,
					variable,
				}) = var_reg_mapping.get_by_right(&reg)
				{
					// If the current instruction is an entry point for a variable and is a QUERY variable present in the variable-register-mapping,
					// Then remember it's current spot in the instruction sequence as its heap address
					let var_heap_address = new_instructions_front.len().into();
					var_heap_mapping.insert(
						ScopedVariable::new(variable.clone(), VariableContext::Query),
						var_heap_address,
					);

					// And add it at the front of the code
					new_instructions_front.push(instruction);
					// and add its non-entry-point alternative instruction in the back (which will read the heap address of the first assignment at the front)
					new_instructions_back.push(non_entry_point_inst);

					continue;
				}
			}

			// Otherwise, just keep the instruction as-is
			new_instructions_back.push(instruction);
		}

		let label_offset = new_instructions_front.len();

		let new_instructions = vec![..new_instructions_front, ..new_instructions_back];

		let new_labels = labels
			.into_iter()
			.map(|(ident, addr)| (ident, addr + label_offset))
			.collect::<HashMap<_, _>>();

		(
			Self {
				instructions: new_instructions,
				labels: new_labels,
				var_reg_mapping: Some(var_reg_mapping),
			},
			var_heap_mapping,
		)
	}
}

#[derive(Clone, Debug, Default, Eq, From, Display, Deref, DerefMut)]
#[display("{{ {} }}", display_map!(_0))]
pub struct Substitution(HashMap<ScopedVariable, SubstTerm>);

impl PartialEq for Substitution {
	fn eq(&self, other: &Self) -> bool {
		let mut mapping_self = AnonymousIdGenerator::default();
		let mut mapping_other = AnonymousIdGenerator::default();

		for (self_var, self_term) in self.0.iter() {
			let Some(other_term) = other.0.get(self_var) else {
				return false;
			};

			if !self_term.anonymous_eq(other_term, &mut mapping_self, &mut mapping_other) {
				return false;
			}
		}

		true
	}
}

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
		let mut mapping_self = AnonymousIdGenerator::default();
		let mut mapping_other = AnonymousIdGenerator::default();

		self.anonymous_eq(other, &mut mapping_self, &mut mapping_other)
	}
}

pub trait ExtractSubstitution<L: Language> {
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

	// fn execute_and_extract_substitution(&mut self, code: Compiled<L>) -> Result<Substitution>
	// where
	// 	Compiled<L>: StaticallyAnalysable,
	// 	L::InstructionSet: Display,
	// {
	// 	let (analysable_code, var_heap_mapping) = code.to_statically_analysable();

	// 	self.execute_static_code(&analysable_code)?;
	// 	self.extract_substitution(var_heap_mapping)
	// }
}

pub fn extract_heap(
	heap: &Heap<Cell>,
	address: HeapAddress,
	anon_gen: &mut AnonymousIdGenerator<HeapAddress>,
	iterations: u32,
) -> Result<SubstTerm> {
	match &heap[address] {
		_ if iterations > heap.len() as u32 => {
			bail!("Non-terminating substitution encountered. The solution doesn't satisfy the occurs-check.")
		}

		Cell::REF(a) if address != *a => extract_heap(heap, *a, anon_gen, iterations + 1),
		Cell::STR(a) if address != *a => extract_heap(heap, *a, anon_gen, iterations + 1),

		Cell::REF(a) => Ok(SubstTerm::Unbound(anon_gen.get_identifier(*a))),

		Cell::Functor(Functor { name, arity }) if *arity == 0 => Ok(SubstTerm::Constant(Constant(name.clone()))),

		Cell::Functor(Functor { name, arity }) => Ok(SubstTerm::Structure(Structure {
			name: name.clone(),
			arguments: (1..=*arity)
				.map(|i| extract_heap(heap, address + i, anon_gen, iterations + 1))
				.collect::<Result<Vec<_>>>()?
				.into(),
		})),

		_ => bail!("Machine yielded an invalid substitution. This might be a bug."),
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[cfg(test)]
mod tests {
	use std::vec;

	use crate::parser::ParseAs;

	use super::*;

	#[test]
	fn test_substitution_eq() -> Result<()> {
		assert_eq!("{}".parse_as::<Substitution>()?, "{}".parse_as::<Substitution>()?);

		assert_eq!(
			"{ A -> c }".parse_as::<Substitution>()?,
			"{A->c}".parse_as::<Substitution>()?
		);

		assert_eq!(
			"{ A -> ?1 }".parse_as::<Substitution>()?,
			"{ A -> ?1 }".parse_as::<Substitution>()?
		);

		assert_eq!(
			"{ A -> ?1 }".parse_as::<Substitution>()?,
			"{ A -> ?2 }".parse_as::<Substitution>()?
		);

		assert_eq!(
			"{ A -> ?1 }".parse_as::<Substitution>()?,
			"{ A -> ?2 }".parse_as::<Substitution>()?
		);

		assert_eq!(
			"{ X -> ?1, Y -> ?2 }".parse_as::<Substitution>()?,
			"{ X -> ?123, Y -> ?456 }".parse_as::<Substitution>()?
		);

		assert_eq!(
			"{ X -> ?1, Y -> ?1 }".parse_as::<Substitution>()?,
			"{ X -> ?123, Y -> ?123 }".parse_as::<Substitution>()?
		);

		Ok(())
	}

	#[test]
	fn test_substitution_ne() -> Result<()> {
		assert_ne!(
			"{ A -> c }".parse_as::<Substitution>()?,
			"{ X -> c }".parse_as::<Substitution>()?
		);

		assert_ne!(
			"{ A -> c }".parse_as::<Substitution>()?,
			"{ A -> d }".parse_as::<Substitution>()?
		);

		assert_ne!(
			"{ A -> X }".parse_as::<Substitution>()?,
			"{ A -> Y }".parse_as::<Substitution>()?
		);

		assert_ne!(
			"{ X -> ?1, Y -> ?1 }".parse_as::<Substitution>()?,
			"{ X -> ?123, Y -> ?456 }".parse_as::<Substitution>()?
		);

		Ok(())
	}

	#[test]
	fn test_subst_term_eq() {
		assert_eq!(SubstTerm::Constant("a".into()), SubstTerm::Constant("a".into()));
		assert_eq!(SubstTerm::Variable("X".into()), SubstTerm::Variable("X".into()));

		assert_eq!(SubstTerm::Unbound(1_u32.into()), SubstTerm::Unbound(1_u32.into()));

		assert_ne!(SubstTerm::Constant("a".into()), SubstTerm::Constant("b".into()));
		assert_ne!(SubstTerm::Variable("X".into()), SubstTerm::Variable("Y".into()));

		assert_eq!(SubstTerm::Unbound(1_u32.into()), SubstTerm::Unbound(2_u32.into()));

		assert_eq!(
			SubstTerm::Structure(Structure {
				name: "f".into(),
				arguments: vec![SubstTerm::Unbound(1_u32.into()), SubstTerm::Unbound(1_u32.into())].into()
			}),
			SubstTerm::Structure(Structure {
				name: "f".into(),
				arguments: vec![SubstTerm::Unbound(2_u32.into()), SubstTerm::Unbound(2_u32.into())].into()
			})
		);

		assert_ne!(
			SubstTerm::Structure(Structure {
				name: "f".into(),
				arguments: vec![SubstTerm::Unbound(1_u32.into()), SubstTerm::Unbound(2_u32.into())].into()
			}),
			SubstTerm::Structure(Structure {
				name: "f".into(),
				arguments: vec![SubstTerm::Unbound(2_u32.into()), SubstTerm::Unbound(2_u32.into())].into()
			})
		);
	}
}
