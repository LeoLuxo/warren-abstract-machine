use std::collections::{hash_map::Entry, HashMap};

use anyhow::Result;
use derive_more::derive::{Display, From};

use crate::{
	ast::{Constant, Structure, Variable},
	display_map,
	util::Successor,
	RegisterMapping, VarRegister,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, Default, PartialEq, Eq, From, Display)]
#[display("{{ {} }}", display_map!(_0))]
pub struct Substitution(HashMap<Variable, SubstTerm>);

// #[derive(Clone, Debug, Default, PartialEq, Eq, From, Display)]
// #[display("{}", _0.as_ref().map_or("(unbound)".to_string(), |t| format!("{}", t)))]
// pub struct SubstitutionEntry(Option<Term>);

#[derive(Clone, Debug, PartialEq, Eq, Display, From)]
pub enum SubstTerm {
	Constant(Constant),
	Unbound(UnboundIdentifier),
	Variable(Variable),
	Structure(Structure<SubstTerm>),
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, From, Display)]
#[display("?{}", format_unbound_identifier(*_0))]
pub struct UnboundIdentifier(u32);

fn format_unbound_identifier(mut x: u32) -> String {
	let mut result = Vec::new();
	loop {
		let d = x % 26;
		x /= 26;

		result.push(std::char::from_u32(d + 0x61).unwrap());
		if x <= 0 {
			break;
		}
	}
	result.into_iter().rev().collect()
}

impl Successor for UnboundIdentifier {
	fn next(&self) -> Self {
		Self(self.0 + 1)
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
			Entry::Vacant(vacant_entry) => *vacant_entry.insert(self.next_id.incr()),
		}
	}
}

pub trait ExtractSubstitution {
	fn extract_reg(&self, reg: VarRegister, unbound_map: &mut UnboundMapping) -> Result<SubstTerm>;

	fn extract_mapping(&self, mapping: RegisterMapping) -> Result<Substitution> {
		let mut substitution = HashMap::new();
		let mut unbound_map = UnboundMapping::default();

		for (var, register) in mapping.into_iter() {
			let entry = self.extract_reg(register, &mut unbound_map)?;
			substitution.insert(var, entry);
		}

		Ok(substitution.into())
	}
}
