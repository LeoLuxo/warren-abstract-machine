use crate::{
	ast::{Constant, Identifier, Structure, Variable},
	display_map,
	machine_types::{HeapAddress, VarRegister},
	subst::SubstTerm,
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

pub trait AnonymousEq {
	fn anonymous_eq(
		&self,
		other: &Self,
		mapping_self: &mut AnonymousIdGenerator<AnonymousIdentifier>,
		mapping_other: &mut AnonymousIdGenerator<AnonymousIdentifier>,
	) -> bool;
}

impl AnonymousEq for SubstTerm {
	fn anonymous_eq(
		&self,
		other: &Self,
		mapping_self: &mut AnonymousIdGenerator<AnonymousIdentifier>,
		mapping_other: &mut AnonymousIdGenerator<AnonymousIdentifier>,
	) -> bool {
		match (self, other) {
			(Self::Constant(s), Self::Constant(o)) => s == o,
			(Self::Variable(s), Self::Variable(o)) => s == o,
			(Self::Unbound(s), Self::Unbound(o)) => s.anonymous_eq(o, mapping_self, mapping_other),
			(Self::Structure(s), Self::Structure(o)) => s.anonymous_eq(o, mapping_self, mapping_other),
			_ => false,
		}
	}
}

impl AnonymousEq for AnonymousIdentifier {
	fn anonymous_eq(
		&self,
		other: &Self,
		mapping_self: &mut AnonymousIdGenerator<AnonymousIdentifier>,
		mapping_other: &mut AnonymousIdGenerator<AnonymousIdentifier>,
	) -> bool {
		let s = mapping_self.get_identifier(*self);
		let o = mapping_other.get_identifier(*other);

		s == o
	}
}

impl AnonymousEq for Structure<SubstTerm> {
	fn anonymous_eq(
		&self,
		other: &Self,
		mapping_self: &mut AnonymousIdGenerator<AnonymousIdentifier>,
		mapping_other: &mut AnonymousIdGenerator<AnonymousIdentifier>,
	) -> bool {
		if self.name != other.name {
			return false;
		}

		if self.arguments.len() != other.arguments.len() {
			return false;
		}

		for (s, o) in self.arguments.iter().zip(other.arguments.iter()) {
			if !s.anonymous_eq(o, mapping_self, mapping_other) {
				return false;
			}
		}

		true
	}
}
