use std::{
	collections::{HashMap},
	fmt::Display,
	ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign},
};

use derive_more::derive::{Add, AddAssign, Deref, DerefMut, Display, From, Into, IntoIterator, Sub, SubAssign};

use crate::{display_iter, display_map, Successor};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Display, From, Deref, DerefMut, Add, Sub)]
#[from(forward)]
#[display("X{_0}")]
pub struct VarRegister(usize);

impl Default for VarRegister {
	fn default() -> Self {
		Self(1)
	}
}

impl Successor for VarRegister {
	fn next(&self) -> Self {
		Self(self.0 + 1)
	}
}

#[rustfmt::skip] impl Add<usize> for VarRegister { type Output = Self; fn add(self, rhs: usize) -> Self::Output { Self(self.0 + rhs) } }
#[rustfmt::skip] impl Sub<usize> for VarRegister { type Output = Self; fn sub(self, rhs: usize) -> Self::Output { Self(self.0 - rhs) } }
#[rustfmt::skip] impl AddAssign<usize> for VarRegister { fn add_assign(&mut self, rhs: usize) { self.0 += rhs } }
#[rustfmt::skip] impl SubAssign<usize> for VarRegister { fn sub_assign(&mut self, rhs: usize) { self.0 -= rhs } }

#[derive(Clone, Debug, PartialEq, Eq, Display)]
#[display("{}", display_map!(_0, "\n", "{} = {}"))]
#[display(bounds(C: Display + Ord))]
pub struct VarRegisters<C>(HashMap<VarRegister, C>);

impl<C> VarRegisters<C> {
	pub fn set(&mut self, index: VarRegister, value: C) {
		self.0.insert(index, value);
	}
}

impl<C> Index<VarRegister> for VarRegisters<C> {
	type Output = C;

	fn index(&self, index: VarRegister) -> &Self::Output {
		self.0.get(&index).expect("Attempted reading an uninitialized register")
	}
}

impl<C> Default for VarRegisters<C> {
	fn default() -> Self {
		Self(Default::default())
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(
	Copy,
	Clone,
	Debug,
	Default,
	PartialEq,
	Eq,
	PartialOrd,
	Ord,
	Hash,
	Display,
	Deref,
	DerefMut,
	From,
	Add,
	Sub,
	AddAssign,
	SubAssign,
)]
pub struct HeapAddress(usize);

#[rustfmt::skip] impl Add<usize> for HeapAddress { type Output = Self; fn add(self, rhs: usize) -> Self::Output { Self(self.0 + rhs) } }
#[rustfmt::skip] impl Sub<usize> for HeapAddress { type Output = Self; fn sub(self, rhs: usize) -> Self::Output { Self(self.0 - rhs) } }
#[rustfmt::skip] impl AddAssign<usize> for HeapAddress { fn add_assign(&mut self, rhs: usize) { self.0 += rhs } }
#[rustfmt::skip] impl SubAssign<usize> for HeapAddress { fn sub_assign(&mut self, rhs: usize) { self.0 -= rhs } }

#[derive(Clone, Debug, PartialEq, Eq, Display, From, Into, IntoIterator, Deref, DerefMut)]
#[display("{}", display_iter!(_0, "\n"))]
#[display(bounds(C: Display))]
pub struct Heap<C>(Vec<C>);

impl<C> Heap<C> {
	pub fn push(&mut self, value: C) {
		self.0.push(value);
	}

	pub fn top(&self) -> HeapAddress {
		HeapAddress(self.0.len())
	}
}

impl<C> Index<HeapAddress> for Heap<C> {
	type Output = C;

	fn index(&self, index: HeapAddress) -> &Self::Output {
		self.0.get(*index).expect("Attempted reading an invalid heap address")
	}
}

impl<C> IndexMut<HeapAddress> for Heap<C> {
	fn index_mut(&mut self, index: HeapAddress) -> &mut Self::Output {
		self.0
			.get_mut(*index)
			.expect("Attempted accessing an invalid heap address")
	}
}

impl<C> Default for Heap<C> {
	fn default() -> Self {
		Self(Default::default())
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(
	Copy, Clone, Debug, Default, PartialEq, Eq, Display, Deref, DerefMut, From, Add, Sub, AddAssign, SubAssign,
)]
pub struct CodeAddress(usize);
