//! Provides the types common between certain language-specific implementations of the WAM.

use std::{
	collections::HashMap,
	fmt::Display,
	ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign},
};

use derive_more::derive::{Add, AddAssign, Deref, DerefMut, Display, From, Into, IntoIterator, Sub, SubAssign};

use crate::{
	ast::Functor,
	display_iter, display_map,
	universal_compiler::{Combinable, Labels},
	Successor,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Display)]
pub enum Cell {
	#[display("REF {}", _0)]
	REF(HeapAddress),

	#[display("STR {}", _0)]
	STR(HeapAddress),

	Functor(Functor),
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Display)]
pub enum ReadWrite {
	#[default]
	Read,
	Write,
}

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
#[display(bounds(T: Display + Ord))]
pub struct VarRegisters<T>(HashMap<VarRegister, T>);

impl<T> VarRegisters<T> {
	pub fn set(&mut self, index: VarRegister, value: T) {
		self.0.insert(index, value);
	}
}

impl<T> Index<VarRegister> for VarRegisters<T> {
	type Output = T;

	fn index(&self, index: VarRegister) -> &Self::Output {
		self.0.get(&index).expect("Attempted reading an uninitialized register")
	}
}

impl<T> Default for VarRegisters<T> {
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
#[display(bounds(T: Display))]
pub struct Heap<T>(Vec<T>);

impl<T> Heap<T> {
	pub fn push(&mut self, value: T) {
		self.0.push(value);
	}

	pub fn top(&self) -> HeapAddress {
		HeapAddress(self.0.len())
	}
}

impl<T> Index<HeapAddress> for Heap<T> {
	type Output = T;

	fn index(&self, index: HeapAddress) -> &Self::Output {
		self.0.get(*index).expect("Attempted reading an invalid heap address")
	}
}

impl<T> IndexMut<HeapAddress> for Heap<T> {
	fn index_mut(&mut self, index: HeapAddress) -> &mut Self::Output {
		self.0
			.get_mut(*index)
			.expect("Attempted accessing an invalid heap address")
	}
}

impl<T> Default for Heap<T> {
	fn default() -> Self {
		Self(Default::default())
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Copy, Clone, Debug, PartialEq, Eq, Display)]
pub enum StoreAddress {
	Register(VarRegister),
	Heap(HeapAddress),
}

#[rustfmt::skip] impl PartialEq<VarRegister> for StoreAddress { fn eq(&self, other: &VarRegister) -> bool { match self  { StoreAddress::Register(var_register) => var_register == other, _ => false, } } }
#[rustfmt::skip] impl PartialEq<StoreAddress> for VarRegister { fn eq(&self, other: &StoreAddress)     -> bool { match other { StoreAddress::Register(var_register) => var_register == self,  _ => false, } } }
#[rustfmt::skip] impl PartialEq<HeapAddress> for StoreAddress { fn eq(&self, other: &HeapAddress) -> bool { match self  { StoreAddress::Heap(heap_address)     => heap_address == other, _ => false, } } }
#[rustfmt::skip] impl PartialEq<StoreAddress> for HeapAddress { fn eq(&self, other: &StoreAddress)     -> bool { match other { StoreAddress::Heap(heap_address)     => heap_address == self,  _ => false, } } }

#[rustfmt::skip] impl Add<usize> for StoreAddress { type Output = Self; fn add(self, rhs: usize) -> Self::Output { match self { StoreAddress::Register(var_register) => StoreAddress::Register(var_register + rhs), StoreAddress::Heap(heap_address) => StoreAddress::Heap(heap_address + rhs), } } }
#[rustfmt::skip] impl Sub<usize> for StoreAddress { type Output = Self; fn sub(self, rhs: usize) -> Self::Output { match self { StoreAddress::Register(var_register) => StoreAddress::Register(var_register - rhs), StoreAddress::Heap(heap_address) => StoreAddress::Heap(heap_address - rhs), } } }
#[rustfmt::skip] impl AddAssign<usize> for StoreAddress { fn add_assign(&mut self, rhs: usize)  {  match self { StoreAddress::Register(var_register) => *var_register += rhs, StoreAddress::Heap(heap_address) => *heap_address += rhs, } } }
#[rustfmt::skip] impl SubAssign<usize> for StoreAddress { fn sub_assign(&mut self, rhs: usize)  {  match self { StoreAddress::Register(var_register) => *var_register -= rhs, StoreAddress::Heap(heap_address) => *heap_address -= rhs, } } }

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(
	Copy, Clone, Debug, Default, PartialEq, Eq, Display, Deref, DerefMut, From, Add, Sub, AddAssign, SubAssign,
)]
pub struct CodeAddress(usize);

#[rustfmt::skip] impl Add<usize> for CodeAddress { type Output = Self; fn add(self, rhs: usize) -> Self::Output { Self(self.0 + rhs) } }
#[rustfmt::skip] impl Sub<usize> for CodeAddress { type Output = Self; fn sub(self, rhs: usize) -> Self::Output { Self(self.0 - rhs) } }
#[rustfmt::skip] impl AddAssign<usize> for CodeAddress { fn add_assign(&mut self, rhs: usize) { self.0 += rhs } }
#[rustfmt::skip] impl SubAssign<usize> for CodeAddress { fn sub_assign(&mut self, rhs: usize) { self.0 -= rhs } }

#[derive(Clone, Debug, PartialEq, Eq, Display, From, Into)]
#[display("{}\n{}", display_iter!(instructions, "\n"), display_map!(labels))]
#[display(bounds(T: Display))]
pub struct Code<T> {
	pub instructions: Vec<T>,
	pub labels: Labels,
}

impl<T> Default for Code<T> {
	fn default() -> Self {
		Self {
			instructions: Default::default(),
			labels: Default::default(),
		}
	}
}

impl<T> Index<CodeAddress> for Code<T> {
	type Output = T;

	fn index(&self, index: CodeAddress) -> &Self::Output {
		self.instructions
			.get(*index)
			.expect("Attempted reading an invalid code address")
	}
}

impl<T> IndexMut<CodeAddress> for Code<T> {
	fn index_mut(&mut self, index: CodeAddress) -> &mut Self::Output {
		self.instructions
			.get_mut(*index)
			.expect("Attempted accessing an invalid code address")
	}
}

impl<T> Combinable for Code<T> {
	fn combined(self, other: Self) -> Self {
		let (instructions, labels) = (self.instructions, self.labels).combined((other.instructions, other.labels));

		Self { instructions, labels }
	}
}
