use std::{
	borrow::Cow,
	fmt::{self, Debug, Display},
};

use derive_more::derive::{Constructor, Deref, DerefMut, Display, From, Index, IndexMut, Into, IntoIterator};

use crate::display_iter;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Deref, DerefMut, Constructor, Display, From)]
#[from(forward)]
pub struct Identifier(Cow<'static, str>);

/// Represents a Prolog clause, which is either a fact or a rule.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Clause {
	Fact(Fact),
	Rule(Rule),
}

impl Display for Clause {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Clause::Fact(fact) => Display::fmt(&fact, f),
			Clause::Rule(rule) => Display::fmt(&rule, f),
		}
	}
}

/// Represents a Prolog fact.
///
/// Examples:
/// - `IsList(nil).`
/// - `IsList(X).`
/// - `append(X, Y, c).`
#[derive(Clone, Debug, Default, PartialEq, Eq, Deref, DerefMut, Constructor, Display, From, Into)]
#[display("{_0}.")]
pub struct Fact(pub Atom);

/// Represents a Prolog rule.
///
/// A rule is made up of a head atom and 1 or more body atoms.
///
/// Examples:
/// - `path(X, Y) :- edge(X, Y).`
/// - `Path(X, Z) :- edge(X, Y), Path(Y, Z).`
#[derive(Clone, Debug, Default, PartialEq, Eq, Constructor, Display, From, Into)]
#[display("{head} :- {body}.")]
pub struct Rule {
	pub head: Atom,
	pub body: Atoms,
}

/// Represents a sequence of atoms.
#[derive(Clone, Debug, Default, PartialEq, Eq, Deref, DerefMut, Display, From, Into, IntoIterator, Index, IndexMut)]
#[display("{}", display_iter!(_0, ", "))]
pub struct Atoms(Vec<Atom>);

impl Atoms {
	pub fn new() -> Self {
		Self(Vec::new())
	}
}

/// Represents a Prolog atom.
///
/// Examples:
/// - `IsList(nil)`
/// - `IsList(X)`
/// - `append(X, Y, c)`
#[derive(Clone, Debug, Default, PartialEq, Eq, Constructor, Display, From, Into)]
#[display("{name}({terms})")]
pub struct Atom {
	pub name: Identifier,
	pub terms: Terms,
}

/// Represents a sequence of terms.
#[derive(Clone, Debug, Default, PartialEq, Eq, Display, From, Into, IntoIterator, Deref, DerefMut, Index, IndexMut)]
#[display("{}", display_iter!(_0, ", "))]
pub struct Terms(Vec<Term>);

impl Terms {
	pub fn new() -> Self {
		Self(Vec::new())
	}
}

/// Represents a Prolog term.
///
/// These can be a constant (lowercase-starting string or integer), variable (uppercase-starting string) or a structure.
///
/// Examples:
/// - `1`
/// - `nil`
/// - `s(1, 2)`
/// - `X`
/// - `cons(1, Y)`
#[derive(Clone, Debug, PartialEq, Eq, Display, From)]
pub enum Term {
	Constant(Constant),
	Variable(Variable),
	Structure(Structure),
}



/// Represents a Prolog constant.
///
/// Examples:
/// - `c`
/// - `0`
/// - `1`
/// - `nil`
#[derive(Clone, Debug, Default, PartialEq, Eq, Constructor, Display, From, Into)]
pub struct Constant(pub Identifier);

/// Represents a Prolog variable.
///
/// Examples:
/// - `V`
/// - `Var`
/// - `X`
/// - `ABC`
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Constructor, Display, From, Into)]
pub struct Variable(pub Identifier);

/// Represents a Prolog structure.
///
/// Examples:
/// - `V`
/// - `Var`
/// - `X`
/// - `ABC`
#[derive(Clone, Debug, Default, PartialEq, Eq, Constructor, Display, From, Into)]
#[display("{name}({arguments})")]
pub struct Structure {
	pub name: Identifier,
	pub arguments: Terms,
}

type Arity = usize;

#[derive(Clone, Debug, Default, PartialEq, Eq, Constructor, Display, From, Into)]
#[display("{name}/{arity}")]
pub struct Functor {
	pub name: Identifier,
	pub arity: Arity,
}

impl Functor {
	pub fn is_constant(&self) -> bool {
		self.arity == 0
	}
}

pub trait GetFunctor {
	fn get_functor(&self) -> Functor;
}

impl GetFunctor for Atom {
	fn get_functor(&self) -> Functor {
		Functor {
			name: self.name.clone(),
			arity: self.terms.len(),
		}
	}
}

impl GetFunctor for Structure {
	fn get_functor(&self) -> Functor {
		Functor {
			name: self.name.clone(),
			arity: self.arguments.len(),
		}
	}
}

impl GetFunctor for Constant {
	fn get_functor(&self) -> Functor {
		Functor {
			name: self.0.clone(),
			arity: 0,
		}
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[cfg(test)]
mod tests {
	use anyhow::Result;

	use super::*;

	#[test]
	fn test_display() -> Result<()> {
		assert_eq!(
			format!("{}", "path(X,Z):-edge(X,Y),edge(Y,Z).".parse::<Clause>()?),
			"path(X, Z) :- edge(X, Y), edge(Y, Z)."
		);

		assert_eq!(format!("{}", "edge(1,2).".parse::<Clause>()?), "edge(1, 2).");

		assert_eq!(
			format!("{}", "a(b(c),d(E)):-f(g(h(I,j,K),L),m,N).".parse::<Clause>()?),
			"a(b(c), d(E)) :- f(g(h(I, j, K), L), m, N)."
		);

		Ok(())
	}
}
