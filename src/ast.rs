use std::fmt::{self, Debug, Display};

use derive_more::derive::{Constructor, Display, From, Into, IntoIterator};

use crate::{util::VecLike, vec_like};

pub type Identifier = String;

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
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Fact(pub Atom);

impl Display for Fact {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.pad(&format!("{}.", self.0))
	}
}

/// Represents a Prolog rule.
///
/// A rule is made up of a head atom and 1 or more body atoms.
///
/// Examples:
/// - `path(X, Y) :- edge(X, Y).`
/// - `Path(X, Z) :- edge(X, Y), Path(Y, Z).`
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Rule {
	pub head: Atom,
	pub body: Atoms,
}

impl Display for Rule {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.pad(&format!("{} :- {}.", self.head, self.body))
	}
}

/// Represents a sequence of atoms.
#[derive(Clone, Debug, Default, PartialEq, Eq, Constructor, Display, From, Into, IntoIterator)]
#[display("{}", _0.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join(", "),)]
pub struct Atoms(Vec<Atom>);

vec_like!(Atoms; Atom);

/// Represents a Prolog atom.
///
/// Examples:
/// - `IsList(nil)`
/// - `IsList(X)`
/// - `append(X, Y, c)`
#[derive(Clone, Debug, Default, PartialEq, Eq, Constructor, Display, From, Into)]
#[display("{}({})", self.name, self.terms)]
pub struct Atom {
	pub name: Identifier,
	pub terms: Terms,
}

/// Represents a sequence of terms.
#[derive(Clone, Debug, Default, PartialEq, Eq, Constructor, Display, From, Into, IntoIterator)]
#[display("{}", _0.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join(", "),)]
pub struct Terms(Vec<Term>);

vec_like!(Terms; Term);

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
#[display("{}({})", self.name, self.arguments)]
pub struct Structure {
	pub name: Identifier,
	pub arguments: Terms,
}

type Arity = usize;

#[derive(Clone, Debug, Default, PartialEq, Eq, Constructor, Display, From, Into)]
#[display("{}/{}", self.name, self.arity)]
pub struct Functor {
	pub name: Identifier,
	pub arity: Arity,
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

	use crate::parser::Parsable;

	use super::*;

	#[test]
	fn test_display() -> Result<()> {
		assert_eq!(
			format!("{}", Clause::parse_from("path(X,Z):-edge(X,Y),edge(Y,Z).")?),
			"path(X, Z) :- edge(X, Y), edge(Y, Z)."
		);

		assert_eq!(format!("{}", Clause::parse_from("edge(1,2).")?), "edge(1, 2).");

		assert_eq!(
			format!("{}", Clause::parse_from("a(b(c),d(E)):-f(g(h(I,j,K),L),m,N).")?),
			"a(b(c), d(E)) :- f(g(h(I, j, K), L), m, N)."
		);

		Ok(())
	}
}
