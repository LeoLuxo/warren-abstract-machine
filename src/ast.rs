use std::fmt::{self, Debug, Display};

use crate::{newtype, util::NewTypeVec};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub type Identifier = String;
pub type Arity = usize;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Functor {
	pub name: Identifier,
	pub arity: Arity,
}

impl Display for Functor {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.pad(&format!("{}/{}", self.name, self.arity))
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
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Atoms(Vec<Atom>);

newtype!(Atoms{Vec<Atom>});

/// Represents a Prolog atom.
///
/// Examples:
/// - `IsList(nil)`
/// - `IsList(X)`
/// - `append(X, Y, c)`
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Atom {
	pub name: Identifier,
	pub terms: Terms,
}

impl Display for Atom {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.pad(&format!("{}({})", self.name, self.terms))
	}
}

/// Represents a sequence of terms.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Terms(Vec<Term>);

newtype!(Terms{Vec<Term>});

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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
	Constant(Constant),
	Variable(Variable),
	Structure(Structure),
}

impl Display for Term {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Term::Constant(constant) => Display::fmt(&constant, f),
			Term::Variable(variable) => Display::fmt(&variable, f),
			Term::Structure(structure) => Display::fmt(&structure, f),
		}
	}
}

/// Represents a Prolog constant.
///
/// Examples:
/// - `c`
/// - `0`
/// - `1`
/// - `nil`
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Constant(pub Identifier);

impl Display for Constant {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(&self.0, f)
	}
}

/// Represents a Prolog variable.
///
/// Examples:
/// - `V`
/// - `Var`
/// - `X`
/// - `ABC`
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Variable(pub Identifier);

impl Display for Variable {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(&self.0, f)
	}
}

/// Represents a Prolog structure.
///
/// Examples:
/// - `V`
/// - `Var`
/// - `X`
/// - `ABC`
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Structure {
	pub name: Identifier,
	pub arguments: Terms,
}

impl Display for Structure {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.pad(&format!("{}({})", self.name, self.arguments))
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
