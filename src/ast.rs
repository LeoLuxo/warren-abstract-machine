pub type Identifier = String;

/// Represents a Prolog clause, which is either a fact or a rule.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Clause {
	Fact(Fact),
	Rule(Rule),
}

/// Represents a Prolog fact.
///
/// Examples:
/// - `IsList(nil).`
/// - `IsList(X).`
/// - `append(X, Y, c).`
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Fact(pub Atom);

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
	pub body: Vec<Atom>,
}

/// Represents a Prolog atom.
///
/// Examples:
/// - `IsList(nil)`
/// - `IsList(X)`
/// - `append(X, Y, c)`
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Atom(pub Vec<Term>);

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

/// Represents a Prolog constant.
///
/// Examples:
/// - `c`
/// - `0`
/// - `1`
/// - `nil`
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Constant(pub Identifier);

/// Represents a Prolog variable.
///
/// Examples:
/// - `V`
/// - `Var`
/// - `X`
/// - `ABC`
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Variable(pub Identifier);

/// Represents a Prolog structure.
///
/// Examples:
/// - `V`
/// - `Var`
/// - `X`
/// - `ABC`
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Structure {
	pub functor: Identifier,
	pub arguments: Vec<Term>,
}
