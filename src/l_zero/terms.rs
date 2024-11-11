use anyhow::{bail, Result};

use crate::{
	ast::{Constant, Structure, Term},
	parser::Parsable,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub enum FirstOrderTerm {
	Constant(Constant),
	Structure(Structure),
}

impl TryInto<FirstOrderTerm> for Term {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<FirstOrderTerm, Self::Error> {
		Ok(match self {
			Term::Constant(constant) => FirstOrderTerm::Constant(constant),
			Term::Structure(structure) => FirstOrderTerm::Structure(structure),
			_ => bail!("Cannot convert term to first order term"),
		})
	}
}

impl Parsable for FirstOrderTerm {
	fn parse_from(source: &str) -> Result<Self> {
		Term::parse_from(source)?.try_into()
	}
}
