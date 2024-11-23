use anyhow::{Ok, Result};

use crate::{
	anonymous::AnonymousIdentifier,
	ast::{Atom, Atoms, Clause, Clauses, Constant, Fact, Functor, Rule, Structure, Term, Terms, Variable},
	machine_types::VarRegister,
	subst::{ScopedVariable, SubstTerm, Substitution, VariableContext},
};

use super::{Parsable, Parser};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

impl Parsable for VarRegister {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.disjunction().or_token("X").or_token("A").end()?;

		let reg = parser.match_integer()?.parse::<usize>()?;

		Ok(reg.into())
	}
}

impl Parsable for Functor {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let name = parser.match_identifier()?.to_owned().into();
		parser.match_token("/")?;
		let arity = parser.match_integer()?.parse()?;

		Ok(Self { name, arity })
	}
}

impl Parsable for Substitution {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.match_token("{")?;

		let elems = parser.match_sequence_with(",", None, |p| {
			let var = p.match_type::<Variable>()?.into();

			p.match_token("->")?;

			let subst_term = p.match_type::<SubstTerm>()?;

			Ok((var, subst_term))
		})?;

		parser.match_token("}")?;

		Ok(elems.into_iter().collect())
	}
}

impl Parsable for SubstTerm {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser
			.disjunction()
			.or_type::<Structure<SubstTerm>>()
			.or_type::<AnonymousIdentifier>()
			.or_type::<Variable>()
			.or_type::<Constant>()
			.end()
	}
}

impl Parsable for AnonymousIdentifier {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.match_token("?")?;
		let id = parser.match_integer()?.parse::<u32>()?;

		Ok(id.into())
	}
}

impl Parsable for ScopedVariable {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let variable = parser.match_type::<Variable>()?;

		let context = if parser.match_token("<").is_ok() {
			let ctx = parser.match_identifier()?.to_owned().into();
			parser.match_token(">")?;
			VariableContext::Local(ctx)
		} else {
			VariableContext::Query
		};

		Ok(ScopedVariable::new(variable, context))
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

impl Parsable for Clauses {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.match_sequence_by_type::<Clause>("\n", None).map(Into::into)
	}
}

impl Parsable for Clause {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.disjunction().or_type::<Fact>().or_type::<Rule>().end()
	}
}

impl Parsable for Fact {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let atom = parser.match_type::<Atom>()?;
		parser.match_token(".")?;

		Ok(Self(atom))
	}
}

impl Parsable for Rule {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let head = parser.match_type::<Atom>()?;
		parser.match_token(":-")?;
		let body = parser.match_sequence_by_type::<Atom>(",", Some(1))?.into();
		parser.match_token(".")?;

		Ok(Self { head, body })
	}
}

impl Parsable for Atoms {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.match_sequence_by_type::<Atom>(",", None).map(Into::into)
	}
}

impl Parsable for Atom {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let name = parser.match_identifier()?.to_owned().into();
		parser.match_token("(")?;
		let terms = parser.match_sequence_by_type::<Term>(",", Some(1))?.into();
		parser.match_token(")")?;

		Ok(Self { name, terms })
	}
}

impl Parsable for Terms {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.match_sequence_by_type::<Term>(",", None).map(Into::into)
	}
}

impl Parsable for Term {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser
			.disjunction()
			.or_type::<Structure>()
			.or_type::<Variable>()
			.or_type::<Constant>()
			.end()
	}
}

impl<T: Parsable> Parsable for Structure<T> {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let name = parser.match_identifier()?.to_owned().into();
		parser.match_token("(")?;
		let arguments = parser.match_sequence_by_type::<T>(",", Some(1))?.into();
		parser.match_token(")")?;

		Ok(Self { name, arguments })
	}
}

impl Parsable for Constant {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let constant = parser
			.disjunction()
			.or_signed_integer()
			.or_lowercase_identifier()
			.end()?;

		Ok(Self(constant.into()))
	}
}

impl Parsable for Variable {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let var = parser.match_uppercase_identifier()?.to_owned();

		Ok(Self(var.into()))
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[cfg(test)]
mod tests {
	use velcro::hash_map;

	use crate::parser::ParseAs;

	use super::*;

	#[test]
	fn test_parse_functor() -> Result<()> {
		assert_eq!(
			"a/0".parse_as::<Functor>()?,
			Functor {
				name: "a".into(),
				arity: 0
			}
		);

		assert_eq!(
			"f/1".parse_as::<Functor>()?,
			Functor {
				name: "f".into(),
				arity: 1
			}
		);

		assert_eq!(
			"loooooooooooooooooong/123456".parse_as::<Functor>()?,
			Functor {
				name: "loooooooooooooooooong".into(),
				arity: 123456
			}
		);

		assert!("".parse_as::<Functor>().is_err());
		assert!("/".parse_as::<Functor>().is_err());
		assert!("f/".parse_as::<Functor>().is_err());
		assert!("/0".parse_as::<Functor>().is_err());
		assert!("//".parse_as::<Functor>().is_err());
		assert!("f//".parse_as::<Functor>().is_err());
		assert!("/a/".parse_as::<Functor>().is_err());
		assert!("//0".parse_as::<Functor>().is_err());
		assert!("f/a/0".parse_as::<Functor>().is_err());

		Ok(())
	}

	#[test]
	fn test_parse_var_register() -> Result<()> {
		assert_eq!(*"X0".parse_as::<VarRegister>()?, 0);
		assert_eq!(*"X12345".parse_as::<VarRegister>()?, 12345);
		assert_eq!(*"A0".parse_as::<VarRegister>()?, 0);
		assert_eq!(*"A12345".parse_as::<VarRegister>()?, 12345);

		assert!("".parse_as::<VarRegister>().is_err());
		assert!("X".parse_as::<VarRegister>().is_err());
		assert!("A".parse_as::<VarRegister>().is_err());
		assert!("0".parse_as::<VarRegister>().is_err());
		assert!("1232345".parse_as::<VarRegister>().is_err());
		assert!("E1".parse_as::<VarRegister>().is_err());
		assert!("AX1".parse_as::<VarRegister>().is_err());

		Ok(())
	}

	#[test]
	fn test_parse_substitution() -> Result<()> {
		assert_eq!("{}".parse_as::<Substitution>()?, Substitution::default());

		assert_eq!(
			"{ X -> ?1, Y -> ?1, Z -> ?2 }".parse_as::<Substitution>()?,
			hash_map!(
				"X".parse_as()?: "?1".parse_as()?,
				"Y".parse_as()?: "?1".parse_as()?,
				"Z".parse_as()?: "?2".parse_as()?,
			)
			.into()
		);

		assert_eq!(
			"{ X -> ?1, Y -> ?1, Z -> ?2 }".parse_as::<Substitution>()?,
			hash_map!(
				"X".parse_as()?: "?567".parse_as()?,
				"Y".parse_as()?: "?567".parse_as()?,
				"Z".parse_as()?: "?789".parse_as()?,
			)
			.into()
		);

		assert_eq!(
			"{ C -> f(a, X) }".parse_as::<Substitution>()?,
			hash_map!(
				"C".parse_as()?: "f(a, X)".parse_as()?,
			)
			.into()
		);

		Ok(())
	}

	#[test]
	fn test_parse_term_success() -> Result<()> {
		assert_eq!("const".parse_as::<Term>()?, Term::Constant(Constant("const".into())));

		assert_eq!("Var".parse_as::<Term>()?, Term::Variable(Variable("Var".into())));

		assert_eq!(
			"func(1)".parse_as::<Term>()?,
			Term::Structure(Structure {
				name: "func".into(),
				arguments: vec![Term::Constant(Constant("1".into()))].into()
			})
		);

		assert_eq!(
			"FUNC123(ASD)".parse_as::<Term>()?,
			Term::Structure(Structure {
				name: "FUNC123".into(),
				arguments: vec![Term::Variable(Variable("ASD".into()))].into()
			})
		);

		assert_eq!(
			"long(  1 ,x, 			X		,y)".parse_as::<Term>()?,
			Term::Structure(Structure {
				name: "long".into(),
				arguments: vec![
					Term::Constant(Constant("1".into())),
					Term::Constant(Constant("x".into())),
					Term::Variable(Variable("X".into())),
					Term::Constant(Constant("y".into())),
				]
				.into()
			})
		);

		assert_eq!(
			"recursive(a(b(c(d))),x(y(Z)))".parse_as::<Term>()?,
			Term::Structure(Structure {
				name: "recursive".into(),
				arguments: vec![
					Term::Structure(Structure {
						name: "a".into(),
						arguments: vec![Term::Structure(Structure {
							name: "b".into(),
							arguments: vec![Term::Structure(Structure {
								name: "c".into(),
								arguments: vec![Term::Constant(Constant("d".into()))].into()
							})]
							.into()
						})]
						.into()
					}),
					Term::Structure(Structure {
						name: "x".into(),
						arguments: vec![Term::Structure(Structure {
							name: "y".into(),
							arguments: vec![Term::Variable(Variable("Z".into()))].into()
						})]
						.into()
					})
				]
				.into()
			})
		);

		Ok(())
	}

	#[test]
	fn test_parse_term_failure() {
		assert!("()".parse_as::<Term>().is_err());
		assert!("asd()".parse_as::<Term>().is_err());
		assert!("Asd()".parse_as::<Term>().is_err());
		assert!("(1)".parse_as::<Term>().is_err());
		assert!("(1,)".parse_as::<Term>().is_err());
		assert!("(1,3)".parse_as::<Term>().is_err());
	}

	#[test]
	fn test_parse_clause_success() -> Result<()> {
		assert_eq!(
			"edge(1, X).".parse_as::<Clause>()?,
			Clause::Fact(Fact(Atom {
				name: "edge".into(),
				terms: vec![
					Term::Constant(Constant("1".into())),
					Term::Variable(Variable("X".into()))
				]
				.into()
			}))
		);

		assert_eq!(
			"a(b, c(d), e(f(G))).".parse_as::<Clause>()?,
			Clause::Fact(Fact(Atom {
				name: "a".into(),
				terms: vec![
					Term::Constant(Constant("b".into())),
					Term::Structure(Structure {
						name: "c".into(),
						arguments: vec![Term::Constant(Constant("d".into()))].into()
					}),
					Term::Structure(Structure {
						name: "e".into(),
						arguments: vec![Term::Structure(Structure {
							name: "f".into(),
							arguments: vec![Term::Variable(Variable("G".into()))].into()
						})]
						.into()
					})
				]
				.into()
			}))
		);

		assert_eq!(
			"path(X, Z) :- edge(X, Y), edge(Y, Z).".parse_as::<Clause>()?,
			Clause::Rule(Rule {
				head: Atom {
					name: "path".into(),
					terms: vec![
						Term::Variable(Variable("X".into())),
						Term::Variable(Variable("Z".into()))
					]
					.into()
				},
				body: vec![
					Atom {
						name: "edge".into(),
						terms: vec![
							Term::Variable(Variable("X".into())),
							Term::Variable(Variable("Y".into()))
						]
						.into()
					},
					Atom {
						name: "edge".into(),
						terms: vec![
							Term::Variable(Variable("Y".into())),
							Term::Variable(Variable("Z".into()))
						]
						.into()
					}
				]
				.into()
			})
		);

		Ok(())
	}

	#[test]
	fn test_parse_clause_failure() {
		assert!("asd :- asd.".parse_as::<Clause>().is_err());
		assert!(".".parse_as::<Clause>().is_err());
		assert!("()".parse_as::<Clause>().is_err());
		assert!("Path()".parse_as::<Clause>().is_err());
		assert!("Path().".parse_as::<Clause>().is_err());
		assert!("Path(1) :-".parse_as::<Clause>().is_err());
		assert!("Path(1) :- .".parse_as::<Clause>().is_err());
		assert!("Path(1) :- path().".parse_as::<Clause>().is_err());
		assert!("Path(1) : path(1).".parse_as::<Clause>().is_err());
	}
}
