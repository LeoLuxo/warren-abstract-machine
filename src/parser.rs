use anyhow::{bail, ensure, Context, Ok, Result};
use regex::Regex;

use crate::{
	ast::{Atom, Atoms, Clause, Clauses, Constant, Fact, Functor, Rule, Structure, Term, Terms, Variable},
	machine_types::VarRegister,
	regex_force_beginning, static_regex,
	subst::{AnonymousIdentifier, SubstTerm, Substitution},
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait ParseAs {
	fn parse_as<T: Parsable>(&self) -> Result<T>;
}

impl ParseAs for str {
	fn parse_as<T: Parsable>(&self) -> Result<T> {
		Parser::parse(self)
	}
}

pub trait Parsable
where
	Self: Sized,
{
	fn parser_match(parser: &mut Parser) -> Result<Self>;
}

pub struct Parser<'source> {
	source: &'source str,
	checkpoints: Vec<&'source str>,
}

macro_rules! either {
	{parser: $parser:expr, $($options:expr);* $(;)?} => {{
		$parser.push_checkpoint();
		let result = Err(anyhow::anyhow!("None of the either!() options worked"))$(.or_else(|_| {$parser.rewind_checkpoint(); println!("next try!"); $options}))*;
		$parser.pop_checkpoint();
		result
	}};
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

impl Parsable for VarRegister {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		either! {parser:parser,
			parser.match_token("X");
			parser.match_token("A");
		}?;

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
		either! {parser: parser,
			parser.match_type::<Structure<SubstTerm>>().map(Into::into);
			parser.match_type::<AnonymousIdentifier>().map(Into::into);
			parser.match_type::<Variable>().map(Into::into);
			parser.match_type::<Constant>().map(Into::into);
		}
	}
}

impl Parsable for AnonymousIdentifier {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.match_token("?")?;
		let id = parser.match_integer()?.parse::<u32>()?;

		Ok(id.into())
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
		either! {parser: parser,
			parser.match_type::<Fact>().map(Into::into);
			parser.match_type::<Rule>().map(Into::into);
		}
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
		either! {parser: parser,
			parser.match_type::<Structure>().map(Into::into);
			parser.match_type::<Variable>().map(Into::into);
			parser.match_type::<Constant>().map(Into::into);
		}
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
		let constant = either! {parser: parser,
			parser.match_signed_integer().map(ToOwned::to_owned);
			parser.match_lowercase_identifier().map(ToOwned::to_owned);
		}?;

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

impl<'source> Parser<'source> {
	pub fn new(source: &'source str) -> Self {
		Self {
			source,
			checkpoints: Default::default(),
		}
	}

	pub fn parse<T: Parsable>(source: &'source str) -> Result<T> {
		let mut parser = Self::new(source);
		let result = parser.match_type()?;
		parser.match_end()?;

		Ok(result)
	}

	pub fn push_checkpoint(&mut self) {
		self.checkpoints.push(self.source);
	}

	pub fn rewind_checkpoint(&mut self) {
		self.source = self
			.checkpoints
			.last()
			.expect("Tried to rewind to checkpoint, but no checkpoint available")
	}

	pub fn pop_checkpoint(&mut self) {
		self.checkpoints.pop();
	}

	fn trim_offset(&mut self, length: usize) {
		println!("before: {:?}", self.source);
		self.source = &self.source[length..];
		println!("after: {:?}", self.source);
	}

	fn trim_whitespace(&mut self) {
		self.source = self.source.trim_start()
	}

	pub fn match_end(&mut self) -> Result<()> {
		println!("match_end | source:{}", self.source);

		self.trim_whitespace();

		if !self.source.is_empty() {
			bail!("End not matched, found '{}'", self.source)
		}

		dbg!(Ok(()))
	}

	pub fn match_token(&mut self, token: &str) -> Result<()> {
		println!("match_token | source:{}", self.source);
		self.trim_whitespace();

		if !self.source.starts_with(token) {
			bail!("Token '{}' not matched, found '{}'", token, self.source)
		}

		self.trim_offset(token.len());

		dbg!(Ok(()))
	}

	pub fn match_regex(&mut self, regex: &str) -> Result<&str> {
		println!("match_regex regex: {} | source:{}", regex, self.source);
		self.trim_whitespace();

		let re_match = static_regex!(regex_force_beginning!(regex))
			// .is_match_at(&self.source, 0)
			.find_at(self.source, 0)
			.context(format!(
				"Regex pattern '{}' not matched, found '{}'",
				regex, self.source
			))?;

		self.trim_offset(re_match.end());

		dbg!(Ok(re_match.as_str()))
	}

	pub fn match_regex_captures<const N: usize>(&mut self, regex: &str) -> Result<[&str; N]> {
		println!("match_regex_captures | source:{}", self.source);
		self.trim_whitespace();

		let (full_match, captures) = static_regex!(regex_force_beginning!(regex))
			.captures_at(self.source, 0)
			.context(format!(
				"Regex pattern '{}' not matched, found '{}'",
				regex, self.source
			))?
			.extract::<{ N }>();

		self.trim_offset(full_match.len());

		dbg!(Ok(captures))
	}

	pub fn match_type<T: Parsable>(&mut self) -> Result<T> {
		println!("match_type | source:{}", self.source);
		self.trim_whitespace();

		<T as Parsable>::parser_match(self)
	}

	pub fn match_sequence_by_type<T: Parsable>(
		&mut self,
		separator: &str,
		minimum: Option<usize>,
		// until: impl Fn(&mut Self) -> Result<()>,
	) -> Result<Vec<T>> {
		println!("match_sequence | source:{}", self.source);
		self.match_sequence_with(separator, minimum, |p| p.match_type::<T>())
	}

	pub fn match_sequence_with<T>(
		&mut self,
		separator: &str,
		minimum: Option<usize>,
		inner_fn: impl Fn(&mut Self) -> Result<T>,
	) -> Result<Vec<T>> {
		println!("match_sequence | source:{}", self.source);

		self.push_checkpoint();

		let result = self.match_sequence_with_unchecked(separator, minimum, inner_fn);

		// If an error occurs while matching the sequence, rollback the source string
		if result.is_err() {
			self.rewind_checkpoint();
		}

		self.pop_checkpoint();
		result
	}

	fn match_sequence_with_unchecked<T>(
		&mut self,
		separator: &str,
		minimum: Option<usize>,
		inner_fn: impl Fn(&mut Self) -> Result<T>,
	) -> Result<Vec<T>> {
		let mut result = Vec::new();

		loop {
			let inner = inner_fn(self);
			if let Result::Ok(inner) = inner {
				result.push(inner);
			} else {
				break;
			}

			// Optionally match the separator
			let _ = self.match_token(separator);
		}

		if let Some(min) = minimum {
			ensure!(
				result.len() >= min,
				"Matched only {} element(s) in the sequence, expected at least {}",
				result.len(),
				min
			);
		}

		Ok(result)
	}

	pub fn match_identifier(&mut self) -> Result<&str> {
		println!("match_identifier | source:{}", self.source);
		self.match_regex(r"[a-zA-Z_][a-zA-Z0-9_]*")
	}

	pub fn match_lowercase_identifier(&mut self) -> Result<&str> {
		println!("match_lowercase_identifier | source:{}", self.source);
		self.match_regex(r"[a-z][a-zA-Z0-9]*")
	}

	pub fn match_uppercase_identifier(&mut self) -> Result<&str> {
		println!("match_uppercase_identifier | source:{}", self.source);
		self.match_regex(r"[A-Z][a-zA-Z0-9]*")
	}

	pub fn match_integer(&mut self) -> Result<&str> {
		println!("match_integer | source:{}", self.source);
		self.match_regex(r"[1-9][0-9]*|0")
	}

	pub fn match_signed_integer(&mut self) -> Result<&str> {
		println!("match_signed_integer | source:{}", self.source);
		self.match_regex(r"[+-]?[1-9][0-9]*|0")
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[cfg(test)]
mod tests {
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
