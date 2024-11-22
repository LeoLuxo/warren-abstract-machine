use std::{ops::Index, str::FromStr};

use anyhow::{bail, ensure, Context, Ok, Result};
use logos::{Lexer, Logos};
use regex::Regex;

use crate::{
	ast::{Atom, Atoms, Clause, Clauses, Constant, Fact, Functor, Rule, Structure, Term, Terms, Variable},
	machine_types::VarRegister,
	static_regex,
	subst::Substitution,
};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

macro_rules! parse_regex {
	($in:expr, $regex:expr) => {{
		parse_regex!($in, $regex, 1)[0]
	}};

	($in:expr, $regex:expr, $cap:expr) => {{
		static_regex!($regex)
			.captures(dbg!($in.trim()))
			.context(format!("Regex parsing error in '{}'", $in))?
			.extract::<$cap>()
			.1
	}};
}

macro_rules! either {
	{$first:expr; $($others:expr);+ $(;)?} => {
		either!($first).or_else(|_| either!($($others);*))
	};

	{$action:expr} => {
		$action
	};
}

macro_rules! impl_fromstr_from_parsable {
	($type:ty) => {
		impl FromStr for $type {
			type Err = anyhow::Error;

			fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
				Parser::parse(s)
			}
		}
	};
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

impl FromStr for VarRegister {
	type Err = anyhow::Error;

	fn from_str(s: &str) -> Result<Self> {
		let reg = parse_regex!(s, r"^(?:X|A)(\d+)$");
		let reg = reg.parse::<usize>()?.into();

		Ok(reg)
	}
}

impl FromStr for Functor {
	type Err = anyhow::Error;

	fn from_str(s: &str) -> Result<Self> {
		let [name, arity] = parse_regex!(s, r"^(\w+?)/(\d+?)$", 2);

		Ok(Self {
			name: name.to_owned().into(),
			arity: arity.parse()?,
		})
	}
}

impl FromStr for Substitution {
	type Err = anyhow::Error;

	fn from_str(s: &str) -> Result<Self> {
		// let outer_re = Regex::new(r"{\s*(.*?)\s*}").unwrap();
		// let outer_re = Regex::new(r"{\s*(.*?)\s*}").unwrap();

		todo!()
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

impl Parsable for Clauses {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.match_sequence::<Clause>("\n", None).map(Into::into)
	}
}

impl Parsable for Clause {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		either! {
			parser.match_type::<Fact>().map(Into::into);
			parser.match_type::<Rule>().map(Into::into);
		}
	}
}

impl Parsable for Fact {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let atom = parser.match_type::<Atom>()?;
		parser.match_token(".")?;

		Ok(atom.into())
	}
}

impl Parsable for Rule {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let head = parser.match_type::<Atom>()?;
		parser.match_token(":-")?;
		let body = parser.match_sequence::<Atom>(",", Some(1))?;
		parser.match_token(".")?;

		Ok((head, body.into()).into())
	}
}

impl Parsable for Atoms {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.match_sequence::<Atom>(",", None).map(Into::into)
	}
}

impl Parsable for Atom {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let name = parser.match_identifier()?.to_owned();
		parser.match_token("(")?;
		let terms = parser.match_sequence::<Term>(",", Some(1))?;
		parser.match_token(")")?;

		Ok((name.into(), terms.into()).into())
	}
}

impl Parsable for Terms {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		parser.match_sequence::<Term>(",", None).map(Into::into)
	}
}

impl Parsable for Term {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		either! {
			parser.match_type::<Structure>().map(Into::into);
			parser.match_type::<Variable>().map(Into::into);
			parser.match_type::<Constant>().map(Into::into);
		}
	}
}

impl Parsable for Structure {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let name = parser.match_identifier()?.to_owned();
		parser.match_token("(")?;
		let terms = parser.match_sequence::<Term>(",", Some(1))?;
		parser.match_token(")")?;

		Ok((name.into(), terms.into()).into())
	}
}

impl Parsable for Constant {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let constant = either! {
			parser.match_signed_integer().map(ToOwned::to_owned);
			parser.match_lowercase_identifier().map(ToOwned::to_owned);
		}?;

		Ok(constant.into())
	}
}

impl Parsable for Variable {
	fn parser_match(parser: &mut Parser) -> Result<Self> {
		let var = parser.match_uppercase_identifier()?.to_owned();

		Ok(var.into())
	}
}

impl_fromstr_from_parsable!(Clauses);
impl_fromstr_from_parsable!(Clause);
impl_fromstr_from_parsable!(Fact);
impl_fromstr_from_parsable!(Rule);
impl_fromstr_from_parsable!(Atoms);
impl_fromstr_from_parsable!(Atom);
impl_fromstr_from_parsable!(Term);
impl_fromstr_from_parsable!(Terms);
impl_fromstr_from_parsable!(Structure);
impl_fromstr_from_parsable!(Constant);
impl_fromstr_from_parsable!(Variable);

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait Parsable
where
	Self: Sized,
{
	fn parser_match(parser: &mut Parser) -> Result<Self>;
}

pub struct Parser<'source> {
	source: &'source str,
}

impl<'source> Parser<'source> {
	pub fn new(source: &'source str) -> Self {
		Self { source }
	}

	pub fn parse<T: Parsable>(source: &'source str) -> Result<T> {
		let mut parser = Self::new(source);
		let result = parser.match_type()?;
		parser.match_end()?;

		Ok(result)
	}

	pub fn trim(&mut self) {
		self.source = self.source.trim_start()
	}

	pub fn match_end(&mut self) -> Result<()> {
		self.trim();

		if !self.source.is_empty() {
			bail!("End not matched, found '{}'", self.source)
		}

		Ok(())
	}

	pub fn match_token(&mut self, token: &str) -> Result<()> {
		self.trim();

		if !self.source.starts_with(token) {
			bail!("Token '{}' not matched, found '{}'", token, self.source)
		}

		self.source = &self.source[token.len()..];

		Ok(())
	}

	pub fn match_regex(&mut self, regex: &str) -> Result<&str> {
		self.trim();

		let re_match = static_regex!(&format!("^{}", regex))
			.find_at(&self.source, 0)
			.context(format!(
				"Regex pattern '{}' not matched, found '{}'",
				regex, self.source
			))?;

		self.source = &self.source[re_match.end()..];

		Ok(re_match.as_str())
	}

	pub fn match_regex_captures<const N: usize>(&mut self, regex: &str) -> Result<[&str; N]> {
		self.trim();

		let (full_match, captures) = static_regex!(&format!("^{}", regex))
			.captures(&self.source)
			.context(format!(
				"Regex pattern '{}' not matched, found '{}'",
				regex, self.source
			))?
			.extract::<{ N }>();

		self.source = &self.source[full_match.len()..];

		Ok(captures)
	}

	pub fn match_type<T: Parsable>(&mut self) -> Result<T> {
		self.trim();

		<T as Parsable>::parser_match(self)
	}

	pub fn match_sequence<T: Parsable>(
		&mut self,
		separator: &str,
		minimum: Option<usize>,
		// until: impl Fn(&mut Self) -> Result<()>,
	) -> Result<Vec<T>> {
		let backup_source = self.source;

		let result = self.match_sequence_unchecked(separator, minimum);

		// If an error occurs while matching the sequence, rollback the source string
		if result.is_err() {
			self.source = backup_source;
		}

		result
	}

	fn match_sequence_unchecked<T: Parsable>(&mut self, separator: &str, minimum: Option<usize>) -> Result<Vec<T>> {
		let mut result = Vec::new();

		loop {
			self.trim();

			let inner = self.match_type::<T>();
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
		self.match_regex(r"[\p{XID_Start}_][\p{XID_Continue}#]*")
	}

	pub fn match_lowercase_identifier(&mut self) -> Result<&str> {
		self.match_regex(r"[a-z][a-zA-Z0-9]*")
	}

	pub fn match_uppercase_identifier(&mut self) -> Result<&str> {
		self.match_regex(r"[A-Z][a-zA-Z0-9]*")
	}

	pub fn match_integer(&mut self) -> Result<&str> {
		self.match_regex(r"[1-9][0-9]*|0")
	}

	pub fn match_signed_integer(&mut self) -> Result<&str> {
		self.match_regex(r"[+-]?[1-9][0-9]*|0")
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

// impl FromStr for Clauses {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		parse_sequence!(s, "\n", parse: Clause, collect: Vec<_>).map(Into::into)
// 	}
// }

// impl FromStr for Clause {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		try_each_and_map! {(Into::into);
// 			s.parse::<Fact>(),
// 			s.parse::<Rule>()
// 		}
// 	}
// }

// impl FromStr for Fact {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		let atom = parse_regex!(s, r"^([^.:-]*?)\.$").parse::<Atom>()?;

// 		Ok(Fact(atom))
// 	}
// }

// impl FromStr for Rule {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		let [head, body] = parse_regex!(s, r"^([^.:-]*?):-([^.:-]*?)\.$", 2);

// 		Ok(Rule {
// 			head: head.parse()?,
// 			body: body.parse()?,
// 		})
// 	}
// }

// impl FromStr for Atoms {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		parse_sequence!(s, ",", parse: Atom, collect: Vec<_>).map(Into::into)
// 	}
// }

// impl FromStr for Atom {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		let [name, terms] = parse_regex!(s, r"^(.*?)\((.*?)\)$", 2);

// 		Ok(Atom {
// 			name: name.to_owned().into(),
// 			terms: terms.parse()?,
// 		})
// 	}
// }

// impl FromStr for Terms {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		parse_sequence!(s, ",", parse: Term, collect: Vec<_>).map(Into::into)
// 	}
// }

// impl FromStr for Term {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		try_each_and_map! {(Into::into);
// 			s.parse::<Constant>(),
// 			s.parse::<Variable>(),
// 			s.parse::<Structure>()
// 		}
// 		// let [name, terms] = parse_regex!(s, r"^(.*?)\((.*?)\)$", 2);

// 		// Ok(Atom {
// 		// 	name: name.into(),
// 		// 	terms: terms.parse()?,
// 		// })
// 	}
// }

// impl FromStr for Constant {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		Ok(parse_regex!(s, r"^([a-z0-9][a-zA-Z0-9]*)$").to_owned().into())
// 	}
// }

// impl FromStr for Variable {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		Ok(parse_regex!(s, r"^([A-Z][a-zA-Z0-9]*)$").to_owned().into())
// 	}
// }

// impl FromStr for Structure {
// 	type Err = Err;

// 	fn from_str(s: &str) -> Result<Self> {
// 		let [name, terms] = parse_regex!(s, r"^([\p{XID_Start}_#][\p{XID_Continue}#]*)\((.*)\)$", 2);

// 		Ok(Self {
// 			name: name.to_owned().into(),
// 			arguments: terms.parse()?,
// 		})
// 	}
// }

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

// #[derive(Clone, Debug, Logos, Eq, PartialEq)]
// #[logos(skip r"[ \t\f]+")]
// enum PrologToken {
// 	/// A comment, a percent followed by any number of characters until the end of the line.
// 	/// Is automatically skipped by the lexer
// 	#[regex(r"%[^\n\r]*", logos::skip)]
// 	Comment,

// 	/// A line break, so any permutation of \n and \r for one of more empty lines.
// 	/// Is automatically skipped by the lexer
// 	#[regex(r"[\n|\r|\r\n]+", logos::skip)]
// 	LineBreak,

// 	#[token(",")]
// 	Comma,
// 	#[token(".")]
// 	Dot,
// 	#[token(":-")]
// 	Implies,
// 	#[token(")")]
// 	CloseParenthesis,

// 	/// An uppercase identifier
// 	#[regex(r"[A-Z][a-zA-Z0-9]*", lex_identifier)]
// 	VariableIdentifier(String),

// 	/// A lowercase identifier
// 	#[regex(r"[a-z0-9][a-zA-Z0-9]*", lex_identifier)]
// 	ConstantIdentifier(String),

// 	/// An identifier for a functor (matches the open parenthesis)
// 	#[regex(r"[\p{XID_Start}_#][\p{XID_Continue}#]*\(", lex_functor)]
// 	Functor(String),
// }

// /// The callback to extract an identifier.
// /// Simply gets the string
// fn lex_identifier(lexer: &mut Lexer<PrologToken>) -> String {
// 	lexer.slice().to_owned()
// }

// fn lex_functor(lexer: &mut Lexer<PrologToken>) -> String {
// 	let slice = lexer.slice();
// 	slice[..slice.len() - 1].to_owned()
// }

// struct Parser<'source>(Lexer<'source, PrologToken>);

// impl<'a> Parser<'a> {
// 	fn new(source: &'a str) -> Parser<'a> {
// 		Parser(PrologToken::lexer(source))
// 	}

// 	fn next(&mut self) -> Option<PrologToken> {
// 		if let Some(Ok(token)) = self.0.next() {
// 			Some(token)
// 		} else {
// 			None
// 		}
// 	}

// 	fn has_next(&self) -> bool {
// 		let next = self.0.clone().next();

// 		// with a logos lexer, the outer Option denotes whether there is a next lexed token to be parsed, while the inner Result denotes whether the token was lexed successfully
// 		next.is_some()
// 	}

// 	fn parse_clauses(&mut self, minimum: Option<usize>) -> Result<Clauses> {
// 		let mut clauses = Clauses::new();

// 		while self.has_next() {
// 			clauses.push(self.parse_clause()?);
// 		}

// 		if let Some(minimum) = minimum {
// 			ensure!(
// 				clauses.len() >= minimum,
// 				"found zero clauses, expected at least {minimum}"
// 			);
// 		}

// 		Ok(clauses)
// 	}

// 	fn parse_clause(&mut self) -> Result<Clause> {
// 		let head = self.parse_atom()?;

// 		match self.next() {
// 			Some(token) => match token {
// 				PrologToken::Dot => Ok(Clause::Fact(Fact(head))),

// 				PrologToken::Implies => Ok(Clause::Rule(Rule {
// 					head,
// 					body: self.parse_atoms(Some(1))?,
// 				})),

// 				_ => bail!("syntax error, expected . or :-"),
// 			},
// 			_ => bail!("syntax error, unexpected end of clause"),
// 		}
// 	}

// 	fn parse_atoms(&mut self, minimum: Option<usize>) -> Result<Atoms> {
// 		let mut atoms = Atoms::new();

// 		loop {
// 			atoms.push(self.parse_atom()?);

// 			match self.next() {
// 				Some(token) => match token {
// 					PrologToken::Comma => continue,

// 					PrologToken::Dot => {
// 						if let Some(minimum) = minimum {
// 							ensure!(
// 								atoms.len() >= minimum,
// 								"rule has zero body atoms, expected at least {minimum}"
// 							);
// 						}

// 						return Ok(atoms);
// 					}

// 					_ => bail!("syntax error, expected , or ."),
// 				},
// 				_ => bail!("syntax error, unexpected end of body atoms"),
// 			}
// 		}
// 	}

// 	fn parse_atom(&mut self) -> Result<Atom> {
// 		if let Some(PrologToken::Functor(functor)) = self.next() {
// 			Ok(Atom {
// 				name: functor.into(),
// 				terms: self.parse_terms(Some(1))?,
// 			})
// 		} else {
// 			bail!("syntax error, expected atom functor")
// 		}
// 	}

// 	fn parse_term(&mut self) -> Result<Term> {
// 		match self.next() {
// 			Some(token) => match token {
// 				PrologToken::Functor(functor) => Ok(Term::Structure(Structure {
// 					name: functor.into(),
// 					arguments: self.parse_terms(Some(1))?,
// 				})),

// 				PrologToken::VariableIdentifier(ident) => Ok(Term::Variable(Variable(ident.into()))),

// 				PrologToken::ConstantIdentifier(ident) => Ok(Term::Constant(Constant(ident.into()))),

// 				_ => bail!("syntax error, expected identifier"),
// 			},
// 			_ => bail!("syntax error, unexpected end of term"),
// 		}
// 	}

// 	fn parse_terms(&mut self, minimum: Option<usize>) -> Result<Terms> {
// 		let mut terms = Terms::new();

// 		loop {
// 			terms.push(self.parse_term()?);

// 			match self.next() {
// 				Some(token) => match token {
// 					PrologToken::Comma => continue,

// 					PrologToken::CloseParenthesis => {
// 						if let Some(minimum) = minimum {
// 							ensure!(
// 								terms.len() >= minimum,
// 								"structure has zero arguments, expected at least {minimum}"
// 							);
// 						}

// 						return Ok(terms);
// 					}

// 					_ => bail!("syntax error, expected , or )"),
// 				},
// 				_ => bail!("syntax error, unexpected end of arguments"),
// 			}
// 		}
// 	}
// }

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
			"a/0".parse::<Functor>()?,
			Functor {
				name: "a".into(),
				arity: 0
			}
		);

		assert_eq!(
			"f/1".parse::<Functor>()?,
			Functor {
				name: "f".into(),
				arity: 1
			}
		);

		assert_eq!(
			"loooooooooooooooooong/123456".parse::<Functor>()?,
			Functor {
				name: "loooooooooooooooooong".into(),
				arity: 123456
			}
		);

		assert!("".parse::<Functor>().is_err());
		assert!("/".parse::<Functor>().is_err());
		assert!("f/".parse::<Functor>().is_err());
		assert!("/0".parse::<Functor>().is_err());
		assert!("//".parse::<Functor>().is_err());
		assert!("f//".parse::<Functor>().is_err());
		assert!("/a/".parse::<Functor>().is_err());
		assert!("//0".parse::<Functor>().is_err());
		assert!("f/a/0".parse::<Functor>().is_err());

		Ok(())
	}

	#[test]
	fn test_parse_var_register() -> Result<()> {
		assert_eq!(*"X0".parse::<VarRegister>()?, 0);
		assert_eq!(*"X12345".parse::<VarRegister>()?, 12345);
		assert_eq!(*"A0".parse::<VarRegister>()?, 0);
		assert_eq!(*"A12345".parse::<VarRegister>()?, 12345);

		assert!("".parse::<VarRegister>().is_err());
		assert!("X".parse::<VarRegister>().is_err());
		assert!("A".parse::<VarRegister>().is_err());
		assert!("0".parse::<VarRegister>().is_err());
		assert!("1232345".parse::<VarRegister>().is_err());
		assert!("E1".parse::<VarRegister>().is_err());
		assert!("AX1".parse::<VarRegister>().is_err());

		Ok(())
	}

	#[test]
	fn test_parse_term_success() -> Result<()> {
		assert_eq!("const".parse::<Term>()?, Term::Constant(Constant("const".into())));

		assert_eq!("Var".parse::<Term>()?, Term::Variable(Variable("Var".into())));

		assert_eq!(
			"func(1)".parse::<Term>()?,
			Term::Structure(Structure {
				name: "func".into(),
				arguments: vec![Term::Constant(Constant("1".into()))].into()
			})
		);

		assert_eq!(
			"FUNC123(ASD)".parse::<Term>()?,
			Term::Structure(Structure {
				name: "FUNC123".into(),
				arguments: vec![Term::Variable(Variable("ASD".into()))].into()
			})
		);

		assert_eq!(
			"long(  1 ,x, 			X		,y)".parse::<Term>()?,
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
			"recursive(a(b(c(d))),x(y(Z)))".parse::<Term>()?,
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
		assert!("()".parse::<Term>().is_err());
		assert!("asd()".parse::<Term>().is_err());
		assert!("Asd()".parse::<Term>().is_err());
		assert!("(1)".parse::<Term>().is_err());
		assert!("(1,)".parse::<Term>().is_err());
		assert!("(1,3)".parse::<Term>().is_err());
	}

	#[test]
	fn test_parse_clause_success() -> Result<()> {
		assert_eq!(
			"edge(1, X).".parse::<Clause>()?,
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
			"a(b, c(d), e(f(G))).".parse::<Clause>()?,
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
			"path(X, Z) :- edge(X, Y), edge(Y, Z).".parse::<Clause>()?,
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
		assert!("asd :- asd.".parse::<Clause>().is_err());
		assert!(".".parse::<Clause>().is_err());
		assert!("()".parse::<Clause>().is_err());
		assert!("Path()".parse::<Clause>().is_err());
		assert!("Path().".parse::<Clause>().is_err());
		assert!("Path(1) :-".parse::<Clause>().is_err());
		assert!("Path(1) :- .".parse::<Clause>().is_err());
		assert!("Path(1) :- path().".parse::<Clause>().is_err());
		assert!("Path(1) : path(1).".parse::<Clause>().is_err());
	}
}
