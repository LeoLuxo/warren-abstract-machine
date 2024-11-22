use std::str::FromStr;

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

macro_rules! try_each_and_map {
	{$($any:expr),+} => {
		try_each_and_map!((|x| { x }); $($any),*)
	};

	{($map:expr); $first:expr, $($others:expr),+} => {
		try_each_and_map!(($map); $first).or_else(|_| try_each_and_map!(($map); $($others),*))
	};

	{($map:expr); $each:expr} => {
		$each.map($map)
	};
}

macro_rules! parse_sequence {
	($in:expr) => {
		parse_sequence!($in, ",")
	};

	($in:expr, $sep:expr) => {{
		dbg!($in).split($sep).map(str::trim)
	}};

	($in:expr, $sep:expr, parse: $parse:ty) => {{
		parse_sequence!($in, $sep).map(str::parse::<$parse>)
	}};

	($in:expr, $sep:expr, collect: $collect:ty) => {{
		parse_sequence!($in, $sep).collect::<Result<$collect>>()
	}};

	($in:expr, $sep:expr, parse: $parse:ty, collect: $collect:ty) => {{
		parse_sequence!($in, $sep, parse: $parse).collect::<Result<$collect>>()
	}};
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

type Err = anyhow::Error;

impl FromStr for VarRegister {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		let reg = parse_regex!(s, r"^(?:X|A)(\d+)$");
		let reg = reg.parse::<usize>()?.into();

		Ok(reg)
	}
}

impl FromStr for Functor {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		let [name, arity] = parse_regex!(s, r"^(\w+?)/(\d+?)$", 2);

		Ok(Self {
			name: name.to_owned().into(),
			arity: arity.parse()?,
		})
	}
}

impl FromStr for Substitution {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		// let outer_re = Regex::new(r"{\s*(.*?)\s*}").unwrap();
		// let outer_re = Regex::new(r"{\s*(.*?)\s*}").unwrap();

		todo!()
	}
}

impl FromStr for Clauses {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		parse_sequence!(s, "\n", parse: Clause, collect: Vec<_>).map(Into::into)
	}
}

impl FromStr for Clause {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		try_each_and_map! {(Into::into);
			s.parse::<Fact>(),
			s.parse::<Rule>()
		}
	}
}

impl FromStr for Fact {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		let atom = parse_regex!(s, r"^([^.:-]*?)\.$").parse::<Atom>()?;

		Ok(Fact(atom))
	}
}

impl FromStr for Rule {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		let [head, body] = parse_regex!(s, r"^([^.:-]*?):-([^.:-]*?)\.$", 2);

		Ok(Rule {
			head: head.parse()?,
			body: body.parse()?,
		})
	}
}

impl FromStr for Atoms {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		parse_sequence!(s, ",", parse: Atom, collect: Vec<_>).map(Into::into)
	}
}

impl FromStr for Atom {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		let [name, terms] = parse_regex!(s, r"^(.*?)\((.*?)\)$", 2);

		Ok(Atom {
			name: name.to_owned().into(),
			terms: terms.parse()?,
		})
	}
}

impl FromStr for Terms {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		parse_sequence!(s, ",", parse: Term, collect: Vec<_>).map(Into::into)
	}
}

impl FromStr for Term {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		try_each_and_map! {(Into::into);
			s.parse::<Constant>(),
			s.parse::<Variable>(),
			s.parse::<Structure>()
		}
		// let [name, terms] = parse_regex!(s, r"^(.*?)\((.*?)\)$", 2);

		// Ok(Atom {
		// 	name: name.into(),
		// 	terms: terms.parse()?,
		// })
	}
}

impl FromStr for Constant {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		Ok(parse_regex!(s, r"^([a-z0-9][a-zA-Z0-9]*)$").to_owned().into())
	}
}

impl FromStr for Variable {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		Ok(parse_regex!(s, r"^([A-Z][a-zA-Z0-9]*)$").to_owned().into())
	}
}

impl FromStr for Structure {
	type Err = Err;

	fn from_str(s: &str) -> Result<Self> {
		let [name, terms] = parse_regex!(s, r"^([\p{XID_Start}_#][\p{XID_Continue}#]*)\((.*)\)$", 2);

		Ok(Self {
			name: name.to_owned().into(),
			arguments: terms.parse()?,
		})
	}
}

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
