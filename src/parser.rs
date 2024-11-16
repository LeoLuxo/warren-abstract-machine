use anyhow::{bail, ensure, Result};
use logos::{Lexer, Logos};

use crate::ast::{Atom, Atoms, Clause, Constant, Fact, Rule, Structure, Term, Terms, Variable};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait ParsableFrom<Source>: Sized {
	fn parse_from(source: Source) -> Result<Self>;
}

impl<T> ParsableFrom<T> for T {
	fn parse_from(source: T) -> Result<Self> {
		Ok(source)
	}
}

impl<S: AsRef<str>> ParsableFrom<S> for Term {
	fn parse_from(source: S) -> Result<Self> {
		Parser::new(source.as_ref()).parse_term()
	}
}

impl<S: AsRef<str>> ParsableFrom<S> for Clause {
	fn parse_from(source: S) -> Result<Self> {
		Parser::new(source.as_ref()).parse_clause()
	}
}

pub trait ParsableInto<Output>: Sized {
	fn parse(self) -> Result<Output>;
}

impl<T, U: ParsableFrom<T>> ParsableInto<U> for T {
	fn parse(self) -> Result<U> {
		U::parse_from(self)
	}
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, Logos, Eq, PartialEq)]
#[logos(skip r"[ \t\f]+")]
enum Token {
	/// A comment, a percent followed by any number of characters until the end of the line.
	/// Is automatically skipped by the lexer
	#[regex(r"%[^\n\r]*", logos::skip)]
	Comment,

	/// A line break, so any permutation of \n and \r for one of more empty lines.
	/// Is automatically skipped by the lexer
	#[regex(r"[\n|\r|\r\n]+", logos::skip)]
	LineBreak,

	#[token(",")]
	Comma,
	#[token(".")]
	Dot,
	#[token(":-")]
	Implies,
	#[token(")")]
	CloseParenthesis,

	/// An uppercase identifier
	#[regex(r"[A-Z][a-zA-Z0-9]*", lex_identifier)]
	VariableIdentifier(String),

	/// A lowercase identifier
	#[regex(r"[a-z0-9][a-zA-Z0-9]*", lex_identifier)]
	ConstantIdentifier(String),

	/// An identifier for a functor (matches the open parenthesis)
	#[regex(r"[\p{XID_Start}_#][\p{XID_Continue}#]*\(", lex_functor)]
	Functor(String),
}

/// The callback to extract an identifier.
/// Simply gets the string
fn lex_identifier(lexer: &mut Lexer<Token>) -> String {
	lexer.slice().to_owned()
}

fn lex_functor(lexer: &mut Lexer<Token>) -> String {
	let slice = lexer.slice();
	slice[..slice.len() - 1].to_owned()
}

struct Parser<'source>(Lexer<'source, Token>);

impl Parser<'_> {
	fn new(source: &str) -> Parser<'_> {
		Parser(Token::lexer(source))
	}

	fn next(&mut self) -> Option<Token> {
		if let Some(Ok(token)) = self.0.next() {
			Some(token)
		} else {
			None
		}
	}

	fn parse_clause(&mut self) -> Result<Clause> {
		let head = self.parse_atom()?;

		match self.next() {
			Some(token) => match token {
				Token::Dot => Ok(Clause::Fact(Fact(head))),

				Token::Implies => Ok(Clause::Rule(Rule {
					head,
					body: self.parse_atoms(Some(1))?,
				})),

				_ => bail!("syntax error, expected . or :-"),
			},
			_ => bail!("syntax error, unexpected end of clause"),
		}
	}

	fn parse_atoms(&mut self, minimum: Option<usize>) -> Result<Atoms> {
		let mut atoms = Atoms::new();

		loop {
			atoms.push(self.parse_atom()?);

			match self.next() {
				Some(token) => match token {
					Token::Comma => continue,

					Token::Dot => {
						if let Some(minimum) = minimum {
							ensure!(
								atoms.len() >= minimum,
								"rule has zero body atoms, expected at least {minimum}"
							);
						}

						return Ok(atoms);
					}

					_ => bail!("syntax error, expected , or ."),
				},
				_ => bail!("syntax error, unexpected end of body atoms"),
			}
		}
	}

	fn parse_atom(&mut self) -> Result<Atom> {
		if let Some(Token::Functor(functor)) = self.next() {
			Ok(Atom {
				name: functor.into(),
				terms: self.parse_terms(Some(1))?,
			})
		} else {
			bail!("syntax error, expected atom functor")
		}
	}

	fn parse_term(&mut self) -> Result<Term> {
		match self.next() {
			Some(token) => match token {
				Token::Functor(functor) => Ok(Term::Structure(Structure {
					name: functor.into(),
					arguments: self.parse_terms(Some(1))?,
				})),

				Token::VariableIdentifier(ident) => Ok(Term::Variable(Variable(ident.into()))),

				Token::ConstantIdentifier(ident) => Ok(Term::Constant(Constant(ident.into()))),

				_ => bail!("syntax error, expected identifier"),
			},
			_ => bail!("syntax error, unexpected end of term"),
		}
	}

	fn parse_terms(&mut self, minimum: Option<usize>) -> Result<Terms> {
		let mut terms = Terms::new();

		loop {
			terms.push(self.parse_term()?);

			match self.next() {
				Some(token) => match token {
					Token::Comma => continue,

					Token::CloseParenthesis => {
						if let Some(minimum) = minimum {
							ensure!(
								terms.len() >= minimum,
								"structure has zero arguments, expected at least {minimum}"
							);
						}

						return Ok(terms);
					}

					_ => bail!("syntax error, expected , or )"),
				},
				_ => bail!("syntax error, unexpected end of arguments"),
			}
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_parse_term_success() {
		assert_eq!(
			Parser::new("const").parse_term().unwrap(),
			Term::Constant(Constant("const".into()))
		);

		assert_eq!(
			Parser::new("Var").parse_term().unwrap(),
			Term::Variable(Variable("Var".into()))
		);

		assert_eq!(
			Parser::new("func(1)").parse_term().unwrap(),
			Term::Structure(Structure {
				name: "func".into(),
				arguments: vec![Term::Constant(Constant("1".into()))].into()
			})
		);

		assert_eq!(
			Parser::new("FUNC123(ASD)").parse_term().unwrap(),
			Term::Structure(Structure {
				name: "FUNC123".into(),
				arguments: vec![Term::Variable(Variable("ASD".into()))].into()
			})
		);

		assert_eq!(
			Parser::new("long(  1 ,x, 			X		,y)").parse_term().unwrap(),
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
			Parser::new("recursive(a(b(c(d))),x(y(Z)))").parse_term().unwrap(),
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
	}

	#[test]
	fn test_parse_term_failure() {
		assert!(Parser::new("()").parse_term().is_err());
		assert!(Parser::new("asd()").parse_term().is_err());
		assert!(Parser::new("Asd()").parse_term().is_err());
		assert!(Parser::new("(1)").parse_term().is_err());
		assert!(Parser::new("(1,)").parse_term().is_err());
		assert!(Parser::new("(1,3)").parse_term().is_err());
	}

	#[test]
	fn test_parse_clause_success() {
		assert_eq!(
			Parser::new("edge(1, X).").parse_clause().unwrap(),
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
			Parser::new("a(b, c(d), e(f(G))).").parse_clause().unwrap(),
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
			Parser::new("path(X, Z) :- edge(X, Y), edge(Y, Z).")
				.parse_clause()
				.unwrap(),
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
	}

	#[test]
	fn test_parse_clause_failure() {
		assert!(Parser::new("asd :- asd.").parse_clause().is_err());
		assert!(Parser::new(".").parse_clause().is_err());
		assert!(Parser::new("()").parse_clause().is_err());
		assert!(Parser::new("Path()").parse_clause().is_err());
		assert!(Parser::new("Path().").parse_clause().is_err());
		assert!(Parser::new("Path(1) :-").parse_clause().is_err());
		assert!(Parser::new("Path(1) :- .").parse_clause().is_err());
		assert!(Parser::new("Path(1) :- path().").parse_clause().is_err());
		assert!(Parser::new("Path(1) : path(1).").parse_clause().is_err());
	}
}
