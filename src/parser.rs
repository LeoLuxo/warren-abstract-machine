use anyhow::{anyhow, bail, ensure, Context, Result};
use logos::{Lexer, Logos};

use crate::ast::{Atom, Clause, Constant, Fact, Rule, Structure, Term, Variable};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub trait Parsable: Sized {
	fn parse(source: &str) -> Result<Self>;
}

impl Parsable for Term {
	fn parse(source: &str) -> Result<Self> {
		Parser::new(source).parse_term()
	}
}

impl Parsable for Vec<Term> {
	fn parse(source: &str) -> Result<Self> {
		Parser::new(source).parse_term_arguments(None)
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
	fn new<'source>(source: &'source str) -> Parser<'source> {
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

				Token::Implies => {
					let body = self.parse_body_atoms(Some(1))?;

					ensure!(body.len() >= 1, "rule has zero body atoms, expected at least one");

					Ok(Clause::Rule(Rule { head, body }))
				}

				_ => bail!("syntax error, expected . or :-"),
			},
			_ => bail!("syntax error, unexpected end of clause"),
		}
	}

	fn parse_body_atoms(&mut self, minimum: Option<usize>) -> Result<Vec<Atom>> {
		let mut atoms = Vec::<Atom>::new();

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
				functor,
				terms: self.parse_term_arguments(Some(1))?,
			})
		} else {
			bail!("syntax error, expected atom functor")
		}
	}

	fn parse_term(&mut self) -> Result<Term> {
		match self.next() {
			Some(token) => match token {
				Token::Functor(functor) => Ok(Term::Structure(Structure {
					functor,
					arguments: self.parse_term_arguments(Some(1))?,
				})),

				Token::VariableIdentifier(ident) => Ok(Term::Variable(Variable(ident))),

				Token::ConstantIdentifier(ident) => Ok(Term::Constant(Constant(ident))),

				_ => bail!("syntax error, expected identifier"),
			},
			_ => bail!("syntax error, unexpected end of term"),
		}
	}

	fn parse_term_arguments(&mut self, minimum: Option<usize>) -> Result<Vec<Term>> {
		let mut args = Vec::<Term>::new();

		loop {
			args.push(self.parse_term()?);

			match self.next() {
				Some(token) => match token {
					Token::Comma => continue,

					Token::CloseParenthesis => {
						if let Some(minimum) = minimum {
							ensure!(
								args.len() >= minimum,
								"structure has zero arguments, expected at least {minimum}"
							);
						}

						return Ok(args);
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
			Term::Constant(Constant("const".to_string()))
		);

		assert_eq!(
			Parser::new("Var").parse_term().unwrap(),
			Term::Variable(Variable("Var".to_string()))
		);

		assert_eq!(
			Parser::new("func(1)").parse_term().unwrap(),
			Term::Structure(Structure {
				functor: "func".to_string(),
				arguments: vec![Term::Constant(Constant("1".to_string()))]
			})
		);

		assert_eq!(
			Parser::new("FUNC123(ASD)").parse_term().unwrap(),
			Term::Structure(Structure {
				functor: "FUNC123".to_string(),
				arguments: vec![Term::Variable(Variable("ASD".to_string()))]
			})
		);

		assert_eq!(
			Parser::new("long(1,x,X,y)").parse_term().unwrap(),
			Term::Structure(Structure {
				functor: "long".to_string(),
				arguments: vec![
					Term::Constant(Constant("1".to_string())),
					Term::Constant(Constant("x".to_string())),
					Term::Variable(Variable("X".to_string())),
					Term::Constant(Constant("y".to_string())),
				]
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
				functor: "edge".to_string(),
				terms: vec![
					Term::Constant(Constant("1".to_string())),
					Term::Variable(Variable("X".to_string()))
				]
			}))
		);

		assert_eq!(
			Parser::new("path(X, Z) :- edge(X, Y), edge(Y, Z).")
				.parse_clause()
				.unwrap(),
			Clause::Rule(Rule {
				head: Atom {
					functor: "path".to_string(),
					terms: vec![
						Term::Variable(Variable("X".to_string())),
						Term::Variable(Variable("Z".to_string()))
					]
				},
				body: vec![
					Atom {
						functor: "edge".to_string(),
						terms: vec![
							Term::Variable(Variable("X".to_string())),
							Term::Variable(Variable("Y".to_string()))
						]
					},
					Atom {
						functor: "edge".to_string(),
						terms: vec![
							Term::Variable(Variable("Y".to_string())),
							Term::Variable(Variable("Z".to_string()))
						]
					}
				]
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
