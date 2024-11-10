use anyhow::{anyhow, bail, ensure, Context, Result};
use logos::{Lexer, Logos};

use crate::ast::{Atom, Clause, Constant, Fact, Rule, Structure, Term, Variable};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, Logos)]
#[logos(skip r"[ \t\f]+")]
pub enum Token {
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
	UppercaseIdentifier(String),

	/// A lowercase identifier
	#[regex(r"[a-z][a-zA-Z0-9]*", lex_identifier)]
	LowercaseIdentifier(String),

	/// An identifier for a functor
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
	pub fn new<'source>(source: &'source str) -> Parser<'source> {
		Parser(Token::lexer(source))
	}

	fn next(&mut self) -> Option<Token> {
		match self.0.next() {
			Some(Ok(token)) => Some(token),
			_ => None,
		}
	}

	pub fn parse_clause(&mut self) -> Result<Clause> {
		let head = self.parse_atom()?;

		match self.next() {
			Some(Token::Dot) => Ok(Clause::Fact(Fact(head))),

			Some(Token::Implies) => Ok(Clause::Rule(Rule {
				head,
				body: self.parse_body_atoms()?,
			})),

			_ => bail!("syntax error, expected clause"),
		}
	}

	fn parse_body_atoms(&mut self) -> Result<Vec<Atom>> {
		let mut atoms = Vec::<Atom>::new();

		loop {
			atoms.push(self.parse_atom()?);

			match self.next() {
				Some(Token::Comma) => continue,

				Some(Token::Dot) => {
					ensure!(atoms.len() >= 1, "rule has zero body atoms, expected at least one");

					return Ok(atoms);
				}

				_ => bail!("syntax error, expected comma or dot"),
			}
		}
	}

	fn parse_atom(&mut self) -> Result<Atom> {
		Ok(Atom(self.parse_term_arguments()?))
	}

	fn parse_term(&mut self) -> Result<Term> {
		match self.next() {
			Some(Token::Functor(ident)) => Ok(Term::Structure(Structure {
				functor: ident,
				arguments: self.parse_term_arguments()?,
			})),

			Some(Token::UppercaseIdentifier(ident)) => Ok(Term::Variable(Variable(ident))),

			Some(Token::LowercaseIdentifier(ident)) => Ok(Term::Constant(Constant(ident))),

			_ => bail!("syntax error, expected term"),
		}
	}

	fn parse_term_arguments(&mut self) -> Result<Vec<Term>> {
		let mut args = Vec::<Term>::new();

		loop {
			args.push(self.parse_term()?);

			match self.next() {
				Some(Token::Comma) => continue,

				Some(Token::CloseParenthesis) => {
					ensure!(args.len() >= 1, "structure has zero arguments, expected at least one");

					return Ok(args);
				}

				_ => bail!("syntax error, expected comma or right parenthesis"),
			}
		}
	}
}
