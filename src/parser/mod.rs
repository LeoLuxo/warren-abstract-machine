use std::{iter::Peekable, str::Chars};

use anyhow::{anyhow, bail, ensure, Context, Result};
use logos::{Lexer, Logos};
use tokens::Token;

use crate::ast::{Constant, Structure, Term, Variable};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod tokens;

pub fn parse(str: String) {}

struct Parser<'a>(Peekable<Chars<'a>>);

impl Parser<'_> {
	fn new<'a>(string: &'a str) -> Parser<'a> {
		Parser(string.chars().peekable())
	}

	fn next(&mut self) -> Option<char> {
		self.0.next()
	}

	fn peek(&mut self) -> Option<char> {
		self.0.peek().copied()
	}

	fn skip_whitespace(&mut self) {
		while self.0.peek().map_or(false, |c| c.is_whitespace()) {
			self.0.next();
		}
	}

	fn parse_term(mut self) -> Result<Term> {
		self.skip_whitespace();

		let mut identifier = String::new();

		loop {
			match self.peek().context("syntax error, expected term")? {
				'(' => {
					self.next();

					ensure!(identifier.len() >= 1, "identifier cannot be empty");

					return Ok(Term::Structure(Structure {
						functor: identifier,
						arguments: self.parse_structure_arguments()?,
					}));
				}

				c if c.is_ascii_alphanumeric() => {
					self.next();
					identifier.push(c);
				}

				_ => break,
			}
		}

		todo!()

		// if let Some(Ok(Token::LeftParenthesis)) = l.peekable().peek() {
		// 	// term is a structure
		// 	let _ = l.next();

		// 	let identifier = match ident_token {
		// 		Token::UppercaseIdentifier(ident) => ident,
		// 		Token::LowercaseIdentifier(ident) => ident,
		// 		Token::AnyIdentifier(ident) => ident,
		// 		_ => bail!("invalid token, expected structure identifier"),
		// 	};

		// 	Ok(Term::Structure(Structure {
		// 		functor: identifier,
		// 		arguments: parse_structure_arguments(l)?,
		// 	}))
		// } else {
		// 	// term is either a constant or a variable

		// 	Ok(match ident_token {
		// 		Token::UppercaseIdentifier(ident) => Term::Variable(Variable(ident)),
		// 		Token::LowercaseIdentifier(ident) => Term::Constant(Constant(ident)),
		// 		_ => bail!("invalid token, expected variable/constant identifier"),
		// 	})
		// }
	}

	fn parse_structure_arguments(mut self) -> Result<Vec<Term>> {
		let mut args = Vec::<Term>::new();
		let mut expecting_term = true;

		loop {
			match self.peek().context("syntax error, expected structure arguments")? {
				_ if expecting_term => {
					args.push(self.parse_term()?);
					expecting_term = false;
				}

				Token::Comma => {
					l.next();
					expecting_term = true;
				}

				Token::RightParenthesis => {
					l.next();

					if args.len() < 1 {
						bail!("structure has zero arguments, expected at least one")
					}

					return Ok(args);
				}

				_ => bail!("syntax error, expected comma or right parenthesis"),
			}
		}
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
	fn test_parse_term() {
		println!()
	}
}
