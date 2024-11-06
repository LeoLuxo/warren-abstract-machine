use anyhow::{anyhow, bail, Result};
use logos::{Lexer, Logos};
use tokens::Token;

use crate::ast::{Constant, Structure, Term, Variable};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod tokens;

pub fn parse_term(l: &mut Lexer<'_, Token>) -> Result<Term> {
	if let Some(Ok(ident_token)) = l.next() {
		if let Some(Ok(Token::LeftParenthesis)) = l.peekable().peek() {
			// term is a structure
			let _ = l.next();

			let identifier = match ident_token {
				Token::UppercaseIdentifier(ident) => ident,
				Token::LowercaseIdentifier(ident) => ident,
				Token::AnyIdentifier(ident) => ident,
				_ => bail!("invalid token, expected structure identifier"),
			};

			Ok(Term::Structure(Structure {
				functor: identifier,
				arguments: parse_structure_arguments(l)?,
			}))
		} else {
			// term is either a constant or a variable

			Ok(match ident_token {
				Token::UppercaseIdentifier(ident) => Term::Variable(Variable(ident)),
				Token::LowercaseIdentifier(ident) => Term::Constant(Constant(ident)),
				_ => bail!("invalid token, expected variable/constant identifier"),
			})
		}
	} else {
		bail!("syntax error, expected term")
	}
}

pub fn parse_structure_arguments(l: &mut Lexer<'_, Token>) -> Result<Vec<Term>> {
	let mut args = Vec::<Term>::new();
	let mut expecting_term = true;

	while let Some(Ok(token)) = l.peekable().peek() {
		match token {
			_ if expecting_term => {
				args.push(parse_term(l)?);
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

	// didn't encounter right parenthesis
	bail!("syntax error, expected structure argument")
}
