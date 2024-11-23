use anyhow::{Context, Result};

use super::{parser_sequence::Separator, Parsable, Parser};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub struct ParserDisjunction<'source, 'parser, T> {
	parser: &'parser mut Parser<'source>,
	result: Result<T>,
}

impl<'source> Parser<'source> {
	pub fn disjunction<'parser, T>(&'parser mut self) -> ParserDisjunction<'source, 'parser, T> {
		ParserDisjunction::new(self)
	}
}

impl<'source, 'parser, T> ParserDisjunction<'source, 'parser, T> {
	fn new(parser: &'parser mut Parser<'source>) -> Self {
		parser.push_checkpoint();
		Self {
			parser,
			result: Err(anyhow::anyhow!("No arms were given to the disjunction")),
		}
	}

	#[inline]
	pub fn finish(self) -> Result<T> {
		self.parser.pop_checkpoint();
		self.result.context("All arms of the disjunction failed")
	}

	#[inline]
	pub fn or_with(mut self, func: impl Fn(&mut Parser) -> Result<T>) -> Self {
		if self.result.is_err() {
			self.parser.rewind_checkpoint();
			let result = func(self.parser);
			self.result = result;
		}
		self
	}
}

#[rustfmt::skip]
impl ParserDisjunction<'_, '_, ()> {
	#[inline] pub fn or_end(self)                             -> Self { self.or_with(|p| p.match_end()                  ) }
	#[inline] pub fn or_token(self, token: &str)              -> Self { self.or_with(|p| p.match_string(token)          ) }
	#[inline] pub fn or_separator(self, separator: Separator) -> Self { self.or_with(|p| p.match_separator(separator)   ) }
	#[inline] pub fn or_horizontal_whitespace(self)           -> Self { self.or_with(|p| p.match_horizontal_whitespace()) }
	#[inline] pub fn or_any_whitespace(self)                  -> Self { self.or_with(|p| p.match_any_whitespace()       ) }
	#[inline] pub fn or_single_linebreak(self)                -> Self { self.or_with(|p| p.match_single_linebreak()     ) }
	#[inline] pub fn or_multiple_linebreaks(self)             -> Self { self.or_with(|p| p.match_multiple_linebreaks()  ) }
}

#[rustfmt::skip]
impl<T: Parsable> ParserDisjunction<'_, '_, T> {
	#[inline] pub fn or_type<U: Parsable + Into<T>>(self) -> Self { self.or_with(|p| p.match_type::<U>().map(Into::into)) }
}

#[rustfmt::skip]
impl ParserDisjunction<'_, '_, String> {
	#[inline] pub fn or_regex(self, regex: &str)   -> Self { self.or_with(|p| p.match_regex(regex)          .map(ToOwned::to_owned)) }
	#[inline] pub fn or_identifier(self)           -> Self { self.or_with(|p| p.match_identifier()          .map(ToOwned::to_owned)) }
	#[inline] pub fn or_lowercase_identifier(self) -> Self { self.or_with(|p| p.match_lowercase_identifier().map(ToOwned::to_owned)) }
	#[inline] pub fn or_uppercase_identifier(self) -> Self { self.or_with(|p| p.match_uppercase_identifier().map(ToOwned::to_owned)) }
	#[inline] pub fn or_integer(self)              -> Self { self.or_with(|p| p.match_integer()             .map(ToOwned::to_owned)) }
	#[inline] pub fn or_signed_integer(self)       -> Self { self.or_with(|p| p.match_signed_integer()      .map(ToOwned::to_owned)) }
}
