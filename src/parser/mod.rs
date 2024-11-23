use std::mem;

use anyhow::{bail, ensure, Context, Ok, Result};
use regex::Regex;

use crate::static_regex;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod type_parsers;

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
	checkpoint_stack: Vec<&'source str>,

	pub trim_whitespace: bool,
	pub trim_newlines: bool,
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
			checkpoint_stack: Vec::new(),
			trim_whitespace: true,
			trim_newlines: true,
		}
	}

	pub fn parse<T: Parsable>(source: &'source str) -> Result<T> {
		let mut parser = Self::new(source);
		let result = parser.match_type()?;
		parser.match_end()?;

		Ok(result)
	}

	fn push_checkpoint(&mut self) {
		self.checkpoint_stack.push(self.source);
	}

	fn rewind_checkpoint(&mut self) {
		self.source = self
			.checkpoint_stack
			.last()
			.expect("Tried to rewind to checkpoint, but no checkpoint available")
	}

	fn pop_checkpoint(&mut self) {
		self.checkpoint_stack.pop();
	}

	fn advance_source_by(&mut self, length: usize) {
		self.source = &self.source[length..];
	}

	fn auto_trim(&mut self) {
		match (self.trim_whitespace, self.trim_newlines) {
			(true, true) => {
				self.source = self.source.trim_start();
			}

			(true, false) => {
				self.advance_source_by(static_regex!(^ r"[^\S\r\n]*").find_at(self.source, 0).unwrap().end());
			}

			(false, true) => {
				self.advance_source_by(
					static_regex!(^ r"(?:\r\n|\r|\n)*")
						.find_at(self.source, 0)
						.unwrap()
						.end(),
				);
			}

			_ => {}
		}
	}

	pub fn match_end(&mut self) -> Result<()> {
		self.auto_trim();

		if !self.source.is_empty() {
			bail!("End not matched, found '{}'", self.source)
		}

		Ok(())
	}

	pub fn match_string(&mut self, string: &str) -> Result<()> {
		self.auto_trim();

		if !self.source.starts_with(string) {
			bail!("String '{}' not matched, found '{}'", string, self.source)
		}

		self.advance_source_by(string.len());

		Ok(())
	}

	pub fn match_regex(&mut self, regex: &str) -> Result<&str> {
		self.auto_trim();

		let re_match = static_regex!(^ regex)
			// .is_match_at(&self.source, 0)
			.find_at(self.source, 0)
			.context(format!(
				"Regex pattern '{}' not matched, found '{}'",
				regex, self.source
			))?;

		self.advance_source_by(re_match.end());

		Ok(re_match.as_str())
	}

	pub fn match_regex_captures<const N: usize>(&mut self, regex: &str) -> Result<[&str; N]> {
		self.auto_trim();

		let (full_match, captures) = static_regex!(^ regex)
			.captures_at(self.source, 0)
			.context(format!(
				"Regex pattern '{}' not matched, found '{}'",
				regex, self.source
			))?
			.extract::<{ N }>();

		self.advance_source_by(full_match.len());

		Ok(captures)
	}

	pub fn match_type<T: Parsable>(&mut self) -> Result<T> {
		self.auto_trim();

		<T as Parsable>::parser_match(self)
	}

	pub fn match_horizontal_whitespace(&mut self) -> Result<()> {
		let old_trim = mem::replace(&mut self.trim_whitespace, false);

		let result = self.match_regex(r"[^\S\r\n]+").map(|_| ());

		self.trim_whitespace = old_trim;
		result
	}

	pub fn match_any_whitespace(&mut self) -> Result<()> {
		let old_trim = mem::replace(&mut (self.trim_whitespace, self.trim_newlines), (false, false));

		let result = self.match_regex(r"\s+").map(|_| ());

		(self.trim_whitespace, self.trim_newlines) = old_trim;
		result
	}

	pub fn match_single_linebreak(&mut self) -> Result<()> {
		let old_trim = mem::replace(&mut self.trim_newlines, false);

		let result = self.match_regex(r"(?:\r\n|\r|\n)").map(|_| ());

		self.trim_newlines = old_trim;
		result
	}

	pub fn match_multiple_linebreaks(&mut self) -> Result<()> {
		let old_trim = mem::replace(&mut self.trim_newlines, false);

		let result = self.match_regex(r"(?:\r\n|\r|\n)+").map(|_| ());

		self.trim_newlines = old_trim;
		result
	}

	pub fn match_identifier(&mut self) -> Result<&str> {
		self.match_regex(r"[a-zA-Z_][a-zA-Z0-9_]*")
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

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum Separator<'a> {
	#[default]
	Whitespace,
	AnyWhitespace,
	SingleLinebreak,
	MultipleLinebreaks,
	String(&'a str),
}

impl<'source> Parser<'source> {
	pub fn match_separator(&mut self, separator: Separator) -> Result<()> {
		match separator {
			Separator::Whitespace => self.match_horizontal_whitespace(),

			Separator::AnyWhitespace => self.match_any_whitespace(),

			Separator::SingleLinebreak => self.match_single_linebreak(),

			Separator::MultipleLinebreaks => self.match_multiple_linebreaks(),

			Separator::String(str) => self.match_string(str),
		}
	}

	pub fn match_sequence_by_type<T: Parsable>(
		&mut self,
		separator: Separator,
		minimum: Option<usize>,
	) -> Result<Vec<T>> {
		self.match_sequence_with(separator, minimum, |p| p.match_type::<T>())
	}

	pub fn match_sequence_with<T>(
		&mut self,
		separator: Separator,
		minimum: Option<usize>,
		inner_fn: impl Fn(&mut Self) -> Result<T>,
	) -> Result<Vec<T>> {
		self.push_checkpoint();

		// Workaround for try{} blocks until they arrive in stable rust
		let result = (|| {
			let mut sequence = Vec::new();

			loop {
				// Match the inner element
				let element = inner_fn(self);

				if let Result::Ok(e) = element {
					sequence.push(e);
				} else {
					break;
				}

				// Match the separator
				if self.match_separator(separator).is_err() {
					// If the separator didn't match, break peacefully (ie. we're done finding elements but we get to keep the ones we have)
					break;
				}
			}

			if let Some(min) = minimum {
				ensure!(
					sequence.len() >= min,
					"Matched only {} element(s) in the sequence, expected at least {}",
					sequence.len(),
					min
				);
			}

			Ok(sequence)
		})();

		if result.is_err() {
			// If an error occurs while matching the sequence, rollback the source string
			self.rewind_checkpoint();
		}
		self.pop_checkpoint();

		// Restore settings

		result
	}
}

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

impl ParserDisjunction<'_, '_, ()> {
	#[inline]
	pub fn or_end(self) -> Self {
		self.or_with(|p| p.match_end())
	}

	#[inline]
	pub fn or_token(self, token: &str) -> Self {
		self.or_with(|p| p.match_string(token))
	}
}

impl<T: Parsable> ParserDisjunction<'_, '_, T> {
	#[inline]
	pub fn or_type<U: Parsable + Into<T>>(self) -> Self {
		self.or_with(|p| p.match_type::<U>().map(Into::into))
	}
}

impl ParserDisjunction<'_, '_, String> {
	#[inline]
	pub fn or_regex(self, regex: &str) -> Self {
		self.or_with(|p| p.match_regex(regex).map(ToOwned::to_owned))
	}

	#[inline]
	pub fn or_identifier(self) -> Self {
		self.or_with(|p| p.match_identifier().map(ToOwned::to_owned))
	}

	#[inline]
	pub fn or_lowercase_identifier(self) -> Self {
		self.or_with(|p| p.match_lowercase_identifier().map(ToOwned::to_owned))
	}

	#[inline]
	pub fn or_uppercase_identifier(self) -> Self {
		self.or_with(|p| p.match_uppercase_identifier().map(ToOwned::to_owned))
	}

	#[inline]
	pub fn or_integer(self) -> Self {
		self.or_with(|p| p.match_integer().map(ToOwned::to_owned))
	}

	#[inline]
	pub fn or_signed_integer(self) -> Self {
		self.or_with(|p| p.match_signed_integer().map(ToOwned::to_owned))
	}
}
