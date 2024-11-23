use anyhow::{bail, ensure, Context, Ok, Result};
use regex::Regex;

use crate::{regex_force_beginning, static_regex};

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
	checkpoints: Vec<&'source str>,
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

	fn push_checkpoint(&mut self) {
		self.checkpoints.push(self.source);
	}

	fn rewind_checkpoint(&mut self) {
		self.source = self
			.checkpoints
			.last()
			.expect("Tried to rewind to checkpoint, but no checkpoint available")
	}

	fn pop_checkpoint(&mut self) {
		self.checkpoints.pop();
	}

	fn trim_offset(&mut self, length: usize) {
		self.source = &self.source[length..];
	}

	fn trim_whitespace(&mut self) {
		self.source = self.source.trim_start()
	}

	pub fn match_end(&mut self) -> Result<()> {
		self.trim_whitespace();

		if !self.source.is_empty() {
			bail!("End not matched, found '{}'", self.source)
		}

		Ok(())
	}

	pub fn match_token(&mut self, token: &str) -> Result<()> {
		self.trim_whitespace();

		if !self.source.starts_with(token) {
			bail!("Token '{}' not matched, found '{}'", token, self.source)
		}

		self.trim_offset(token.len());

		Ok(())
	}

	pub fn match_regex(&mut self, regex: &str) -> Result<&str> {
		self.trim_whitespace();

		let re_match = static_regex!(regex_force_beginning!(regex))
			// .is_match_at(&self.source, 0)
			.find_at(self.source, 0)
			.context(format!(
				"Regex pattern '{}' not matched, found '{}'",
				regex, self.source
			))?;

		self.trim_offset(re_match.end());

		Ok(re_match.as_str())
	}

	pub fn match_regex_captures<const N: usize>(&mut self, regex: &str) -> Result<[&str; N]> {
		self.trim_whitespace();

		let (full_match, captures) = static_regex!(regex_force_beginning!(regex))
			.captures_at(self.source, 0)
			.context(format!(
				"Regex pattern '{}' not matched, found '{}'",
				regex, self.source
			))?
			.extract::<{ N }>();

		self.trim_offset(full_match.len());

		Ok(captures)
	}

	pub fn match_type<T: Parsable>(&mut self) -> Result<T> {
		self.trim_whitespace();

		<T as Parsable>::parser_match(self)
	}

	pub fn match_sequence_by_type<T: Parsable>(&mut self, separator: &str, minimum: Option<usize>) -> Result<Vec<T>> {
		self.match_sequence_with(separator, minimum, |p| p.match_type::<T>())
	}

	pub fn match_sequence_with<T>(
		&mut self,
		separator: &str,
		minimum: Option<usize>,
		inner_fn: impl Fn(&mut Self) -> Result<T>,
	) -> Result<Vec<T>> {
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

			if let Result::Ok(r) = inner {
				result.push(r);
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
	pub fn end(self) -> Result<T> {
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
		self.or_with(|p| p.match_token(token))
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
