use std::mem;

use anyhow::{bail, Context, Ok, Result};
use regex::Regex;

use crate::static_regex;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod parser_disjunction;
pub mod parser_sequence;
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
		let old_trim = std::mem::take(&mut (self.trim_whitespace, self.trim_newlines));

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
