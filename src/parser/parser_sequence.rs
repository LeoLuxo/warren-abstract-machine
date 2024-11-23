use anyhow::{ensure, Ok, Result};

use super::{Parsable, Parser};

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
	Linebreaks,
	String(&'a str),
}

impl<'source> Parser<'source> {
	pub fn match_separator(&mut self, separator: Separator) -> Result<()> {
		match separator {
			Separator::Whitespace => self.match_horizontal_whitespace(),

			Separator::AnyWhitespace => self.match_any_whitespace(),

			Separator::SingleLinebreak => self.match_single_linebreak(),

			Separator::Linebreaks => self.match_multiple_linebreaks(),

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
