use std::fmt::Debug;

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
	None,
	Whitespace,
	AnyWhitespace,
	SingleLinebreak,
	Linebreaks,
	String(&'a str),
}

impl Parser<'_> {
	#[rustfmt::skip]
	pub fn match_separator(&mut self, separator: Separator) -> Result<()> {
		match separator {
			Separator::None            => Ok(()),
			Separator::Whitespace      => self.match_horizontal_whitespace(),
			Separator::AnyWhitespace   => self.match_any_whitespace(),
			Separator::SingleLinebreak => self.match_single_linebreak(),
			Separator::Linebreaks      => self.match_multiple_linebreaks(),
			Separator::String(str)     => self.match_string(str),
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

				// If we didn't match, then that means we're hitting things outside the sequence (potentially a closed paren or other stuff), break peacefully.
				// We don't want to explicitly rewind the sequence, we're assuming the inner element rewound as needed in case of an error.
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
			// If any error occurs while matching the sequence (ie the WHOLE sequence is invalid), rollback the ENTIRE source string of the sequence
			self.rewind_checkpoint();
		}
		self.pop_checkpoint();

		result
	}
}
