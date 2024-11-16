pub trait Successor: Clone {
	fn next(&self) -> Self;

	fn incr(&mut self) -> Self {
		let old = self.clone();
		*self = self.next();
		old
	}
}

#[macro_export]
macro_rules! sequence {
	($outer:ty{Vec<$inner:ty>}) => {
		impl $outer {
			pub fn new() -> Self {
				Self(Vec::new())
			}

			pub fn len(&self) -> usize {
				self.0.len()
			}

			pub fn is_empty(&self) -> bool {
				self.0.is_empty()
			}

			pub fn push(&mut self, value: $inner) {
				self.0.push(value)
			}
		}

		impl From<Vec<$inner>> for $outer {
			fn from(value: Vec<$inner>) -> Self {
				Self(value)
			}
		}

		impl Display for $outer {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				f.pad(
					&self
						.0
						.iter()
						.map(|e| format!("{}", e))
						.collect::<Vec<_>>()
						.join(", "),
				)
			}
		}

		impl IntoIterator for $outer {
			type Item = $inner;
			type IntoIter = <Vec<$inner> as IntoIterator>::IntoIter;

			fn into_iter(self) -> Self::IntoIter {
				self.0.into_iter()
			}
		}

		impl FromIterator<$inner> for $outer {
			fn from_iter<T: IntoIterator<Item = $inner>>(iter: T) -> Self {
				Self(Vec::from_iter(iter))
			}
		}
	};
}
