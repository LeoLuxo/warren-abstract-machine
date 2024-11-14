pub trait Successor: Clone {
	fn next(&self) -> Self;

	fn incr(&mut self) -> Self {
		let old = self.clone();
		*self = self.next();
		old
	}
}

struct Innertype;
struct OuterType(Vec<Innertype>);

#[macro_export]
macro_rules! newtype {
	($outer:ty{Vec<$elem:ty>}) => {
		newtype!(generalized $outer{Vec<$elem>});

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

			pub fn push(&mut self, value: $elem) {
				self.0.push(value)
			}

			pub fn pop(&mut self) -> Option<$elem> {
				self.0.pop()
			}

			pub fn get<I>(&self, index: I) -> Option<&I::Output>
			where
				I: std::slice::SliceIndex<[$elem]>,
			{
				self.0.get(index)
			}

			pub fn get_mut<I>(&mut self, index: I) -> Option<&mut I::Output>
			where
				I: std::slice::SliceIndex<[$elem]>,
			{
				self.0.get_mut(index)
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
			type Item = $elem;
			type IntoIter = <Vec<$elem> as IntoIterator>::IntoIter;

			fn into_iter(self) -> Self::IntoIter {
				self.0.into_iter()
			}
		}

		impl FromIterator<$elem> for $outer {
			fn from_iter<T: IntoIterator<Item = $elem>>(iter: T) -> Self {
				Self(Vec::from_iter(iter))
			}
		}

		impl<I: std::slice::SliceIndex<[$elem]>> std::ops::Index<I> for $outer {
			type Output = I::Output;

			#[inline]
			fn index(&self, index: I) -> &Self::Output {
				std::ops::Index::index(&self.0, index)
			}
		}

		impl<I: std::slice::SliceIndex<[$elem]>> std::ops::IndexMut<I> for $outer {
			#[inline]
			fn index_mut(&mut self, index: I) -> &mut Self::Output {
				std::ops::IndexMut::index_mut(&mut self.0, index)
			}
		}
	};



	($outer:ty{String}) => {
		newtype!(generalized+display $outer{String});

		impl From<&str> for $outer {
			fn from(value: &str) -> Self {
				Self(value.to_string())
			}
		}
	};



	($outer:ty{$inner:ty}) => {
		newtype!(generalized $outer{$inner});
	};



	(generalized+display $outer:ty{$inner:ty}) => {
		newtype!(generalized $outer{$inner});

		impl Display for $outer {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				Display::fmt(&self.0, f)
			}
		}
	};



	(generalized $outer:ty{$inner:ty}) => {
		impl From<$inner> for $outer {
			fn from(value: $inner) -> Self {
				Self(value)
			}
		}

		impl From<$outer> for $inner {
			fn from(value: $outer) -> Self {
				value.0
			}
		}
	};
}
