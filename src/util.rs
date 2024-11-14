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

pub trait NewType {
	type Inner;

	fn constructor(inner: Self::Inner) -> Self;
	fn inner(self) -> Self::Inner;
	fn inner_ref(&self) -> &Self::Inner;
	fn inner_mut(&mut self) -> &mut Self::Inner;
}

impl NewType for OuterType {
	type Inner = Vec<Innertype>;

	#[inline]
	fn constructor(inner: Self::Inner) -> Self {
		Self(inner)
	}

	#[inline]
	fn inner(self) -> Self::Inner {
		self.0
	}

	#[inline]
	fn inner_ref(&self) -> &Self::Inner {
		&self.0
	}

	#[inline]
	fn inner_mut(&mut self) -> &mut Self::Inner {
		&mut self.0
	}
}

pub trait NewTypeVec<E>: NewType<Inner = Vec<E>> + Sized {
	fn new() -> Self {
		Self::constructor(Vec::new())
	}

	delegate::delegate! {
		to self.inner_ref() {
			fn len(&self) -> usize;
			fn is_empty(&self) -> bool;
		}

		to self.inner_mut() {
			fn push(&mut self, value: E);
			fn pop(&mut self) -> Option<E>;
		}
	}
}

impl<T, E> NewTypeVec<E> for T where T: NewType<Inner = Vec<E>> + Sized {}

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
	};

	($outer:ty{String}) => {};

	($outer:ty{$inner:ty}) => {
		newtype!(generalized $outer{$inner});
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
