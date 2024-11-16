pub trait Successor: Clone {
	fn next(&self) -> Self;

	fn incr(&mut self) -> Self {
		let old = self.clone();
		*self = self.next();
		old
	}
}

pub trait VecLike {
	type Elem;

	fn constructor(value: Vec<Self::Elem>) -> Self;
	fn inner(self) -> Vec<Self::Elem>;
	fn inner_ref(&self) -> &Vec<Self::Elem>;
	fn inner_mut(&mut self) -> &mut Vec<Self::Elem>;

	fn new_empty() -> Self
	where
		Self: Sized,
	{
		Self::constructor(Vec::new())
	}

	fn len(&self) -> usize {
		self.inner_ref().len()
	}

	fn is_empty(&self) -> bool {
		self.inner_ref().is_empty()
	}

	fn push(&mut self, value: Self::Elem) {
		self.inner_mut().push(value)
	}
}

#[macro_export]
macro_rules! vec_like {
	($outer:ty; $elem:ty) => {
		impl VecLike for $outer {
			type Elem = $elem;

			fn constructor(value: Vec<Self::Elem>) -> Self {
				Self(value)
			}

			fn inner(self) -> Vec<Self::Elem> {
				self.0
			}

			fn inner_ref(&self) -> &Vec<Self::Elem> {
				&self.0
			}

			fn inner_mut(&mut self) -> &mut Vec<Self::Elem> {
				&mut self.0
			}
		}
	};
}

// #[macro_export]
// macro_rules! sequence {
// 	($outer:ty{Vec<$inner:ty>}) => {
// 		impl $outer {
// 			pub fn new() -> Self {
// 				Self(Vec::new())
// 			}

// 			pub fn len(&self) -> usize {
// 				self.0.len()
// 			}

// 			pub fn is_empty(&self) -> bool {
// 				self.0.is_empty()
// 			}

// 			pub fn push(&mut self, value: $inner) {
// 				self.0.push(value)
// 			}
// 		}

// 		impl From<Vec<$inner>> for $outer {
// 			fn from(value: Vec<$inner>) -> Self {
// 				Self(value)
// 			}
// 		}

// 		impl Display for $outer {
// 			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
// 				f.pad(
// 					&self
// 						.0
// 						.iter()
// 						.map(|e| format!("{}", e))
// 						.collect::<Vec<_>>()
// 						.join(", "),
// 				)
// 			}
// 		}

// 		impl IntoIterator for $outer {
// 			type Item = $inner;
// 			type IntoIter = <Vec<$inner> as IntoIterator>::IntoIter;

// 			fn into_iter(self) -> Self::IntoIter {
// 				self.0.into_iter()
// 			}
// 		}

// 		impl FromIterator<$inner> for $outer {
// 			fn from_iter<T: IntoIterator<Item = $inner>>(iter: T) -> Self {
// 				Self(Vec::from_iter(iter))
// 			}
// 		}
// 	};
// }
