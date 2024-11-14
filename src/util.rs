

#[macro_export]
macro_rules! newtype {
	// ($outer:ty: $inner:tt}: Display) => {
	// 	newtype!($outer{$inner});

	// 	impl std::fmt::Display for $outer {
	// 		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	// 			std::fmt::Debug::fmt(&self.0, f)
	// 		}
	// 	}
	// };

	($outer:ty | $inner:ty | Display) => {
		newtype!($outer: $inner);

		impl std::fmt::Display for $outer {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				std::fmt::Debug::fmt(&self.0, f)
			}
		}
	};

	($outer:ty: Vec<$elem:ty>) => {
		newtype!(end $outer{Vec<$elem>});

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



	($outer:ty: String) => {
		newtype!(end $outer{String});

		impl From<&str> for $outer {
			fn from(value: &str) -> Self {
				Self(value.to_string())
			}
		}
	};



	($outer:ty: u8) => {
		newtype!($outer{uint u8});
	};

	($outer:ty: u16) => {
		newtype!($outer{uint u16});
	};

	($outer:ty: u32) => {
		newtype!($outer{uint u32});
	};

	($outer:ty: u64) => {
		newtype!($outer{uint u64});
	};

	($outer:ty: usize) => {
		newtype!($outer{uint usize});
	};

	($outer:ty: i8) => {
		newtype!($outer{sint i8});
	};

	($outer:ty: i16) => {
		newtype!($outer{sint i16});
	};

	($outer:ty: i32) => {
		newtype!($outer{sint i32});
	};

	($outer:ty: i64) => {
		newtype!($outer{sint i64});
	};

	($outer:ty: isize) => {
		newtype!($outer{sint isize});
	};



	($outer:ty: uint $i:ty) => {
		newtype!($outer{int $i});
	};

	($outer:ty: sint $i:ty) => {
		newtype!($outer{int $i});
	};



	($outer:ty: int $i:ty) => {
		newtype!(end $outer{$i});

		#[rustfmt::skip] impl PartialEq<$i> for $outer { fn eq(&self, other: &$i)     -> bool { self.0 == *other } }
		#[rustfmt::skip] impl PartialEq<$outer> for $i { fn eq(&self, other: &$outer) -> bool { other.0 == *self } }

		#[rustfmt::skip] impl std::ops::Add    for $outer {type Output = Self; fn add    (self, rhs: Self) -> Self::Output {Self(self.0 + rhs.0)}}
		#[rustfmt::skip] impl std::ops::Sub    for $outer {type Output = Self; fn sub    (self, rhs: Self) -> Self::Output {Self(self.0 - rhs.0)}}
		#[rustfmt::skip] impl std::ops::BitAnd for $outer {type Output = Self; fn bitand (self, rhs: Self) -> Self::Output {Self(self.0 & rhs.0)}}
		#[rustfmt::skip] impl std::ops::BitOr  for $outer {type Output = Self; fn bitor  (self, rhs: Self) -> Self::Output {Self(self.0 | rhs.0)}}
		#[rustfmt::skip] impl std::ops::BitXor for $outer {type Output = Self; fn bitxor (self, rhs: Self) -> Self::Output {Self(self.0 ^ rhs.0)}}
		#[rustfmt::skip] impl std::ops::Mul    for $outer {type Output = Self; fn mul    (self, rhs: Self) -> Self::Output {Self(self.0 * rhs.0)}}
		#[rustfmt::skip] impl std::ops::Div    for $outer {type Output = Self; fn div    (self, rhs: Self) -> Self::Output {Self(self.0 / rhs.0)}}
		#[rustfmt::skip] impl std::ops::Rem    for $outer {type Output = Self; fn rem    (self, rhs: Self) -> Self::Output {Self(self.0 % rhs.0)}}
		#[rustfmt::skip] impl std::ops::Shl    for $outer {type Output = Self; fn shl    (self, rhs: Self) -> Self::Output {Self(self.0 << rhs.0)}}
		#[rustfmt::skip] impl std::ops::Shr    for $outer {type Output = Self; fn shr    (self, rhs: Self) -> Self::Output {Self(self.0 >> rhs.0)}}

		#[rustfmt::skip] impl std::ops::Add<$i>    for $outer {type Output = Self; fn add    (self, rhs: $i) -> Self::Output {Self(self.0 + rhs)}}
		#[rustfmt::skip] impl std::ops::Sub<$i>    for $outer {type Output = Self; fn sub    (self, rhs: $i) -> Self::Output {Self(self.0 - rhs)}}
		#[rustfmt::skip] impl std::ops::BitAnd<$i> for $outer {type Output = Self; fn bitand (self, rhs: $i) -> Self::Output {Self(self.0 & rhs)}}
		#[rustfmt::skip] impl std::ops::BitOr<$i>  for $outer {type Output = Self; fn bitor  (self, rhs: $i) -> Self::Output {Self(self.0 | rhs)}}
		#[rustfmt::skip] impl std::ops::BitXor<$i> for $outer {type Output = Self; fn bitxor (self, rhs: $i) -> Self::Output {Self(self.0 ^ rhs)}}
		#[rustfmt::skip] impl std::ops::Mul<$i>    for $outer {type Output = Self; fn mul    (self, rhs: $i) -> Self::Output {Self(self.0 * rhs)}}
		#[rustfmt::skip] impl std::ops::Div<$i>    for $outer {type Output = Self; fn div    (self, rhs: $i) -> Self::Output {Self(self.0 / rhs)}}
		#[rustfmt::skip] impl std::ops::Rem<$i>    for $outer {type Output = Self; fn rem    (self, rhs: $i) -> Self::Output {Self(self.0 % rhs)}}
		#[rustfmt::skip] impl std::ops::Shl<$i>    for $outer {type Output = Self; fn shl    (self, rhs: $i) -> Self::Output {Self(self.0 << rhs)}}
		#[rustfmt::skip] impl std::ops::Shr<$i>    for $outer {type Output = Self; fn shr    (self, rhs: $i) -> Self::Output {Self(self.0 >> rhs)}}

		#[rustfmt::skip] impl std::ops::AddAssign    for $outer {fn add_assign    (&mut self, rhs: Self) {self.0 += rhs.0}}
		#[rustfmt::skip] impl std::ops::SubAssign    for $outer {fn sub_assign    (&mut self, rhs: Self) {self.0 -= rhs.0}}
		#[rustfmt::skip] impl std::ops::BitAndAssign for $outer {fn bitand_assign (&mut self, rhs: Self) {self.0 &= rhs.0}}
		#[rustfmt::skip] impl std::ops::BitOrAssign  for $outer {fn bitor_assign  (&mut self, rhs: Self) {self.0 |= rhs.0}}
		#[rustfmt::skip] impl std::ops::BitXorAssign for $outer {fn bitxor_assign (&mut self, rhs: Self) {self.0 ^= rhs.0}}
		#[rustfmt::skip] impl std::ops::MulAssign    for $outer {fn mul_assign    (&mut self, rhs: Self) {self.0 *= rhs.0}}
		#[rustfmt::skip] impl std::ops::DivAssign    for $outer {fn div_assign    (&mut self, rhs: Self) {self.0 /= rhs.0}}
		#[rustfmt::skip] impl std::ops::RemAssign    for $outer {fn rem_assign    (&mut self, rhs: Self) {self.0 %= rhs.0}}
		#[rustfmt::skip] impl std::ops::ShlAssign    for $outer {fn shl_assign    (&mut self, rhs: Self) {self.0 <<= rhs.0}}
		#[rustfmt::skip] impl std::ops::ShrAssign    for $outer {fn shr_assign    (&mut self, rhs: Self) {self.0 >>= rhs.0}}

		#[rustfmt::skip] impl std::ops::AddAssign<$i>    for $outer {fn add_assign    (&mut self, rhs: $i) {self.0 += rhs}}
		#[rustfmt::skip] impl std::ops::SubAssign<$i>    for $outer {fn sub_assign    (&mut self, rhs: $i) {self.0 -= rhs}}
		#[rustfmt::skip] impl std::ops::BitAndAssign<$i> for $outer {fn bitand_assign (&mut self, rhs: $i) {self.0 &= rhs}}
		#[rustfmt::skip] impl std::ops::BitOrAssign<$i>  for $outer {fn bitor_assign  (&mut self, rhs: $i) {self.0 |= rhs}}
		#[rustfmt::skip] impl std::ops::BitXorAssign<$i> for $outer {fn bitxor_assign (&mut self, rhs: $i) {self.0 ^= rhs}}
		#[rustfmt::skip] impl std::ops::MulAssign<$i>    for $outer {fn mul_assign    (&mut self, rhs: $i) {self.0 *= rhs}}
		#[rustfmt::skip] impl std::ops::DivAssign<$i>    for $outer {fn div_assign    (&mut self, rhs: $i) {self.0 /= rhs}}
		#[rustfmt::skip] impl std::ops::RemAssign<$i>    for $outer {fn rem_assign    (&mut self, rhs: $i) {self.0 %= rhs}}
		#[rustfmt::skip] impl std::ops::ShlAssign<$i>    for $outer {fn shl_assign    (&mut self, rhs: $i) {self.0 <<= rhs}}
		#[rustfmt::skip] impl std::ops::ShrAssign<$i>    for $outer {fn shr_assign    (&mut self, rhs: $i) {self.0 >>= rhs}}
	};



	($outer:ty: $inner:ty) => {
		newtype!(end $outer: $inner);
	};



	(end $outer:ty: $inner:ty) => {
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
