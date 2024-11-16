pub trait Successor: Clone {
	fn next(&self) -> Self;

	fn incr(&mut self) -> Self {
		let old = self.clone();
		*self = self.next();
		old
	}
}

pub trait Sorted {
	fn sorted(self) -> Self;
}

impl<T: Ord> Sorted for Vec<T> {
	fn sorted(mut self) -> Self {
		self.sort();
		self
	}
}

#[macro_export]
macro_rules! indent {
	($num:expr, $vec:expr) => {
		$vec.split("\n")
			.map(|l| " ".repeat($num) + l)
			.collect::<Vec<_>>()
			.join("\n")
	};
}

#[macro_export]
macro_rules! display_iter {
	($vec:expr) => {
		display_iter!($vec, ", ")
	};

	($vec:expr, $sep:expr) => {
		display_iter!($vec, $sep, "{}")
	};

	($vec:expr, $sep:expr, $fmt:expr) => {
		$vec.iter().map(|e| format!($fmt, e)).collect::<Vec<_>>().join($sep)
	};
}

#[macro_export]
macro_rules! display_map {
	($vec:expr) => {
		display_map!($vec, ", ")
	};

	($vec:expr, $sep:expr) => {
		display_map!($vec, $sep, "{} -> {}")
	};

	($vec:expr, $sep:expr, $fmt:expr) => {{
		$crate::util::Sorted::sorted($vec.iter().map(|(k, v)| format!($fmt, k, v)).collect::<Vec<_>>()).join($sep)
	}};
}
