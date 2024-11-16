pub trait Successor: Clone {
	fn next(&self) -> Self;

	fn incr(&mut self) -> Self {
		let old = self.clone();
		*self = self.next();
		old
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
	($vec:expr, $sep:expr) => {
		$vec.iter().map(|e| format!("{e}")).collect::<Vec<_>>().join($sep)
	};
}
