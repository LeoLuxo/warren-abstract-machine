pub trait Successor: Clone {
	fn next(&self) -> Self;

	fn post_incr(&mut self) -> Self {
		let old = self.clone();
		*self = self.next();
		old
	}

	fn pre_incr(&mut self) -> Self {
		*self = self.next();
		self.clone()
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

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[macro_export]
macro_rules! static_regex {
	(^ $regex:expr) => {{
		// Forces the regex to match the beginning of the string, more convenient than putting this pattern in all my regexes
		static_regex!(&format!(r"\A(?:{})", $regex))
	}};

	($regex:expr) => {{
		// This would be even shorter using LazyLock, but the feature is still unstable
		const RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
		RE.get_or_init(|| Regex::new($regex).unwrap())
	}};
}

#[macro_export]
macro_rules! indent {
	($str:expr) => {
		indent!($str, 2)
	};

	($str:expr, $num:expr) => {
		$str.split("\n")
			.map(|l| " ".repeat($num) + l)
			.collect::<Vec<_>>()
			.join("\n")
	};
}

#[macro_export]
macro_rules! enumerate {
	($str:expr) => {
		enumerate!($str, "{}. {}")
	};

	($str:expr, $fmt:expr) => {
		$str.split("\n")
			.enumerate()
			.map(|(i, l)| format!($fmt, i, l))
			.collect::<Vec<_>>()
			.join("\n")
	};
}

#[macro_export]
macro_rules! display_iter {
	($target:expr) => {
		display_iter!($target, ", ")
	};

	($target:expr, $sep:expr) => {
		display_iter!($target, $sep, "{}")
	};

	($target:expr, $sep:expr, $fmt:expr) => {
		$target.iter().map(|e| format!($fmt, e)).collect::<Vec<_>>().join($sep)
	};
}

#[macro_export]
macro_rules! display_map {
	($target:expr) => {
		display_map!($target, ", ")
	};

	($target:expr, $sep:expr) => {
		display_map!($target, $sep, "{} -> {}")
	};

	($target:expr, $sep:expr, $fmt:expr) => {{
		let mut v = $target.iter().collect::<Vec<_>>();
		v.sort_by(|(a, _), (b, _)| a.cmp(b));
		v.into_iter()
			.map(|(k, v)| format!($fmt, k, v))
			.collect::<Vec<_>>()
			.join($sep)
	}};
}
