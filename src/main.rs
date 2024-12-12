//! The main starting point of the machine.
//!
//! As the project doesn't have a true end-user interface, this file is mainly used to
//! quickly test some small functionality; see the commented-out fragments of code below.
//!
//! The real root of the project is defined in [lib.rs].
//! The integration tests are in the [../tests] folder.

use anyhow::Result;
use warren_abstract_machine::{l_one::L1, parser::ParseAs, solve};

fn main() -> Result<()> {
	// solve::<L0>("p(a)".parse_as()?, "p(X)".parse_as()?)?;
	// solve::<L0>("p(X, Y, X)".parse_as()?, "p(X, Y, Z)".parse_as()?)?;
	// solve::<L0>("p(Y, Y, Z)".parse_as()?, "p(X, Y, Z)".parse_as()?)?;
	// solve::<L0>("p(X, Y, Y)".parse_as()?, "p(X, Y, Z)".parse_as()?)?;
	// solve::<L0>("p(f(X), h(Y, f(a)), Y)".parse_as()?, "p(Z, h(Z,W), f(W))".parse_as()?)?;
	// solve::<L0>("p(f(X), h(Y, f(Z)), Y)".parse_as()?, "p(Z, h(Z,W), f(W))".parse_as()?)?;

	// println!("{}", "p(f(X), h(Y, f(a)), Y).".parse::<Facts>()?.compile_as_program()?);

	// println!("{}", "const".parse_as::<Term>()?);

	solve::<L1>("p(X, Y, Y). f(1).".parse_as()?, "f(X).".parse_as()?)?;

	Ok(())
}
