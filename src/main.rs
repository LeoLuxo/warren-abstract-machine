use anyhow::Result;
use warren_abstract_machine::{ast::Term, l_zero::L0, parser::ParseAs, solve};

fn main() -> Result<()> {
	// solve_single::<L0>("p(a)".parse_as()?, "p(X)".parse_as()?)?;
	// solve_single::<L0>("p(X, Y, X)".parse_as()?, "p(X, Y, Z)".parse_as()?)?;
	solve::<L0>("p(Y, Y, Z)".parse_as()?, "p(X, Y, Z)".parse_as()?)?;
	// solve_single::<L0>("p(f(X), h(Y, f(a)), Y)".parse_as()?, "p(Z, h(Z,W), f(W))".parse_as()?)?;
	// solve_single::<L0>("p(f(X), h(Y, f(Z)), Y)".parse_as()?, "p(Z, h(Z,W), f(W))".parse_as()?)?;

	// println!("{}", "p(f(X), h(Y, f(a)), Y).".parse::<Facts>()?.compile_as_program()?);

	// println!("{}", "const".parse_as::<Term>()?);

	Ok(())
}
