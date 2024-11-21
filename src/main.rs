use anyhow::Result;
use warren_abstract_machine::{l_one::Facts, l_zero::L0, solve, universal_compiler::CompilableProgram};

fn main() -> Result<()> {
	// solve_single::<L0>("p(a)".parse()?, "p(X)".parse()?)?;
	// solve_single::<L0>("p(X, Y, X)".parse()?, "p(X, Y, Z)".parse()?)?;
	// solve::<L0>("p(Y, Y, Z)".parse()?, "p(X, Y, Z)".parse()?)?;
	// solve_single::<L0>("p(f(X), h(Y, f(a)), Y)".parse()?, "p(Z, h(Z,W), f(W))".parse()?)?;
	// solve_single::<L0>("p(f(X), h(Y, f(Z)), Y)".parse()?, "p(Z, h(Z,W), f(W))".parse()?)?;

	println!("{}", "p(f(X), h(Y, f(a)), Y).".parse::<Facts>()?.compile_as_program()?);

	Ok(())
}
