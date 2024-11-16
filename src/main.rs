use anyhow::Result;
use warren_abstract_machine::{l_zero::L0, solve_single};

fn main() -> Result<()> {
	solve_single::<L0>("p(a)".parse()?, "p(X)".parse()?)?;
	solve_single::<L0>("p(f(X), h(Y, f(a)), Y)".parse()?, "p(Z, h(Z,W), f(W))".parse()?)?;

	Ok(())
}
