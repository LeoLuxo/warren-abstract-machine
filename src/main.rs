use anyhow::Result;
use warren_abstract_machine::{l_zero::L0, Interpreter, Language};

fn main() -> Result<()> {
	let mut interpreter = <L0 as Language>::Interpreter::from_program("p(a)".parse()?);
	interpreter.submit_query("p(X)".parse()?)?;

	Ok(())
}
