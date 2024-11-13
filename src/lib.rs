use anyhow::Result;
use parser::Parsable;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

pub mod ast;
pub mod l_zero;
pub mod parser;

// type Substitution = HashMap<Variable, Term>;
type Substitution = ();

pub trait Language {
	type Program;
	type Query;
	type Interpreter: Interpreter;
}

pub trait Interpreter: Sized {
	type Lang: Language;

	fn submit_query(&mut self, query: <Self::Lang as Language>::Query) -> Result<Substitution>;
	fn from_program(program: <Self::Lang as Language>::Program) -> Self;

	fn from_program_source(source: &str) -> Result<Self>
	where
		<Self::Lang as Language>::Program: Parsable,
	{
		let program = <Self::Lang as Language>::Program::parse_from(source)?;
		Ok(Self::from_program(program))
	}

	fn submit_query_source(&mut self, source: &str) -> Result<Substitution>
	where
		<Self::Lang as Language>::Query: Parsable,
	{
		let query = <Self::Lang as Language>::Query::parse_from(source)?;
		self.submit_query(query)?;
		Ok(())
	}
}

pub trait CompilableProgram<L: Language> {
	type Target;
	fn compile_as_program(self) -> Self::Target;
}

pub trait CompilableQuery<L: Language> {
	type Target;
	fn compile_as_query(self) -> Self::Target;
}
