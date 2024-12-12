//! This project implements the Warren Astract Machine as described in the "wambook";
//! "Warren’s Abstract Machine - A Tutorial Reconstruction" by Hassan Aït-Kaci
//!
//! This is the real root of the project.

use anyhow::Result;
use substitution::Substitution;
use universal_compiler::{CompilableProgram, CompilableQuery};
use util::Successor;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

// Visit each of these modules for a documentation of their purpose
pub mod anonymous;
pub mod ast;
pub mod machine_types;
pub mod parser;
pub mod substitution;
pub mod universal_compiler;
pub mod util;

// The two languages currently implemented.
// Each of these modules provides an implementation for:
// - the language abstraction
// - the warren machine
// - the compiler
// for that language.
pub mod l_one;
pub mod l_zero;

/// Solves a given program-query pair for the specified language.
///
/// Uses the right interpreter automatically, and returns the substitution set.
pub fn solve<L: Language>(program: L::Program, query: L::Query) -> Result<Substitution> {
	L::Interpreter::solve(program, query)
}

/// Describes a language (ie. L0, L1, L2, L3, Prolog).
///
/// Very useful to generalize code for all languages and
/// properly type-check certain required features
/// (for example that the types of both programs and query must be compilable).
pub trait Language: Sized {
	/// The (AST-) type the language takes as a program.
	type Program: CompilableProgram<Self>;

	/// The (AST-) type the language takes as a query.
	type Query: CompilableQuery<Self>;

	/// The instruction set the warren machine of the language uses.
	type InstructionSet;

	/// The interpreter for the language. Is used to infer how to directly compute the solution for a program-query pair of a given language.
	type Interpreter: Interpreter<Self>;
}

/// Describes an interpreter for a specified language.
///
/// A interpreter is tasked to manage:
/// - compilation,
/// - machine execution,
/// - and substitution extraction
///
/// and provides a nicer interface to the end-user.
/// All it requires is a base program, and then arbitrary queries can be submitted.
pub trait Interpreter<L: Language>: Sized {
	fn from_program(program: L::Program) -> Result<Self>;
	fn submit_query(&mut self, query: L::Query) -> Result<Substitution>;

	fn solve(program: L::Program, query: L::Query) -> Result<Substitution> {
		Self::from_program(program)?.submit_query(query)
	}
}
