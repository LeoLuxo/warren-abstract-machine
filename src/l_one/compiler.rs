use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::Context;
use anyhow::Result;
use velcro::hash_map;

use crate::ast::Fact;
use crate::ast::GetFunctor;
use crate::substitution::{VarToRegMapping, VariableContext};
use crate::universal_compiler;
use crate::universal_compiler::Combinable;
use crate::universal_compiler::CompilableProgram;
use crate::universal_compiler::CompilableQuery;
use crate::universal_compiler::Compiled;
use crate::universal_compiler::FlatteningOrder;
use crate::universal_compiler::MappingToken;

use super::Facts;
use super::L1Instruction;
use super::L1;

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

impl CompilableProgram<L1> for Facts {
	fn compile_as_program(self) -> Result<Compiled<L1>> {
		self.into_iter()
			.map(|fact| fact.compile_as_program())
			.collect::<Result<Vec<_>>>()?
			.into_iter()
			.reduce(Combinable::combined)
			.context("empty set of facts cannot be compiled")
	}
}

impl CompilableProgram<L1> for Fact {
	fn compile_as_program(self) -> Result<Compiled<L1>> {
		let fact_label = self.get_functor().as_identifier();

		let order = FlatteningOrder::for_program();
		let context = VariableContext::Program(fact_label.clone());
		let (tokens, var_mapping) = flatten_fact(self, order, context);

		let mut instructions = compile_program_tokens(tokens);
		instructions.push(L1Instruction::Proceed);

		Ok(Compiled {
			instructions,
			var_reg_mapping: Some(var_mapping),
			labels: hash_map![fact_label: 0.into()],
		})
	}
}

impl CompilableQuery<L1> for Fact {
	fn compile_as_query(self) -> Result<Compiled<L1>> {
		let fact_label = self.get_functor().as_identifier();

		let order = FlatteningOrder::for_query();
		let context = VariableContext::Query;
		let (tokens, var_mapping) = flatten_fact(self, order, context);

		let mut instructions = compile_query_tokens(tokens);
		instructions.push(L1Instruction::Call(fact_label));

		Ok(Compiled {
			instructions,
			var_reg_mapping: Some(var_mapping),
			..Default::default()
		})
	}
}

fn flatten_fact(fact: Fact, order: FlatteningOrder, context: VariableContext) -> (Vec<MappingToken>, VarToRegMapping) {
	let mut tokens = Vec::new();
	let mut mapping = HashMap::new();
	let mut reserved_registers = fact.terms.len().into();

	// TODO: order of tokens is wrong

	for (i, term) in fact.0.terms.into_iter().enumerate() {
		tokens.extend(universal_compiler::flatten_term(
			(i + 1).into(),
			term,
			&mut mapping,
			&mut reserved_registers,
			order,
		));
	}

	let var_reg_mapping = VarToRegMapping::from_hashmap_with_context(mapping, context);

	(tokens, var_reg_mapping)
}

fn compile_program_tokens(tokens: Vec<MappingToken>) -> Vec<L1Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for token in tokens {
		#[rustfmt::skip]
		let (reg, inst) = match token {
			MappingToken::Functor(reg, functor)                                 => (reg, L1Instruction::GetStructure(functor, reg)),
			MappingToken::VarRegister(reg) if encountered.contains(&reg)        => (reg, L1Instruction::UnifyValue(reg)),
			MappingToken::VarRegister(reg)                                      => (reg, L1Instruction::UnifyVariable(reg)),
			MappingToken::ArgumentRegister(xn, ai) if encountered.contains(&xn) => (xn,  L1Instruction::GetValue(xn, ai)),
			MappingToken::ArgumentRegister(xn, ai)                              => (xn,  L1Instruction::GetVariable(xn, ai)),
		};

		encountered.insert(reg);
		instructions.push(inst);
	}

	instructions
}

fn compile_query_tokens(tokens: Vec<MappingToken>) -> Vec<L1Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for token in tokens {
		#[rustfmt::skip]
		let (reg, inst) = match token {
			MappingToken::Functor(reg, functor)                                 => (reg, L1Instruction::PutStructure(functor, reg)),
			MappingToken::VarRegister(reg) if encountered.contains(&reg)        => (reg, L1Instruction::SetValue(reg)),
			MappingToken::VarRegister(reg)                                      => (reg, L1Instruction::SetVariable(reg)),
			MappingToken::ArgumentRegister(xn, ai) if encountered.contains(&xn) => (xn,  L1Instruction::PutValue(xn, ai)),
			MappingToken::ArgumentRegister(xn, ai)                              => (xn,  L1Instruction::PutVariable(xn, ai)),
		};

		encountered.insert(reg);
		instructions.push(inst);
	}

	instructions
}

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[cfg(test)]
mod tests {

	use crate::parser::ParseAs;

	use super::*;
	use anyhow::Result;

	#[test]
	fn test_compile_program() -> Result<()> {
		// The wambook has the last getvalue X5,A3 earlier (before get_structure f/1);
		// I conjecture that this variation doesn't change the validity of the instruction sequence
		assert_eq!(
			"p(f(X), h(Y, f(a)), Y)."
				.parse_as::<Facts>()?
				.compile_as_program()?
				.instructions,
			vec![
				L1Instruction::GetStructure("f/1".parse_as()?, "A1".parse_as()?),
				L1Instruction::UnifyVariable("X4".parse_as()?),
				L1Instruction::GetStructure("h/2".parse_as()?, "A2".parse_as()?),
				L1Instruction::UnifyVariable("X5".parse_as()?),
				L1Instruction::UnifyVariable("X6".parse_as()?),
				L1Instruction::GetStructure("f/1".parse_as()?, "X6".parse_as()?),
				L1Instruction::UnifyVariable("X7".parse_as()?),
				L1Instruction::GetStructure("a/0".parse_as()?, "X7".parse_as()?),
				L1Instruction::GetValue("X5".parse_as()?, "A3".parse_as()?),
				L1Instruction::Proceed
			]
		);

		assert_eq!(
			{
				let Compiled {
					instructions,
					var_reg_mapping: _,
					labels,
				} = "a(const). b(const). c(const). d(const)."
					.parse_as::<Facts>()?
					.compile_as_program()?;
				(instructions, labels)
			},
			(
				vec![
					L1Instruction::GetStructure("const/0".parse_as()?, "A1".parse_as()?),
					L1Instruction::Proceed,
					L1Instruction::GetStructure("const/0".parse_as()?, "A1".parse_as()?),
					L1Instruction::Proceed,
					L1Instruction::GetStructure("const/0".parse_as()?, "A1".parse_as()?),
					L1Instruction::Proceed,
					L1Instruction::GetStructure("const/0".parse_as()?, "A1".parse_as()?),
					L1Instruction::Proceed,
				],
				hash_map!(
					"a/1".into(): 0.into(),
					"b/1".into(): 2.into(),
					"c/1".into(): 4.into(),
					"d/1".into(): 6.into(),
				)
			)
		);

		Ok(())
	}

	#[test]
	fn test_compile_query() -> Result<()> {
		assert_eq!(
			"p(Z, h(Z,W), f(W))."
				.parse_as::<Fact>()?
				.compile_as_query()?
				.instructions,
			vec![
				L1Instruction::PutVariable("X4".parse_as()?, "X1".parse_as()?),
				L1Instruction::PutStructure("h/2".parse_as()?, "X2".parse_as()?),
				L1Instruction::SetValue("X4".parse_as()?),
				L1Instruction::SetVariable("X5".parse_as()?),
				L1Instruction::PutStructure("f/1".parse_as()?, "X3".parse_as()?),
				L1Instruction::SetValue("X5".parse_as()?),
				L1Instruction::Call("p/3".into())
			]
		);

		Ok(())
	}
}
