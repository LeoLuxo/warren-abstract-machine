use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Add;

use anyhow::Context;
use anyhow::Result;
use velcro::hash_map;

use crate::ast::Fact;
use crate::ast::GetFunctor;
use crate::subst::{VarToRegMapping, VariableContext};
use crate::universal_compiler;
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
			.reduce(Add::add)
			.context("empty set of facts cannot be compiled")
	}
}

impl CompilableProgram<L1> for Fact {
	fn compile_as_program(self) -> Result<Compiled<L1>> {
		let fact_label = self.get_functor().as_identifier();

		let order = FlatteningOrder::for_program();
		let context = VariableContext::Local(fact_label.clone());
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

		let mut instructions = compile_program_tokens(tokens);
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

	use super::*;
	use anyhow::Result;

	#[test]
	fn test_compile_program() -> Result<()> {
		// The wambook has the last getvalue X5,A3 3 instructions earlier,
		// I posit that this variation doesn't change the validity of the instruction sequence
		#[rustfmt::skip]
		assert_eq!(
			"p(f(X), h(Y, f(a)), Y)."
				.parse::<Facts>()?
				.compile_as_program()?.instructions,
			vec![
				L1Instruction::GetStructure("f/1".parse()?, "A1".parse()?),
				L1Instruction::UnifyVariable("X4".parse()?),
				L1Instruction::GetStructure("h/2".parse()?, "A2".parse()?),
				L1Instruction::UnifyVariable("X5".parse()?),
				L1Instruction::UnifyVariable("X6".parse()?),
				L1Instruction::GetStructure("f/1".parse()?, "X6".parse()?),
				L1Instruction::UnifyVariable("X7".parse()?),
				L1Instruction::GetStructure("a/0".parse()?, "X7".parse()?),
				L1Instruction::GetValue("X5".parse()?, "A3".parse()?),
				L1Instruction::Proceed
			]
		);

		Ok(())
	}

	#[test]
	fn test_compile_query() -> Result<()> {
		// #[rustfmt::skip]
		// assert_eq!(
		// 	"p(Z, h(Z,W), f(W))"
		// 		.parse::<Fact>()?
		// 		.compile_as_query()?.instructions,
		// 	vec![
		// 		L1Instruction::PutStructure("h/2".parse()?, 3_usize.into() ),
		// 		L1Instruction::SetVariable(2_usize.into()),
		// 		L1Instruction::SetVariable(5_usize.into()),
		// 		L1Instruction::PutStructure("f/1".parse()?, 4_usize.into() ),
		// 		L1Instruction::SetValue(5_usize.into()),
		// 		L1Instruction::PutStructure("p/3".parse()?, 1_usize.into() ),
		// 		L1Instruction::SetValue(2_usize.into()),
		// 		L1Instruction::SetValue(3_usize.into()),
		// 		L1Instruction::SetValue(4_usize.into()),
		// 	]
		// );

		Ok(())
	}
}
