use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Add;
use std::ops::AddAssign;

use anyhow::Context;
use anyhow::Result;
use velcro::hash_map;

use crate::ast::Fact;
use crate::ast::GetFunctor;
use crate::ast::Identifier;
use crate::universal_compiler;
use crate::universal_compiler::CompilableProgram;
use crate::universal_compiler::CompilableQuery;
use crate::universal_compiler::Compiled;
use crate::universal_compiler::FlatteningOrder;
use crate::universal_compiler::MappingToken;
use crate::{
	ast::Term,
	machine_types::VarRegister,
	subst::{VarToRegMapping, VariableContext},
};

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
			MappingToken::ArgumentRegister(xn, ai) if encountered.contains(&xn) => (xn,  L1Instruction::GetVariable(xn, ai)),
			MappingToken::ArgumentRegister(xn, ai)                              => (xn,  L1Instruction::GetValue(xn, ai)),
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
			MappingToken::ArgumentRegister(xn, ai) if encountered.contains(&xn) => (xn,  L1Instruction::PutVariable(xn, ai)),
			MappingToken::ArgumentRegister(xn, ai)                              => (xn,  L1Instruction::PutValue(xn, ai)),
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
	use crate::ast::Functor;

	use super::*;
	use anyhow::Result;
	use velcro::vec;

	#[test]
	fn test_compile_program() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			"p(f(X), h(Y, f(a)), Y)."
				.parse::<Facts>()?
				.compile_as_program()?.instructions,
			vec![
				L1Instruction::GetStructure(Functor { name: "f".into(), arity: 1 }, 1_usize.into() ),
				L1Instruction::UnifyVariable(4_usize.into()),
				L1Instruction::GetStructure(Functor { name: "h".into(), arity: 2 }, 2_usize.into() ),
				L1Instruction::UnifyVariable(5_usize.into()),
				L1Instruction::UnifyVariable(6_usize.into()),
				L1Instruction::GetValue(5_usize.into(), 3_usize.into()),
				L1Instruction::GetStructure(Functor { name: "f".into(), arity: 1 }, 6_usize.into() ),
				L1Instruction::UnifyVariable(7_usize.into()),
				L1Instruction::GetStructure(Functor { name: "a".into(), arity: 0 }, 7_usize.into() ),
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
		// 		L1Instruction::PutStructure(Functor { name: "h".into(), arity: 2 }, 3_usize.into() ),
		// 		L1Instruction::SetVariable(2_usize.into()),
		// 		L1Instruction::SetVariable(5_usize.into()),
		// 		L1Instruction::PutStructure(Functor { name: "f".into(), arity: 1 }, 4_usize.into() ),
		// 		L1Instruction::SetValue(5_usize.into()),
		// 		L1Instruction::PutStructure(Functor { name: "p".into(), arity: 3 }, 1_usize.into() ),
		// 		L1Instruction::SetValue(2_usize.into()),
		// 		L1Instruction::SetValue(3_usize.into()),
		// 		L1Instruction::SetValue(4_usize.into()),
		// 	]
		// );

		Ok(())
	}
}
