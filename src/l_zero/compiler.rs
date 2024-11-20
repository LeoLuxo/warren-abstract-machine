use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::Result;

use crate::universal_compiler;
use crate::universal_compiler::CompilableProgram;
use crate::universal_compiler::CompilableQuery;
use crate::universal_compiler::Compiled;
use crate::universal_compiler::FlatteningOrder;
use crate::{
	machine_types::VarRegister,
	subst::{VarToRegMapping, VariableContext},
};

use crate::universal_compiler::MappingToken;

use super::{L0Instruction, NonVariableTerm, L0};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

impl CompilableProgram<L0> for NonVariableTerm {
	fn compile_as_program(self) -> Result<Compiled<L0>> {
		let order = FlatteningOrder::for_program();
		let context = VariableContext::Local("prg".into());
		let (tokens, var_mapping) = flatten_term(self, order, context);

		let instructions = compile_program_tokens(tokens);

		Ok(Compiled {
			instructions,
			var_reg_mapping: Some(var_mapping),
			..Default::default()
		})
	}
}

impl CompilableQuery<L0> for NonVariableTerm {
	fn compile_as_query(self) -> Result<Compiled<L0>> {
		let order = FlatteningOrder::for_query();
		let context = VariableContext::Query;
		let (tokens, var_mapping) = flatten_term(self, order, context);

		let instructions = compile_query_tokens(tokens);

		Ok(Compiled {
			instructions,
			var_reg_mapping: Some(var_mapping),
			..Default::default()
		})
	}
}

fn flatten_term(
	term: NonVariableTerm,
	order: FlatteningOrder,
	context: VariableContext,
) -> (Vec<MappingToken>, VarToRegMapping) {
	let mut mapping = HashMap::default();

	let tokens = universal_compiler::flatten_term(
		VarRegister::default(),
		term.into(),
		&mut mapping,
		&mut VarRegister::default(),
		order,
	);

	let var_reg_mapping = VarToRegMapping::from_hashmap_with_context(mapping, context);

	(tokens, var_reg_mapping)
}

fn compile_program_tokens(tokens: Vec<MappingToken>) -> Vec<L0Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for token in tokens {
		#[rustfmt::skip]
		let (reg, inst) = match token {
			MappingToken::Functor(reg, functor)                          => (reg, L0Instruction::GetStructure(functor, reg)),
			MappingToken::VarRegister(reg) if encountered.contains(&reg) => (reg, L0Instruction::UnifyValue(reg)),
			MappingToken::VarRegister(reg)                               => (reg, L0Instruction::UnifyVariable(reg)),
			MappingToken::ArgumentRegister(_, _)                         => unimplemented!("this token cannot exist for L0, this is a compiler bug"),
		};

		encountered.insert(reg);
		instructions.push(inst);
	}

	instructions
}

fn compile_query_tokens(tokens: Vec<MappingToken>) -> Vec<L0Instruction> {
	let mut instructions = Vec::with_capacity(tokens.len());
	let mut encountered = HashSet::new();

	for token in tokens {
		#[rustfmt::skip]
		let (reg, inst) = match token {
			MappingToken::Functor(reg, functor)                          => (reg, L0Instruction::PutStructure(functor, reg)),
			MappingToken::VarRegister(reg) if encountered.contains(&reg) => (reg, L0Instruction::SetValue(reg)),
			MappingToken::VarRegister(reg)                               => (reg, L0Instruction::SetVariable(reg)),
			MappingToken::ArgumentRegister(_, _)                         => unimplemented!("this token cannot exist for L0, this is a compiler bug"),
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
	use velcro::vec;

	#[test]
	fn test_compile_program() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			"p(f(X), h(Y, f(a)), Y)"
				.parse::<NonVariableTerm>()?
				.compile_as_program()?.instructions,
			vec![
				L0Instruction::GetStructure("p/3".parse()?, 1_usize.into() ),
				L0Instruction::UnifyVariable(2_usize.into()),
				L0Instruction::UnifyVariable(3_usize.into()),
				L0Instruction::UnifyVariable(4_usize.into()),
				L0Instruction::GetStructure("f/1".parse()?, 2_usize.into() ),
				L0Instruction::UnifyVariable(5_usize.into()),
				L0Instruction::GetStructure("h/2".parse()?, 3_usize.into() ),
				L0Instruction::UnifyValue(4_usize.into()),
				L0Instruction::UnifyVariable(6_usize.into()),
				L0Instruction::GetStructure("f/1".parse()?, 6_usize.into() ),
				L0Instruction::UnifyVariable(7_usize.into()),
				L0Instruction::GetStructure("a/0".parse()?, 7_usize.into() ),
			]
		);

		Ok(())
	}

	#[test]
	fn test_compile_query() -> Result<()> {
		#[rustfmt::skip]
		assert_eq!(
			"p(Z, h(Z,W), f(W))"
				.parse::<NonVariableTerm>()?
				.compile_as_query()?.instructions,
			vec![
				L0Instruction::PutStructure("h/2".parse()?, 3_usize.into() ),
				L0Instruction::SetVariable(2_usize.into()),
				L0Instruction::SetVariable(5_usize.into()),
				L0Instruction::PutStructure("f/1".parse()?, 4_usize.into() ),
				L0Instruction::SetValue(5_usize.into()),
				L0Instruction::PutStructure("p/3".parse()?, 1_usize.into() ),
				L0Instruction::SetValue(2_usize.into()),
				L0Instruction::SetValue(3_usize.into()),
				L0Instruction::SetValue(4_usize.into()),
			]
		);

		Ok(())
	}
}
