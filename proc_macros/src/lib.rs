use proc_macro::{Ident, TokenStream};
use quote::quote;
use syn::{
	self, parse_macro_input, Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, FieldsUnnamed, Index, TypeTuple,
};

#[proc_macro_derive(NewType)]
pub fn impl_newtype(input: TokenStream) -> TokenStream {
	let DeriveInput { ident, data, .. } = parse_macro_input!(input);

	let field = match data {
		Data::Struct(DataStruct { ref fields, .. }) => match fields {
			Fields::Named(FieldsNamed { named, .. }) if named.len() == 1 => named.first().unwrap(),
			Fields::Unnamed(FieldsUnnamed { unnamed, .. }) if unnamed.len() == 1 => unnamed.first().unwrap(),

			_ => panic!("Needs to have exactly one field"),
		},

		Data::Enum(data_enum) => todo!(),

		_ => panic!("Needs to be struct or enum "),
	};

	todo!()
}

#[proc_macro_derive(AutoDisplay)]
pub fn impl_autodisplay(input: TokenStream) -> TokenStream {
	let DeriveInput { ident, data, .. } = parse_macro_input!(input);

	let field_name = match data {
		Data::Struct(DataStruct { ref fields, .. }) => match fields {
			Fields::Named(FieldsNamed { ref named, .. }) if named.len() == 1 => {
				let ident = named.first().unwrap().ident.as_ref().unwrap();
				quote! {#ident}
			}
			Fields::Unnamed(FieldsUnnamed { unnamed, .. }) if unnamed.len() == 1 => quote! {0},

			_ => panic!("Needs to have exactly one field"),
		},
		_ => panic!("Needs to be a struct "),
	};

	quote! {
		impl std::fmt::Display for #ident {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				std::fmt::Debug::fmt(&self.#field_name, f)
			}
		}
	}
	.into()
}
