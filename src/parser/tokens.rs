use logos::{Lexer, Logos};

/*
--------------------------------------------------------------------------------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--------------------------------------------------------------------------------
*/

#[derive(Clone, Debug, Logos)]
#[logos(skip r"[ \t\f]+")]
pub enum Token {
	/// A comment, a percent followed by any number of characters until the end of the line.
	/// Is automatically skipped by the lexer
	#[regex(r"%[^\n\r]*", logos::skip)]
	Comment,

	/// A line break, so any permutation of \n and \r for one of more empty lines.
	/// Is automatically skipped by the lexer
	#[regex(r"[\n|\r|\r\n]+", logos::skip)]
	LineBreak,

	/// A comma ,
	#[token(",")]
	Comma,

	/// A dot .
	#[token(".")]
	Dot,

	/// An implication :-
	#[token(":-")]
	Implies,

	/// A left parentesis (
	#[token("(")]
	LeftParenthesis,
	/// A right parentesis )
	#[token(")")]
	RightParenthesis,

	/// An uppercase identifier
	#[regex(r"[A-Z][a-zA-Z0-9]*", priority = 20, callback = ident_callback)]
	UppercaseIdentifier(String),

	/// A lowercase identifier
	#[regex(r"[a-z][a-zA-Z0-9]*", priority = 19, callback = ident_callback)]
	LowercaseIdentifier(String),

	/// A generic identifier
	#[regex(r"[\p{XID_Start}_#][\p{XID_Continue}#]*", priority = 10, callback = ident_callback)]
	AnyIdentifier(String),
}

/// The callback to extract an identifier.
/// Simply gets the string
fn ident_callback(lexer: &mut Lexer<Token>) -> String {
	lexer.slice().to_owned()
}
