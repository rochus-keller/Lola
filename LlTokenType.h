#ifndef __LL_TOKENTYPE__
#define __LL_TOKENTYPE__
// This file was automatically generated by EbnfStudio; don't modify it!


#include <QByteArray>

namespace Ll {
	enum TokenType {
		Tok_Invalid = 0,

		TT_Literals,
		Tok_Bang,
		Tok_Hash,
		Tok_Amp,
		Tok_Lpar,
		Tok_Latt,
		Tok_Rpar,
		Tok_Star,
		Tok_Ratt,
		Tok_Plus,
		Tok_Comma,
		Tok_Minus,
		Tok_MinusGt,
		Tok_Dot,
		Tok_Colon,
		Tok_ColonEq,
		Tok_Semi,
		Tok_Lt,
		Tok_Leq,
		Tok_Eq,
		Tok_Gt,
		Tok_Geq,
		Tok_Lbrack,
		Tok_Rbrack,
		Tok_Hat,
		Tok_Lbrace,
		Tok_Bar,
		Tok_Rbrace,
		Tok_Tilde,

		TT_Keywords,
		Tok_BEGIN,
		Tok_BIT,
		Tok_BYTE,
		Tok_CONST,
		Tok_END,
		Tok_IN,
		Tok_INOUT,
		Tok_MODULE,
		Tok_OUT,
		Tok_REG,
		Tok_TS,
		Tok_TYPE,
		Tok_VAR,
		Tok_WORD,

		TT_Specials,
		Tok_identifier,
		Tok_integer,
		Tok_Comment,
		Tok_Eof,

		TT_MaxToken,

		TT_Max
	};

	const char* tokenTypeString( int ); // Pretty with punctuation chars
	const char* tokenTypeName( int ); // Just the names without punctuation chars
	bool tokenTypeIsLiteral( int );
	bool tokenTypeIsKeyword( int );
	bool tokenTypeIsSpecial( int );
	TokenType tokenTypeFromString( const QByteArray& str, int* pos = 0 );
}
#endif // __LL_TOKENTYPE__
