#include "Tokens.h"

#include "JuneContext.h"

#include <assert.h>

std::string june::Token::GetLexicalPresentation(const JuneContext& Context) {
	return std::string("(") + GetTokenKindPresentation(Kind, Context)
		             + ", \'" + Loc.Text.str() + "\')";
}

std::string june::GetTokenKindPresentation(u16 Kind, const JuneContext& Context) {
	switch (Kind) {
	case TokenKind::INT_LITERAL:      return "int-literal";
	case TokenKind::HEX_LITERAL:      return "hex-literal";
	case TokenKind::BIN_LITERAL:      return "bin-literal";
	case TokenKind::STRING_LITERAL:   return "string-literal";
	case TokenKind::FLOAT_LITERAL:    return "float-literal";
	case TokenKind::CHAR_LITERAL:     return "char-literal";
	case TokenKind::IDENT:            return "ident";
	case TokenKind::TK_EOF:           return "eof";
	case TokenKind::PLUS_EQ:          return "+=";
	case TokenKind::MINUS_EQ:         return "-=";
	case TokenKind::SLASH_EQ:         return "/=";
	case TokenKind::STAR_EQ:          return "*=";
	case TokenKind::MOD_EQ:	          return "%=";
	case TokenKind::AMP_EQ:	          return "&=";
	case TokenKind::BAR_EQ:	          return "|=";
	case TokenKind::CRT_EQ:	          return "^=";
	case TokenKind::LT_LT:	          return "<<";
	case TokenKind::GT_GT:	          return ">>";
	case TokenKind::LT_LT_EQ:         return "<<=";
	case TokenKind::GT_GT_EQ:         return ">>=";
	case TokenKind::LT_EQ:            return "<=";
	case TokenKind::GT_EQ:            return ">=";
	case TokenKind::AMP_AMP:          return "&&";
	case TokenKind::BAR_BAR:          return "||";
	case TokenKind::EQ_EQ:            return "==";
	case TokenKind::EXL_EQ:           return "!=";
	case TokenKind::COL_COL:          return "::"; 
	case TokenKind::MINUS_GT:         return "->";
	case TokenKind::PLUS_PLUS:
	case TokenKind::POST_PLUS_PLUS:   return "++";
	case TokenKind::MINUS_MINUS:
	case TokenKind::POST_MINUS_MINUS: return "--";
	case TokenKind::BAR_GT:           return "|>";
	default:
		if (Kind < 257) {
			if (Kind >= 33 && Kind <= 126)
				return std::string(1, (c8)Kind);
			else
				return std::to_string(Kind);
		}
		//else if (Kind >= __TK_KEYWORD_START__ && Kind <= __TK_KEYWORD_END__)
		//	return Context.GetKwAsString(Kind).str();

		assert("Failed to implement case for token presentation.");
		break;
	}
	return std::string();
}
