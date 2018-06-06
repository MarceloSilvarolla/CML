signature CML_TOKENS =
sig
type ('a,'b) token
type svalue
val DIVIDED:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val RSQUARE_BRACKET:  'a * 'a -> (svalue,'a) token
val LSQUARE_BRACKET:  'a * 'a -> (svalue,'a) token
val RCURLY_BRACKET:  'a * 'a -> (svalue,'a) token
val LCURLY_BRACKET:  'a * 'a -> (svalue,'a) token
val GE_OP:  'a * 'a -> (svalue,'a) token
val GT_OP:  'a * 'a -> (svalue,'a) token
val LE_OP:  'a * 'a -> (svalue,'a) token
val LT_OP:  'a * 'a -> (svalue,'a) token
val NE_OP:  'a * 'a -> (svalue,'a) token
val EQ_OP:  'a * 'a -> (svalue,'a) token
val NEG_OP:  'a * 'a -> (svalue,'a) token
val AND_OP:  'a * 'a -> (svalue,'a) token
val OR_OP:  'a * 'a -> (svalue,'a) token
val STRING_LITERAL: (string) *  'a * 'a -> (svalue,'a) token
val CHAR_LITERAL: (string) *  'a * 'a -> (svalue,'a) token
val BOOL_LITERAL: (string) *  'a * 'a -> (svalue,'a) token
val REAL_LITERAL: (string) *  'a * 'a -> (svalue,'a) token
val INT_LITERAL: (string) *  'a * 'a -> (svalue,'a) token
val IDENTIFIER: (string) *  'a * 'a -> (svalue,'a) token
val MODEL:  'a * 'a -> (svalue,'a) token
val DATASET:  'a * 'a -> (svalue,'a) token
val STRING:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val CHAR:  'a * 'a -> (svalue,'a) token
val REAL:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val VOID:  'a * 'a -> (svalue,'a) token
val SKIP:  'a * 'a -> (svalue,'a) token
val RETURN:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature CML_LRVALS=
sig
structure Tokens : CML_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
