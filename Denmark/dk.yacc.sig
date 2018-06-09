signature _TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val BEGIN:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val INT_LITERAL:  'a * 'a -> (svalue,'a) token
val ID:  'a * 'a -> (svalue,'a) token
end
signature _LRVALS=
sig
structure Tokens : _TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
