open DataTypes

%%

%name CML
%term EOF 
 | IF | ELSE | WHILE | RETURN | SKIP 
 | VOID | INT | REAL | CHAR | BOOL | STRING | DATASET | MODEL
 | IDENTIFIER of string
 | INT_LITERAL of string | REAL_LITERAL of string | BOOL_LITERAL of string 
 | CHAR_LITERAL of string | STRING_LITERAL of string
 | OR_OP | AND_OP | NEG_OP | EQ_OP | NE_OP | LT_OP | LE_OP | GT_OP | GE_OP
 | LCURLY_BRACKET | RCURLY_BRACKET | LSQUARE_BRACKET | RSQUARE_BRACKET
 | LPAREN | RPAREN
 | SEMICOLON | COMMA
 | EQUALS
 | PLUS | MINUS | TIMES | DIVIDED
 
%nonterm primary_expression of Exp | literal of Lit 
 | expression_list of Exp list | expression of Exp 
 | array_access of Id * (Exp list) | logical_or_expression of Exp 
 | logical_and_expression of Exp | relational_expression of Exp 
 | additive_expression of Exp | multiplicative_expression of Exp 
 | unary_minus_expression of Exp | neg_expression of Exp
 | command of Cmd | compound_command of Cmd 
 | declaration_or_command_list of DecOrCmd list
 | declaration_or_command of DecOrCmd
 | expression_command of Cmd | selection_command of Cmd
 | iteration_command of Cmd | jump_command of Cmd
 | declaration of Dec | parameter_declaration_list of Dec list
 | parameter_declaration of Dec | type_specifier of TypeSpec
 | function_definition of FunDef
 | program of Prog
 | declaration_or_function_definition_list of DecOrFunDef list
 | declaration_or_function_definition of DecOrFunDef
%pos int
%eop EOF
%noshift EOF
%nodefault
%verbose
%keyword IF ELSE WHILE RETURN SKIP VOID INT REAL CHAR BOOL STRING DATASET MODEL
%arg (fileName) : string
%start program

%%

primary_expression
  : IDENTIFIER								((IdOrArrAccessExp (Id IDENTIFIER, nil)))
  | literal 								((LitExp literal))
  | LCURLY_BRACKET RCURLY_BRACKET					((ArrExp nil))
  | LCURLY_BRACKET expression_list RCURLY_BRACKET 			((ArrExp expression_list))
  | LPAREN expression RPAREN 						((expression))
  | array_access 							((IdOrArrAccessExp array_access))
  | IDENTIFIER LPAREN RPAREN						((AppExp (Id IDENTIFIER, nil)))
  | IDENTIFIER LPAREN expression_list RPAREN				((AppExp (Id IDENTIFIER, expression_list)))
  
literal
  : INT_LITERAL								((IntLit INT_LITERAL))
  | REAL_LITERAL							((RealLit REAL_LITERAL))
  | BOOL_LITERAL							((BoolLit BOOL_LITERAL))
  | CHAR_LITERAL							((CharLit CHAR_LITERAL))
  | STRING_LITERAL							((StringLit STRING_LITERAL))

expression_list
  : expression								(([expression]))
  | expression_list COMMA expression					((expression_list @ [expression]))

expression
  : logical_or_expression						((logical_or_expression))
  | IDENTIFIER EQUALS expression					((AssignExp (Id IDENTIFIER, nil, expression)))
  | array_access EQUALS expression					((AssignExp (#1 array_access, #2 array_access, expression)))

array_access
  : IDENTIFIER LSQUARE_BRACKET expression RSQUARE_BRACKET		(((Id IDENTIFIER, [expression])))
  | array_access LSQUARE_BRACKET expression RSQUARE_BRACKET		(((#1 array_access, #2 array_access @ [expression])))

logical_or_expression
  : logical_and_expression						((logical_and_expression))
  | logical_or_expression OR_OP logical_and_expression			((OrExp (logical_or_expression, logical_and_expression)))

logical_and_expression
  : relational_expression						((relational_expression))
  | logical_and_expression AND_OP relational_expression			((AndExp (logical_and_expression, relational_expression)))

relational_expression
  : additive_expression							((additive_expression))
  | additive_expression LT_OP additive_expression			((LtExp (additive_expression1, additive_expression2)))
  | additive_expression GT_OP additive_expression			((GtExp (additive_expression1, additive_expression2)))
  | additive_expression LE_OP additive_expression			((LeExp (additive_expression1, additive_expression2)))
  | additive_expression GE_OP additive_expression			((GeExp (additive_expression1, additive_expression2)))
  | additive_expression EQ_OP additive_expression			((EqExp (additive_expression1, additive_expression2)))
  | additive_expression NE_OP additive_expression			((NeExp (additive_expression1, additive_expression2)))

additive_expression
  : multiplicative_expression						((multiplicative_expression))
  | additive_expression PLUS multiplicative_expression			((AddExp (additive_expression, multiplicative_expression)))
  | additive_expression MINUS multiplicative_expression			((SubExp (additive_expression, multiplicative_expression)))

multiplicative_expression
  : unary_minus_expression						((unary_minus_expression))
  | multiplicative_expression TIMES unary_minus_expression 		((MultExp (multiplicative_expression, unary_minus_expression)))
  | multiplicative_expression DIVIDED unary_minus_expression 		((DivExp (multiplicative_expression, unary_minus_expression)))

unary_minus_expression
  : neg_expression							((neg_expression))
  | MINUS neg_expression						((UMinusExp neg_expression))

neg_expression
  : primary_expression							((primary_expression))
  | NEG_OP neg_expression						((NegExp neg_expression))


command
  : compound_command							((compound_command))
  | expression_command							((expression_command))
  | selection_command							((selection_command))
  | iteration_command							((iteration_command))
  | jump_command							((jump_command))
  | SKIP SEMICOLON							((Skip))

compound_command
  : LCURLY_BRACKET declaration_or_command_list RCURLY_BRACKET		((CompCmd declaration_or_command_list))

declaration_or_command_list
  : declaration_or_command						(([declaration_or_command]))
  | declaration_or_command_list declaration_or_command			((declaration_or_command_list @ [declaration_or_command]))

declaration_or_command
  : declaration								((DecNotCmd declaration))
  | command								((CmdNotDec command))

expression_command
  : SEMICOLON								((ExpCmd NONE))
  | expression SEMICOLON						((ExpCmd (SOME expression)))

selection_command
  : IF LPAREN expression RPAREN compound_command ELSE compound_command	((SelCmd (expression, compound_command1, SOME compound_command2)))
  | IF LPAREN expression RPAREN compound_command			((SelCmd (expression, compound_command, NONE)))

iteration_command
  : WHILE LPAREN expression RPAREN command				((IterCmd (expression, command)))

jump_command
  : RETURN SEMICOLON							((JumpCmd NONE))
  | RETURN expression SEMICOLON						((JumpCmd (SOME expression)))


declaration
  : type_specifier IDENTIFIER SEMICOLON					((Dec (type_specifier, Id IDENTIFIER, NONE)))
  | type_specifier IDENTIFIER EQUALS expression SEMICOLON 		((Dec (type_specifier, Id IDENTIFIER, SOME expression)))

parameter_declaration_list
  : parameter_declaration						(([parameter_declaration]))
  | parameter_declaration_list COMMA parameter_declaration		((parameter_declaration_list @ [parameter_declaration]))

parameter_declaration
  : type_specifier IDENTIFIER						((Dec (type_specifier, Id IDENTIFIER, NONE)))

type_specifier
  : VOID								((Void))
  | CHAR								((Char))
  | INT									((Int))
  | REAL								((Real))
  | BOOL								((Bool))
  | STRING								((String))
  | DATASET								((Dataset))
  | MODEL								((Model))
  | type_specifier LSQUARE_BRACKET RSQUARE_BRACKET			((Array type_specifier))


function_definition
  : type_specifier IDENTIFIER 
	LPAREN RPAREN compound_command					((FunDef (type_specifier, Id IDENTIFIER, nil, compound_command)))
  | type_specifier IDENTIFIER 
	LPAREN parameter_declaration_list RPAREN compound_command	((FunDef (type_specifier, Id IDENTIFIER, 
										parameter_declaration_list, compound_command)))

program
  : declaration_or_function_definition_list				((Prog declaration_or_function_definition_list))

declaration_or_function_definition_list
  : declaration_or_function_definition					(([declaration_or_function_definition]))
  | declaration_or_function_definition_list declaration_or_function_definition ((declaration_or_function_definition_list @ [declaration_or_function_definition]))


declaration_or_function_definition
  : function_definition							((FunDefNotDec function_definition))
  | declaration								((DecNotFunDef declaration))
 
