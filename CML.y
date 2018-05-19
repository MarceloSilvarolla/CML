%{
int yylex();
void yyerror(const char *s);
%}
/* Baseado em http://www.quut.com/c/ANSI-C-grammar-y.html */
/* https://docs.google.com/document/d/1ICumehrbqgM_OBzdH0uYGFk2DyQDmpr00JclKpi8acY/edit?usp=sharing */

%token IDENTIFIER INT_LITERAL REAL_LITERAL BOOL_LITERAL CHAR_LITERAL STRING_LITERAL
%token OR_OP AND_OP EQ_OP NE_OP LE_OP GE_OP
%token IF ELSE WHILE RETURN SKIP
%token VOID INT REAL CHAR BOOL STRING DATASET MODEL

%start program

%%


/* 1. Expressões */

/* TODO: a operação sobre datasets (tipo d[2:5]) */

/* 1.1 Expressões necessariamente "atômicas":
    Expressões que não causam ambiguidades quando dentro de uma expressão maior, mesmo quando a precedência das operações não é conhecida
    Estas expressões podem, portanto, ser pensadas como identificadores ou literais. */

primary_expression
    : IDENTIFIER
    | literal
    | '{' '}'
    | '{' expression_list '}'
    | '(' expression ')'
    | array_access
    | IDENTIFIER '(' ')'
    | IDENTIFIER '(' expression_list ')'
    ;


literal
    : INT_LITERAL
    | REAL_LITERAL
    | BOOL_LITERAL
    | CHAR_LITERAL
    | STRING_LITERAL
    ;


/* 1.2 Expressões "não-atômicas" */
expression_list
    : expression
    | expression_list ',' expression
    ;

expression
    : logical_or_expression
    | IDENTIFIER '=' expression
    | array_access '=' expression
    ;

array_access
    : IDENTIFIER '[' expression ']'
    | array_access '[' expression ']'
    ;

logical_or_expression
    : logical_and_expression
    | logical_or_expression OR_OP logical_and_expression
    ;

logical_and_expression
    : relational_expression
    | logical_and_expression AND_OP relational_expression
    ;

relational_expression
    : additive_expression
    | additive_expression '<' additive_expression
    | additive_expression '>' additive_expression
    | additive_expression LE_OP additive_expression
    | additive_expression GE_OP additive_expression
    | additive_expression EQ_OP additive_expression
    | additive_expression NE_OP additive_expression
    ;

additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression
    | additive_expression '-' multiplicative_expression
    ;

multiplicative_expression
    : unary_minus_expression
    | multiplicative_expression '*' unary_minus_expression
    | multiplicative_expression '/' unary_minus_expression
    ;

unary_minus_expression
    : neg_expression
    | '-' neg_expression
    ;

neg_expression
    : primary_expression
    | '!' neg_expression
    ;


/* 2. Comandos */
command
    : compound_command
    | expression_command
    | selection_command
    | iteration_command
    | jump_command
    | SKIP ';'
    ;

compound_command
    : '{' declaration_or_command_list '}'
    ;

declaration_or_command_list 
    : declaration_or_command
    | declaration_or_command_list declaration_or_command
    ;

declaration_or_command
    : declaration
    | command
    ;

expression_command
    : ';'
    | expression ';'
    ;

selection_command
    : IF '(' expression ')' compound_command ELSE compound_command
    | IF '(' expression ')' compound_command
    ;

iteration_command
    : WHILE '(' expression ')' command
    ;

jump_command
    : RETURN ';'
    | RETURN expression ';'
    ;

/* 3. Declarações */

declaration
    : type_specifier IDENTIFIER ';'
    | type_specifier IDENTIFIER '=' expression ';'
    ;

parameter_declaration_list
    : parameter_declaration
    | parameter_declaration_list ',' parameter_declaration
    ;

parameter_declaration
    : type_specifier IDENTIFIER
    ;


type_specifier
    : VOID
    | CHAR
    | INT
    | REAL
    | BOOL
    | STRING
    | DATASET
    | MODEL
    | type_specifier '[' ']'
    ;


/* 4. Programa */
program
    : declaration_or_function_definition
    | program declaration_or_function_definition
    ;

declaration_or_function_definition
    : function_definition
    | declaration
    ;

function_definition
    : type_specifier IDENTIFIER '(' ')' compound_command
    | type_specifier IDENTIFIER '(' parameter_declaration_list ')' compound_command
    ;

%%
#include <stdio.h>

void yyerror(const char *s)
{
	fflush(stdout);
	fprintf(stderr, "*** %s\n", s);
}


void main(void) {
   printf("CML\n");
   yyparse();
}
