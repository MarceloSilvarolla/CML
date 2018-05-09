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

%start translation_unit

%%


/* 1. Expressões */

/* TODO: a operação sobre datasets (tipo d[2:5]) */
/* TODO: - unario */

/* 1.1 Expressões necessariamente “atômicas”:
    Expressões que não causam ambiguidades quando dentro de uma expressão maior, mesmo quando a precedência das operações não é conhecida
    Estas expressões podem, portanto, ser pensadas como identificadores ou literais. */

primary_expression
    : IDENTIFIER
    | literal
    | array
    | '(' expression ')'
    | array_expression
    | primary_expression '(' ')'
    | primary_expression '(' expression_list ')'
    ;


literal
    : INT_LITERAL
    | REAL_LITERAL
    | BOOL_LITERAL
    | CHAR_LITERAL
    | STRING_LITERAL
    ;

array
    : '{' '}'
    | '{' expression_list '}'
    ;

expression_list
    : expression
    | expression_list ',' expression
    ;

/* 1.2 Expressões “não-atômicas” */

expression
    : logical_or_expression
    | IDENTIFIER '=' expression
    | array_expression '=' expression
    ;

array_expression
    : IDENTIFIER '[' expression ']'
    | array_expression '[' expression ']'
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
    : neg_expression
    | multiplicative_expression '*' neg_expression
    | multiplicative_expression '/' neg_expression
    ;

neg_expression
    : primary_expression
    | '!' neg_expression
    ;

/* 2. Comandos */
statement
    : compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    ;

compound_statement
    : '{' SKIP ';' '}'
    | '{' block_item_list '}'
    ;

block_item_list
    : block_item
    | block_item_list block_item
    ;

block_item
    : declaration
    | statement
    ;

expression_statement
    : ';'
    | expression ';'
    ;

selection_statement
    : IF '(' expression ')' compound_statement ELSE compound_statement
    | IF '(' expression ')' compound_statement
    ;

iteration_statement
    : WHILE '(' expression ')' statement
    ;

jump_statement
    : RETURN ';'
    | RETURN expression ';'
    ;

/* 3. Declarações */

declaration
    : type_specifier IDENTIFIER ';'
    | type_specifier IDENTIFIER '=' expression ';'
    | type_specifier IDENTIFIER '(' ')' ';'
    | type_specifier IDENTIFIER '(' parameter_declaration_list ')' ';'
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


/* 4. Unidade de tradução */
translation_unit
    : external_declaration
    | translation_unit external_declaration
    ;

external_declaration
    : function_definition
    | declaration
    ;

function_definition
    : type_specifier IDENTIFIER '(' ')' compound_statement
    | type_specifier IDENTIFIER '(' parameter_declaration_list ')' compound_statement
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
