/* Baseado em http://www.quut.com/c/ANSI-C-grammar-y.html */
/* https://docs.google.com/document/d/1ICumehrbqgM_OBzdH0uYGFk2DyQDmpr00JclKpi8acY/edit?usp=sharing */

%token IDENTIFIER INT_CONSTANT REAL_CONSTANT BOOL_CONSTANT STRING_LITERAL
%token OR_OP AND_OP EQ_OP NE_OP LE_OP GE_OP
%token IF ELSE WHILE
%token INT REAL CHAR BOOL STRING DATASET MODEL

%start translation_unit

%%


/* 1. Expressões */

/* TODO: acrescentar operador unário ! e a operação sobre datasets (tipo d[2:5]) */
/* TODO: preencher regras de “declaration” e “function_definition” */
/* TODO: comentários */
/* TODO: expressões da forma {e_1, …, e_n} para definir vetores */

/* 1.1 Expressões “atômicas”:
    Expressões que não causam ambiguidades quando dentro de uma expressão maior, mesmo quando a precedência das operações não é conhecida
    Estas expressões podem, portanto, ser pensadas como identificadores, constantes ou literais. */

primary_expression
    : IDENTIFIER
    | constant
    | string
    | '(' expression ')'
    ;

constant
    : INT_CONSTANT
    | REAL_CONSTANT
    | BOOL_CONSTANT
    ;

string
    : STRING_LITERAL
    ;

/* 1.2 Expressões “não-atômicas” */

expression
    : assignment_expression
    ;

assignment_expression
    : logical_or_expression
    | array_expression '=' assignment_expression
    ;

array_expression
    : IDENTIFIER
    | array_expression '[' expression ']'
    ;

logical_or_expression
    : logical_and_expression
    | logical_or_expression OR_OP logical_and_expression
    ;

logical_and_expression
    : equality_expression
    | logical_and_expression AND_OP equality_expression
    ;

equality_expression
    : relational_expression
    | equality_expression EQ_OP relational_expression
    | equality_expression NE_OP relational_expression
    ;

relational_expression
    : additive_expression
    | relational_expression '<' additive_expression
    | relational_expression '>' additive_expression
    | relational_expression LE_OP additive_expression
    | relational_expression GE_OP additive_expression
    ;

additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression
    | additive_expression '-' multiplicative_expression
    ;

multiplicative_expression
    : postfix_expression
    | multiplicative_expression '*' postfix_expression
    | multiplicative_expression '/' postfix_expression
    ;

postfix_expression
    : primary_expression
    | postfix_expression '[' expression ']'
    | postfix_expression '(' ')'
    | postfix_expression '(' argument_expression_list ')'
    ;

argument_expression_list
    : expression
    | argument_expression_list ',' expression
    ;

/* 2. Comandos */
statement
    : compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    ;

compound_statement
    : '{' '}'
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

/* 3. Declarações */
declaration
    : declaration_specifiers init_declarator_list ';'
    ;

declaration_specifiers
    : type_specifier declaration_specifiers
    ;

init_declarator_list
    : 


/* 4. Unidade de tradução */
translation_unit
    : external_declaration
    | translation_unit external_declaration
    ;

external_declaration
    : function_definition
    | declaration
    ;

/*function_defintion
    :
*/ 

declaration_list
    : declaration
    | declaration_list declaration
    ;


