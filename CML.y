/* Baseado em http://www.quut.com/c/ANSI-C-grammar-y.html */
/* https://docs.google.com/document/d/1ICumehrbqgM_OBzdH0uYGFk2DyQDmpr00JclKpi8acY/edit?usp=sharing */

%token IDENTIFIER INT_CONSTANT REAL_CONSTANT BOOL_CONSTANT STRING_LITERAL
%token OR_OP AND_OP EQ_OP NE_OP LE_OP GE_OP
%token IF ELSE WHILE
%token VOID INT REAL CHAR BOOL STRING DATASET MODEL

%start translation_unit

%%


/* 1. Expressões */

/* TODO: acrescentar operador unário ! e a operação sobre datasets (tipo d[2:5]) */

/* 1.1 Expressões “atômicas”:
    Expressões que não causam ambiguidades quando dentro de uma expressão maior, mesmo quando a precedência das operações não é conhecida
    Estas expressões podem, portanto, ser pensadas como identificadores, constantes ou literais. */

/* LEO:   Não está faltando acesso a array: IDENTIFIER '[' expression ']' ?
    Da forma como está, acesso a array só pode acontecer através de 'expression',
    mas acho que também cai no caso de ser uma expressão primária.
 */

primary_expression
    : IDENTIFIER
    | constant
    | string
    | array
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

array
    : '{' '}'
    | '{' expression_list '}'
    ;

expression_list
    : expression
    | expression_list ',' expression
    ;

/* 1.2 Expressões “não-atômicas” */

/* LEO:   Podemos remover 'assignment_expression', e utilizar apenas 'expression' */

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

/* LEO:   Posso estar errado, mas isto vai acabar permitindo coisas do tipo:
      E == E == E
   Eu sei que em C isto é possível (e tem um resultado diferente do intuitivo),
   Mas não lembro de termos discutido isto. Então, vamos deixar?
*/

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

/* LEO:   Não deveria ser prefix_expression?
   Me corrijam se estiver viajando, mas o 'pre' não é quando o 'operando',
   no caso aqui é o nome da função/vetor vem antes?
 */

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

/* LEO:   Podemos remover este também? Ou ele é útil para mantermos uniforme o 'statment' acima? */

expression_statement
    : expression ';'
    ;

selection_statement
    : IF '(' expression ')' compound_statement ELSE compound_statement
    | IF '(' expression ')' compound_statement
    ;

iteration_statement
    : WHILE '(' expression ')' statement
    ;

/* 3. Declarações */

/* LEO:   Falta o ';' na primeira produção */

declaration
    : type_specifier IDENTIFIER
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
    : type_specifier IDENTIFIER '(' parameter_declaration_list ')' compound_statement
    ;
