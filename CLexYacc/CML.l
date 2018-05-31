%e  1019
%p  2807
%n  371
%k  284
%a  1213
%o  1117

D   [0-9]
L   [a-zA-Z_]
A   [a-zA-Z_0-9]
ES  \\['"\\nt]
WS  [ \t\n]

%{
#include <stdio.h>
#include "y.tab.h"

extern void yyerror(const char *);  /* prints grammar violation message */

extern int sym_type(const char *);  /* returns type from symbol table */

#define sym_type(identifier) IDENTIFIER /* with no symbol table, fake it */

static void comment(void);
static int check_type(void);
%}

%%

"/*"                                    { comment(); }
"//".*                                  { /* consume //-comment */ }
"char"					{ return(CHAR); }
"else"					{ return(ELSE); }
"real"					{ return(REAL); }
"bool"					{ return(BOOL); }
"string"				{ return(STRING); }
"dataset"				{ return(DATASET); }
"model"					{ return(MODEL); }
"if"					{ return(IF); }
"int"					{ return(INT); }
"return"				{ return(RETURN); }
"void"					{ return(VOID); }
"while"					{ return(WHILE); }
{L}{A}*					{ return (IDENTIFIER); }
{D}+					{ return INT_LITERAL; }
{D}*"."{D}+				{ return REAL_LITERAL; }
{D}+"."					{ return REAL_LITERAL; }
\'([^'\\\n]|{ES})\'			{ return CHAR_LITERAL; }
\"([^"\\\n]|{ES})*\"			{ return STRING_LITERAL; }
"&&"					{ return AND_OP; }
"||"					{ return OR_OP; }
"<="					{ return LE_OP; }
">="					{ return GE_OP; }
"=="					{ return EQ_OP; }
"!="					{ return NE_OP; }
";"					{ return ';'; }
"{"					{ return '{'; }
"}"					{ return '}'; }
","					{ return ','; }
"="					{ return '='; }
"("					{ return '('; }
")"					{ return ')'; }
"["					{ return '['; }
"]"					{ return ']'; }
"!"					{ return '!'; }
"-"					{ return '-'; }
"+"					{ return '+'; }
"*"					{ return '*'; }
"/"					{ return '/'; }
"<"					{ return '<'; }
">"					{ return '>'; }
{WS}+					{ /* whitespace separates tokens */ }
.					{ /* discard bad characters */ }

%%

int yywrap(void)        /* called at end of input */
{
    return 1;           /* terminate now */
}

static void comment(void)
{
    int c;

    while ((c = input()) != 0)
        if (c == '*')
        {
            while ((c = input()) == '*')
                ;

            if (c == '/')
                return;

            if (c == 0)
                break;
        }
    yyerror("unterminated comment");
}