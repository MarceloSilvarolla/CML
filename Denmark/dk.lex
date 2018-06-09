type lexresult = Tokens.token
fun eof() = Tokens.EOF(0, 0)
fun error(position, msg) = output(std_out,msg ^ "\n")
%%
digits = [0-9]+
ws = [ \n\t]
%%
if			=> (Tokens.IF(yypos, yypos+2));
[a-z][a-z0-9]		=> (Tokens.ID(yytext,yypos,yypos+size yytext));
{digits}		=> (Tokens.INT_LITERAL(Int.fromtString yytext,yypos,yypos+size yytext));
{ws}+			=> (continue());
.			=> (error (yypos, "Illegal character"); continue());
