%%
%term ID | INT_LITERAL | WHILE | BEGIN | END | DO | IF | THEN | ELSE | SEMI | ASSIGN | EOF
%nonterm prog | stm | stmlist
%pos int
%verbose
%start prog
%eop EOF %noshift EOF
%%
prog : stmlist ()
stm : ID ASSIGN ID ()
| WHILE ID DO stm ()
| BEGIN stmlist END ()
| IF ID THEN stm ()
| IF ID THEN stm ELSE stm ()
stmlist : stm ()
| stmlist SEMI stm ()
