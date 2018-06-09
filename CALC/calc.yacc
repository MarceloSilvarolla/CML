(* Sample interactive calculator for ML-Yacc *)

fun lookup "bogus" = 10000
  | lookup s = 0

%%

%eop EOF SEMI

(* %pos declares the type of positions for terminals.
   Each symbol has an associated left and right position. *)

%pos int

%left SUB PLUS
%left TIMES DIV
%right CARAT

%term ID of string | NUM of int | PLUS | TIMES | PRINT |
      SEMI | EOF | CARAT | DIV | SUB
%nonterm EXP of int | START of int option

%name Calc

%subst PRINT for ID
%prefer PLUS TIMES DIV SUB
%keyword PRINT SEMI

%noshift EOF
%value ID ("bogus")
%nodefault
%verbose
%%

(* the parser returns the value associated with the expression *)

  START : PRINT EXP (print EXP;
                     print "\n";
                     flush_out std_out; SOME EXP)
        | EXP (SOME EXP)
        | (NONE)
  EXP : NUM             (NUM)
      | ID              (lookup ID)
      | EXP PLUS EXP    (EXP1+EXP2)
      | EXP TIMES EXP   (EXP1*EXP2)
      | EXP DIV EXP     (EXP1 div EXP2)
      | EXP SUB EXP     (EXP1-EXP2)
      | EXP CARAT EXP   (let fun e (m,0) = 1
                                | e (m,l) = m*e(m,l-1)
                         in e (EXP1,EXP2)       
                         end)

