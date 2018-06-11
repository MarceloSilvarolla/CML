structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val badCh : string * string * int * int -> unit = fn
    (fileName, bad, line, col) =>
    TextIO.output(TextIO.stdOut,fileName^"[aee"
    ^Int.toString line^"."^Int.toString col
    ^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF(!lin,!col);

structure KeyWord :
sig val find : string ->
                (int * int -> (svalue,int) token) option
end =
struct
 val TableSize = 422
 val HashFactor = 5
 val hash = fn
     s => List.foldr (fn (c,v) =>
           (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
 val HashTable = Array.array(TableSize,nil) :
              (string * (int * int -> (svalue,int) token))
              list Array.array
 val add = fn
      (s,v) => let val i = hash s
               in Array.update(HashTable,i,(s,v)
                  :: (Array.sub(HashTable,i)))
               end
 val find = fn
     s => let val i = hash s
              fun f ((key,v)::r) = if s=key then SOME v
                                            else f r
                | f nil = NONE
          in f (Array.sub(HashTable,i))
          end
 val _ = (List.app add [
         ("if", T.IF),
         ("else", T.ELSE),
         ("while", T.WHILE),
         ("return", T.RETURN),
         ("skip", T.SKIP),
         ("void", T.VOID),
         ("int", T.INT),
         ("real", T.REAL),
         ("char", T.CHAR),
         ("bool", T.BOOL),
         ("string", T.STRING),
         ("dataset", T.DATASET),
         ("model", T.MODEL)
       ])
end;

open KeyWord;

%%
%full
%header (functor CMLLexFun(structure Tokens: CML_TOKENS));
%arg (fileName:string);
%s CML COMMENT;
D = [0-9];
L = [a-zA-Z_];
A = [a-zA-Z_0-9];
ES = \\[\'\"\\nt];
WS = [\ \t];
EOL = ("\013\010" | "\010" | "\013");
%%

<INITIAL>{WS}* => (lin:=1;eolpos:=0;YYBEGIN CML; continue());
<CML>{WS}* => (continue());
<CML>{EOL} => ( lin:=(!lin)+1;eolpos:=yypos+size yytext; 
                        print( "EOL(yypos=" ^ Int.toString(yypos) ^ ", lin=" ^ Int.toString(!lin) ^ 
                         ", eolpos=" ^ Int.toString(!eolpos) ^ ") " ); continue());

<CML>"/*" => ( YYBEGIN COMMENT; continue() );
<CML>"//".* => ( continue() );
<CML>{D}+ => (col:=yypos-(!eolpos); T.INT_LITERAL(yytext,!lin,!col));
<CML>{D}*"."{D}+ => (col:=yypos-(!eolpos); T.REAL_LITERAL(yytext,!lin,!col));
<CML>{D}+"." => (col:=yypos-(!eolpos); T.REAL_LITERAL(yytext,!lin,!col));	
<CML>("true"|"false") => (col:=yypos-(!eolpos); T.BOOL_LITERAL(yytext,!lin,!col));	
<CML>\'([^\'\\\n]|{ES})\' => (col:=yypos-(!eolpos); T.CHAR_LITERAL(yytext,!lin,!col));
<CML>\"([^\"\\\n]|{ES})*\" => (col:=yypos-(!eolpos); T.STRING_LITERAL(yytext,!lin,!col));
<CML>"||" => (col:=yypos-(!eolpos); T.OR_OP(!lin,!col));
<CML>"&&" => (col:=yypos-(!eolpos); T.AND_OP(!lin,!col));
<CML>"!" => (col:=yypos-(!eolpos); T.NEG_OP(!lin,!col));
<CML>"==" => (col:=yypos-(!eolpos); T.EQ_OP(!lin,!col));
<CML>"!=" => (col:=yypos-(!eolpos); T.NE_OP(!lin,!col));
<CML>"<" => (col:=yypos-(!eolpos); T.LT_OP(!lin,!col));
<CML>"<=" => (col:=yypos-(!eolpos); T.LE_OP(!lin,!col));
<CML>">" => (col:=yypos-(!eolpos); T.GT_OP(!lin,!col));
<CML>">=" => (col:=yypos-(!eolpos); T.GE_OP(!lin,!col));
<CML>"{" => (col:=yypos-(!eolpos); T.LCURLY_BRACKET(!lin,!col));
<CML>"}" => (col:=yypos-(!eolpos); T.RCURLY_BRACKET(!lin,!col));
<CML>"[" => (col:=yypos-(!eolpos); T.LSQUARE_BRACKET(!lin,!col));
<CML>"]" => (col:=yypos-(!eolpos); T.RSQUARE_BRACKET(!lin,!col));
<CML>"(" => (col:=yypos-(!eolpos); T.LPAREN(!lin,!col));
<CML>")" => (col:=yypos-(!eolpos); T.RPAREN(!lin,!col));
<CML>";" => (col:=yypos-(!eolpos); T.SEMICOLON(!lin,!col));
<CML>"," => (col:=yypos-(!eolpos); T.COMMA(!lin,!col));
<CML>"=" => (col:=yypos-(!eolpos); T.EQUALS(!lin,!col));
<CML>"+" => (col:=yypos-(!eolpos); T.PLUS(!lin,!col));
<CML>"-" => (col:=yypos-(!eolpos); T.MINUS(!lin,!col));
<CML>"*" => (col:=yypos-(!eolpos); T.TIMES(!lin,!col));
<CML>"/" => (col:=yypos-(!eolpos); T.DIVIDED(!lin,!col));
<CML>{L}{A}* => (case find yytext of SOME v => (col:=yypos-(!eolpos);
                                               v(!lin,!col))
                 | _ => (col:=yypos-(!eolpos);
                         T.IDENTIFIER(yytext,!lin,!col)));
<CML> . => (col:=yypos-(!eolpos); badCh(fileName,yytext,!lin,!col); T.BOGUS_SYMBOL(!lin,!col));

<COMMENT>"*/" => ( YYBEGIN CML; continue());
<COMMENT> . => (continue());







