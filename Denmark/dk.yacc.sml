functor LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : _TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\008\000\003\000\007\000\004\000\006\000\007\000\005\000\000\000\
\\001\000\001\000\010\000\000\000\
\\001\000\001\000\012\000\000\000\
\\001\000\001\000\018\000\000\000\
\\001\000\005\000\016\000\010\000\009\000\000\000\
\\001\000\006\000\017\000\000\000\
\\001\000\008\000\015\000\000\000\
\\001\000\011\000\013\000\000\000\
\\001\000\012\000\000\000\000\000\
\\024\000\010\000\009\000\000\000\
\\025\000\000\000\
\\026\000\000\000\
\\027\000\000\000\
\\028\000\009\000\021\000\000\000\
\\029\000\000\000\
\\030\000\000\000\
\\031\000\000\000\
\"
val actionRowNumbers =
"\000\000\009\000\015\000\001\000\
\\000\000\002\000\007\000\000\000\
\\006\000\004\000\005\000\003\000\
\\016\000\000\000\012\000\000\000\
\\010\000\013\000\011\000\000\000\
\\014\000\008\000"
val gotoT =
"\
\\001\000\021\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\002\000\003\000\009\000\000\000\
\\000\000\
\\000\000\
\\002\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\017\000\000\000\
\\000\000\
\\002\000\018\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\020\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 22
val numrules = 8
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 11) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "INT_LITERAL"
  | (T 2) => "WHILE"
  | (T 3) => "BEGIN"
  | (T 4) => "END"
  | (T 5) => "DO"
  | (T 6) => "IF"
  | (T 7) => "THEN"
  | (T 8) => "ELSE"
  | (T 9) => "SEMI"
  | (T 10) => "ASSIGN"
  | (T 11) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID stmlist1, stmlist1left, 
stmlist1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  stmlist1 = stmlist1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, stmlist1left, stmlist1right), rest671)

end
|  ( 1, ( ( _, ( _, _, ID2right)) :: _ :: ( _, ( _, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 1, ( result, ID1left, ID2right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID stm1, _, stm1right)) :: _ :: _ :: ( _
, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  stm1 = stm1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, WHILE1left, stm1right), rest671)
end
|  ( 3, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID stmlist1,
 _, _)) :: ( _, ( _, BEGIN1left, _)) :: rest671)) => let val  result =
 MlyValue.ntVOID (fn _ => ( let val  stmlist1 = stmlist1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, BEGIN1left, END1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ntVOID stm1, _, stm1right)) :: _ :: _ :: ( _
, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  stm1 = stm1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, IF1left, stm1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID stm2, _, stm2right)) :: _ :: ( _, ( 
MlyValue.ntVOID stm1, _, _)) :: _ :: _ :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
stm1 = stm1 ()
 val  stm2 = stm2 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, IF1left, stm2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID stm1, stm1left, stm1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
stm1 = stm1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, stm1left, stm1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ntVOID stm1, _, stm1right)) :: _ :: ( _, ( 
MlyValue.ntVOID stmlist1, stmlist1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  stmlist1 = stmlist1 ()
 val  stm1 = stm1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, stmlist1left, stm1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : _TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun INT_LITERAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun BEGIN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
end
end
