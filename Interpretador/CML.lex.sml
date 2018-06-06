functor CMLLexFun(structure Tokens: CML_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
CML | COMMENT | INITIAL
    structure UserDeclarations = 
      struct

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
    TextIO.output(TextIO.stdOut,fileName^"["
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



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as (fileName:string)) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lin:=1;eolpos:=0;YYBEGIN CML; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lin:=(!lin)+1;eolpos:=yypos+size yytext;continue())
      end
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      ( YYBEGIN COMMENT; continue() ))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm; ( continue() ))
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (case find yytext of SOME v => (col:=yypos-(!eolpos);
                                               v(!lin,!col))
                 | _ => (col:=yypos-(!eolpos);
                         T.IDENTIFIER(yytext,!lin,!col)))
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=yypos-(!eolpos); T.INT_LITERAL(yytext,!lin,!col))
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos); T.REAL_LITERAL(yytext,!lin,!col))
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos); T.REAL_LITERAL(yytext,!lin,!col))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos); T.BOOL_LITERAL(yytext,!lin,!col))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos); T.CHAR_LITERAL(yytext,!lin,!col))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos); T.STRING_LITERAL(yytext,!lin,!col))
      end
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.OR_OP(!lin,!col)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.AND_OP(!lin,!col)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.NEG_OP(!lin,!col)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.EQ_OP(!lin,!col)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.NE_OP(!lin,!col)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.LT_OP(!lin,!col)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.LE_OP(!lin,!col)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.GT_OP(!lin,!col)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.GE_OP(!lin,!col)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.LCURLY_BRACKET(!lin,!col)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.RCURLY_BRACKET(!lin,!col)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.LSQUARE_BRACKET(!lin,!col)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.RSQUARE_BRACKET(!lin,!col)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.LPAREN(!lin,!col)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.RPAREN(!lin,!col)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.SEMICOLON(!lin,!col)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.COMMA(!lin,!col)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.EQUALS(!lin,!col)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.PLUS(!lin,!col)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.MINUS(!lin,!col)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TIMES(!lin,!col)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.DIVIDED(!lin,!col)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      ( YYBEGIN CML; continue()))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ55(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp <= #"\b"
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ61(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ61(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp <= #"\b"
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ61(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ61(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyQ63(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyQ62(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ61(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yyAction0(strm, yyNO_MATCH)
                else if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ61(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ60(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ55(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyQ58(strm', lastMatch)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyQ55(strm', lastMatch)
                  else yyQ57(strm', lastMatch)
            else if inp = #"*"
              then yyQ59(strm', lastMatch)
              else yyQ57(strm', lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ32(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"`"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"`"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ36(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ35(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"`"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ34(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ35(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ38(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"`"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ39(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ41(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ44(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"0"
              then yyAction6(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ44(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ44(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"0"
              then yyAction7(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ44(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ42(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ43(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ42(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ43(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction3(strm, yyNO_MATCH)
              else yyQ46(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"+"
              then yyAction32(strm, yyNO_MATCH)
            else if inp < #"+"
              then if inp = #"*"
                  then yyQ45(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp = #"/"
              then yyQ46(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ44(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp < #"0"
              then yyAction34(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ44(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyQ47(strm', lastMatch)
            else if inp < #"\\"
              then if inp = #"#"
                  then yystuck(lastMatch)
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ47(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"'"
                  then yyQ47(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"o"
              then yystuck(lastMatch)
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ47(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"t"
              then yyQ47(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #"'"
              then if inp = #"\n"
                  then yyAction34(strm, yyNO_MATCH)
                  else yyQ47(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ48(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyQ47(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ50(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyQ51(strm', lastMatch)
            else if inp < #"\\"
              then if inp = #"#"
                  then yystuck(lastMatch)
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ51(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"'"
                  then yyQ51(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"o"
              then yystuck(lastMatch)
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ51(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"t"
              then yyQ51(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ52(strm', lastMatch)
            else if inp < #"\""
              then if inp = #"\n"
                  then yystuck(lastMatch)
                  else yyQ51(strm', lastMatch)
            else if inp = #"\\"
              then yyQ53(strm', lastMatch)
              else yyQ51(strm', lastMatch)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ52(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp < #"\""
              then if inp = #"\n"
                  then yyAction34(strm, yyNO_MATCH)
                  else yyQ51(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ53(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyQ51(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ54(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ55(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp <= #"\b"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ56(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #" "
              then yyQ56(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp <= #"\b"
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ56(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #" "
              then yyQ56(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp <= #"\b"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ56(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #" "
              then yyQ56(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #":"
              then if inp = #"&"
                  then yyQ9(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"&"
                  then if inp = #"\^N"
                      then yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                    else if inp < #"\^N"
                      then if inp = #"\n"
                          then yyQ5(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ4(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                              else yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                        else if inp = #"\r"
                          then yyQ6(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                          else yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                    else if inp = #"!"
                      then yyQ7(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                    else if inp < #"!"
                      then if inp = #" "
                          then yyQ4(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                          else yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                    else if inp = #"\""
                      then yyQ8(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #","
                  then yyQ15(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #","
                  then if inp = #")"
                      then yyQ12(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                    else if inp < #")"
                      then if inp = #"'"
                          then yyQ10(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                          else yyQ11(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                    else if inp = #"*"
                      then yyQ13(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyQ14(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ18(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"/"
                  then if inp = #"-"
                      then yyQ16(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyQ17(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ19(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"_"
              then yyQ24(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ24(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"="
                      then yyQ22(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                    else if inp < #"="
                      then if inp = #";"
                          then yyQ20(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                          else yyQ21(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                    else if inp = #">"
                      then yyQ23(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"\\"
                  then if inp = #"["
                      then yyQ25(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyQ24(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #"]"
                  then yyQ26(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"u"
              then yyQ24(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"f"
                  then yyQ27(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"f"
                  then if inp = #"`"
                      then yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyQ24(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #"t"
                  then yyQ28(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ24(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"|"
              then yyQ30(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"|"
              then if inp = #"{"
                  then yyQ29(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ24(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"}"
              then yyQ31(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyQ3(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
      (* end case *))
in
  (case (!(yyss))
   of CML => yyQ0(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
