structure CML :
sig val parse : string -> DataTypes.Prog
    val run : DataTypes.Prog -> unit
    val interpret : string -> unit
end =
struct
  exception CMLError;
  fun parse (fileName:string):DataTypes.Prog=
  let val inStream = TextIO.openIn fileName;
      val grab : int -> string = fn
          n => if TextIO.endOfStream inStream
               then ""
               else TextIO.inputN (inStream,n);
      val printError : string * int * int -> unit = fn
          (msg,line,col) =>
           print(fileName^"["^Int.toString line^":"
                 ^Int.toString col^"] "^msg^"\n");
      val _ = Compiler.Control.Print.printDepth:=50;
      val (tree, rem) = CMLParser.parse
            (15,
            (CMLParser.makeLexer grab fileName),
            printError,
            fileName)
          handle CMLParser.ParseError => raise CMLError;
      val _ = TextIO.closeIn inStream;
  in tree
  end
  and run(parseTree:DataTypes.Prog):unit =
     let val _ = P(parseTree)
     in ()
     end
  and P(parseTree:DataTypes.Prog):int = ()
  and P1(parseTree:DataTypes.Prog):int = ()
  and interpret(fileName:string):unit =
    let val parseTree = parse(fileName)
    in run(parseTree)
    end

end;
