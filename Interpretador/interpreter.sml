structure CML :
sig val parse : string -> DataTypes.Prog
    val run : DataTypes.Prog -> unit
    val interpret : string -> unit
end =
struct
  type environment = Env.environment
  type store = Store.store

  type expressibleValue = ExpressibleValue.expressibleValue
  type returnValue = ReturnValue.returnValue
  type returnFlag = ReturnFlag.returnFlag

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
    and P(parseTree:DataTypes.Prog):int =
        let
            val (env, sto) = P3(parseTree) (P2(parseTree) (P1(parseTree)(Env.empty, Store.empty)))
            val main = Env.apply(env, DataTypes.Id "main")
            val (sto_f, ExpressibleValue.Int mainReturn) = E(DataTypes.AppExp (DataTypes.Id "main", []))(env, sto)
        in
            mainReturn
        end

    and P1(DataTypes.Prog [DataTypes.DecNotFunDef (DataTypes.Dec (typeSpec:DataTypes.TypeSpec, DataTypes.Id id, _))])(env:environment,sto:store):environment*store =
            Dec(DataTypes.Dec (typeSpec, DataTypes.Id id, NONE))(env,sto)
      | P1(DataTypes.Prog [DataTypes.FunDefNotDec funDef])(env:environment, sto:store):environment*store =
            let
                val env_1 = Def(funDef)(env, sto)
            in
                (env_1,sto)
            end
      | P1(DataTypes.Prog (decOrFunDef::decOrFunDefList))(env:environment,sto:store):environment*store =
            P1(DataTypes.Prog decOrFunDefList)(P1(DataTypes.Prog [decOrFunDef])(env,sto))

    and P2(prog:DataTypes.Prog)(env:environment,sto:store):environment*store =
            P1(prog:DataTypes.Prog)(env,sto)

    and P3(DataTypes.Prog [DataTypes.DecNotFunDef (DataTypes.Dec (typeSpec:DataTypes.TypeSpec, DataTypes.Id id, exp:(DataTypes.Exp option)))])(env:environment,sto:store):environment*store =
            Dec(DataTypes.Dec (typeSpec, DataTypes.Id id, exp))(env,sto)
      | P3(DataTypes.Prog [DataTypes.FunDefNotDec funDef])(env:environment, sto:store):environment*store =
            let
                val env_1 = Def(funDef)(env, sto)
            in
                (env_1,sto)
            end
      | P3(DataTypes.Prog (decOrFunDef::decOrFunDefList))(env:environment,sto:store):environment*store =
            P3(DataTypes.Prog decOrFunDefList)(P3(DataTypes.Prog [decOrFunDef])(env,sto))

    and Def(DataTypes.FunDef (typeSpec:DataTypes.TypeSpec, DataTypes.Id id, params, cmd))(env:environment, sto:store):environment = env

    and Dec(DataTypes.Dec (typeSpec:DataTypes.TypeSpec, DataTypes.Id id, exp:(DataTypes.Exp option)))(env:environment,sto:store):environment*store = (env,sto)

    and E(exp:DataTypes.Exp)(env:environment,sto:store):store*expressibleValue = (sto, ExpressibleValue.Int 42)

    and C(cmd)(env:environment,sto:store):store*returnFlag*returnValue = (sto, false, ReturnValue.ExpressibleValue (ExpressibleValue.Int 1337))

    and interpret(fileName:string):unit =
        let val parseTree = parse(fileName)
        in run(parseTree)
    end

end;
