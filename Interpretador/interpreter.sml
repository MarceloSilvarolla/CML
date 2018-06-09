structure CML :
sig val parse : string -> DataTypes.Prog
    val run : DataTypes.Prog -> unit
    val interpret : string -> unit
end =
struct
  type environment = Env.environment
  type store = Store.store

  type expressibleValue = ExpressibleValue.expressibleValue
  type returnValue = ExpressibleValue.expressibleValue
  type returnFlag = ReturnFlag.returnFlag

  exception CMLError
  exception EmptyProgram
  exception IdentifierNotAFunction

  fun parse (fileName:string):DataTypes.Prog =
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
  
  and P1(DataTypes.Prog [DataTypes.DecNotFunDef (DataTypes.Dec (typeSpec, DataTypes.Id id, _))])(env,sto):environment*store =
            Dec(DataTypes.Dec (typeSpec, DataTypes.Id id, NONE))(env,sto)
      | P1(DataTypes.Prog [DataTypes.FunDefNotDec funDef])(env, sto):environment*store =
            let
                val env_1 = Def(funDef)(env, sto)
            in
                (env_1,sto)
            end
      | P1(DataTypes.Prog (decOrFunDef::decOrFunDefList))(env,sto):environment*store =
            P1(DataTypes.Prog decOrFunDefList)( P1(DataTypes.Prog [decOrFunDef])(env,sto) )

      | P1(DataTypes.Prog []) (env,sto) = raise EmptyProgram

  and P2(prog:DataTypes.Prog)(env,sto):environment*store =
            P1(prog)(env,sto)

  and P3(DataTypes.Prog [DataTypes.DecNotFunDef (DataTypes.Dec
    (typeSpec, DataTypes.Id id, expOption))])(env,sto):environment*store =
            Dec(DataTypes.Dec (typeSpec, DataTypes.Id id, expOption))(env,sto)
      | P3(DataTypes.Prog [DataTypes.FunDefNotDec funDef])(env, sto):environment*store =
            let
                val env_1 = Def(funDef)(env, sto)
            in
                (env_1,sto)
            end
      | P3(DataTypes.Prog (decOrFunDef::decOrFunDefList))(env,sto):environment*store =
            P3(DataTypes.Prog decOrFunDefList)(P3(DataTypes.Prog [decOrFunDef])(env,sto))
      | P3(DataTypes.Prog []) (env,sto) = raise EmptyProgram

  and Def(DataTypes.FunDef (typeSpec, DataTypes.Id id, [], cmd))(env, sto):environment = 
    let
      fun e() = Env.extend(env, DataTypes.Id id, DenotableValue.Function f)
      and f([], sto) =
        let
          val (sto_1, retFlag, retVal) = c()
        in
          (sto_1,retVal)
        end
      and c() = C(cmd)(e(), sto)
    in
      e()
    end
  and Dec(DataTypes.Dec (typeSpec, DataTypes.Id id, NONE))(env,sto):environment*store =
          let
            val (sto_f,loc) = Store.allocate(sto)
            val env_f = Env.extend(env,DataTypes.Id id, DenotableValue.Location loc)
          in
            (env_f,sto_f)
          end

  |   Dec(DataTypes.Dec (typeSpec, DataTypes.Id id, SOME exp))(env,sto):environment*store =
          let
            val (env_f,sto_1) = Dec(DataTypes.Dec(typeSpec, DataTypes.Id id, NONE)) (env,sto)
            val (sto_f,expVal) = E(DataTypes.AssignExp (DataTypes.Id id, [], exp)) (env_f,sto_1)
            val ExpressibleValue.Int intExpVal = expVal
            val _ = print(Int.toString(intExpVal) ^ "\n")
          in
            (env_f,sto_f)
          end

  and E(DataTypes.LitExp lit)(env,sto):store*expressibleValue =
      (case lit of
        DataTypes.IntLit intLit => (sto, ExpressibleValue.Int (let val SOME intValue = Int.fromString(intLit) in intValue end))
      | DataTypes.RealLit realLit => (sto, ExpressibleValue.Real (let val SOME realValue = Real.fromString(realLit) in realValue end))
      | DataTypes.BoolLit boolLit => (sto, ExpressibleValue.Bool (let val SOME boolValue = Bool.fromString(boolLit) in boolValue end))
      | DataTypes.CharLit charLit => (sto, ExpressibleValue.Char (let val SOME charValue = Char.fromString(charLit) in charValue end))
      | DataTypes.StringLit stringLit => (sto, ExpressibleValue.String stringLit))

  |   E(DataTypes.IdOrArrAccessExp (DataTypes.Id id, [])) (env,sto) = 
        let
          val DenotableValue.Location loc = Env.apply(env,DataTypes.Id id)
          val expVal = Store.storableToExpressible (Store.apply(sto,loc))
        in
          (sto,expVal)
        end

  |   E(DataTypes.AssignExp (DataTypes.Id id, [], exp)) (env,sto) =
          let
            val (sto_exp, expVal) = E(exp)(env,sto)
            val DenotableValue.Location loc = Env.apply(env,DataTypes.Id id)
            val sto_f = Store.update(sto_exp, loc, Store.expressibleToStorable(expVal))
          in
            (sto_f, expVal)
          end

  |   E(DataTypes.AddExp (exp_1, exp_2)) (env,sto) = 
        let 
          val (sto_1, ExpressibleValue.Int intVal_1) = E(exp_1)(env,sto)
          val (sto_f, ExpressibleValue.Int intVal_2) = E(exp_2)(env,sto_1)
        in
          (sto_f,ExpressibleValue.Int (intVal_1 + intVal_2))
        end

  |   E(DataTypes.AppExp (DataTypes.Id id, [])) (env,sto) =
        let 
          val DenotableValue.Function f = Env.apply(env, DataTypes.Id id)
          handle Bind => raise IdentifierNotAFunction
          val (sto_f,retVal) = f([], sto)
        in
          (sto_f, retVal)
        end 
  and C(CompCmd decOrCmdList)(env,sto):store*returnFlag*returnValue = 
    case decOrCmdList of
      [] => (sto,false,ExpressibleValue.VoidValue)
    | [DecNotCmd dec] => Dec(dec)(env,sto)
    | [CmdNotDec cmd] => C(cmd)(env,sto)
    (*| decOrCmd::decOrCmdListTail => *)

  and interpret(fileName:string):unit =
        let val parseTree = parse(fileName)
        in run(parseTree)
        end

end
