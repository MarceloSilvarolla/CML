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
  exception NonIntMainReturn
  exception NonValidTypesOnAritmeticOperation
  exception DivisionByZero
  exception NonBooleanTypeOnLogicOperation
  exception NonBooleanTypeOnLogicOperation
  exception VoidValueInComparison
  exception NonCorrespondingTypesInComparison
  exception NonComparableTypes
  exception NotAnArrayAcess
  exception NotAnIntegerIndex
  exception OutOfRangeIndex

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
            val (env_1, sto_1) = P1(parseTree)(Env.empty, Store.empty)
            val (env_2, sto_2) = P2(parseTree)(env_1, Store.empty)
            val (env_f, sto_3) = P3(parseTree)(env_2, Store.empty)
            val main = Env.apply(env_f, DataTypes.Id "main")
            val (sto_f, ExpressibleValue.Int mainReturn) = E(DataTypes.AppExp (DataTypes.Id "main", []))(env_f, sto_3)
        in
            mainReturn
        end
        (*handle Bind => raise NonIntMainReturn*)

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
      and f([], sto_func) =
        let
          val (sto_1, retFlag, retVal) = c(sto_func)
        in
          (sto_1,retVal)
        end
      and c(sto_cmd) = C(cmd)(e(), sto_cmd)
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
            val (sto_exp, expVal) = E(exp)(env,sto)
            val (sto_dec, loc) = Store.allocate(sto_exp)
            val sto_f = Store.update(sto_dec, loc, Store.expressibleToStorable(expVal))
            val env_f = Env.extend(env, DataTypes.Id id, DenotableValue.Location loc)

            (*val ExpressibleValue.Int intExpVal = expVal
            val _ = print(Int.toString(intExpVal) ^ "\n")*)
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

    |   E(DataTypes.ArrExp expList)(env, sto) =
        (case expList of
            [] => (sto, ExpressibleValue.ArrayValue [])
        |   (exp :: expListTail) =>
                let
                    val (sto_1, expVal) = E(exp)(env,sto)
                    val (sto_2, loc) = Store.allocate(sto_1)
                    val sto_3 = Store.update(sto_2, loc, Store.expressibleToStorable(expVal))
                    val (sto_f, ExpressibleValue.ArrayValue arrTail) = E(DataTypes.ArrExp expListTail)(env, sto_3)
                in
                    (sto_f, ExpressibleValue.ArrayValue (loc :: arrTail))
                end
        )

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
        (case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Int (intVal_1 + intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (Real.fromInt(intVal_1) + realVal_2))
                |   _ => raise NonValidTypesOnAritmeticOperation
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 + Real.fromInt(intVal_2)))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 + realVal_2))
                |   _ => raise NonValidTypesOnAritmeticOperation
                )
            |   _ => raise NonValidTypesOnAritmeticOperation
        )

    |   E(DataTypes.SubExp (exp_1, exp_2)) (env,sto) =
          (case E(exp_1)(env,sto) of
              (sto_1, ExpressibleValue.Int intVal_1) =>
                  (case E(exp_2)(env,sto_1) of
                      (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Int (intVal_1 - intVal_2))
                  |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (Real.fromInt(intVal_1) - realVal_2))
                  |   _ => raise NonValidTypesOnAritmeticOperation
                  )
          |   (sto_1, ExpressibleValue.Real realVal_1) =>
                  (case E(exp_2)(env,sto_1) of
                      (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 - Real.fromInt(intVal_2)))
                  |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 - realVal_2))
                  |   _ => raise NonValidTypesOnAritmeticOperation
                  )
          |   _ => raise NonValidTypesOnAritmeticOperation
          )

    |   E(DataTypes.MultExp (exp_1, exp_2)) (env,sto) =
        (case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Int (intVal_1 * intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (Real.fromInt(intVal_1) * realVal_2))
                |   _ => raise NonValidTypesOnAritmeticOperation
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 * Real.fromInt(intVal_2)))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 * realVal_2))
                |   _ => raise NonValidTypesOnAritmeticOperation
                )
        |   _ => raise NonValidTypesOnAritmeticOperation
        )

    |   E(DataTypes.DivExp (exp_1, exp_2)) (env,sto) =
        ((case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Int (intVal_1 div intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (Real.fromInt(intVal_1) / realVal_2))
                |   _ => raise NonValidTypesOnAritmeticOperation
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 / Real.fromInt(intVal_2)))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 / realVal_2))
                |   _ => raise NonValidTypesOnAritmeticOperation
                )
            |   _ => raise NonValidTypesOnAritmeticOperation
        ) handle Div => raise DivisionByZero)

    |   E(DataTypes.UMinusExp exp) (env, sto) =
        (case E(exp)(env,sto) of
            (sto_f, ExpressibleValue.Int intVal) => (sto_f, ExpressibleValue.Int (~intVal))
        |   (sto_f, ExpressibleValue.Real realVal) => (sto_f, ExpressibleValue.Real (~realVal))
        |   _ => raise NonValidTypesOnAritmeticOperation
        )

    |   E(DataTypes.OrExp (exp_1, exp_2)) (env, sto) =
        (case E(exp_1)(env, sto) of
            (sto_1, ExpressibleValue.Bool boolVal_1) =>
                (case E(exp_2)(env, sto_1) of
                    (sto_f, ExpressibleValue.Bool boolVal_2) => (sto_f, ExpressibleValue.Bool (boolVal_1 orelse boolVal_2))
                |   _ => raise NonBooleanTypeOnLogicOperation
                )
        |   _ => raise NonBooleanTypeOnLogicOperation
        )

    |   E(DataTypes.AndExp (exp_1, exp_2)) (env, sto) =
        (case E(exp_1)(env, sto) of
            (sto_1, ExpressibleValue.Bool boolVal_1) =>
                (case E(exp_2)(env, sto_1) of
                    (sto_f, ExpressibleValue.Bool boolVal_2) => (sto_f, ExpressibleValue.Bool (boolVal_1 andalso boolVal_2))
                |   _ => raise NonBooleanTypeOnLogicOperation
                )
        |   _ => raise NonBooleanTypeOnLogicOperation
        )

    |   E(DataTypes.EqExp (exp_1, exp_2)) (env, sto) =
        (case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.VoidValue) => raise VoidValueInComparison
        |   (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise VoidValueInComparison
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (intVal_1 = intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.==(Real.fromInt(intVal_1), realVal_2)))
                |   _ => raise NonCorrespondingTypesInComparison
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise VoidValueInComparison
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (Real.==(realVal_1, Real.fromInt(intVal_2))))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.==(realVal_1, realVal_2)))
                |   _ => raise NonCorrespondingTypesInComparison
                )
        |   (sto_1, ExpressibleValue.Bool boolVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise VoidValueInComparison
                |   (sto_f, ExpressibleValue.Bool boolVal_2) => (sto_f, ExpressibleValue.Bool (boolVal_1 = boolVal_2))
                |   _ => raise NonCorrespondingTypesInComparison
                )
        |   (sto_1, ExpressibleValue.Char charVal_1) =>
                (case E(exp_2)(env, sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise VoidValueInComparison
                |   (sto_f, ExpressibleValue.Char charVal_2) => (sto_f, ExpressibleValue.Bool (charVal_1 = charVal_2))
                |   _ => raise NonCorrespondingTypesInComparison
                )
        |   (sto_1, ExpressibleValue.String stringVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise VoidValueInComparison
                |   (sto_f, ExpressibleValue.String stringVal_2) => (sto_f, ExpressibleValue.Bool (stringVal_1 = stringVal_2))
                |   _ => raise NonCorrespondingTypesInComparison
                )
        (*TODO: comparações entre datasets, models e arrays?*)
        |   _ => raise NonComparableTypes
        )

    |   E(DataTypes.NeExp (exp_1, exp_2)) (env, sto) =
        let
            val (sto_f, ExpressibleValue.Bool boolVal) = E(DataTypes.EqExp (exp_1, exp_2))(env, sto)
        in
            (sto_f, ExpressibleValue.Bool (not(boolVal)))
        end

    |   E(DataTypes.LtExp (exp_1, exp_2)) (env, sto) =
        (case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.VoidValue) => raise VoidValueInComparison
        |   (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise VoidValueInComparison
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (intVal_1 < intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.fromInt(intVal_1) < realVal_2))
                |   _ => raise NonComparableTypes
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise VoidValueInComparison
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 < Real.fromInt(intVal_2)))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 < realVal_2))
                |   _ => raise NonComparableTypes
                )
        |   _ => raise NonComparableTypes
        )

    |   E(DataTypes.LeExp (exp_1, exp_2)) (env, sto) =
        (case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.VoidValue) => raise VoidValueInComparison
        |   (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise VoidValueInComparison
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (intVal_1 <= intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.fromInt(intVal_1) <= realVal_2))
                |   _ => raise NonComparableTypes
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise VoidValueInComparison
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 <= Real.fromInt(intVal_2)))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 <= realVal_2))
                |   _ => raise NonComparableTypes
                )
        |   _ => raise NonComparableTypes
        )

    |   E(DataTypes.GtExp (exp_1, exp_2)) (env, sto) =
        let
            val (sto_f, ExpressibleValue.Bool boolVal) = E(DataTypes.LeExp (exp_1, exp_2))(env,sto)
        in
            (sto_f, ExpressibleValue.Bool (not(boolVal)))
        end

    |   E(DataTypes.GeExp (exp_1, exp_2)) (env, sto) =
        let
            val (sto_f, ExpressibleValue.Bool boolVal) = E(DataTypes.LtExp (exp_1, exp_2))(env,sto)
        in
            (sto_f, ExpressibleValue.Bool (not(boolVal)))
        end

    |   E(DataTypes.NegExp exp) (env, sto) =
        (case E(exp)(env, sto) of
            (sto_f, ExpressibleValue.Bool boolVal) => (sto_f, ExpressibleValue.Bool (not(boolVal)))
        |   _ => raise NonBooleanTypeOnLogicOperation)

    |   E(DataTypes.ParenExp exp) (env, sto) = E(exp)(env, sto)


    |   E(DataTypes.IdOrArrAccessExp (DataTypes.Id id, expList)) (env, sto) =
        let
            val DenotableValue.Location loc = Env.apply(env, DataTypes.Id id)

            fun getElementAt([], pos) = raise OutOfRangeIndex
            |   getElementAt(arr, pos) = if pos = 0 then hd(arr) else getElementAt(tl(arr), pos-1)

            fun applyArray(loc, expList, sto) =
                (case expList of
                    [] => (sto, Store.storableToExpressible(Store.apply(sto, loc)))
                |   (exp :: expListTail) =>
                        (case Store.apply(sto, loc) of
                            StorableValue.ArrayValue arr =>
                                (case E(exp)(env, sto) of
                                    (sto_exp, ExpressibleValue.Int pos) =>
                                        applyArray(getElementAt(arr, pos), expListTail, sto_exp)
                                |   _ => raise NotAnIntegerIndex
                                )
                        |   _ => raise NotAnArrayAcess
                        )
                )
        in
            applyArray(loc, expList, sto)
        end


  |   E(DataTypes.AppExp (DataTypes.Id id, [])) (env,sto) =
        let
          val DenotableValue.Function f = Env.apply(env, DataTypes.Id id)
          handle Bind => raise IdentifierNotAFunction
          val (sto_f,retVal) = f([], sto)

          (*val ExpressibleValue.Int intRetVal = retVal
          val _ = print("intRetVal = " ^ Int.toString(intRetVal) ^ "\n")*)
        in
          (sto_f, retVal)
        end

  and C(DataTypes.CompCmd decOrCmdList)(env,sto):store*returnFlag*returnValue =
    let
      fun sequence((DataTypes.DecNotCmd dec) :: decOrCmdListTail)(env,sto) =
        sequence(decOrCmdListTail) (Dec(dec)(env,sto))
      |   sequence((DataTypes.CmdNotDec cmd) :: decOrCmdListTail)(env,sto) =
        let
          val (sto_1, retFlag, retVal) = C(cmd)(env,sto)
        in
          if retFlag then (env,sto_1,true,retVal) else sequence(decOrCmdListTail)(env,sto_1)
        end
      |   sequence([])(env,sto) = (env,sto,false,ExpressibleValue.VoidValue)
      val (env_f,sto_f,retFlag,retVal) = sequence(decOrCmdList)(env,sto)
    in
      (sto_f, retFlag, retVal)
    end

  |   C(DataTypes.ExpCmd expOption)(env,sto) =
    (case expOption of
      SOME exp =>
        let val (sto_f,expVal) = E(exp)(env,sto)
        in
          (Store.printStore(sto_f); (sto_f,false,ExpressibleValue.VoidValue))
        end
    | NONE => (sto,false,ExpressibleValue.VoidValue)

    )

  |   C(DataTypes.JumpCmd expOption)(env,sto) =
    (case expOption of
      NONE => (sto,true,ExpressibleValue.VoidValue)
    | SOME exp =>
        let
          val (sto_f, retVal) = E(exp)(env,sto)
        in
          (sto_f, true, retVal)
        end
    )

  |   C(DataTypes.SelCmd (exp, cmd, NONE))(env,sto) =
    let
      val (sto_exp, ExpressibleValue.Bool exp_val) = E(exp)(env,sto)
    in
      if exp_val then C(cmd)(env,sto_exp) else (sto_exp,false,ExpressibleValue.VoidValue)
    end

  |   C(DataTypes.SelCmd (exp, cmd_1, SOME cmd_2))(env,sto) =
    let
      val (sto_exp, ExpressibleValue.Bool exp_val) = E(exp)(env,sto)
    in
      if exp_val then C(cmd_1)(env,sto_exp) else C(cmd_2)(env,sto_exp)
    end


    |   C(DataTypes.IterCmd (exp, cmd)) (env,sto) =
        let
            fun loop(sto_loop, retFlag, retValue) =
                if retFlag then
                    (sto_loop, true, retValue)
                else
                    (case E(exp)(env, sto_loop) of
                        (sto_exp, ExpressibleValue.Bool b) =>
                            if b then
                                let
                                    val (sto_f, retFlag, retValue) = C(cmd)(env, sto_exp)
                                in
                                    loop(sto_f, retFlag, retValue)
                                end
                            else
                                (sto_exp, false, ExpressibleValue.VoidValue)
                    |   _ => raise NonBooleanTypeOnLogicOperation
                    )
        in
            loop(sto, false, ExpressibleValue.VoidValue)
        end


  |   C(DataTypes.Skip)(env,sto) = (sto, false, ExpressibleValue.VoidValue)

  and interpret(fileName:string):unit =
        let val parseTree = parse(fileName)
        in run(parseTree)
        end

end
