structure CML :
sig val parse : string -> DataTypes.Prog
    val typeCheck : DataTypes.Prog -> LocalTypeEnv.environment * GlobalTypeEnv.environment
    val run : DataTypes.Prog -> unit
    val interpret : string -> unit
    val verbosity : int ref
end =
struct
  type environment = Env.environment
  type store = Store.store

  type expressibleValue = ExpressibleValue.expressibleValue
  type returnValue = ExpressibleValue.expressibleValue
  type returnFlag = ReturnFlag.returnFlag

  val verbosity = ref 0

  exception ParseError
  exception NonFunctionApplication
  exception NonFunctionApplicationBug
  exception NonIntMainReturn
  exception InvalidTypeInArithmeticOperation
  exception InvalidTypeInArithmeticOperationBug
  exception DivisionByZero
  exception InvalidTypeInLogicalOperation
  exception InvalidTypeInLogicalOperationBug
  exception IncomparableTypes
  exception IncomparableTypesBug
  exception InvalidTypeInComparison
  exception InvalidTypeInComparisonBug
  exception TooManyIndicesInArrayAccess
  exception TooManyIndicesInArrayAccessBug
  exception InvalidTypeInArrayAccessIndex
  exception InvalidTypeInArrayAccessIndexBug
  exception OutOfRangeIndex
  exception DefBug
  exception ArrayElementsOfInconsistentTypes
  (* 1: parsing (using MLLex and MLYacc) *)
  fun parse (fileName:string):DataTypes.Prog =
  let
    val inStream = TextIO.openIn fileName
    val grab : int -> string = fn
        n => if TextIO.endOfStream inStream
             then ""
             else TextIO.inputN (inStream,n);
    val printError : string * int * int -> unit = fn
        (msg,line,col) =>
         (print("Source file " ^ fileName ^ " contains error at line " ^ Int.toString line ^ ": " ^ msg^"\n"); raise ParseError)
    (*val _ = Compiler.Control.Print.printDepth:=50;*)
    val (tree, rem) = CMLParser.parse
          (15,
          (CMLParser.makeLexer grab fileName),
          printError,
          fileName)
      (*handle CMLParser.ParseError => raise ParseError*)
    val _ = TextIO.closeIn inStream;
  in tree
  end
  (* 2: type-checking *)
  and typeCheck(parseTree:DataTypes.Prog):LocalTypeEnv.environment * GlobalTypeEnv.environment =
    typeCheckP(parseTree)

  and typeCheckP(prog:DataTypes.Prog):LocalTypeEnv.environment * GlobalTypeEnv.environment =
    let
      val (localFuncTypeEnv, globalFuncTypeEnv) = typeCheckP1(prog)(LocalTypeEnv.initial, GlobalTypeEnv.initial)
      val (finalLocalTypeEnv, finalGlobalTypeEnv) = typeCheckP2(prog)(localFuncTypeEnv, globalFuncTypeEnv)
    in
      (finalLocalTypeEnv, finalGlobalTypeEnv)
    end
  (* 2.1: type-checking function definitions and adding them to the type environment *)
  and typeCheckP1((DataTypes.Prog []) : DataTypes.Prog)(localEnv, globalEnv) = (localEnv, globalEnv)
  |   typeCheckP1(DataTypes.Prog ( (DataTypes.FunDefNotDec (DataTypes.FunDef (typeSpec, DataTypes.Id id, params, cmd))) :: progTail) )
   (localEnv, globalEnv) =
    let
      val returnSort = Sort.typeSpecSort(typeSpec)
      val inputSort = Sort.Product (
        map
        (fn DataTypes.Dec (typeSpec, _, _) => Sort.typeSpecSort(typeSpec))
        params)
      val functionSort = Sort.To (inputSort, returnSort)
      val newLocalEnv = LocalTypeEnv.extend(localEnv, DataTypes.Id id, functionSort)
      val newGlobalEnv = GlobalTypeEnv.extend(globalEnv, DataTypes.Id id, functionSort)
    in
      typeCheckP1(DataTypes.Prog progTail)(newLocalEnv, newGlobalEnv)
    end
  |   typeCheckP1(DataTypes.Prog (_:: progTail))(localEnv, globalEnv) = typeCheckP1(DataTypes.Prog progTail)(localEnv, globalEnv)
  (* 2.2: type-checking everything using function types from step 2.1 *)
  and typeCheckP2(DataTypes.Prog [])(localEnv, globalEnv) = (localEnv, globalEnv)

  |   typeCheckP2(DataTypes.Prog ((DataTypes.DecNotFunDef dec) :: progTail))(localEnv, globalEnv) =
    let
      val (newLocalEnv, newGlobalEnv) = typeCheckDec(dec)(localEnv, globalEnv)
    in
      typeCheckP2(DataTypes.Prog progTail)(newLocalEnv, newGlobalEnv)
    end

  |   typeCheckP2( DataTypes.Prog ((DataTypes.FunDefNotDec funDef) :: progTail) )(localEnv, globalEnv) =
    let
      val (newLocalEnv, newGlobalEnv) = typeCheckDef(funDef)(LocalTypeEnv.initial, globalEnv)
    in
      typeCheckP2(DataTypes.Prog progTail)(newLocalEnv, newGlobalEnv)
    end

  and typeCheckDec(DataTypes.Dec (typeSpec, DataTypes.Id id, NONE))(localEnv, globalEnv) =
    let
      val newLocalEnv = LocalTypeEnv.extend(localEnv, DataTypes.Id id, Sort.typeSpecSort(typeSpec))
      val newGlobalEnv =  GlobalTypeEnv.extend(globalEnv, DataTypes.Id id, Sort.typeSpecSort(typeSpec))
    in
      (newLocalEnv, newGlobalEnv)
    end

  |   typeCheckDec(DataTypes.Dec (typeSpec, DataTypes.Id id, SOME exp))(localEnv, globalEnv) =
    let
      val expSrt = typify(exp)(globalEnv)
      val typeSpecSrt = Sort.typeSpecSort(typeSpec)
      val newLocalEnv = LocalTypeEnv.extend(localEnv, DataTypes.Id id, typeSpecSrt)
      val newGlobalEnv =  GlobalTypeEnv.extend(globalEnv, DataTypes.Id id, typeSpecSrt)
      val _ = Sort.commonSort(expSrt, typeSpecSrt)
    in
      (newLocalEnv, newGlobalEnv)
    end
  and  typeCheckDecList(decList)(localEnv, globalEnv) =
    foldl (fn (dec, (localEnv, globalEnv)) => typeCheckDec(dec)(localEnv, globalEnv)) (localEnv, globalEnv) decList
  and typeCheckDef(DataTypes.FunDef (typeSpec, _, params, cmd)) (localEnv, globalEnv) =
    let
      val (newLocalEnv, newGlobalEnv) = typeCheckDecList(params)(localEnv, globalEnv)
      val _ = typeCheckC(cmd)(newLocalEnv, newGlobalEnv)(Sort.typeSpecSort(typeSpec))
    in
      (localEnv, globalEnv)
    end

  and typeCheckDecOrCmd(DataTypes.DecNotCmd dec)(localEnv, globalEnv) (returnSrt) = typeCheckDec(dec) (localEnv, globalEnv)
  |   typeCheckDecOrCmd(DataTypes.CmdNotDec cmd)(localEnv, globalEnv) (returnSrt) = typeCheckC(cmd)(localEnv, globalEnv)(returnSrt)

  and typeCheckC(DataTypes.CompCmd (decOrCmdList))(localEnv, globalEnv)(returnSrt) =
    let val _ = (foldl
      (fn (decOrCmd, (localEnv, globalEnv)) => typeCheckDecOrCmd(decOrCmd) (localEnv, globalEnv) (returnSrt))
      (LocalTypeEnv.initial, globalEnv)
      decOrCmdList)
    in
      (localEnv, globalEnv)
    end
  |   typeCheckC(DataTypes.ExpCmd NONE)(localEnv, globalEnv)(returnSrt) = (localEnv, globalEnv)
  |   typeCheckC(DataTypes.ExpCmd (SOME exp))(localEnv, globalEnv)(returnSrt) = (typify(exp)(globalEnv);(localEnv, globalEnv))
  |   typeCheckC(DataTypes.SelCmd (exp, cmd, NONE)) (localEnv, globalEnv)(returnSrt) =
    let
      val _ = (case typify(exp)(globalEnv) of
        Sort.Bool => () | _ => raise InvalidTypeInLogicalOperation)
      val _ = typeCheckC(cmd)(localEnv, globalEnv)(returnSrt)
    in
      (localEnv, globalEnv)
    end
  |   typeCheckC(DataTypes.SelCmd (exp, cmd_1, SOME cmd_2)) (localEnv, globalEnv)(returnSrt) =
     let
      val _ = (case typify(exp)(globalEnv) of
        Sort.Bool => () | _ => raise InvalidTypeInLogicalOperation)
      val _ = typeCheckC(cmd_1)(localEnv, globalEnv)(returnSrt)
      val _ = typeCheckC(cmd_2)(localEnv, globalEnv)(returnSrt)
    in
      (localEnv, globalEnv)
    end
  |   typeCheckC(DataTypes.IterCmd (exp, cmd))(localEnv, globalEnv)(returnSrt) =
    let
      val _ = (case typify(exp)(globalEnv) of
        Sort.Bool => () | _ => raise InvalidTypeInLogicalOperation)
      val _ = typeCheckC(cmd)(localEnv, globalEnv)(returnSrt)
    in
      (localEnv, globalEnv)
    end
  |   typeCheckC(DataTypes.JumpCmd (NONE)) (localEnv, globalEnv) (returnSrt) = (localEnv, globalEnv)
  |   typeCheckC(DataTypes.JumpCmd (SOME exp)) (localEnv, globalEnv) (returnSrt) =
    let
      val expSrt = typify(exp)(globalEnv)
      val _ = Sort.commonSort(expSrt, returnSrt)
    in
      (localEnv, globalEnv)
    end

  |   typeCheckC(DataTypes.Skip) (localEnv, globalEnv) (returnSrt) = (localEnv, globalEnv)

  and typify(DataTypes.LitExp litExp)(globalEnv):Sort.sort  =
    (case litExp of
      DataTypes.IntLit intLit => Sort.Int
    | DataTypes.RealLit realLit => Sort.Real
    | DataTypes.BoolLit realLit => Sort.Bool
    | DataTypes.CharLit realLit => Sort.Char
    | DataTypes.StringLit realLit => Sort.String
    )

  |   typify(DataTypes.ArrExp arrExp)(globalEnv):Sort.sort =
   (let
     val expSorts = map (fn exp => typify(exp)(globalEnv)) arrExp
   in
     Sort.Array (Sort.commonSortList(expSorts))
   end
   handle Sort.InconsistentSorts => raise ArrayElementsOfInconsistentTypes
   )

  |   typify(DataTypes.OrExp (exp_1, exp_2))(globalEnv):Sort.sort =
    (let
      val sort_1 = typify(exp_1)(globalEnv)
      val sort_2 = typify(exp_2)(globalEnv)
      val _ = (case Sort.commonSort(sort_1, sort_2) of Sort.Bool => ()
              | _ => raise InvalidTypeInLogicalOperation)
      handle Sort.InconsistentSorts => raise InvalidTypeInLogicalOperation
    in
      Sort.Bool
    end)

   |   typify(DataTypes.AndExp (exp_1, exp_2))(globalEnv):Sort.sort =
    (let
      val sort_1 = typify(exp_1)(globalEnv)
      val sort_2 = typify(exp_2)(globalEnv)
      val _ = (case Sort.commonSort(sort_1, sort_2) of Sort.Bool => ()
              | _ => raise InvalidTypeInLogicalOperation)
      handle Sort.InconsistentSorts => raise InvalidTypeInLogicalOperation
    in
      Sort.Bool
    end)

  |   typify(DataTypes.NegExp exp_1)(globalEnv):Sort.sort =
    (let
      val sort_1 = typify(exp_1)(globalEnv)
      val _ = (case sort_1 of Sort.Bool => () | _ => raise InvalidTypeInLogicalOperation)
    in
      Sort.Bool
    end)

  |   typify(DataTypes.EqExp (exp_1, exp_2))(globalEnv):Sort.sort =
    (let
      val sort_1 = typify(exp_1)(globalEnv)
      val sort_2 = typify(exp_2)(globalEnv)
      val _ =
        (case (sort_1, sort_2) of
          (Sort.Int, Sort.Int) => ()
        | (Sort.Int, Sort.Real) => ()
        | (Sort.Real, Sort.Int) => ()
        | (Sort.Real, Sort.Real) => ()
        | (Sort.Char, Sort.Char) => ()
        | (Sort.Bool, Sort.Bool) => ()
        | (Sort.String, Sort.String) => ()
        | (Sort.Dataset, _) => raise InvalidTypeInComparison
        | (_, Sort.Dataset) => raise InvalidTypeInComparison
        | (Sort.Model, _) => raise InvalidTypeInComparison
        | (_, Sort.Model) => raise InvalidTypeInComparison
        | (Sort.Array _, _) => raise InvalidTypeInComparison
        | (_, Sort.Array _) => raise InvalidTypeInComparison
        | (Sort.Void, _) => raise InvalidTypeInComparison
        | (_, Sort.Void) => raise InvalidTypeInComparison
        | (Sort.Unbound, _) => raise InvalidTypeInComparisonBug
        | (_, Sort.Unbound) => raise InvalidTypeInComparisonBug
        | (Sort.Any, _) => raise InvalidTypeInComparisonBug
        | (_, Sort.Any) => raise InvalidTypeInComparisonBug
        | (Sort.Product _, _) => raise InvalidTypeInComparisonBug
        | (_, Sort.Product _) => raise InvalidTypeInComparisonBug
        | (Sort.To _, _) => raise InvalidTypeInComparisonBug
        | (_, Sort.To _) => raise InvalidTypeInComparisonBug
        | _ => raise IncomparableTypes
        )
    in
      Sort.Bool
    end)

  |   typify(DataTypes.NeExp (exp_1, exp_2))(globalEnv):Sort.sort =
    typify(DataTypes.EqExp (exp_1, exp_2)) globalEnv

  |   typify(DataTypes.LtExp (exp_1, exp_2))(globalEnv):Sort.sort =
    (let
      val sort_1 = typify(exp_1)(globalEnv)
      val sort_2 = typify(exp_2)(globalEnv)
      val _ =
        (case (sort_1, sort_2) of
          (Sort.Int, Sort.Int) => ()
        | (Sort.Int, Sort.Real) => ()
        | (Sort.Real, Sort.Int) => ()
        | (Sort.Real, Sort.Real) => ()
        | (Sort.Char, Sort.Char) => ()
        | (Sort.Bool, Sort.Bool) => raise InvalidTypeInComparison
        | (Sort.String, Sort.String) => ()
        | (Sort.Dataset, _) => raise InvalidTypeInComparison
        | (_, Sort.Dataset) => raise InvalidTypeInComparison
        | (Sort.Model, _) => raise InvalidTypeInComparison
        | (_, Sort.Model) => raise InvalidTypeInComparison
        | (Sort.Array _, _) => raise InvalidTypeInComparison
        | (_, Sort.Array _) => raise InvalidTypeInComparison
        | (Sort.Void, _) => raise InvalidTypeInComparison
        | (_, Sort.Void) => raise InvalidTypeInComparison
        | (Sort.Unbound, _) => raise InvalidTypeInComparisonBug
        | (_, Sort.Unbound) => raise InvalidTypeInComparisonBug
        | (Sort.Any, _) => raise InvalidTypeInComparisonBug
        | (_, Sort.Any) => raise InvalidTypeInComparisonBug
        | (Sort.Product _, _) => raise InvalidTypeInComparisonBug
        | (_, Sort.Product _) => raise InvalidTypeInComparisonBug
        | (Sort.To _, _) => raise InvalidTypeInComparisonBug
        | (_, Sort.To _) => raise InvalidTypeInComparisonBug
        | _ => raise IncomparableTypes
        )
    in
      Sort.Bool
    end)


 |   typify(DataTypes.LeExp (exp_1, exp_2))(globalEnv):Sort.sort =
    typify(DataTypes.LtExp (exp_1, exp_2)) globalEnv

  |   typify(DataTypes.GtExp (exp_1, exp_2))(globalEnv):Sort.sort =
    typify(DataTypes.LtExp (exp_1, exp_2)) globalEnv

  |   typify(DataTypes.GeExp (exp_1, exp_2))(globalEnv):Sort.sort =
    typify(DataTypes.LtExp (exp_1, exp_2)) globalEnv

  |   typify(DataTypes.IdOrArrAccessExp (DataTypes.Id id, expList)) (globalEnv) =
    let
      val _ =
        (case Sort.commonSortList(map (fn exp => typify(exp)(globalEnv)) expList) of
          Sort.Int => ()
        | Sort.Any => ()
        | _ => raise InvalidTypeInArrayAccessIndex
        )
      fun arrAccessType(arrSrt, expList) =
        (case (arrSrt, expList) of
          (_, []) => arrSrt
        | (Sort.Array srt, exp::exps) => arrAccessType(srt, exps)
        | _ => raise TooManyIndicesInArrayAccess
        )
    in
      arrAccessType( GlobalTypeEnv.apply(globalEnv, DataTypes.Id id), expList )
    end

  |   typify(DataTypes.AssignExp (DataTypes.Id id, expList, exp)) (globalEnv) =
    let
      val expSrt = typify(exp) (globalEnv)
      val arrAccessSrt = typify(DataTypes.IdOrArrAccessExp (DataTypes.Id id, expList)) (globalEnv)
    in
      Sort.commonSort(expSrt, arrAccessSrt)
    end

  |   typify(DataTypes.UMinusExp exp) (globalEnv) =
    let val expSrt = typify(exp) (globalEnv) in
      case expSrt of
        Sort.Int => expSrt
      | Sort.Real => expSrt
      |  _ => raise InvalidTypeInArithmeticOperation
    end

  |   typify(DataTypes.AddExp (exp_1, exp_2)) (globalEnv) =
     let
       val exp_1Srt = typify(exp_1) (globalEnv)
       val exp_2Srt = typify(exp_2) (globalEnv)
       in
       case (exp_1Srt, exp_2Srt) of
         (Sort.Int, Sort.Int) => (Sort.Int)
       | (Sort.Int, Sort.Real) => (Sort.Real)
       | (Sort.Real, Sort.Int) => (Sort.Real)
       | (Sort.Real, Sort.Real) => (Sort.Real)
       |  _ => raise InvalidTypeInArithmeticOperation
     end

  |   typify(DataTypes.SubExp (exp_1, exp_2)) (globalEnv) =
     let
       val exp_1Srt = typify(exp_1) (globalEnv)
       val exp_2Srt = typify(exp_2) (globalEnv)
       in
       case (exp_1Srt, exp_2Srt) of
         (Sort.Int, Sort.Int) => (Sort.Int)
       | (Sort.Int, Sort.Real) => (Sort.Real)
       | (Sort.Real, Sort.Int) => (Sort.Real)
       | (Sort.Real, Sort.Real) => (Sort.Real)
       |  _ => raise InvalidTypeInArithmeticOperation
     end

  |   typify(DataTypes.MultExp (exp_1, exp_2)) (globalEnv) =
    let
      val exp_1Srt = typify(exp_1) (globalEnv)
      val exp_2Srt = typify(exp_2) (globalEnv)
      in
      case (exp_1Srt, exp_2Srt) of
         (Sort.Int, Sort.Int) => (Sort.Int)
       | (Sort.Int, Sort.Real) => (Sort.Real)
       | (Sort.Real, Sort.Int) => (Sort.Real)
       | (Sort.Real, Sort.Real) => (Sort.Real)
      |  _ => raise InvalidTypeInArithmeticOperation
    end

 |   typify(DataTypes.DivExp (exp_1, exp_2)) (globalEnv) =
    let
      val exp_1Srt = typify(exp_1) (globalEnv)
      val exp_2Srt = typify(exp_2) (globalEnv)
      in
      case (exp_1Srt, exp_2Srt) of
         (Sort.Int, Sort.Int) => (Sort.Int)
       | (Sort.Int, Sort.Real) => (Sort.Real)
       | (Sort.Real, Sort.Int) => (Sort.Real)
       | (Sort.Real, Sort.Real) => (Sort.Real)
      |  _ => raise InvalidTypeInArithmeticOperation
    end
 |   typify(DataTypes.ParenExp exp) (globalEnv) =
   typify(exp) (globalEnv)
 |   typify(DataTypes.AppExp (DataTypes.Id id, expList)) (globalEnv) =
   let
     val functionSrt = GlobalTypeEnv.apply(globalEnv, DataTypes.Id id)
     val argListSrt = Sort.Product (map (fn exp => typify(exp)(globalEnv)) expList)
   in
     case functionSrt of
       Sort.To (paramListSrt, returnSrt) => (Sort.commonSort(argListSrt, paramListSrt); returnSrt)
     | _ => raise NonFunctionApplication
   end

  (* 3: running *)
  and run(parseTree:DataTypes.Prog):unit =
    let val _ = P(parseTree)
    in ()
    end

  and P(parseTree:DataTypes.Prog):int =
        let
            val _ = if !verbosity > 0 then print("Agora é o P1\n") else ()
            val (env_1, sto_1) = P1(parseTree)(Env.initial, Store.empty)
            val _ = if !verbosity > 0 then print("Agora é o P2\n") else ()
            val (env_2, sto_2) = P2(parseTree)(env_1, sto_1)
            val _ = if !verbosity > 0 then print("Agora é o P3\n") else ()
            val (env_f, sto_3) = P3(parseTree)(env_2, sto_2)
            val _ = if !verbosity > 0 then print("Agora é o main\n") else ()
            val main = Env.apply(env_f, DataTypes.Id "main")
            val _ = Env.printEnv(env_f, !verbosity)
            val _ = Store.printStore(sto_3, !verbosity)
            val _ = if !verbosity > 0 then print("Vou executar o main!\n") else ()
            val (sto_f,  originalMainReturn) = E(DataTypes.AppExp (DataTypes.Id "main", []))(env_f, sto_3)
            val _ = case originalMainReturn of ExpressibleValue.Int mainReturn => () | _ => print(Sort.expValSortAsString(originalMainReturn))
            val ExpressibleValue.Int mainReturn = originalMainReturn
        in
            (print("...Program finished with exit code " ^ Int.toString(mainReturn) ^ "\n");mainReturn)
        end
        (*handle Bind => raise NonIntMainReturn*)

  and P1(DataTypes.Prog ((DataTypes.DecNotFunDef (DataTypes.Dec (typeSpec, DataTypes.Id id, _))) :: progTail))(env,sto):environment*store =
            P1(DataTypes.Prog progTail) (Dec(DataTypes.Dec (typeSpec, DataTypes.Id id, NONE)) (env,sto))
      | P1(DataTypes.Prog ((DataTypes.FunDefNotDec funDef) :: progTail))(env, sto) = 
            P1(DataTypes.Prog progTail) (env, sto)
      | P1(DataTypes.Prog []) (env,sto) = (env,sto)

  (*and P2(prog:DataTypes.Prog)(env,sto):environment*store =
            P1(prog)(env,sto)*)
  and P2(DataTypes.Prog prog)(env, sto):environment*store =
      let 
            fun multipleExtends(env, []) = env
            |   multipleExtends(env, (DataTypes.DecNotFunDef (DataTypes.Dec _)) :: progTail) = multipleExtends(env, progTail)
            |   multipleExtends(env, (DataTypes.FunDefNotDec (DataTypes.FunDef (typeSpec, DataTypes.Id id, [], cmd))) :: progTail) = 
                let
                  fun e() = Env.extend(env, DataTypes.Id id, DenotableValue.Function f)
                  and f([], sto_func) =
                    let
                      val (sto_1, retFlag, retVal) = c(sto_func) 
                    in
                      (sto_1, retVal)
                    end
                  |   f(_) = raise DefBug
                  and c(sto_cmd) = C(cmd)(multipleExtends(e(), prog), sto_cmd)
                in
                  multipleExtends(e(), progTail)
                end

            |   multipleExtends(env, (DataTypes.FunDefNotDec (DataTypes.FunDef (typeSpec, DataTypes.Id id, param_dec_list, cmd))) :: progTail) =
                let
                  fun e() = Env.extend(env, DataTypes.Id id, DenotableValue.Function f)
                  and f(arr, sto_func) =
                    let
                      val env_1 = modifyEnv(e(), param_dec_list, arr)
                      val (sto_1, retFlag, retVal) = c(sto_func, env_1)
                    in
                      (sto_1, retVal)
                    end
                  and c(sto_cmd, env) = C(cmd)(multipleExtends(env, prog), sto_cmd)
                  and modifyEnv(env, param_dec_list, arr) =
                    (case param_dec_list of
                      [] => env
                    | (DataTypes.Dec (typeSpec, DataTypes.Id id, NONE)) :: param_dec_list_tail =>
                        let
                          val env_2 = Env.extend(env, DataTypes.Id id, DenotableValue.Location (hd(arr)))
                        in
                          modifyEnv(env_2, param_dec_list_tail, tl(arr))
                        end
                    | _ => raise DefBug
                    )
                in
                  multipleExtends(e(), progTail)
                end
      in
        (multipleExtends(env, prog), sto)
      end

   and P3(DataTypes.Prog ((DataTypes.DecNotFunDef (DataTypes.Dec (typeSpec, DataTypes.Id id, SOME exp))) :: progTail))(env,sto):environment*store =
            let val (sto_exp, expVal) = E(DataTypes.AssignExp (DataTypes.Id id, [], exp)) (env, sto)
            in P3(DataTypes.Prog progTail) (env, sto_exp)
            end
      | P3(DataTypes.Prog (_ :: progTail)) (env, sto) = P3(DataTypes.Prog progTail) (env, sto)
      | P3(DataTypes.Prog []) (env, sto) = (env, sto)

  (*and Def(DataTypes.FunDef (typeSpec, DataTypes.Id id, [], cmd))(env, sto):environment =
    let
      fun e() = Env.extend(env, DataTypes.Id id, DenotableValue.Function f)
      and f([], sto_func) =
        (let
          val (sto_1, retFlag, retVal) = c(sto_func)
        in
          (sto_1,retVal)
        end
        )
      |   f(_) = raise DefBug
      and c(sto_cmd) = C(cmd)(e(), sto_cmd)
    in
      e()
    end

  |   Def(DataTypes.FunDef (typeSpec, DataTypes.Id id, param_dec_list, cmd))(env, sto):environment =
    let
        fun e() = Env.extend(env, DataTypes.Id id, DenotableValue.Function f)
        and f(arr, sto_func) =
            let
                val env_1 = modifyEnv(e(), param_dec_list, arr)
                val (sto_1, retFlag, retVal) = c(sto_func, env_1)
            in
                (sto_1, retVal)
            end
        and c(sto_cmd, env) = C(cmd)(env, sto_cmd)
        and modifyEnv(env, param_dec_list, arr) =
            (case param_dec_list of
              [] => env
            | (DataTypes.Dec (typeSpec, DataTypes.Id id, NONE)) :: param_dec_list_tail =>
                let
                  val env_2 = Env.extend(env, DataTypes.Id id, DenotableValue.Location (hd(arr)))
                in
                  modifyEnv(env_2, param_dec_list_tail, tl(arr))
                end
            | _ => raise DefBug
            )
    in
        e()
    end*)

  and Dec(DataTypes.Dec (typeSpec, DataTypes.Id id, NONE))(env,sto):environment*store =
          let
            val (sto_f,loc) = Store.allocate(sto)
            val env_f = Env.extend(env,DataTypes.Id id, DenotableValue.Location loc)
          in
            ( Env.printEnv(env_f, !verbosity) ; Store.printStore(sto_f, !verbosity) ; (env_f,sto_f) )
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
            ( Env.printEnv(env_f, !verbosity) ; Store.printStore(sto_f, !verbosity) ; (env_f,sto_f) )
          end

  and E(DataTypes.LitExp lit)(env,sto):store*expressibleValue =
    let
        fun remove_quotes(str) =
            let
                val strs = String.tokens (fn ch => ch = #"\"") str
                fun join_strs(strs) = if length(strs) = 0 then "" else "\"" ^ hd(strs) ^ join_strs(tl(strs))
            in
                if length(strs) = 0 then "" else hd(strs) ^ join_strs(tl(strs))
            end
    in
      (case lit of
        DataTypes.IntLit intLit => (sto, ExpressibleValue.Int (let val SOME intValue = Int.fromString(intLit) in intValue end))
      | DataTypes.RealLit realLit => (sto, ExpressibleValue.Real (let val SOME realValue = Real.fromString(realLit) in realValue end))
      | DataTypes.BoolLit boolLit => (sto, ExpressibleValue.Bool (let val SOME boolValue = Bool.fromString(boolLit) in boolValue end))
      | DataTypes.CharLit charLit => (sto, ExpressibleValue.Char (let val SOME charValue = Char.fromString(hd(String.tokens (fn ch => ch = #"'") charLit)) in charValue end))
      | DataTypes.StringLit stringLit => (sto, ExpressibleValue.String (remove_quotes(stringLit))))
    end

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
                                |   _ => raise InvalidTypeInArrayAccessIndexBug
                                )
                        |   _ => raise TooManyIndicesInArrayAccessBug
                        )
                )
        in
            applyArray(loc, expList, sto)
        end


  |   E(DataTypes.AssignExp (DataTypes.Id id, [], exp)) (env,sto) =
          let
            val (sto_exp, expVal) = E(exp)(env,sto)
            val DenotableValue.Location loc = Env.apply(env,DataTypes.Id id)
            val sto_f = Store.update(sto_exp, loc, Store.expressibleToStorable(expVal))
          in
            (sto_f, expVal)
          end
  |   E(DataTypes.AssignExp (DataTypes.Id id, expList, exp)) (env, sto) =
        let
            val (sto_exp, expVal) = E(exp)(env,sto)
            val DenotableValue.Location loc = Env.apply(env, DataTypes.Id id)

            fun getElementAt([], pos) = raise OutOfRangeIndex
            |   getElementAt(arr, pos) = if pos = 0 then hd(arr) else getElementAt(tl(arr), pos-1)

            fun updateArray(loc, expList, sto) =
                (case expList of
                    [] => Store.update(sto, loc, Store.expressibleToStorable(expVal))
                |   (exp :: expListTail) =>
                        (case Store.apply(sto, loc) of
                            StorableValue.ArrayValue arr =>
                                (case E(exp)(env, sto) of
                                    (sto_exp, ExpressibleValue.Int pos) =>
                                        updateArray(getElementAt(arr, pos), expListTail, sto_exp)
                                |   _ => raise InvalidTypeInArrayAccessIndexBug
                                )
                        |   _ => raise TooManyIndicesInArrayAccessBug
                        )
                )
        in
            (updateArray(loc, expList, sto), expVal)
        end

  |   E(DataTypes.AddExp (exp_1, exp_2)) (env,sto) =
        (case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Int (intVal_1 + intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (Real.fromInt(intVal_1) + realVal_2))
                |   _ => raise InvalidTypeInArithmeticOperationBug
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 + Real.fromInt(intVal_2)))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 + realVal_2))
                |   _ => raise InvalidTypeInArithmeticOperationBug
                )
            |   _ => raise InvalidTypeInArithmeticOperationBug
        )

    |   E(DataTypes.SubExp (exp_1, exp_2)) (env,sto) =
          (case E(exp_1)(env,sto) of
              (sto_1, ExpressibleValue.Int intVal_1) =>
                  (case E(exp_2)(env,sto_1) of
                      (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Int (intVal_1 - intVal_2))
                  |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (Real.fromInt(intVal_1) - realVal_2))
                  |   _ => raise InvalidTypeInArithmeticOperationBug
                  )
          |   (sto_1, ExpressibleValue.Real realVal_1) =>
                  (case E(exp_2)(env,sto_1) of
                      (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 - Real.fromInt(intVal_2)))
                  |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 - realVal_2))
                  |   _ => raise InvalidTypeInArithmeticOperationBug
                  )
          |   _ => raise InvalidTypeInArithmeticOperationBug
          )

    |   E(DataTypes.MultExp (exp_1, exp_2)) (env,sto) =
        (case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Int (intVal_1 * intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (Real.fromInt(intVal_1) * realVal_2))
                |   _ => raise InvalidTypeInArithmeticOperationBug
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 * Real.fromInt(intVal_2)))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 * realVal_2))
                |   _ => raise InvalidTypeInArithmeticOperationBug
                )
        |   _ => raise InvalidTypeInArithmeticOperationBug
        )

    |   E(DataTypes.DivExp (exp_1, exp_2)) (env,sto) =
        ((case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Int (intVal_1 div intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (Real.fromInt(intVal_1) / realVal_2))
                |   _ => raise InvalidTypeInArithmeticOperationBug
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 / Real.fromInt(intVal_2)))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Real (realVal_1 / realVal_2))
                |   _ => raise InvalidTypeInArithmeticOperationBug
                )
            |   _ => raise InvalidTypeInArithmeticOperationBug
        ) handle Div => raise DivisionByZero)

    |   E(DataTypes.UMinusExp exp) (env, sto) =
        (case E(exp)(env,sto) of
            (sto_f, ExpressibleValue.Int intVal) => (sto_f, ExpressibleValue.Int (~intVal))
        |   (sto_f, ExpressibleValue.Real realVal) => (sto_f, ExpressibleValue.Real (~realVal))
        |   _ => raise InvalidTypeInArithmeticOperationBug
        )

    |   E(DataTypes.OrExp (exp_1, exp_2)) (env, sto) =
        (case E(exp_1)(env, sto) of
            (sto_1, ExpressibleValue.Bool boolVal_1) =>
                (case E(exp_2)(env, sto_1) of
                    (sto_f, ExpressibleValue.Bool boolVal_2) => (sto_f, ExpressibleValue.Bool (boolVal_1 orelse boolVal_2))
                |   _ => raise InvalidTypeInLogicalOperationBug
                )
        |   _ => raise InvalidTypeInLogicalOperationBug
        )

    |   E(DataTypes.AndExp (exp_1, exp_2)) (env, sto) =
        (case E(exp_1)(env, sto) of
            (sto_1, ExpressibleValue.Bool boolVal_1) =>
                (case E(exp_2)(env, sto_1) of
                    (sto_f, ExpressibleValue.Bool boolVal_2) => (sto_f, ExpressibleValue.Bool (boolVal_1 andalso boolVal_2))
                |   _ => raise InvalidTypeInLogicalOperationBug
                )
        |   _ => raise InvalidTypeInLogicalOperationBug
        )

    |   E(DataTypes.EqExp (exp_1, exp_2)) (env, sto) =
        let
          val (sto_1, expVal_1) = E(exp_1)(env,sto)
          val (sto_f, expVal_2) = E(exp_2)(env,sto_1)
        in
            case (expVal_1, expVal_2) of
                (ExpressibleValue.Int intVal_1, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (intVal_1 = intVal_2))
            |   (ExpressibleValue.Int intVal_1, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.==(Real.fromInt(intVal_1), realVal_2)))
            |   (ExpressibleValue.Real realVal_1, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (Real.==(realVal_1, Real.fromInt(intVal_2))))
            |   (ExpressibleValue.Real realVal_1, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.==(realVal_1, realVal_2)))
            |   (ExpressibleValue.Bool boolVal_1, ExpressibleValue.Bool boolVal_2) => (sto_f, ExpressibleValue.Bool (boolVal_1 = boolVal_2))
            |   (ExpressibleValue.Char charVal_1, ExpressibleValue.Char charVal_2) => (sto_f, ExpressibleValue.Bool (charVal_1 = charVal_2))
            |   (ExpressibleValue.String stringVal_1, ExpressibleValue.String stringVal_2) => (sto_f, ExpressibleValue.Bool (stringVal_1 = stringVal_2))
            |   _ => raise IncomparableTypesBug
        end
        (*(case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
        |   (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (intVal_1 = intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.==(Real.fromInt(intVal_1), realVal_2)))
                |   _ => raise IncomparableTypesBug
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (Real.==(realVal_1, Real.fromInt(intVal_2))))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.==(realVal_1, realVal_2)))
                |   _ => raise IncomparableTypesBug
                )
        |   (sto_1, ExpressibleValue.Bool boolVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
                |   (sto_f, ExpressibleValue.Bool boolVal_2) => (sto_f, ExpressibleValue.Bool (boolVal_1 = boolVal_2))
                |   _ => raise IncomparableTypesBug
                )
        |   (sto_1, ExpressibleValue.Char charVal_1) =>
                (case E(exp_2)(env, sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
                |   (sto_f, ExpressibleValue.Char charVal_2) => (sto_f, ExpressibleValue.Bool (charVal_1 = charVal_2))
                |   _ => raise IncomparableTypesBug
                )
        |   (sto_1, ExpressibleValue.String stringVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
                |   (sto_f, ExpressibleValue.String stringVal_2) => (sto_f, ExpressibleValue.Bool (stringVal_1 = stringVal_2))
                |   _ => raise IncomparableTypesBug
                )
        (* comparações entre datasets, models e arrays?*)
        |   _ => raise InvalidTypeInComparisonBug
        )*)

    |   E(DataTypes.NeExp (exp_1, exp_2)) (env, sto) =
        let
            val (sto_f, ExpressibleValue.Bool boolVal) = E(DataTypes.EqExp (exp_1, exp_2))(env, sto)
        in
            (sto_f, ExpressibleValue.Bool (not(boolVal)))
        end

    |   E(DataTypes.LtExp (exp_1, exp_2)) (env, sto) =
        let
          val (sto_1, expVal_1) = E(exp_1)(env,sto)
          val (sto_f, expVal_2) = E(exp_2)(env,sto_1)
        in
            case (expVal_1, expVal_2) of
                (ExpressibleValue.Int intVal_1, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (intVal_1 < intVal_2))
            |   (ExpressibleValue.Int intVal_1, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.fromInt(intVal_1) < realVal_2))
            |   (ExpressibleValue.Real realVal_1, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 < Real.fromInt(intVal_2)))
            |   (ExpressibleValue.Real realVal_1, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 < realVal_2))
            |   (ExpressibleValue.Bool boolVal_1, ExpressibleValue.Bool boolVal_2) => raise IncomparableTypesBug
            |   (ExpressibleValue.Char charVal_1, ExpressibleValue.Char charVal_2) => (sto_f, ExpressibleValue.Bool (charVal_1 < charVal_2))
            |   (ExpressibleValue.String stringVal_1, ExpressibleValue.String stringVal_2) => (sto_f, ExpressibleValue.Bool (stringVal_1 < stringVal_2))
            |   _ => raise IncomparableTypesBug
        end
        (*(case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
        |   (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (intVal_1 < intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.fromInt(intVal_1) < realVal_2))
                |   _ => raise InvalidTypeInComparisonBug
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 < Real.fromInt(intVal_2)))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 < realVal_2))
                |   _ => raise InvalidTypeInComparisonBug
                )
        |   _ => raise InvalidTypeInComparisonBug
        )*)

    |   E(DataTypes.LeExp (exp_1, exp_2)) (env, sto) =
        let
          val (sto_1, expVal_1) = E(exp_1)(env,sto)
          val (sto_f, expVal_2) = E(exp_2)(env,sto_1)
        in
            case (expVal_1, expVal_2) of
                (ExpressibleValue.Int intVal_1, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (intVal_1 <= intVal_2))
            |   (ExpressibleValue.Int intVal_1, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.fromInt(intVal_1) <= realVal_2))
            |   (ExpressibleValue.Real realVal_1, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 <= Real.fromInt(intVal_2)))
            |   (ExpressibleValue.Real realVal_1, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 <= realVal_2))
            |   (ExpressibleValue.Bool boolVal_1, ExpressibleValue.Bool boolVal_2) => raise IncomparableTypesBug
            |   (ExpressibleValue.Char charVal_1, ExpressibleValue.Char charVal_2) => (sto_f, ExpressibleValue.Bool (charVal_1 <= charVal_2))
            |   (ExpressibleValue.String stringVal_1, ExpressibleValue.String stringVal_2) => (sto_f, ExpressibleValue.Bool (stringVal_1 <= stringVal_2))
            |   _ => raise IncomparableTypesBug
        end
        (*(case E(exp_1)(env,sto) of
            (sto_1, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
        |   (sto_1, ExpressibleValue.Int intVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (intVal_1 <= intVal_2))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (Real.fromInt(intVal_1) <= realVal_2))
                |   _ => raise InvalidTypeInComparisonBug
                )
        |   (sto_1, ExpressibleValue.Real realVal_1) =>
                (case E(exp_2)(env,sto_1) of
                    (sto_f, ExpressibleValue.VoidValue) => raise InvalidTypeInComparisonBug
                |   (sto_f, ExpressibleValue.Int intVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 <= Real.fromInt(intVal_2)))
                |   (sto_f, ExpressibleValue.Real realVal_2) => (sto_f, ExpressibleValue.Bool (realVal_1 <= realVal_2))
                |   _ => raise InvalidTypeInComparisonBug
                )
        |   _ => raise InvalidTypeInComparisonBug
        )*)

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
        |   _ => raise InvalidTypeInLogicalOperationBug)

  |   E(DataTypes.ParenExp exp) (env, sto) = E(exp)(env, sto)


  |   E(DataTypes.AppExp (DataTypes.Id id, [])) (env,sto) =
        let
          val DenotableValue.Function f = Env.apply(env, DataTypes.Id id)
          handle Bind => raise NonFunctionApplicationBug
          val (sto_f,retVal) = f([], sto)
        in
          (sto_f, retVal)
        end

    |   E(DataTypes.AppExp (DataTypes.Id id, expList)) (env, sto) =
        let
            val DenotableValue.Function f = Env.apply(env, DataTypes.Id id)
            handle Bind => raise NonFunctionApplicationBug

            fun locations(expList, sto) =
                (case expList of
                    [] => ([], sto)
                |   (exp :: expListTail) =>
                        let
                            val (sto_1, expVal) = E(exp)(env, sto)
                            val (sto_2, loc) = Store.allocate(sto_1)
                            val sto_3 = Store.update(sto_2, loc, Store.expressibleToStorable(expVal))
                            val (arr, sto_4) = locations(expListTail, sto_3)
                        in
                            (loc :: arr, sto_4)
                        end
                )

            val (params, sto_loc) = locations(expList, sto)
            val (sto_f, retVal) = f(params, sto_loc)
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
          (Store.printStore(sto_f, !verbosity); (sto_f,false,ExpressibleValue.VoidValue))
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
                    |   _ => raise InvalidTypeInLogicalOperationBug
                    )
        in
            loop(sto, false, ExpressibleValue.VoidValue)
        end


  |   C(DataTypes.Skip)(env,sto) = (sto, false, ExpressibleValue.VoidValue)

  (* 4: putting everything together (i.e., interpreting) *)
  and interpret(fileName:string):unit =
        let
          val parseTree = parse(fileName)
          val (localEnv, globalEnv) = typeCheck(parseTree)
          (*val _ = print("-----localEnv-----\n")*)
        in run(parseTree)
        end
        handle ParseError => ()
    
    
end
