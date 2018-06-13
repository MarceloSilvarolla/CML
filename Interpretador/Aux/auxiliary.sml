structure Location =
struct
  type location = int

end


structure ArrayValue =
struct
  type arrayValue = Location.location list
  fun
    toString([]) = "{}"
  | toString(locs) = "{" ^ String.concatWith(", ")(map Int.toString locs) ^ "}"
end

structure ExpressibleValue =
struct
  type dataset = Dataset.dataset
  type model = Model.model
  type arrayValue = ArrayValue.arrayValue
  datatype expressibleValue =
      Int of int | Real of real | Bool of bool | Char of char | String of string
    | Dataset of dataset | Model of model
    | ArrayValue of arrayValue | VoidValue
end

structure StorableValue =
struct
  type dataset = Dataset.dataset
  type model = Model.model
  type arrayValue = ArrayValue.arrayValue
  datatype storableValue = Int of int | Real of real | Bool of bool | Char of
     char | String of string | Dataset of dataset | Model of model | ArrayValue of
     arrayValue | Unused | Undefined
end

structure Store =
struct
  type location = Location.location
  type storableValue = StorableValue.storableValue
  val Unused = StorableValue.Unused
  val Undefined = StorableValue.Undefined

  type store = location->storableValue


  fun empty(loc:location):storableValue = Unused

  fun
    update(sto:store,loc:location,stoVal:storableValue)(loc1:location):storableValue =
      if loc1 = loc then stoVal else sto(loc1)

  fun apply(sto:store,loc:location):storableValue = sto(loc)
  exception NonStorableExpressible
  fun expressibleToStorable(expVal) =
    case expVal of
      ExpressibleValue.Int intVal => StorableValue.Int intVal
    | ExpressibleValue.Real realVal => StorableValue.Real realVal
    | ExpressibleValue.Bool boolVal => StorableValue.Bool boolVal
    | ExpressibleValue.Char charVal => StorableValue.Char charVal
    | ExpressibleValue.String stringVal => StorableValue.String stringVal
    | ExpressibleValue.Dataset datasetVal => StorableValue.Dataset datasetVal
    | ExpressibleValue.Model modelVal => StorableValue.Model modelVal
    | ExpressibleValue.ArrayValue arrayVal => StorableValue.ArrayValue arrayVal
    | ExpressibleValue.VoidValue => raise NonStorableExpressible
  exception nonExpressibleStorable
  fun storableToExpressible(stoVal) =
    (case stoVal of
      StorableValue.Int intVal => ExpressibleValue.Int intVal
    | StorableValue.Real realVal => ExpressibleValue.Real realVal
    | StorableValue.Bool boolVal => ExpressibleValue.Bool boolVal
    | StorableValue.Char charVal => ExpressibleValue.Char charVal
    | StorableValue.String stringVal => ExpressibleValue.String stringVal
    | StorableValue.Dataset datasetVal => ExpressibleValue.Dataset datasetVal
    | StorableValue.Model modelVal => ExpressibleValue.Model modelVal
    | StorableValue.ArrayValue arrayVal => ExpressibleValue.ArrayValue arrayVal
    | _ => raise nonExpressibleStorable
    )
  fun allocate(sto:store):store*location =
  let
    fun leastUnusedLocationAfter(loc:location):location =
      case sto(loc) of
          StorableValue.Unused => loc
        | _      =>  let val loc = leastUnusedLocationAfter(loc+1)
                     in loc
                     end
    val loc = leastUnusedLocationAfter(0)
  in
    (update(sto,loc,Undefined),loc)
  end

  fun deallocate(sto:store,loc:location):store = update(sto,loc,Unused)
  fun printStore(sto):unit =
    let
      fun printStoreFrom(sto, loc) =
        (case sto(loc) of
          StorableValue.Int x => (print("[" ^ Int.toString(loc) ^ ": " ^ Int.toString(x) ^ "] "); printStoreFrom(sto, loc+1))
        | StorableValue.Real x => (print("[" ^ Int.toString(loc) ^ ": " ^ Real.toString(x) ^ "] "); printStoreFrom(sto, loc+1))
        | StorableValue.Bool x => (print("[" ^ Int.toString(loc) ^ ": " ^ (if x then "true" else "false") ^ "] "); printStoreFrom(sto, loc+1))
        | StorableValue.Char x => (print("[" ^ Int.toString(loc) ^ ": " ^ Char.toString(x) ^ "] "); printStoreFrom(sto, loc+1))
        | StorableValue.String x => (print("[" ^ Int.toString(loc) ^ ": " ^ x ^ "] "); printStoreFrom(sto, loc+1))
        | StorableValue.Dataset x => (print("[" ^ Int.toString(loc) ^ ": " ^ "dataset" ^ "] "); printStoreFrom(sto, loc+1))
        | StorableValue.Model x => (print("[" ^ Int.toString(loc) ^ ": " ^ "model" ^ "] "); printStoreFrom(sto, loc+1))
        | StorableValue.ArrayValue arr => (print("[" ^ Int.toString(loc) ^ ": " ^ ArrayValue.toString(arr) ^ "] "); printStoreFrom(sto, loc+1))
        | StorableValue.Unused => (print("\n"); ())
        | StorableValue.Undefined => (print("[" ^ Int.toString(loc) ^ ": " ^ "undefined" ^ "] "); printStoreFrom(sto, loc+1))
        )
    in
      printStoreFrom(sto, 0)
    end

end

structure Function =
struct
  type location = Location.location
  type store = Store.store
  type returnValue = ExpressibleValue.expressibleValue
  type function = (location list) * store -> store * returnValue
end

structure DenotableValue =
struct
  type location = Location.location
  type function = Function.function
  datatype denotableValue =
      Location of location
    | Function of function
    | Unbound
end


structure ReturnFlag =
struct
  type returnFlag = bool
end

structure LearningAuxBridge =
struct
    exception InvalidLearningArgumentBug 
    fun load_data([filename_loc, separator_loc], sto) =
        (case (Store.apply(sto, filename_loc), Store.apply(sto, separator_loc)) of
          (StorableValue.String filename, StorableValue.Char separator) =>
            (sto, ExpressibleValue.Dataset (Learning.load_data(filename, separator)))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   load_data(_) = raise InvalidLearningArgumentBug 

    fun save_data([dataset_loc, filename_loc, separator_loc], sto) =
        (case (Store.apply(sto, dataset_loc), Store.apply(sto, filename_loc), Store.apply(sto, separator_loc)) of
            (StorableValue.Dataset dataset, StorableValue.String filename, StorableValue.Char separator) =>
                let
                    val _ = Learning.save_data(dataset, filename, separator)
                in
                    (sto, ExpressibleValue.VoidValue)
                end
        | _ => raise InvalidLearningArgumentBug 
        )
    |   save_data(_) = raise InvalidLearningArgumentBug 

    fun get_columns_from_store(location_list, sto) =
        if length(location_list) = 0 then
            []
        else
            (case Store.apply(sto, hd(location_list)) of
                StorableValue.String col => col :: get_columns_from_store(tl(location_list), sto)
            | _ => raise InvalidLearningArgumentBug 
            )

    fun columns([dataset_loc, columns_loc], sto) =
        (case (Store.apply(sto, dataset_loc), Store.apply(sto, columns_loc)) of
            (StorableValue.Dataset dataset, StorableValue.ArrayValue location_list) =>
                (sto, ExpressibleValue.Dataset (Learning.columns(dataset, get_columns_from_store(location_list, sto))))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   columns(_) = raise InvalidLearningArgumentBug 

    fun remove_columns([dataset_loc, columns_loc], sto) =
        (case (Store.apply(sto, dataset_loc), Store.apply(sto, columns_loc)) of
            (StorableValue.Dataset dataset, StorableValue.ArrayValue location_list) =>
                (sto, ExpressibleValue.Dataset (Learning.remove_columns(dataset, get_columns_from_store(location_list, sto))))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   remove_columns(_) = raise InvalidLearningArgumentBug 

    fun rows([dataset_loc, first_loc, qt_loc], sto) =
        (case (Store.apply(sto, dataset_loc), Store.apply(sto, first_loc), Store.apply(sto, qt_loc)) of
            (StorableValue.Dataset dataset, StorableValue.Int first, StorableValue.Int qt) =>
                (sto, ExpressibleValue.Dataset (Learning.rows(dataset, first, qt)))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   rows(_) = raise InvalidLearningArgumentBug 

    fun num_rows([dataset_loc], sto) =
        (case Store.apply(sto, dataset_loc) of
            StorableValue.Dataset dataset => (sto, ExpressibleValue.Int (Learning.num_rows(dataset)))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   num_rows(_) = raise InvalidLearningArgumentBug 

    fun perceptron([X_loc, y_loc, num_iters_loc], sto) =
        (case (Store.apply(sto, X_loc), Store.apply(sto, y_loc), Store.apply(sto, num_iters_loc)) of
            (StorableValue.Dataset X, StorableValue.Dataset y, StorableValue.Int num_iters) =>
                (sto, ExpressibleValue.Model (Learning.perceptron(X, y, num_iters)))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   perceptron(_) = raise InvalidLearningArgumentBug 

    fun pocket_perceptron([X_loc, y_loc, num_iters_loc], sto) =
        (case (Store.apply(sto, X_loc), Store.apply(sto, y_loc), Store.apply(sto, num_iters_loc)) of
            (StorableValue.Dataset X, StorableValue.Dataset y, StorableValue.Int num_iters) =>
                (sto, ExpressibleValue.Model (Learning.pocket_perceptron(X, y, num_iters)))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   pocket_perceptron(_) = raise InvalidLearningArgumentBug 

    fun logistic_regression([X_loc, y_loc, label_of_interest_loc, learning_rate_loc, batch_size_loc, num_epochs_loc], sto) =
        (case (Store.apply(sto, X_loc), Store.apply(sto, y_loc), Store.apply(sto, label_of_interest_loc),
                    Store.apply(sto, learning_rate_loc), Store.apply(sto, batch_size_loc), Store.apply(sto, num_epochs_loc)) of
            (StorableValue.Dataset X, StorableValue.Dataset y, StorableValue.String label_of_interest,
                StorableValue.Real learning_rate, StorableValue.Int batch_size, StorableValue.Int num_epochs) =>
                    (sto, ExpressibleValue.Model (Learning.logistic_regression(X, y, label_of_interest, learning_rate, batch_size, num_epochs)))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   logistic_regression(_) = raise InvalidLearningArgumentBug 

    fun linear_regression([X_loc, y_loc, learning_rate_loc, batch_size_loc, num_epochs_loc], sto) =
        (case (Store.apply(sto, X_loc), Store.apply(sto, y_loc), Store.apply(sto, learning_rate_loc),
                                        Store.apply(sto, batch_size_loc), Store.apply(sto, num_epochs_loc)) of
            (StorableValue.Dataset X, StorableValue.Dataset y, StorableValue.Real learning_rate,
                                        StorableValue.Int batch_size, StorableValue.Int num_epochs) =>
                (sto, ExpressibleValue.Model (Learning.linear_regression(X, y, learning_rate, batch_size, num_epochs)))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   linear_regression(_) = raise InvalidLearningArgumentBug 

    fun predict([X_loc, model_loc], sto) =
        (case (Store.apply(sto, X_loc), Store.apply(sto, model_loc)) of
            (StorableValue.Dataset X, StorableValue.Model model) =>
                (sto, ExpressibleValue.Dataset (Learning.predict(X, model)))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   predict(_) = raise InvalidLearningArgumentBug 

    fun load_model([filename_loc], sto) =
        (case Store.apply(sto, filename_loc) of
            StorableValue.String filename => (sto, ExpressibleValue.Model (Learning.load_model(filename)))
        | _ => raise InvalidLearningArgumentBug 
        )
    |   load_model(_) = raise InvalidLearningArgumentBug 

    fun save_model([model_loc, filename_loc], sto) =
        (case (Store.apply(sto, model_loc), Store.apply(sto, filename_loc)) of
            (StorableValue.Model model, StorableValue.String filename) =>
                let
                    val _ = Learning.save_model(model, filename)
                in
                    (sto, ExpressibleValue.VoidValue)
                end
        | _ => raise InvalidLearningArgumentBug 
        )
    |   save_model(_) = raise InvalidLearningArgumentBug 
end

structure Print =
struct
  exception InvalidPrintArgumentBug
  fun anyToString([loc], sto) =
    let
      val stoVal = Store.apply(sto, loc)
    in
      case stoVal of
        StorableValue.Int x => Int.toString(x)
      | StorableValue.Real x => Real.toString(x)
      | StorableValue.Bool b => Bool.toString(b)
      | StorableValue.Char c => Char.toString(c)
      | StorableValue.String s => String.toString(s)
      | StorableValue.Dataset d => "<dataset>"
      | StorableValue.Model m => "<model>"
      | StorableValue.ArrayValue locs => "{" ^ String.concatWith(", ") (map (fn loc => anyToString([loc], sto)) locs) ^ "}"
      | StorableValue.Unused => raise InvalidPrintArgumentBug
      | StorableValue.Undefined => raise InvalidPrintArgumentBug
    end
  |  anyToString(_) = raise InvalidPrintArgumentBug

  fun printAny([loc], sto) =
    (print(anyToString([loc], sto)); (sto, ExpressibleValue.VoidValue))
  |   printAny(_) = raise InvalidPrintArgumentBug

  fun printlnAny([loc], sto) =
    (print(anyToString([loc], sto)); print("\n"); (sto, ExpressibleValue.VoidValue))
  |   printlnAny(_) = raise InvalidPrintArgumentBug

end

structure Env =
struct
  type Id = DataTypes.Id
  type denotableValue = DenotableValue.denotableValue
  type environment = (Id * denotableValue) list
  val empty = []
  fun apply([]:environment, DataTypes.Id id) = DenotableValue.Unbound
  |   apply((DataTypes.Id id1, denVal)::envTail, DataTypes.Id id) =
    if id1 = id then denVal else apply(envTail, DataTypes.Id id)
  val initial:environment = [
	 (DataTypes.Id "load_data", DenotableValue.Function LearningAuxBridge.load_data), 
         (DataTypes.Id "save_data" , DenotableValue.Function LearningAuxBridge.save_data), 
         (DataTypes.Id "columns" , DenotableValue.Function LearningAuxBridge.columns), 
         (DataTypes.Id "remove_columns"  , DenotableValue.Function LearningAuxBridge.remove_columns), 
         (DataTypes.Id "rows" , DenotableValue.Function LearningAuxBridge.rows), 
         (DataTypes.Id "num_rows" , DenotableValue.Function LearningAuxBridge.num_rows), 

         (DataTypes.Id "perceptron" , DenotableValue.Function LearningAuxBridge.perceptron), 
         (DataTypes.Id "pocket_perceptron" , DenotableValue.Function LearningAuxBridge.pocket_perceptron),
         (DataTypes.Id "logistic_regression" , DenotableValue.Function LearningAuxBridge.logistic_regression),
         (DataTypes.Id "linear_regression" , DenotableValue.Function LearningAuxBridge.linear_regression),

         (DataTypes.Id "predict" , DenotableValue.Function LearningAuxBridge.predict),
         (DataTypes.Id "load_model" , DenotableValue.Function LearningAuxBridge.load_model),
         (DataTypes.Id "save_model" , DenotableValue.Function LearningAuxBridge.save_model),
       
         (DataTypes.Id "print" , DenotableValue.Function Print.printAny),
         (DataTypes.Id "println" , DenotableValue.Function Print.printlnAny)
  ]

  fun extend(env:environment,DataTypes.Id
    id,denVal:denotableValue) = 
      (DataTypes.Id id, denVal) :: env

  fun toStringPair(DataTypes.Id id, denVal):string =
    (case denVal of
      DenotableValue.Location loc => "(" ^ id ^ ": " ^ Int.toString(loc) ^ ")"
    | DenotableValue.Function f => "(" ^ id ^ ": function)"
    | DenotableValue.Unbound => ""
    )
  (*fun charListsWithLengthLessThan(l:int):char list list =
    (case l of
      0 => []
    | l =>
      fun c(
    )*)
  fun toFullString(env):string =
    String.concatWith(" ")(map toStringPair env)

  fun toString(env):string =
    let 
      val envWithoutPredefinedFunctions = List.rev (List.drop ( (List.rev env), List.length initial ))
      fun sameId((DataTypes.Id id1, _), (DataTypes.Id id2, _)) = (id1 = id2)
      fun remRep([]) = []
      |   remRep(p::ps) = p::(List.filter (fn q => not (sameId (p, q))) (remRep ps))
      val envWPFAndNoRep = remRep(envWithoutPredefinedFunctions)
    in String.concatWith(" ")(map toStringPair envWPFAndNoRep)
    end
  
  fun printFullEnv(env):unit =
    (print(toFullString(env)); print("\n"))

  fun printEnv(env):unit =
    (print(toString(env)); print("\n"))

end

structure Sort =
struct
  exception UnboundVariableInExpression
  exception IncorrectNumberOfArguments
  exception InconsistentSorts
  datatype sort = Int | Real | Char | Bool | String | Dataset | Model | Array of sort
    | Void | Unbound | Any | Product of sort list | To of sort * sort
  fun typeSpecSort(typeSpec:DataTypes.TypeSpec):sort =
    (case typeSpec of
      DataTypes.Void => Void
    | DataTypes.Int => Int
    | DataTypes.Real => Real
    | DataTypes.Bool => Bool
    | DataTypes.Char => Char
    | DataTypes.String => String
    | DataTypes.Dataset => Dataset
    | DataTypes.Model => Model
    | DataTypes.Array typeSpec_1 => Array (typeSpecSort(typeSpec_1))
    )
  fun commonSort(srt_1, srt_2):sort =
    (case (srt_1,srt_2) of
      (Any, srt) => srt
    | (srt, Any) => srt
    | (Int, Int) => Int
    | (Int, Real) => Real
    | (Real, Int) => Real
    | (Real, Real) => Real
    | (Char, Char) => Char
    | (Bool, Bool) => Bool
    | (String, String) => String
    | (Dataset, Dataset) => Dataset
    | (Model, Model) => Model
    | (Array srt_1_1, Array srt_2_1) => Array (commonSort (srt_1_1, srt_2_1))
    | (Product srt_1list, Product srt_2list) =>
      let
        fun srtPairs([])([]) = []
        |   srtPairs([])(_) = raise IncorrectNumberOfArguments
        |   srtPairs(_)([]) = raise IncorrectNumberOfArguments
        |   srtPairs(srt_1head::srt_1tail)(srt_2head::srt_2tail) =
          (srt_1head, srt_2head) :: srtPairs(srt_1tail)(srt_2tail)
        (*val (srtPairs, srt_2_1_rest) = foldr
         (fn
           (srtFrom_srt_1_1, (result, srtFrom_srt_2_1::srt_2_1_tail)) => ((srtFrom_srt_1_1, srtFrom_srt_2_1)::result, srt_2_1_tail)
         | (result, []) => raise IncorrectNumberOfArguments)
         ([], srt_2_1)
         srt_1_1
        val _ = if srt_2_1_rest = [] then () else raise IncorrectNumberOfArguments*)
      in
        Product (map commonSort (srtPairs srt_1list srt_2list))
      end
    | (To (srt_1_1, srt_1_2), To (srt_2_1, srt_2_2))  =>
      (To (commonSort(srt_1_1, srt_2_1), commonSort(srt_1_2, srt_2_2)))
    | (Void, Void) => Void
    | (Unbound,_) => raise UnboundVariableInExpression
    | (_,Unbound) => raise UnboundVariableInExpression
    | _ => raise InconsistentSorts
    )

  fun commonSortList(sorts):sort =
    foldl (fn (srt, accSrt) => commonSort(srt, accSrt)) Any sorts

  fun expValSortAsString(expVal) =
    (case expVal of
      ExpressibleValue.Int _ => "int"
    | ExpressibleValue.Real _ => "real"
    | ExpressibleValue.Bool _ => "bool"
    | ExpressibleValue.Char _ => "char"
    | ExpressibleValue.String _ => "string"
    | ExpressibleValue.Dataset _ => "dataset"
    | ExpressibleValue.Model _ => "model"
    | ExpressibleValue.ArrayValue arrVal => "array" (* TODO: melhorar *)
    | ExpressibleValue.VoidValue => "void"
    )

end

structure LocalTypeEnv =
struct
  exception MultipleLocalDeclarations
  type environment = DataTypes.Id -> Sort.sort
  fun empty(_) = Sort.Unbound
  fun initial(DataTypes.Id id) = (
      case id of
	  "load_data"  => Sort.To (Sort.Product [Sort.String, Sort.Char], Sort.Dataset)
       |  "save_data" => Sort.To (Sort.Product [Sort.Dataset, Sort.String, Sort.Char], Sort.Void)
       |  "columns" => Sort.To (Sort.Product [Sort.Dataset, Sort.Array (Sort.String)], Sort.Dataset)
       | "remove_columns"  => Sort.To (Sort.Product [Sort.Dataset, Sort.Array (Sort.String)], Sort.Dataset)
       | "rows" => Sort.To (Sort.Product [Sort.Dataset, Sort.Int, Sort.Int], Sort.Dataset)
       | "num_rows" => Sort.To (Sort.Product [Sort.Dataset], Sort.Int)

       | "perceptron" => Sort.To (Sort.Product [Sort.Dataset, Sort.Dataset, Sort.Int], Sort.Model)
       | "pocket_perceptron" => Sort.To (Sort.Product [Sort.Dataset, Sort.Dataset, Sort.Int], Sort.Model)
       | "logistic_regression" => Sort.To (Sort.Product [Sort.Dataset, Sort.Dataset, Sort.String, Sort.Real, Sort.Int, Sort.Int], Sort.Model)
       | "linear_regression" => Sort.To (Sort.Product [Sort.Dataset, Sort.Dataset, Sort.Real, Sort.Int, Sort.Int], Sort.Model)

       | "predict" => Sort.To (Sort.Product [Sort.Dataset, Sort.Model], Sort.Dataset)
       | "load_model" => Sort.To (Sort.Product [Sort.String], Sort.Model)
       | "save_model" => Sort.To (Sort.Product [Sort.Model, Sort.String], Sort.Void)
    
       | "print" => Sort.To (Sort.Any, Sort.Void)
       | "println" => Sort.To (Sort.Any, Sort.Void)
       (*| "printEnv" => Sort.To (Sort.Product [], Sort.Void)*)
       | _ => empty(id)
  )
  fun extend(env,DataTypes.Id id, srt):environment =
    (case env(DataTypes.Id id) of
      Sort.Unbound => (fn DataTypes.Id id1 => (if id1 = id then srt else env(DataTypes.Id id1)))
    | _ => raise MultipleLocalDeclarations
    )
  fun apply(env,DataTypes.Id id):Sort.sort =
    env(DataTypes.Id id)

end

structure GlobalTypeEnv =
struct
  type environment = DataTypes.Id -> Sort.sort
  fun empty(_) = Sort.Unbound
  fun initial(id) = LocalTypeEnv.initial(id)
  fun extend(env,DataTypes.Id id, srt):environment =
    (fn DataTypes.Id id1 => (if id1 = id then srt else env(DataTypes.Id id1)))
  fun apply(env,DataTypes.Id id):Sort.sort =
    env(DataTypes.Id id)
end
