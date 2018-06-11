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

structure Env =
struct
  type Id = DataTypes.Id
  type denotableValue = DenotableValue.denotableValue
  type environment = Id -> denotableValue
  fun empty(id) = DenotableValue.Unbound
  fun apply(env, DataTypes.Id id) = env(DataTypes.Id id)
  fun extend(env:environment,DataTypes.Id
    id:DataTypes.Id,denVal:denotableValue)(DataTypes.Id id1:DataTypes.Id):denotableValue =
      if id1 = id then denVal else env(DataTypes.Id id1)
  
  fun toStringAt(env, DataTypes.Id id):string =
    (case env(DataTypes.Id id) of
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
  fun toString(env):string =
    let 
      val completeEnv = (List.tabulate(256, fn n => toStringAt(env, DataTypes.Id (String.str(Char.chr(n))))))
      val onlyNonEmpty = List.filter (fn s => not (s = "")) completeEnv
    in
      String.concatWith(" ")(onlyNonEmpty)
    end
  fun printEnv(env):unit = 
    (print(toString(env)); print("\n"))
end

structure Sort =
struct
  exception UnboundBug
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
    | (Real, Real) => Real
    | (Char, Char) => Char
    | (Bool, Bool) => Bool
    | (String, String) => String
    | (Dataset, Dataset) => Dataset
    | (Model, Model) => Model
    | (Array srt_1_1, Array srt_2_1) => commonSort(srt_1_1, srt_2_1)
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
    | (Unbound,_) => raise UnboundBug
    | (_,Unbound) => raise UnboundBug
    | _ => raise InconsistentSorts
    )

  fun commonSortList(sorts):sort =
    foldl (fn (srt, accSrt) => commonSort(srt, accSrt)) Any sorts 
  
end

structure LocalTypeEnv =
struct
  exception MultipleLocalDeclarations
  type environment = DataTypes.Id -> Sort.sort 
  fun empty(_) = Sort.Unbound
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
  fun extend(env,DataTypes.Id id, srt):environment =
    (fn DataTypes.Id id1 => (if id1 = id then srt else env(DataTypes.Id id1)))
  fun apply(env,DataTypes.Id id):Sort.sort =
    env(DataTypes.Id id)
end



