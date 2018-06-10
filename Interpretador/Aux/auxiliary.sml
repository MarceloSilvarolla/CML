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
  exception nonStorableExpressible
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
    | ExpressibleValue.VoidValue => raise nonStorableExpressible
  exception nonExpressibleStorable
  fun storableToExpressible(stoVal) =
    case stoVal of
      StorableValue.Int intVal => ExpressibleValue.Int intVal
    | StorableValue.Real realVal => ExpressibleValue.Real realVal
    | StorableValue.Bool boolVal => ExpressibleValue.Bool boolVal
    | StorableValue.Char charVal => ExpressibleValue.Char charVal
    | StorableValue.String stringVal => ExpressibleValue.String stringVal
    | StorableValue.Dataset datasetVal => ExpressibleValue.Dataset datasetVal
    | StorableValue.Model modelVal => ExpressibleValue.Model modelVal
    | StorableValue.ArrayValue arrayVal => ExpressibleValue.ArrayValue arrayVal
    | _ => raise nonExpressibleStorable
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

  fun extend(env:environment,DataTypes.Id
    id:DataTypes.Id,denVal:denotableValue)(DataTypes.Id id1:DataTypes.Id):denotableValue =
      if id1 = id then denVal else env(DataTypes.Id id1)

  fun apply(env:environment, DataTypes.Id id:DataTypes.Id):denotableValue =
    env(DataTypes.Id id)
  
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

