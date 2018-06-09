structure Location =
struct
  type location = int

end


structure ArrayValue =
struct
  type arrayValue = Location.location list
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
        case sto(loc) of
          StorableValue.Unused => (print("\n"); ())
        | StorableValue.Int x => (print("{" ^ Int.toString(loc) ^ ", " ^ Int.toString(x) ^ "}\n"); printStoreFrom(sto, loc+1))
        | StorableValue.Real x => (print("{" ^ Int.toString(loc) ^ ", " ^ Real.toString(x) ^ "}\n"); printStoreFrom(sto, loc+1))
        | StorableValue.Undefined => (print("{" ^ Int.toString(loc) ^ ", " ^ "Undefined" ^ "}\n"); printStoreFrom(sto, loc+1))
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

    fun printEnvAt(env, DataTypes.Id id):unit =
        case env(DataTypes.Id id) of
          Location loc => print("[" ^ id ^ "|->Int.toString(loc))
        | Function f => print("Function
      fun printEnv(env):unit =
        let
          fun printEnvFrom(env,c) =
            case sto(loc) of
              StorableValue.Unused => (print("\n"); unit)
            | StorableValue.Int x => (print("{" ^ loc ^ ", " ^ Int.toString(x) ^ "}\n"); printStoreFrom(sto, loc+1))
            | StorableValue.Undefined => (print("{" ^ loc ^ ", " ^ "Undefined" ^ "}\n"); printStoreFrom(sto, loc+1))
        in
          printStoreFrom(sto, 0)
        end

    end
