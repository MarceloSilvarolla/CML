structure ExpressibleValue =
struct
  type dataset = Dataset.dataset
  type model = Model.model
  type arrayValue = ArrayValue.arrayValue
  datatype expressibleValue = 
      Int of int | Real of real | Bool of bool | Char of char | String of string 
    | Dataset of dataset | Model of model 
    | ArrayValue of arrayValue
end

structure ReturnValue =
struct
  type expressibleValue = ExpressibleValue.expressibleValue
  datatype returnValue =
      ExpressibleValue of expressibleValue
    | VoidValue
end

structure Function =
struct
  type location = Location.location
  type store = Store.store
  type returnValue = ReturnValue.returnValue
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

structure Env =
struct
  type Id = DATATYPES.Id
  val Id = DATATYPES.Id
  type denotableValue = DenotableValue.denotableValue
  type environment = Id -> denotableValue
  fun (empty:environment)(id) = unbound

  fun extend(env:environment,Id id:Id,denVal:denotableValue)(Id id1:Id):denotableValue =
      if id1 = id then denVal else env(id1)

  fun apply(env:Env, Id id:Id):denotableValue = env(Id id)
end
