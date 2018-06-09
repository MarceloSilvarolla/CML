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

structure ReturnFlag =
struct
  type returnFlag = bool
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
  type Id = DataTypes.Id
  type denotableValue = DenotableValue.denotableValue
  type environment = Id -> denotableValue
  fun empty(id) = DenotableValue.Unbound

  fun extend(env:environment,DataTypes.Id
    id:DataTypes.Id,denVal:denotableValue)(DataTypes.Id id1:DataTypes.Id):denotableValue =
      if id1 = id then denVal else env(DataTypes.Id id1)

  fun apply(env:environment, DataTypes.Id id:DataTypes.Id):denotableValue =
    env(DataTypes.Id id)
end
