structure Location =
struct
  type location = int
end

structure Array =
struct
  type array = Location.location list
end


structure StorableValue =
struct
  type dataset = Dataset.dataset
  type model = Model.model
  type array = Array.array
  datatype storableValue = Int of int | Real of real | Bool of bool | Char of
     char | String of string | Dataset of dataset | Model of model | Array of
     array | Unused | Undefined 
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

  fun allocate(sto:store):store*location = 
  let 
    fun leastUnusedLocationAfter(loc:location):location = 
      case sto(loc) of 
          StorableValue.Unused => loc
        | x      =>  let val loc = leastUnusedLocationAfter(loc+1)
                     in loc
                     end
    val loc = leastUnusedLocationAfter(0)
  in
    (update(sto,loc,Undefined),loc) 
  end

  fun deallocate(sto:store,loc:location):store = update(sto,loc,Unused)
end

