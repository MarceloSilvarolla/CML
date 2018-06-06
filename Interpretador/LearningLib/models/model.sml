structure Model =
struct
  datatype model = 
    Classifier of {model:string, inputs_info : (string list * real
      list list)*(string list * string list list), outputs_info:string * string
      list, parameters:real list}
  | Regressor of {model:string, inputs_info:(string list * real
      list list)*(string list * string list list), outputs_info:string * (string list * real
      list list)*(string list * string list list), parameters:real list}

  type dataset = Dataset.dataset

  val perceptron = Perceptron.perceptron
  val pocket_perceptron = PocketPerceptron.pocket_perceptron
  (*val linear_regression = LinearRegression.linear_regression*)
  val logistic_regression = LogisticRegression.logistic_regression

  fun predict(dataset:dataset, Classifier {model=model_name, inputs_info=inputs_info,
    outputs_info=outputs_info, parameters=parameters}) =
    case model_name of
        "perceptron" => Perceptron.predict_label(dataset, {model=model_name, inputs_info=inputs_info,
          outputs_info=outputs_info, parameters=parameters})
      | "logistic_regression" => LogisticRegression.predict_probabilities(dataset, {model=model_name, inputs_info=inputs_info,
          outputs_info=outputs_info, parameters=parameters})
      
  (*| predict(dataset:dataset, Regressor {model=model_name, inputs_info=inputs_info,
    outputs_info=outputs_info, parameters=parameters}) = 
    case model_name of
        "linear_regression" => LinearRegression.predict_value(dataset, {model=model_name, inputs_info=inputs_info,
          outputs_info=outputs_info, parameters=parameters}) *)



  fun save_model(Classifier {model=model_name, inputs_info=inputs_info,
    outputs_info=outputs_info, parameters=parameters}, filename) =
    case model_name of
        "perceptron" => Perceptron.save_perceptron({model=model_name, inputs_info=inputs_info,
        outputs_info=outputs_info, parameters=parameters}, filename)
      | "logistic_regression" => LogisticRegression.save_logistic_regression({model=model_name, inputs_info=inputs_info,
        outputs_info=outputs_info, parameters=parameters}, filename)
  
  (*| save_model(Regressor {model=model_name, inputs_info=inputs_info,
    outputs_info=outputs_info, parameters=parameters}, filename) =
    case model_name of
        "linear_regression" => LinearRegression.save_linear_regression({model=model_name, inputs_info=inputs_info,
          outputs_info=outputs_info, parameters=parameters}, filename) *)

  fun load_model(filename) =
    let
      val lines = LoadData.read_file(filename)
    in
      case hd(lines) of
          "perceptron" => Perceptron.load_perceptron(filename)
        | "logistic_regression" => LogisticRegression.load_logistic_regression(filename)
      (*  | "linear_regression" =>
      *  LinearRegression.load_linear_regression(filename) *)
    end

end
