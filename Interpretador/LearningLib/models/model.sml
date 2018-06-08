structure Model =
struct
  datatype model =
    Classifier of {model:string, inputs_info : (string list * real
      list list)*(string list * string list list), outputs_info:string * string
      list, parameters:real list}
  | Regressor of {model:string, inputs_info:(string list * real
      list list)*(string list * string list list), outputs_info:string * ((string list * real
      list list)*(string list * string list list)), parameters:real list}

  type dataset = Dataset.dataset

  val perceptron = Perceptron.perceptron
  val pocket_perceptron = PocketPerceptron.pocket_perceptron
  val linear_regression = LinearRegression.linear_regression
  val logistic_regression = LogisticRegression.logistic_regression

  (*TODO: tratar o caso em que model_name não é nem perceptron nem logistic_regression *)
  fun predict(dataset:dataset, Classifier {model=model_name, inputs_info=inputs_info,
    outputs_info=outputs_info, parameters=parameters}) =
    if model_name = "perceptron" then
        Perceptron.predict_label(dataset, {model=model_name, inputs_info=inputs_info,
          outputs_info=outputs_info, parameters=parameters})
    else
        LogisticRegression.predict_probabilities(dataset, {model=model_name, inputs_info=inputs_info,
          outputs_info=outputs_info, parameters=parameters})

    (*TODO: Tratar o caso em que model_name não é linear_regression*)
    | predict(dataset:dataset, Regressor {model=model_name, inputs_info=inputs_info,
        outputs_info=outputs_info, parameters=parameters}) =
            LinearRegression.predict_values(dataset, {model=model_name, inputs_info=inputs_info,
              outputs_info=outputs_info, parameters=parameters})


  (*TODO: tratar o caso em que model_name não é nem perceptron nem logistic_regression *)
  fun save_model(Classifier {model=model_name, inputs_info=inputs_info,
    outputs_info=outputs_info, parameters=parameters}, filename) =
    if model_name = "perceptron" then
        Perceptron.save_perceptron({model=model_name, inputs_info=inputs_info,
            outputs_info=outputs_info, parameters=parameters}, filename)
    else
        LogisticRegression.save_logistic_regression({model=model_name, inputs_info=inputs_info,
            outputs_info=outputs_info, parameters=parameters}, filename)

    (*TODO: Tratar o caso em que model_name não é linear_regression*)
    | save_model(Regressor {model=model_name, inputs_info=inputs_info,
        outputs_info=outputs_info, parameters=parameters}, filename) =
            LinearRegression.save_linear_regression({model=model_name, inputs_info=inputs_info,
                outputs_info=outputs_info, parameters=parameters}, filename)

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
