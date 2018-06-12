structure Model =
struct
  datatype model =
    Classifier of {model:string, inputs_info : (string list * real
      list list)*(string list * string list list), outputs_info:string * string
      list, parameters:real list}
  | Regressor of {model:string, inputs_info:(string list * real
      list list)*(string list * string list list), outputs_info:string * ((string list * real
      list list)*(string list * string list list)), parameters:real list}

  exception CorruptedModel
  type dataset = Dataset.dataset

  fun perceptron(X:dataset, y:dataset, num_iters:int):model =
    Classifier (Perceptron.perceptron(X, y, num_iters))

  fun pocket_perceptron(X:dataset, y:dataset, num_iters:int):model =
    Classifier (PocketPerceptron.pocket_perceptron(X, y, num_iters))

  fun logistic_regression(X:dataset, y:dataset, label_of_interest:string, learning_rate:real, batch_size:int, num_epochs:int):model =
    Classifier (LogisticRegression.logistic_regression(X, y, label_of_interest, learning_rate, batch_size, num_epochs))

  fun linear_regression(X:dataset, y:dataset, learning_rate:real, batch_size:int, num_epochs:int) =
    Regressor (LinearRegression.linear_regression(X, y, learning_rate, batch_size, num_epochs))


    fun predict(dataset:dataset, Classifier {model=model_name, inputs_info=inputs_info,
                                                outputs_info=outputs_info, parameters=parameters}) =
        (case model_name of
            "perceptron" => Perceptron.predict_label(dataset, {model=model_name, inputs_info=inputs_info,
                                                    outputs_info=outputs_info, parameters=parameters})
        |   "logistic_regression" => LogisticRegression.predict_probabilities(dataset, {model=model_name, inputs_info=inputs_info,
                                                    outputs_info=outputs_info, parameters=parameters})
        |   _ => raise CorruptedModel
        )

    |   predict(dataset:dataset, Regressor {model=model_name, inputs_info=inputs_info,
                                                outputs_info=outputs_info, parameters=parameters}) =
        (case model_name of
            "linear_regression" => LinearRegression.predict_values(dataset, {model=model_name, inputs_info=inputs_info,
                                                    outputs_info=outputs_info, parameters=parameters})
        |   _ => raise CorruptedModel
        )

  fun save_model(Classifier {model=model_name, inputs_info=inputs_info,
                                            outputs_info=outputs_info, parameters=parameters}, filename) =
    (case model_name of
        "perceptron" => Perceptron.save_perceptron({model=model_name, inputs_info=inputs_info,
            outputs_info=outputs_info, parameters=parameters}, filename)
    |   "logistic_regression" => LogisticRegression.save_logistic_regression({model=model_name, inputs_info=inputs_info,
            outputs_info=outputs_info, parameters=parameters}, filename)
    |   _ => raise CorruptedModel
    )

  |   save_model(Regressor {model=model_name, inputs_info=inputs_info,
        outputs_info=outputs_info, parameters=parameters}, filename) =
    (case model_name of
        "linear_regression" => LinearRegression.save_linear_regression({model=model_name, inputs_info=inputs_info,
                outputs_info=outputs_info, parameters=parameters}, filename)
    |   _ => raise CorruptedModel)

  fun load_model(filename) =
    let
      val lines = LoadData.read_file(filename)
    in
      case hd(lines) of
          "perceptron" => Classifier (Perceptron.load_perceptron(filename))
        | "logistic_regression" => Classifier (LogisticRegression.load_logistic_regression(filename))
        | "linear_regression" => Regressor (LinearRegression.load_linear_regression(filename))
        | _ => raise CorruptedModel
    end

end
