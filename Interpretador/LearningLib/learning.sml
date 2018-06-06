structure Learning =
struct
  val load_data = Dataset.load_data
  val save_data = Dataset.save_data
  val columns = Dataset.columns
  val remove_columns = Dataset.remove_columns
  val rows = Dataset.rows
  val num_rows = Dataset.num_rows

  val perceptron = Model.perceptron
  val pocket_perceptron = Model.pocket_perceptron
  val logistic_regression = Model.logistic_regression
  (*val linear_regression = Model.linear_regression*)

  val predict = Model.predict

  val save_model = Model.save_model
  val load_model = Model.load_model

end
