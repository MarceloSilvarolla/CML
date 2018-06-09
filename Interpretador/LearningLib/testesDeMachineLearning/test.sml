val dataset_path = "examples/linear_regression_example2.csv";

use "dataset/load_data.sml";
use "dataset/save_data.sml";
use "dataset/columns.sml";
use "dataset/remove_columns.sml";
use "models/linear_regression.sml";
use "models/logistic_regression.sml";
use "models/perceptron.sml";
use "models/pocket_perceptron.sml";
(*use "dataset/rows.sml";
use "dataset/column.sml";
use "dataset/num_rows.sml";
use "models/preprocess_dataset.sml";*)

val dataset = load_data(dataset_path, #",");

val X = remove_columns(dataset, ["price"]);
val y = columns(dataset, ["price"]);

print("==========================================================================================\n");

val model = load_linear_regression("linear_regression.model");

val predictions = predict_values(X, model);
save_data(predictions, "linear_output.csv", ",");

print("==========================================================================================\n");
