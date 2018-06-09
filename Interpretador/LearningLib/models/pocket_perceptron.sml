structure PocketPerceptron =
struct
exception MoreThanOneOutput
exception DivergentNumberOfSamples
exception NonPositiveNumberOfIterations
exception MoreThanTwoLabels

fun num_misclassified_points(X, y, w) =
	if length(X) = 0 then
		0
	else
		let
			val xi = hd(X)
			val yi = hd(y)
			val y_pred = Perceptron.prediction(xi, w)
		in
			if Real.==(yi, y_pred) then
				num_misclassified_points(tl(X), tl(y), w)
			else
				1 + num_misclassified_points(tl(X), tl(y), w)
		end;

fun train(X, y, w, iter, num_iters, best_w, min_wrong) =
	if iter = num_iters then
		best_w
	else
		(print("Iteration: " ^ Int.toString(iter) ^ "\n");
		 if min_wrong = 0 then
			 (print("ALL GOOD at " ^ Int.toString(iter) ^ "\n"); best_w)
		 else
			 let
				 val misclassified_point = Perceptron.get_misclassified_point(X, y, w)
				 val xi = hd(misclassified_point)
				 val yi = hd(hd(tl(misclassified_point)))
				 val new_w = Perceptron.add_vectors(w, Perceptron.scale_vector(yi, xi))
				 val num_wrong = num_misclassified_points(X, y, new_w)
			 in
				 if num_wrong < min_wrong then
					 train(X, y, new_w, iter+1, num_iters, new_w, num_wrong)
				 else
					 train(X, y, new_w, iter+1, num_iters, best_w, min_wrong)
			 end);


fun pocket_perceptron((X_features, X_data), (y_features, y_data), num_iters) =
	if length(y_features) <> 1 then
		raise MoreThanOneOutput
	else if length(X_data) <> length(y_data) then
		raise DivergentNumberOfSamples
	else if num_iters <= 0 then
		raise NonPositiveNumberOfIterations
	else
		let
			val y_col = Column.column((y_features, y_data), hd(y_features));
			val labels = PreprocessDataset.categories(y_col);
		in
			if length(labels) > 2 then
				raise MoreThanTwoLabels
			else
				let
					val X_info = PreprocessDataset.inputs_info((X_features, X_data));
					val X = PreprocessDataset.preprocess_inputs((X_features, X_data), X_info, false);
					val X = PreprocessDataset.add_bias_column(X);
					val y = PreprocessDataset.binarize(y_col, labels, ~1.0, 1.0);
					val w = Perceptron.random_vector(PreprocessDataset.get_real_number_of_features(X_info)+1, Random.rand(0,1000000));
				in
					{model="perceptron", inputs_info=X_info, outputs_info=(hd(y_features), labels), parameters=train(X, y, w, 0, num_iters, w, num_misclassified_points(X, y, w))}
				end
		end;
end
