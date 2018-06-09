structure Perceptron =
struct
fun dot(a, b):real =
	if length(a) = 0 then 0.0 else hd(a) * hd(b) + dot(tl(a), tl(b));

fun sign(num:real) =
	if num < 0.0 then ~1.0 else if num > 0.0 then 1.0 else 0.0;

fun prediction(x, w) = sign(dot(x, w));

fun get_misclassified_point(X, y, w) =
	if length(X) = 0 then
		[]
	else
		let
			val xi = hd(X)
			val yi = hd(y)
			val y_pred = prediction(xi, w)
		in
			if Real.==(yi, y_pred) then
				get_misclassified_point(tl(X), tl(y), w)
			else
				[xi, [yi]]
		end;


fun add_vectors(a, b) : real list =
	if length(a) = 0 then
		[]
	else
		(hd(a)+hd(b)) :: add_vectors(tl(a), tl(b));

fun scale_vector(s, v) : real list =
	if length(v) = 0 then
		[]
	else
		(s * hd(v)) :: scale_vector(s, tl(v));

fun train(X, y, w, iter, num_iters) =
	if iter = num_iters then
		w
	else
		(print("Iteration: " ^ Int.toString(iter) ^ "\n");
		let
			val misclassified_point = get_misclassified_point(X, y, w)
		in
			if length(misclassified_point) = 0 then
				(print("ALL GOOD at " ^ Int.toString(iter) ^ "\n"); w)
			else
				let
					val xi = hd(misclassified_point)
					val yi = hd(hd(tl(misclassified_point)))
					val new_w = add_vectors(w, scale_vector(yi, xi))
				in
					train(X, y, new_w, iter+1, num_iters)
				end
		end);

fun random_vector(length, generator) =
	if length = 0 then
		[]
	else
		(Random.randReal generator) :: random_vector(length-1, generator);

exception MoreThanOneOutput;
exception MoreThanTwoLabels;
exception DivergentNumberOfSamples;
exception NonPositiveNumberOfIterations;

fun perceptron((X_features, X_data), (y_features, y_data), num_iters) =
	if length(y_features) <> 1 then
		raise MoreThanOneOutput
	else if length(X_data) <> length(y_data) then
		raise DivergentNumberOfSamples
	else if num_iters <= 0 then
		raise NonPositiveNumberOfIterations
	else
		let
			val y_col = Column.column((y_features, y_data), hd(y_features))
			val labels = PreprocessDataset.categories(y_col)
		in
			if length(labels) > 2 then
			   raise MoreThanTwoLabels
			else
				let
					val X_info = PreprocessDataset.inputs_info((X_features, X_data))
					val X = PreprocessDataset.preprocess_inputs((X_features, X_data), X_info, false)
					val X = PreprocessDataset.add_bias_column(X)
					val y = PreprocessDataset.binarize(y_col, labels, ~1.0, 1.0)
					val w = random_vector(PreprocessDataset.get_real_number_of_features(X_info)+1, Random.rand(0,1000000))
				in
					{model="perceptron", inputs_info=X_info, outputs_info=(hd(y_features), labels), parameters=train(X, y, w, 0, num_iters)}
				end
		end;

fun predict_itens(itens, labels, w) =
	if length(itens) = 0 then
		[]
	else
		let
			val pred = prediction(hd(itens), w)
		in
			if Real.==(pred, ~1.0) then
				[hd(labels)] :: predict_itens(tl(itens), labels, w)
			else
				[hd(tl(labels))] :: predict_itens(tl(itens), labels, w)
		end;

fun predict_label((X_features, X_data), {model=model_name, inputs_info=inputs_info, outputs_info=(y_feature, labels), parameters=parameters}) =
	let
		val X = PreprocessDataset.preprocess_inputs((X_features, X_data), inputs_info, false)
		val X = PreprocessDataset.add_bias_column(X)
	in
		([y_feature], predict_itens(X, labels, parameters))
	end;

fun save_perceptron({model=model_name, inputs_info=inputs_info, outputs_info=(y_feature, labels), parameters=parameters}, filename) =
	let
		val content = model_name ^ "\n";
		val content = content ^ "INPUT_INFO\n"
		val content = content ^ PreprocessDataset.format_inputs_info(inputs_info)
		val content = content ^ "OUTPUT_INFO\n"
		val content = content ^ y_feature ^ "\n"
		val content = content ^ PreprocessDataset.format_str_list(labels)
		val content = content ^ "PARAMETERS\n"
		val content = content ^ PreprocessDataset.format_real_list(parameters)

		val file = TextIO.openOut(filename);
        val _ = TextIO.output(file, content);
	in
		TextIO.closeOut(file)
	end;

fun load_perceptron(filename) =
	let
		val lines = LoadData.read_file(filename)
		val model_name = hd(lines)

		val (inputs_info, lines) = PreprocessDataset.parse_model_info(tl(lines))

		val lines = tl(lines)
		val y_feature = hd(lines)
		val lines = tl(lines)
		val labels = PreprocessDataset.parse_str_list(hd(lines))
		val lines = tl(lines)

		val lines = tl(lines)
		val parameters = PreprocessDataset.parse_real_list(hd(lines))
	in
		{model=model_name, inputs_info=inputs_info, outputs_info=(y_feature, labels), parameters=parameters}
	end;
end
