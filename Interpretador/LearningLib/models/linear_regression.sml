structure LinearRegression =
struct
exception MoreThanOneOutput
exception DivergentNumberOfSamples
exception NonPositiveLearningRate;
exception NonPositiveNumberOfEpochs;

fun update_weight(w, x, y, y_, learning_rate, batch_size) =
    if length(w) = 0 then
        []
    else
        let
            val wi = hd(w) - learning_rate * 2.0 * (y_ - y) * hd(x) / Real.fromInt(batch_size)
        in
            wi :: update_weight(tl(w), tl(x), y, y_, learning_rate, batch_size)
        end;

fun update_weights(w, X, y_true, y_pred, batch_size, learning_rate) =
    if length(X) = 0 then
        w
    else
        let
            val x = hd(X)
            val y = hd(y_true)
            val y_ = hd(y_pred)
            val w = update_weight(w, x, y, y_, learning_rate, batch_size)
        in
            update_weights(w, tl(X), tl(y_true), tl(y_pred), batch_size, learning_rate)
        end;

fun predict_rec(batch, w) =
    if length(batch) = 0 then
        []
    else
        Perceptron.dot(hd(batch), w) :: predict_rec(tl(batch), w);

fun sgd(X, y, w, learning_rate, batches) =
    if length(batches) = 0 then
        w
    else
        let
            val batch = LogisticRegression.select_elements(X, hd(batches))
            val y_true = LogisticRegression.select_elements(y, hd(batches))
            val y_pred = predict_rec(batch, w)
            val w = update_weights(w, batch, y_true, y_pred, length(batch), learning_rate)
        in
            sgd(X, y, w, learning_rate, tl(batches))
        end;

fun squared_error(X, y, w) =
    if length(X) = 0 then
        0.0
    else
        let
            val x = hd(X)
            val y_true = hd(y)
            val y_pred = Perceptron.dot(x, w)
            val se = (y_pred - y_true) * (y_pred - y_true)
        in
            se + squared_error(tl(X), tl(y), w)
        end;

fun train(X, y, w, learning_rate, batch_size, epoch, num_epochs) =
    if epoch = num_epochs then
        w
    else
        (print("Epoch: " ^ Int.toString(epoch) ^ " ---- ");
        let
            val batches = LogisticRegression.create_batches(LogisticRegression.random_permutation(length(X)), batch_size, [])
        in
            let
                val _ = print("Mean Squared Error = " ^ Real.toString(squared_error(X, y, w) / Real.fromInt(length(X))) ^ "\n")
                val w = sgd(X, y, w, learning_rate, batches)
            in
                train(X, y, w, learning_rate, batch_size, epoch+1, num_epochs)
            end
        end);

exception NonNumericOutputs;

fun linear_regression((X_features, X_data), (y_features, y_data), learning_rate, batch_size, num_epochs) =
	if length(y_features) <> 1 then
		raise MoreThanOneOutput
	else if length(X_data) <> length(y_data) then
		raise DivergentNumberOfSamples
	else if num_epochs <= 0 then
		raise NonPositiveNumberOfEpochs
    else if learning_rate <= 0.0 then
        raise NonPositiveLearningRate
	else
		let
			val y_col = Column.column((y_features, y_data), hd(y_features))
		in
			if not(PreprocessDataset.is_numeric(y_col)) then
                raise NonNumericOutputs
            else
                let
                    val X_info = PreprocessDataset.inputs_info((X_features, X_data))
                    val X = PreprocessDataset.preprocess_inputs((X_features, X_data), X_info, true)
                    val X = PreprocessDataset.add_bias_column(X)

                    val y_info = PreprocessDataset.inputs_info((y_features, y_data))
                    val y = PreprocessDataset.preprocess_inputs((y_features, y_data), y_info, true)
                    val y = PreprocessDataset.extract_values(y)

                    val w = Perceptron.random_vector(PreprocessDataset.get_real_number_of_features(X_info)+1, Random.rand(0,1000000))
                in
                    {model="linear_regression", inputs_info=X_info,
                    outputs_info=(hd(y_features), y_info),
                    parameters=train(X, y, w, learning_rate, batch_size, 0, num_epochs)}
                end
		end;

fun predict_itens(itens, w, ((numeric_features, numeric_info), categorical_info)) =
	if length(itens) = 0 then
		[]
	else
		let
            val mean = hd(hd(numeric_info))
            val stddev = hd(tl(hd(numeric_info)))
			val pred = Perceptron.dot(hd(itens), w) * stddev + mean
		in
			[Real.toString(pred)] :: predict_itens(tl(itens), w, ((numeric_features, numeric_info), categorical_info))
		end;

fun predict_values((X_features, X_data), {model=model_name, inputs_info=inputs_info, outputs_info=(y_feature, outputs_info), parameters=parameters}) =
	let
		val X = PreprocessDataset.preprocess_inputs((X_features, X_data), inputs_info, true)
		val X = PreprocessDataset.add_bias_column(X)
	in
		([y_feature], predict_itens(X, parameters, outputs_info))
	end;

fun format_output_info((((numeric_features, numeric_info), categorical_info))) =
    PreprocessDataset.format_str_list(numeric_features) ^ PreprocessDataset.format_real_list_list(numeric_info);

fun save_linear_regression({model=model_name, inputs_info=inputs_info, outputs_info=(y_feature, outputs_info), parameters=parameters}, filename) =
    let
        val content = model_name ^ "\n";
        val content = content ^ "INPUT_INFO\n"
        val content = content ^ PreprocessDataset.format_inputs_info(inputs_info)
        val content = content ^ "OUTPUT_INFO\n"
        val content = content ^ format_output_info(outputs_info)
        val content = content ^ "PARAMETERS\n"
        val content = content ^ PreprocessDataset.format_real_list(parameters)

        val file = TextIO.openOut(filename);
        val _ = TextIO.output(file, content);
    in
        TextIO.closeOut(file)
    end;


fun load_linear_regression(filename) =
	let
        val lines = LoadData.read_file(filename)

        val model_name = hd(lines)
        val lines = tl(lines)

		val (inputs_info, lines) = PreprocessDataset.parse_model_info(lines)

        val lines = tl(lines)
        val y_feature = hd(lines)
        val lines = tl(lines)
        val y_info = [PreprocessDataset.parse_real_list(hd(lines))]
        val lines = tl(lines)
        val outputs_info = (   ([y_feature], y_info), (tl(["dummy"]), tl([["dummy"]]))   )

		val lines = tl(lines)
		val parameters = PreprocessDataset.parse_real_list(hd(lines))
	in
		{model=model_name, inputs_info=inputs_info, outputs_info=(y_feature, outputs_info), parameters=parameters}
	end;
end
