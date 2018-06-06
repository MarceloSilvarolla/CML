structure LogisticRegression =
struct
exception MoreThanOneOutput
exception DivergentNumberOfSamples
fun remove_from_list(l, i) =
    if i = 0 then
        (hd(l), tl(l))
    else
        let
            val (x, xs) = remove_from_list(tl(l), i-1)
        in
            (x, hd(l) :: xs)
        end;

fun shuffle(l) =
    let
        fun move_random_to_front(v, r) =
            if length(v) = 0 then
                []
            else
                let
                    val (x, xs) = remove_from_list(v, Random.randRange(0, length(v)-1) r)
                in
                    x :: move_random_to_front(xs, r)
                end
    in
        move_random_to_front(l, Random.rand(0,0))
    end;

fun random_permutation_rec(i, max) =
    if i = max then
        []
    else
        i :: random_permutation_rec(i+1, max);

fun random_permutation(len) = shuffle(random_permutation_rec(0, len));

fun create_batches(seq, batch_size, batch) =
    if length(seq) = 0 then
        []
    else
        let
            val batch = hd(seq) :: batch
        in
            if length(batch) = batch_size then
                batch :: create_batches(tl(seq), batch_size, [])
            else
                create_batches(tl(seq), batch_size, batch)
        end;

fun select_elements(v, positions) =
    if length(positions) = 0 then
        []
    else
        let
            val pos = hd(positions)
            val (x, _) = remove_from_list(v, pos)
        in
            x :: select_elements(v, tl(positions))
        end;

fun sigmoid(x) = 1.0 / (1.0 + Math.exp(~x));

fun predict_rec(batch, w) =
    if length(batch) = 0 then
        []
    else
        sigmoid(Perceptron.dot(hd(batch), w)) :: predict_rec(tl(batch), w);

fun update_weight(w, x, y, y_, learning_rate, batch_size) =
    if length(w) = 0 then
        []
    else
        let
            val wi = hd(w) - learning_rate * (y_ - y) * hd(x) / Real.fromInt(batch_size)
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

fun sgd(X, y, w, learning_rate, batches) =
    if length(batches) = 0 then
        w
    else
        let
            val batch = select_elements(X, hd(batches))
            val y_true = select_elements(y, hd(batches))
            val y_pred = predict_rec(batch, w)
            val w = update_weights(w, batch, y_true, y_pred, length(batch), learning_rate)
        in
            sgd(X, y, w, learning_rate, tl(batches))
        end;

fun cross_entropy(X, y, w) =
    if length(X) = 0 then
        0.0
    else
        let
            val x = hd(X)
            val y_true = hd(y)
            val y_pred = sigmoid(Perceptron.dot(x, w))
            val ce = ~y_true * Math.ln(y_pred + 1.0e~8) - (1.0 - y_true) * Math.ln(1.0 - y_pred + 1.0e~8)
        in
            ce + cross_entropy(tl(X), tl(y), w)
        end;

fun train(X, y, w, learning_rate, batch_size, epoch, num_epochs) =
	if epoch = num_epochs then
		w
	else
		(print("Epoch: " ^ Int.toString(epoch) ^ " ---- ");
		let
			val batches = create_batches(random_permutation(length(X)), batch_size, [])
		in
            let
                val _ = print("Mean Cross Entropy = " ^ Real.toString(cross_entropy(X, y, w) / Real.fromInt(length(X))) ^ "\n")
                val w = sgd(X, y, w, learning_rate, batches)
            in
                train(X, y, w, learning_rate, batch_size, epoch+1, num_epochs)
            end
		end);

exception NonPositiveLearningRate;
exception NonPositiveNumberOfEpochs;
exception LabelOfInterestNotPresent;

fun logistic_regression((X_features, X_data), (y_features, y_data), label_of_interest : string, learning_rate, batch_size, num_epochs) =
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
			val labels = PreprocessDataset.categories(y_col)
		in
			if not(PreprocessDataset.contains(labels, label_of_interest)) then
			   raise LabelOfInterestNotPresent
			else
				let
					val X_info = PreprocessDataset.inputs_info((X_features, X_data))
					val X = PreprocessDataset.preprocess_inputs((X_features, X_data), X_info, true)
					val X = PreprocessDataset.add_bias_column(X)

                    val labels = if hd(labels) = label_of_interest then labels else label_of_interest :: labels
					val y = PreprocessDataset.binarize(y_col, labels, 1.0, 0.0)

					val w =
                                          Perceptron.random_vector(PreprocessDataset.get_real_number_of_features(X_info)+1, Random.rand(0,1000000))
				in
					{model="logistic_regression", inputs_info=X_info,
                    outputs_info=(hd(y_features), labels),
                    parameters=train(X, y, w, learning_rate, batch_size, 0, num_epochs)}
				end
		end;

fun predict_itens(itens, w) =
	if length(itens) = 0 then
		[]
	else
		let
			val pred = sigmoid(Perceptron.dot(hd(itens), w))
		in
			[Real.toString(pred)] :: predict_itens(tl(itens), w)
		end;

fun predict_probabilities((X_features, X_data), {model=model_name, inputs_info=inputs_info, outputs_info=(y_feature, labels), parameters=parameters}) =
	let
		val X = PreprocessDataset.preprocess_inputs((X_features, X_data), inputs_info, true)
		val X = PreprocessDataset.add_bias_column(X)
	in
		([y_feature], predict_itens(X, parameters))
	end;

fun save_logistic_regression({model=model_name, inputs_info=inputs_info, outputs_info=(y_feature, labels), parameters=parameters}, filename) =
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

fun load_logistic_regression(filename) =
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
