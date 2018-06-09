structure PreprocessDataset =
struct
fun contains(l, x) =
    if l = [] then
        false
    else
        if hd(l) = x then
            true
        else
            contains(tl(l), x);

fun print_int_list(l) =
    if length(l) = 0 then
        print("\n")
    else
        (print(" " ^ Int.toString(hd(l))); print_int_list(tl(l)));

fun print_int_list_list(l) =
    if length(l) = 0 then
        print("\n")
    else
        (print_int_list(hd(l)); print_int_list_list(tl(l)));

fun print_real_list(l) =
    if length(l) = 0 then
        print("\n")
    else
        (print(" " ^ Real.toString(hd(l))); print_real_list(tl(l)));

fun print_real_list_list(l) =
    if length(l) = 0 then
        print("\n")
    else
        (print_real_list(hd(l)); print_real_list_list(tl(l)));


fun display_list([]) = ()
 |  display_list(h :: t) = (print(h ^ "\n"); display_list(t));

fun is_numeric([]) = true
 |  is_numeric(h :: t) =
    if ((valOf(Real.fromString(h)); false) handle e => true) then
        false
    else
        is_numeric(t);

fun str_to_real([]) = []
 |  str_to_real(h :: t) = valOf(Real.fromString(h)) :: str_to_real(t);

fun mean_rec([], acum, cnt) = acum / cnt
 |  mean_rec((h :: v), acum, cnt) =
        mean_rec(v, acum + h, cnt + 1.0);

fun mean(v) : real = mean_rec(v, 0.0, 0.0);

fun variance_rec([], mean, acum, cnt) = acum / cnt
 |  variance_rec((h :: v), mean, acum, cnt) =
        variance_rec(v, mean, acum + (h-mean)*(h-mean), cnt+1.0);

fun variance(v) = variance_rec(v, mean(v), 0.0, 0.0);

fun stddev(v) = Math.sqrt(variance(v));

fun standardize([], v_mean, v_stddev) = []
 |  standardize(h :: t, v_mean, v_stddev) =
    ((h - v_mean) / v_stddev) :: standardize(t, v_mean, v_stddev);

fun numeric_features_info((features, data), features_to_check, numerical_features, numerical_info) =
    if length(features_to_check) = 0 then
        (numerical_features, numerical_info)
    else
        let
            val col = Column.column((features, data), hd(features_to_check))
        in
            if is_numeric(col) then
                let
                    val col = str_to_real(col)
                    val col_mean = mean(col)
                    val col_stddev = stddev(col)
                    val feature_info = col_mean :: col_stddev :: []
                in
                    numeric_features_info((features, data), tl(features_to_check), hd(features_to_check) :: numerical_features, feature_info :: numerical_info)
                end
            else
                numeric_features_info((features, data), tl(features_to_check), numerical_features, numerical_info)
        end;

fun categories_rec(v, categories) =
    if v = [] then
        categories
    else
        if contains(categories, hd(v)) then
            categories_rec(tl(v), categories)
        else
            categories_rec(tl(v), hd(v) :: categories);

fun categories(v) = categories_rec(v, []);

fun categorical_features_info((features, data), features_to_check, categorical_features, categorical_info) =
    if length(features_to_check) = 0 then
        (categorical_features, categorical_info)
    else
        let
            val col = Column.column((features, data), hd(features_to_check))
        in
            if is_numeric(col) then
                categorical_features_info((features, data), tl(features_to_check), categorical_features, categorical_info)
            else
                let
                    val feature_info = categories(col)
                in
                    categorical_features_info((features, data), tl(features_to_check), hd(features_to_check) :: categorical_features, feature_info :: categorical_info)
                end
        end;

fun inputs_info((features, data)) =
    let
        val numeric_info = numeric_features_info((features, data), features, [], [])
        val categorical_info = categorical_features_info((features, data), features, [], [])
    in
        (numeric_info, categorical_info)
    end;

fun get_info(feature, features, info) = if feature = hd(features) then hd(info) else get_info(feature, tl(features), tl(info));

fun one_hot_encode(item, categories) =
    if length(categories) = 0 then
        []
    else
        if item = hd(categories) then
            1.0 :: one_hot_encode(item, tl(categories))
        else
            0.0 :: one_hot_encode(item, tl(categories))

exception FeatureNotPresentInInfo;
exception NonNumericValueForNumericFeature;

fun preprocess_row(features, row, numerical_features, numerical_info, categorical_features, categorical_info, standardize) =
    if length(row) = 0 then
        []
    else if contains(numerical_features, hd(features)) then
        if ((valOf(Real.fromString(hd(row))); true) handle e => false) then
            let
                val info = get_info(hd(features), numerical_features, numerical_info)
                val mean = hd(info)
                val stddev = hd(tl(info))
                val value = hd(str_to_real(hd(row) :: []))
            in
				if standardize then
					((value - mean) / stddev) :: preprocess_row(tl(features), tl(row), numerical_features, numerical_info, categorical_features, categorical_info, standardize)
				else
					value :: preprocess_row(tl(features), tl(row), numerical_features, numerical_info, categorical_features, categorical_info, standardize)
            end
        else
            raise NonNumericValueForNumericFeature
    else if contains(categorical_features, hd(features)) then
        let
            val categories = get_info(hd(features), categorical_features, categorical_info)
        in
            one_hot_encode(hd(row), categories) @ preprocess_row(tl(features), tl(row), numerical_features, numerical_info, categorical_features, categorical_info, standardize)
        end
    else
        raise FeatureNotPresentInInfo;


fun preprocess_rows(features, rows, numerical_features, numerical_info, categorical_features, categorical_info, standardize) =
    if length(rows) = 0 then
        []
    else
        let
            val row = preprocess_row(features, hd(rows), numerical_features, numerical_info, categorical_features, categorical_info, standardize)
        in
            row :: preprocess_rows(features, tl(rows), numerical_features, numerical_info, categorical_features, categorical_info, standardize)
        end;

fun preprocess_inputs((features, data), ((numerical_features, numerical_info), (categorical_features, categorical_info)), standardize) =
    preprocess_rows(features, data, numerical_features, numerical_info, categorical_features, categorical_info, standardize);

fun binarize(v, labels, l1, l2) =
	if length(v) = 0 then
		[]
	else
		if hd(v) = hd(labels) then
			l1 :: binarize(tl(v), labels, l1, l2)
		else
			l2 :: binarize(tl(v), labels, l1, l2);

fun extract_values(v) = if length(v) = 0 then [] else hd(hd(v)) :: extract_values(tl(v));

fun sum_lengths(l) = if length(l) = 0 then 0 else length(hd(l)) + sum_lengths(tl(l));

fun get_real_number_of_features((numerical_features, numerical_info), (categorical_features, categorical_info)) =
	length(numerical_features) + sum_lengths(categorical_info);

fun add_bias_column(rows) = if length(rows) = 0 then [] else (1.0 :: hd(rows)) :: add_bias_column(tl(rows));

fun format_str_list(l) =
    if length(l) = 0 then
        "\n"
    else if length(l) = 1 then
        hd(l) ^ "\n"
    else
        hd(l) ^ "," ^ format_str_list(tl(l));

fun format_str_list_list(rll) =
    if length(rll) = 0 then
        ""
    else
        format_str_list(hd(rll)) ^ format_str_list_list(tl(rll));

fun format_real_list(rl) =
    if length(rl) = 0 then
        "\n"
    else if length(rl) = 1 then
        Real.toString(hd(rl)) ^ "\n"
    else
        Real.toString(hd(rl)) ^ "," ^ format_real_list(tl(rl));

fun format_real_list_list(rll) =
    if length(rll) = 0 then
        ""
    else
        format_real_list(hd(rll)) ^ format_real_list_list(tl(rll));

fun format_inputs_info((numerical_features, numerical_info), (categorical_features, categorical_info)) =
    let
        val content = "NUMERICAL\n"
        val content = content ^ Int.toString(length(numerical_features)) ^ "\n"
        val content = content ^ format_str_list(numerical_features)
        val content = content ^ format_real_list_list(numerical_info)
        val content = content ^ "CATEGORICAL\n"
        val content = content ^ Int.toString(length(categorical_features)) ^ "\n"
        val content = content ^ format_str_list(categorical_features)
        val content = content ^ format_str_list_list(categorical_info)
    in
        content
    end;

fun parse_str_list(line) = String.tokens (fn c => c = #",") line;

fun parse_str_list_list(lines, qt_lines) =
    if qt_lines = 0 then
        ([], lines)
    else
        let
            val l = parse_str_list(hd(lines))
            val (ls, lines) = parse_str_list_list(tl(lines), qt_lines-1)
        in
            (l :: ls, lines)
        end;

fun parse_real_list(l) = str_to_real(parse_str_list(l));

fun parse_real_list_list(lines, qt_lines) =
    if qt_lines = 0 then
        ([], lines)
    else
        let
            val l = parse_real_list(hd(lines))
            val (ls, lines) = parse_real_list_list(tl(lines), qt_lines-1)
        in
            (l :: ls, lines)
        end;

fun parse_model_info(lines) =
    let
        val lines = tl(lines)

        val lines = tl(lines)
        val num_numerical = hd(lines)
        val lines = tl(lines)

        val numerical_features = if num_numerical = "0" then [] else parse_str_list(hd(lines))
        val lines = if num_numerical = "0" then lines else tl(lines)
        val (numerical_info, lines) = parse_real_list_list(lines, length(numerical_features))

        val lines = tl(lines)
        val num_categorical = hd(lines)
        val lines = tl(lines)

        val categorical_features = if num_categorical = "0" then [] else parse_str_list(hd(lines))
        val lines = if num_categorical = "0" then lines else tl(lines)
        val (categorical_info, lines) = parse_str_list_list(lines, length(categorical_features))
    in
        (((numerical_features, numerical_info), (categorical_features, categorical_info)), lines)
    end;
end
