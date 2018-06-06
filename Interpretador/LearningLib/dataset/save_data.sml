structure SaveData =
struct
fun insert_minus(ls) = if length(ls) = 1 then hd(ls) else hd(ls) ^ "-" ^ insert_minus(tl(ls));

fun compose_csv_row(row, separator) =
	if length(row) = 0 then
		"\n"
	else
		let
			val str = hd(row)
			val strs = String.tokens (fn c => c = #"~") str
		in
			if length(row) = 1 then
				insert_minus(strs) ^ "\n"
			else
				insert_minus(strs) ^ separator ^ compose_csv_row(tl(row), separator)
		end;

fun compose_csv_rows(rows, separator) =
	if length(rows) = 0 then
		""
	else
		compose_csv_row(hd(rows), separator) ^ compose_csv_rows(tl(rows), separator);

fun compose_csv((features, data), separator) =
	let
		val header = compose_csv_row(features, separator);
		val rows = compose_csv_rows(data, separator)
	in
		header ^ rows
	end;

fun save_data((features, data), filename, separator) =
	let
		val content = compose_csv((features, data), separator);
		val file = TextIO.openOut(filename);
        val _ = TextIO.output(file, content);
    in
		TextIO.closeOut(file)
    end;
end
