structure LoadData =
struct
fun read_file(filename : string) =
    let
        val file = TextIO.openIn filename
        val content = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in
        String.tokens (fn c => c = #"\n") content
    end;

exception WrongNumberOfFeatures;

fun parse_data_points([], separator, num_features) = []
 |  parse_data_points((l :: lines), separator, num_features) =
    let
        val data_point = String.tokens (fn c => c = separator orelse c = #"\"") l
    in
        if length(data_point) <> num_features then
            raise WrongNumberOfFeatures
        else
            data_point :: parse_data_points(lines, separator, num_features)
    end;

fun load_data(filename : string, separator : char) =
    let
        val lines = read_file(filename)
        val header = hd(lines)
        val features = String.tokens (fn c => c = separator orelse c = #"\"") header
        val data = parse_data_points(tl(lines), separator, length(features))
    in
    	(features, data)
    end;
end
