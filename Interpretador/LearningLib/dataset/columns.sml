structure Columns =
struct
fun contains(l, x) =
    if l = [] then
        false
    else
        if hd(l) = x then
            true
        else
            contains(tl(l), x);

fun select_features(row, features, features_to_keep) =
    if row = [] then
        []
    else
        if features_to_keep <> [] andalso hd(features) = hd(features_to_keep) then
            hd(row) :: select_features(tl(row), tl(features), tl(features_to_keep))
        else
            select_features(tl(row), tl(features), features_to_keep);

fun select_rows_features(rows, features, features_to_keep) =
    if rows = [] then
        []
    else
        select_features(hd(rows), features, features_to_keep) :: select_rows_features(tl(rows), features, features_to_keep);

fun align_features(l, order) =
    if order = [] then
        []
    else
        if contains(l, hd(order)) then
            hd(order) :: align_features(l, tl(order))
        else
            align_features(l, tl(order));

fun columns((features, data), features_to_keep) =
    let
        val features_to_keep = align_features(features_to_keep, features)
    in
        (features_to_keep, select_rows_features(data, features, features_to_keep))
    end;
end
