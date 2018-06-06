structure Column = 
struct
exception FeatureNotPresent;

fun select_feature(row, features, feature_to_keep) =
    if row = [] then
        raise FeatureNotPresent
    else
        if hd(features) = feature_to_keep then
            hd(row)
        else
            select_feature(tl(row), tl(features), feature_to_keep);

fun select_rows_feature(rows, features, feature_to_keep) =
    if rows = [] then
        []
    else
        select_feature(hd(rows), features, feature_to_keep) :: select_rows_feature(tl(rows), features, feature_to_keep);

fun column((features, data), feature_to_keep) =
    select_rows_feature(data, features, feature_to_keep);
end
