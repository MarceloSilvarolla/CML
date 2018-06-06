structure RemoveColumns =
struct
(* TODO: fix the import so that it can use the path relative to this file *)
(* use "columns.sml"; *)
(*use "dataset/columns.sml";*)
fun contains(l, x) =
    if l = [] then
        false
    else
        if hd(l) = x then
            true
        else
            contains(tl(l), x);

fun list_difference(l1, l2) =
    if l1 = [] then
        []
    else
        if contains(l2, hd(l1)) then
            list_difference(tl(l1), l2)
        else
            hd(l1) :: list_difference(tl(l1), l2);

fun remove_columns((features, data), features_to_remove) =
    let
        val features_to_keep = list_difference(features, features_to_remove)
    in
        Columns.columns((features, data), features_to_keep)
    end;
end
