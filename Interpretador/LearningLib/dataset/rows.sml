structure Rows =
struct
fun select_rows(rows, first, qt) =
    if first > 0 then
        select_rows(tl(rows), first-1, qt)
    else
        if qt = 0 then
            []
        else
            hd(rows) :: select_rows(tl(rows), 0, qt-1);

exception OutOfBoundariesRange;

fun rows((features, data), first, qt) =
    if qt <= 0 orelse first < 0 orelse first+qt > length(data) then
        raise OutOfBoundariesRange
    else
        (features, select_rows(data, first, qt));
end
