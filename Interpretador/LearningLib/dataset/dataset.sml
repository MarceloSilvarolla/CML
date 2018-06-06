structure Dataset =
struct
  type dataset = string list * string list list
  val rows = Rows.rows
  val num_rows = NumRows.num_rows
  val columns = Columns.columns
  val remove_columns = RemoveColumns.remove_columns
  val load_data = LoadData.load_data
  val save_data = SaveData.save_data
end

