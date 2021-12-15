#' @export

ListTables <- function(){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                             "../files/example_19-Oct-21.sample_db.sqlite")
  table_names <- RSQLite::dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_names)
}
