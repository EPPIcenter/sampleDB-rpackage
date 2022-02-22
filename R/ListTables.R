#' @import dplyr
#' @import RSQLite
#' @export

ListTables <- function(database = "/databases/sampledb/v0.0.2/sampledb_database.sqlite"){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                             database)
  table_names <- RSQLite::dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_names)
}
