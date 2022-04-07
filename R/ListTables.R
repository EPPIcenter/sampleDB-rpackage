#' @import dplyr
#' @import RSQLite
#' @export

ListTables <- function(database = "/var/lib/sampleDB/sampledb_database.sqlite"){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                             database)
  table_names <- RSQLite::dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_names)
}
