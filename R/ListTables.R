#' @import dplyr
#' @import RSQLite
#' @export

ListTables <- function(database = Sys.getenv("SDB_PATH")){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                             database)
  table_names <- RSQLite::dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_names)
}
