#' @import dplyr
#' @import RSQLite
#' @export

ListTables <- function(database = "/databases/new.sampleDB.db"){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                             database)
  table_names <- RSQLite::dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_names)
}
