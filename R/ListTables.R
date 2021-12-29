#' @import dplyr
#' @import RSQLite
#' @export

ListTables <- function(database){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                             database)
  table_names <- RSQLite::dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_names)
}
