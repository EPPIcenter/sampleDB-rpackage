#' @import dplyr
#' @export

CheckTable <- function(table, database = "/databases/new.sampleDB.db"){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                             database)
  out_table <- RSQLite::dbGetQuery(conn, paste("SELECT * FROM", table)) %>% tibble()
  RSQLite::dbDisconnect(conn)
  return(out_table)
}
