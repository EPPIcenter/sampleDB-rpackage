#' @import dplyr
#' @export

CheckTable <- function(table, database = "/databases/sampledb/v0.0.2/sampledb_database.sqlite"){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                             database)
  out_table <- RSQLite::dbGetQuery(conn, paste("SELECT * FROM", table)) %>% tibble()
  RSQLite::dbDisconnect(conn)
  return(out_table)
}
