#' @export

CheckTable <- function(table){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                             "~/eppicenter/library/R/sampleDB/files/example_19-Oct-21.sample_db.sqlite")
  out_table <- RSQLite::dbGetQuery(conn, paste("SELECT * FROM", table)) %>% tibble()
  RSQLite::dbDisconnect(conn)
  return(out_table)
}
