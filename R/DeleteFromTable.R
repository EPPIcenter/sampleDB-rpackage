
#' @export

DeleteFromTable <- function(table_name, id){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "/Users/severianovillarruel/eppicenter/library/R/packages/sampleDB/shiny/files/example_19-Oct-21.sample_db.sqlite")
  RSQLite::dbSendQuery(conn,
              paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
  RSQLite::dbDisconnect(conn)
}
