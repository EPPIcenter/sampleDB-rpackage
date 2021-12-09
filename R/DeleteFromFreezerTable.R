
#' @export

delete_from_freezer_table <- function(id){
  conn <- dbConnect(RSQLite::SQLite(), "files/example_19-Oct-21.sample_db.sqlite")
  dbSendQuery(conn,
              paste0("DELETE FROM location WHERE id = ", id,";"))
  dbDisconnect(conn)
}
