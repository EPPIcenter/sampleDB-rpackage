
#' @export

DeleteFromTable <- function(table_name, id){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "../files/example_19-Oct-21.sample_db.sqlite")
  RSQLite::dbSendQuery(conn,
              paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
  RSQLite::dbDisconnect(conn)
}
