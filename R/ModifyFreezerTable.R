#' @export

modify_freezer_table <- function(id, description){
  conn <- dbConnect(RSQLite::SQLite(), "~/eppicenter/library/R/files/example_19-Oct-21.sample_db.sqlite")
  dbSendQuery(conn,
              paste0("UPDATE location SET description = '", description,"' WHERE id = ", id, ";"))
  dbDisconnect(conn)
}
