
#' @export

DeleteFromFreezerTable <- function(id){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "~/eppicenter/library/R/sampleDB/files/example_19-Oct-21.sample_db.sqlite")
  RSQLite::dbSendQuery(conn,
              paste0("DELETE FROM location WHERE id = ", id,";"))
  RSQLite::dbDisconnect(conn)
}
