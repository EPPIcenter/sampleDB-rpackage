#' @export

ModifyFreezerTable <- function(id, description){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "~/eppicenter/library/R/sampleDB/files/example_19-Oct-21.sample_db.sqlite")

  tryCatch(
    RSQLite::dbSendQuery(conn,
                         paste0("UPDATE location SET description = '", description,"' WHERE id = ", id, ";")),
    error=function(e){
      if(e[1] == "UNIQUE constraint failed: location.description"){
        return(warning("FREEZER NAMES CANNOT BE DUPLICATED"))
      }
    }
  )

  RSQLite::dbDisconnect(conn)
}
