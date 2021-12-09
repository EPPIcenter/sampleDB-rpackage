#' @export

#freezer table options
AddToFreezerTable <- function(date_created, date_modified, description){

  #open connection
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(),
                              "~/eppicenter/library/R/sampleDB/files/example_19-Oct-21.sample_db.sqlite")

  #add to database
  tryCatch(
    if(description != ""){
      RSQLite::dbSendQuery(conn,
                           'INSERT INTO location (created, last_updated, description) VALUES (?, ?, ?);',
                           list(date_created, date_modified, description))
      error=function(e){
        if(e[1] == "UNIQUE constraint failed: location.description"){
          return(warning("FREEZER NAMES CANNOT BE DUPLICATED"))
        }
      }
    }

  )

  #close connection
  RSQLite::dbDisconnect(conn)
}
