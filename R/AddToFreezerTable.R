#' @export

#freezer table options
AddToFreezerTable <- function(table_name, info_list){

  #open connection
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(),
                              "~/eppicenter/library/R/sampleDB/files/example_19-Oct-21.sample_db.sqlite")

  #add to database
  tryCatch(
    if(info_list$description != ""){

      #get names of columns to modify
      column_names <- paste0(names(info_list), collapse = ", ")
      #create ??? filler (required by dbSendQuery)
      filler <- replicate(length(info_list), "?") %>% paste0(., collapse = ", ")
      #rename info_list (required by dbSendQuery)
      names(info_list) <- NULL

      #modify database
      RSQLite::dbSendQuery(conn,
                           paste0('INSERT INTO ', table_name, ' (', column_names, ') VALUES (', filler, ');'),
                           info_list)

      #handle duplicate entry error(s)
      error=function(e){
        if(e[1] == "UNIQUE constraint failed: location.description"){
          return(warning("FREEZER NAMES CANNOT BE DUPLICATED"))
        }
      }
    }

  )


  # #add to database
  # tryCatch(
  #   if(description != ""){
  #     RSQLite::dbSendQuery(conn,
  #                          'INSERT INTO location (created, last_updated, description) VALUES (?, ?, ?);',
  #                          list(date_created, date_modified, description))
  #     error=function(e){
  #       if(e[1] == "UNIQUE constraint failed: location.description"){
  #         return(warning("FREEZER NAMES CANNOT BE DUPLICATED"))
  #       }
  #     }
  #   }
  #
  # )

  #close connection
  RSQLite::dbDisconnect(conn)
}
