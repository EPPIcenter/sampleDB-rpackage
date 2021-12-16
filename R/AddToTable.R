#' @import dplyr
#' @export

#table options
AddToTable <- function(table_name, info_list){

  #open connection
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(),
                              "../files/example_19-Oct-21.sample_db.sqlite")

  # info_list <- list(plate_id = 123223, barcode = "dummy2", well_position = "A01")
  # table_name <- "matrix_tube"

  # NOTE need to file below
  for(i in info_list){
    if(i == ""){
      return()
    }
  }

  #get names of columns to modify
  column_names <- paste0(names(info_list), collapse = ", ")
  #create ??? filler (required by dbSendQuery)
  filler <- replicate(length(info_list), "?") %>% paste0(., collapse = ", ")
  #rename info_list (required by dbSendQuery)
  names(info_list) <- NULL

  tryCatch(

      #modify database
      RSQLite::dbSendQuery(conn,
                           paste0('INSERT INTO ', table_name, ' (', column_names, ') VALUES (', filler, ');'),
                           info_list),

      #handle duplicate entry error(s)
      error=function(e){
        print(e)
        # if(e[1] == "UNIQUE constraint failed: location.description"){
          # return(warning("FREEZER NAMES CANNOT BE DUPLICATED"))
        # }
      }
  )

  #close connection
  RSQLite::dbDisconnect(conn)
}
