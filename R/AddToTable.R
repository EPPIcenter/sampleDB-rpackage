#' @import dplyr
#' @export

#table options
AddToTable <- function(table_name, info_list){

  #OPEN THE DATABASE CONNECTION
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(),
                              "../files/example_19-Oct-21.sample_db.sqlite")

  #PREVENT EMPTY ADDITIONS TO DATABASE
  for(i in info_list){
    if(as.character(i) == ""){
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
