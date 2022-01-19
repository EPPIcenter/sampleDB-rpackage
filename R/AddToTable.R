#' @import dplyr
#' @import emojifont
#' @import RSQLite
#' @import purrr
#' @export

#table options
AddToTable <- function(database, table_name, info_list){

  #OPEN THE DATABASE CONNECTION
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(),
                              database)

  #PREVENT EMPTY ADDITIONS TO DATABASE -- REMOVE NAs FROM THIS EVALUATION
  for(i in discard(info_list, is.na)){
    if(as.character(i) == ""){
      return()
    }
  }

  #get names of columns to modify
  column_names <- paste0(names(info_list), collapse = ", ")
  #CREATE ??? FILLER (required by dbSendQuery)
  filler <- replicate(length(info_list), "?") %>% paste0(., collapse = ", ")
  #RENAME info_list (required by dbSendQuery)
  names(info_list) <- NULL

  tryCatch(

      #add to database
      RSQLite::dbSendStatement(conn,
                           paste0('INSERT INTO ', table_name, ' (', column_names, ') VALUES (', filler, ');'),
                           info_list),


      error=function(e){
        print(e)
      }
  )

  #close connection
  tryCatch(
    RSQLite::dbDisconnect(conn),
    warning=function(w){

    })

}
