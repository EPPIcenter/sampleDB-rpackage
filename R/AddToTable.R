#' @import dplyr
#' @import emojifont
#' @import RSQLite
#' @import purrr
#' @export

.insert_row <- function(database, table_name, column_names, filler, info_list)
{
  conn <- NULL
  # start the transaction
  tryCatch({
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
    RSQLite::dbBegin(conn)
    rs <- RSQLite::dbSendQuery(conn, paste0('INSERT INTO ', table_name, ' (', column_names, ') VALUES (', filler, ');'), info_list)
    # message(sprintf("Inserted %d rows.", RSQLite::dbGetRowsAffected(rs)))
    RSQLite::dbClearResult(rs)
    RSQLite::dbCommit(conn)
  },
  error=function(e) {
    message(e)
    RSQLite::dbRollback(conn)
  })

  #close connection
  tryCatch(
    RSQLite::dbDisconnect(conn),
    warning=function(w){
      message(w)
    })
}

#table options
AddToTable <- function(database, table_name, info_list, conn = NULL){

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

  .insert_row(database, table_name, column_names, filler, info_list)
}
