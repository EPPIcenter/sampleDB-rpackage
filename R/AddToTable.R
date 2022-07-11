#' @import dplyr
#' @import emojifont
#' @import RSQLite
#' @import purrr
#' @export

#table options
AddToTable <- function(table_name, info_list, conn){

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

  .insert_row(table_name, column_names, filler, info_list, conn)
}

.insert_row <- function(table_name, column_names, filler, info_list, conn)
{
  # start the transaction
  tryCatch({
    rs <- RSQLite::dbSendQuery(conn, paste0('INSERT INTO ', table_name, ' (', column_names, ') VALUES (', filler, ');'), info_list)
    # message(sprintf("Inserted %d rows.", RSQLite::dbGetRowsAffected(rs)))
    RSQLite::dbClearResult(rs)
  },
  error=function(e) {
    RSQLite::dbRollback(conn)
    RSQLite::dbDisconnect(conn)
    err_values <- paste("ERROR: Database insert failed on:", table_name, paste(info_list, sep = " "))
    validate(need(FALSE,
                  paste("*** ERROR: database rollback triggered due to:", e, "\n", paste(err_values))))
  })
}