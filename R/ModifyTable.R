#' @import dplyr
#' @import RSQLite
#' @export

.update_row <- function(database, table_name, update_str, id)
{
  conn <- NULL
  # start the transaction
  tryCatch({
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
    RSQLite::dbBegin(conn)
    rs <- RSQLite::dbSendQuery(conn, paste0("UPDATE ", table_name," SET ", update_str," WHERE id = ", id, ";"))
    # message(sprintf("Updated %d rows.", RSQLite::dbGetRowsAffected(rs)))
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

ModifyTable <- function(database, table_name, info_list, id, conn = NULL){

  #PREVENT EMPTY ADDITIONS TO DATABASE -- REMOVE NAs FROM THIS EVALUATION
  for(i in discard(info_list, is.na)){
    if(as.character(i) == ""){
      return()
    }
  }

  update_str <- c()
  for(i in 1:length(info_list)){
    if(i != length(info_list)){
      update_str <- c(update_str, names(info_list)[i], " = ", "'", info_list[[i]], "', ")
    }else{
      update_str <- c(update_str, names(info_list)[i], " = ", "'", info_list[[i]], "'")
    }
  }
  update_str <- update_str %>% paste0(., collapse = "")

  .update_row(database, table_name, update_str, id)

}
