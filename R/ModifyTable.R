#' @import dplyr
#' @import RSQLite
#' @export

ModifyTable <- function(table_name, info_list, id, conn){

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

  .update_row(table_name, update_str, id, conn)

}


.update_row <- function(table_name, update_str, id, conn)
{
  # start the transaction
  tryCatch({
    rs <- RSQLite::dbSendQuery(conn, paste0("UPDATE ", table_name," SET ", update_str," WHERE id = ", id, ";"))
    # message(sprintf("Updated %d rows.", RSQLite::dbGetRowsAffected(rs)))
    RSQLite::dbClearResult(rs)
  },
  error=function(e) { 
    message(e)
    RSQLite::dbRollback(conn)
    
    #close connection
    tryCatch(
      RSQLite::dbDisconnect(conn),
      warning=function(w){
        message(w)
    })  

    validate(need(FALSE, paste("*** ERROR: database rollback triggered due to:", e)))
  })
}