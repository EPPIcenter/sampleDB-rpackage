#' @import dplyr
#' @import RSQLite
#' @export

DeleteFromTable <- function(table_name, id, conn) {

  #check and see if table is a reference table. if it is make sure the reference being deleted is not in use
  if(table_name == "location"){
    if(!(id %in% sampleDB::CheckTableTx(conn = conn, "micronix_plate")$location_id)){
      .delete_row(table_name, id, conn)
    }else{
      warning("Storage Location is in use, it cannot be deleted.")
    }
  }

  else if(table_name == "study"){
    if(!(id %in% sampleDB::CheckTableTx(conn = conn, "study_subject")$study_id)){
      .delete_row(table_name, id, conn)
    }else{
      warning("Study is in use, it cannot be deleted.")
    }
  }

  else if(table_name == "specimen_type"){
    if(!(id %in% sampleDB::CheckTableTx(conn = conn, "specimen")$specimen_type_id)){
      .delete_row(table_name, id, conn)
    }else{
      warning("Specimen Type is in use, it cannot be deleted.")
    }
  }
  
  else{
    .delete_row(table_name, id, conn)
  }
}

.delete_row <- function(table_name, id, conn)
{
  # start the transaction
  tryCatch({
    rs <- RSQLite::dbSendQuery(conn, paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
    # message(sprintf("Deleted %d rows.", RSQLite::dbGetRowsAffected(rs)))
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
