#' @import dplyr
#' @import RSQLite
#' @export

.delete_row <- function(database, table_name, id)
{
  conn <- NULL
  # start the transaction
  tryCatch({
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
    RSQLite::dbBegin(conn)
    rs <- RSQLite::dbSendQuery(conn, paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
    # message(sprintf("Deleted %d rows.", RSQLite::dbGetRowsAffected(rs)))
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

DeleteFromTable <- function(table_name, id, database = sampleDB:::.GetSampleDBPath()) {
  
  #check and see if table is a reference table. if it is make sure the reference being deleted is not in use
  if(table_name == "location"){
    if(!(id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id)){
      .delete_row(table_name, id)
    }else{
      warning("Storage Location is in use, it cannot be deleted.")
    }
  }

  else if(table_name == "study"){
    if(!(id %in% sampleDB::CheckTable(database = database, "study_subject")$study_id)){
      .delete_row(table_name, id)
    }else{
      warning("Study is in use, it cannot be deleted.")
    }
  }

  else if(table_name == "specimen_type"){
    if(!(id %in% sampleDB::CheckTable(database = database, "specimen")$specimen_type_id)){
      .delete_row(table_name, id)
    }else{
      warning("Specimen Type is in use, it cannot be deleted.")
    }
  }
  
  else{
    .delete_row(database, table_name, id)
  }
}
