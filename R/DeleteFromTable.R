#' @import dplyr
#' @import RSQLite
#' @export

DeleteFromTable <- function(database, table_name, id){
  
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)

  #check and see if table is a reference table. if it is make sure the reference being deleted is not in use
  if(table_name == "location"){
    if(!(id %in% sampleDB::CheckTable(database = database, "matrix_plate")$location_id)){
      RSQLite::dbSendQuery(conn, paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
    }
  }

  else if(table_name == "study"){
    if(!(id %in% sampleDB::CheckTable(database = database, "study_subject")$study_id)){
      RSQLite::dbSendQuery(conn, paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
    }
  }

  else if(table_name == "specimen_type"){
    if(!(id %in% sampleDB::CheckTable(database = database, "specimen")$specimen_type_id)){
      RSQLite::dbSendQuery(conn, paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
    }
  }
  
  else{
    RSQLite::dbSendQuery(conn, paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
  }

  #close connection
  tryCatch(
    RSQLite::dbDisconnect(conn),
    warning=function(w){
      
    })
}
