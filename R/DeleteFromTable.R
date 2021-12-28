#' @import dplyr
#' @import RSQLite
#' @export

DeleteFromTable <- function(table_name, id){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "../files/example_19-Oct-21.sample_db.sqlite")

  #check and see if table is a reference table. if it is make sure the reference being deleted is not in use
  if(table_name == "location"){
    if(!(id %in% sampleDB::CheckTable("matrix_plate")$location_id)){
      RSQLite::dbSendQuery(conn,
                           paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
    }
  }

  if(table_name == "study"){
    if(!(id %in% sampleDB::CheckTable("study_subject")$study_id)){
      RSQLite::dbSendQuery(conn,
                           paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
    }
  }

  if(table_name == "specimen_type"){
    if(!(id %in% sampleDB::CheckTable("specimen")$specimen_type_id)){
      RSQLite::dbSendQuery(conn,
                           paste0("DELETE FROM ", table_name, " WHERE id = ", id,";"))
    }
  }

  RSQLite::dbDisconnect(conn)
}
