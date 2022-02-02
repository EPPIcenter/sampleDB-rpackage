
DeleteEmptyPlateReset <- function(session, database){
  #UPDATE DELETE PLATE DROPDOWN
  updateSelectizeInput(session = session,
                       "DeletePlateName",
                       choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$uid)) 
}