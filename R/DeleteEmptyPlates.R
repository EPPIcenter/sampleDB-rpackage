#' @import dplyr
#' @export

DeleteEmptyPlates <- function(database, plate_name){
  
  id.plate <- filter(CheckTable(database = database, "matrix_plate"), uid == plate_name)$id
  sampleDB::DeleteFromTable(database = database, 
                            table_name = "matrix_plate", 
                            id = as.character(id.plate))
  
  return(paste0("Successfully Deleted Plate: \n", plate_name))
}