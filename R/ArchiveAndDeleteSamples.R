#' Archive and Delete EPPIcenter Wetlab Samples
#' 
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export
#' 


ArchiveAndDeleteSamples <- function(operation, filters){
  
  database <- "/databases/new.sampleDB.db"
  stopifnot("Operation is not valid" = operation %in% c("archive", "delete", "make_active"))
  
  storage_container_id <- sampleDB::SearchSamples(filters)$storage_container_id
  
  if(operation == "archive"){
    for(eval.id %in% storage_container_id){
      sampleDB::ModifyTable(database = database,
                            "storage_container",
                            info_list = list(exhausted == 1),
                            id = eval.id)
    }
  }
  else if(operation == "make_active"){
    for(eval.id %in% storage_container_id){
      sampleDB::ModifyTable(database = database,
                            "storage_container",
                            info_list = list(exhausted == 0),
                            id = eval.id)
    }
  }else{
    message("still havent made delete function...")
  }
}