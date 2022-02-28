#' Delete Empty Wetlab Containers in the EPPIcenter SampleDB database
#' @import dplyr
#' @export

DeleteEmptyContainers <- function(database, type, container_name){
  
  stopifnot("Sample Type is not valid" = type %in% c("micronix", "cryovile", "rdt", "paper"))
  if(type == "micronix"){
    id.container <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), plate_name == container_name)$id
    if(filter(sampleDB::CheckTable("matrix_tube"), plate_id %in% id.container) %>% nrow() == 0){
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "matrix_plate", 
                                id = as.character(id.container))  
      message(paste0("Successfully Deleted Container: \n", container_name))
    }else{
      message("Error Contianer is not empty")
    }
  }
  else if(type == "cryovile"){
    id.container <- filter(sampleDB::CheckTable(database = database, "box"), box_name == container_name)$id
    if(filter(sampleDB::CheckTable("tube"), plate_id %in% id.container) %>% nrow() == 0){
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "box", 
                                id = as.character(id.container))  
      message(paste0("Successfully Deleted Container: \n", container_name))
    }else{
      message("Error Contianer is not empty")
    }
  }
  else if(type == "rdt"){
    id.container <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == container_name)$id
    if(filter(sampleDB::CheckTable("rdt"), bag_id %in% id.container) %>% nrow() == 0){
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "bag", 
                                id = as.character(id.container)) 
      message(paste0("Successfully Deleted Container: \n", container_name))
    }else{
      message("Error Contianer is not empty")
    }
  }
  else{
    id.container <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == container_name)$id
    if(filter(sampleDB::CheckTable("paper"), bag_id %in% id.container) %>% nrow() == 0){
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "bag", 
                                id = as.character(id.container))
      message(paste0("Successfully Deleted Container: \n", container_name))
    }else{
      message("Error Contianer is not empty")
    }
  }
  
  message(paste0("Successfully Deleted Container: \n", container_name))
}