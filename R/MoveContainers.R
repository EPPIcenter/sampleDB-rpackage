#' location <- list(name.freezer = "", level_II = "", level_II = "")
#' @import dplyr
#' @export

MoveContainers <- function(type, container_name, location){
  
  database <- "/databases/sampledb/v0.0.2/sampledb_database.sqlite"
  stopifnot("Sample Type is not valid" = type %in% c("micronix", "cryovile", "rdt", "paper"))
  stopifnot("Location does not exist" = nrow(filter(sampleDB::CheckTable(database = database, table = "location"), 
                                                    location_name == location$name.freezer & level_I == location$level_I & level_II == location$level_II) == 0))
            
  eval.location_id <- filter(sampleDB::CheckTable(database = database, table = "location"), 
                             location_name == location$name.freezer & level_I == location$level_I & level_II == location$level_II)$id
  if(type == "micronix"){
    container_id <- filter(sampleDB::CheckTable("matrix_plate"), plate_name == container_name)$id
    sampleDB::ModifyTable(database = database,
                          table_name = "matrix_plate",
                          info_list = list(location_id = eval.location_id),
                          id = container_id)
    message(paste0("Successfully Moved Container: \n", container_name))
  }
  else if(type == "cryovile"){
    container_id <- filter(sampleDB::CheckTable("box"), box_name == container_name)$id
    sampleDB::ModifyTable(database = database,
                          table_name = "box",
                          info_list = list(location_id = eval.location_id),
                          id = container_id)
    message(paste0("Successfully Moved Container: \n", container_name))
  }
  else{
    container_id <- filter(sampleDB::CheckTable("bag"), bag_name == container_name)$id
    sampleDB::ModifyTable(database = database,
                          table_name = "bag",
                          info_list = list(location_id = eval.location_id),
                          id = container_id)
    message(paste0("Successfully Moved Container: \n", container_name))
  }
}
