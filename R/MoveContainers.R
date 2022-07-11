#' Move Wetlab Containers in the EPPIcenter SampleDB database
#' 
#' @param sample_type A string specifying the type of samples that are being moved. Options include: `micronix`, `cryovial`, `rdt` and `paper`
#' @param container_name A vector specifying the name of the container being moved
#' @param freezer A list specifying the vector `location_name`, `level_I`, and/or`level_II`
#' @import dplyr
#' @export

MoveContainers <- function(sample_type, container_name, freezer){
  
  database <- sampleDB:::.GetSampleDBPath()
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)

  RSQLite::dbBegin(conn)

  stopifnot("Sample Type is not valid" = sample_type %in% c("micronix", "cryovile", "rdt", "paper"))
  eval.location <- filter(sampleDB::CheckTableTx(conn = conn, table = "location"), 
                          location_name == freezer$freezer.name & level_I == freezer$freezer.levelI & level_II == freezer$freezer.levelII)
  stopifnot("Location does not exist" = nrow(eval.location) > 0)
  eval.location_id <- eval.location$id
  
  if(sample_type == "micronix"){
    container_id <- filter(sampleDB::CheckTableTx(conn = conn, "matrix_plate"), plate_name == container_name)$id
    ModifyTable(conn = conn,
                          table_name = "matrix_plate",
                          info_list = list(location_id = eval.location_id),
                          id = container_id)
    return_message <- paste0("Successfully Moved Container: \n", container_name)
  }
  else if(sample_type == "cryovile"){
    container_id <- filter(sampleDB::CheckTableTx(conn = conn, "box"), box_name == container_name)$id
    ModifyTable(conn = conn,
                          table_name = "box",
                          info_list = list(location_id = eval.location_id),
                          id = container_id)
    return_message <- paste0("Successfully Moved Container: \n", container_name)
  }
  else{
    container_id <- filter(sampleDB::CheckTableTx(conn = conn, "bag"), bag_name == container_name)$id
    ModifyTable(conn = conn,
                          table_name = "bag",
                          info_list = list(location_id = eval.location_id),
                          id = container_id)
    return_message <- paste0("Successfully Moved Container: \n", container_name)
  }

  dbCommit(conn)
  dbDisconnect(conn)

  message(return_message)
  return(return_message)
}
