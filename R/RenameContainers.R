#' Rename Wetlab Containers in the EPPIcenter SampleDB database
#' 
#' @param sample_type A string specifying the type of samples that are being moved. Options include: `micronix`, `cryovial`, `rdt` and `paper`
#' @param current_container_name A vector specifying the name of the container being moved
#' @param new_container_name A vector specifying the name of the container being moved
#' @import dplyr
#' @export

RenameContainers <- function(sample_type, new_container_name, current_container_name){

  database <- Sys.getenv("SDB_PATH")
  stopifnot("ERROR: Sample Type is not valid" = sample_type %in% c("micronix"))
  stopifnot("ERROR: Container Name does not exist" = current_container_name %in% CheckTable(database = database, table = "matrix_plate")$plate_name)
  stopifnot("ERROR: New Container name is not unique" = sampleDB:::.CheckUploadContainerNameDuplication(database = database, plate_name = new_container_name))
  
  container_id <- filter(sampleDB::CheckTable("matrix_plate"), plate_name == current_container_name)$id
  sampleDB::ModifyTable(database = database,
                        table_name = "matrix_plate",
                        info_list = list(plate_name = new_container_name),
                        id = container_id)
  return_message <- paste0("Successfully Renamed Container\nWas: ", current_container_name, "\nNow: ", new_container_name)

  message(return_message)
  return(return_message)
}