#' Rename Wetlab Containers in the EPPIcenter SampleDB database
#' 
#' @param sample_type A string specifying the type of samples that are being moved. Options include: `micronix`, `cryovial`, `rdt` and `paper`
#' @param current_container_name A vector specifying the name of the container being moved
#' @param new_container_name A vector specifying the name of the container being moved
#' @import dplyr
#' @export

RenameContainers <- function(sample_type, new_container_name, current_container_name, conn){

  stopifnot("ERROR: Sample Type is not valid" = sample_type %in% c("micronix"))

  if (new_container_name == "") {
    warning("Container name cannot be empty!")
  }

  if (!current_container_name %in% CheckTable(database = database, table = "micronix_plate")$plate_name) {
    warning("plate matrix not found in the database!")
  }
  # stopifnot("ERROR: New Container name is not unique" = sampleDB:::.CheckUploadContainerNameDuplication(database = database, plate_name = new_container_name, only_active = T))
  
  if (new_container_name %in% CheckTable(database = database, table = "micronix_plate")$plate_name) {
    warning("Plate name already exists!")
  }

  # already check container existance above
  container_id <- filter(sampleDB::CheckTable("micronix_plate"), plate_name == current_container_name)$id
  ModifyTable(conn = conn,
                        table_name = "micronix_plate",
                        info_list = list(plate_name = new_container_name),
                        id = container_id)
  return_message <- paste0("Successfully Renamed Container\nWas: ", current_container_name, "\nNow: ", new_container_name)

  message(return_message)
  return(return_message)
}