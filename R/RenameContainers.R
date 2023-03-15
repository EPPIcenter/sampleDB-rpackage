#' Rename Wetlab Containers in the EPPIcenter SampleDB database
#' 
#' @param sample_type A string specifying the type of samples that are being moved. Options include: `micronix`, `cryovial`, `rdt` and `paper`
#' @param current_container_name A vector specifying the name of the container being moved
#' @param new_container_name A vector specifying the name of the container being moved
#' @import dplyr
#' @export

RenameContainers <- function(sample_type, new_container_name, current_container_name, conn){

  stopifnot("ERROR: Sample Type is not valid" = sample_type %in% c(1,2,3))

  if (sample_type == 1) {
    if (new_container_name == "") {
      warning("Container name cannot be empty!")
    }

    if (!current_container_name %in% CheckTable(database = database, table = "micronix_plate")$name) {
      warning("plate matrix not found in the database!")
    }
    # stopifnot("ERROR: New Container name is not unique" = :.CheckUploadContainerNameDuplication(database = database, plate_name = new_container_name, only_active = T))
    
    if (new_container_name %in% CheckTable(database = database, table = "micronix_plate")$name) {
      warning("Plate name already exists!")
    }

    # already check container existance above
    container_id <- filter(CheckTable("micronix_plate"), name == current_container_name)$id
    ModifyTable(conn = conn,
                          table_name = "micronix_plate",
                          info_list = list(name = new_container_name),
                          id = container_id)
    return_message <- paste0("Successfully Renamed Container\nWas: ", current_container_name, "\nNow: ", new_container_name)
  } else if (sample_type == 2) {
    if (new_container_name == "") {
      warning("Container name cannot be empty!")
    }

    if (!current_container_name %in% CheckTable(database = database, table = "cryovial_box")$name) {
      warning("plate matrix not found in the database!")
    }
    # stopifnot("ERROR: New Container name is not unique" = :.CheckUploadContainerNameDuplication(database = database, plate_name = new_container_name, only_active = T))
    
    if (new_container_name %in% CheckTable(database = database, table = "cryovial_box")$name) {
      warning("Plate name already exists!")
    }

    # already check container existance above
    container_id <- filter(CheckTable("cryovial_box"), name == current_container_name)$id
    ModifyTable(conn = conn,
                          table_name = "cryovial_box",
                          info_list = list(name = new_container_name),
                          id = container_id)
    return_message <- paste0("Successfully Renamed Container\nWas: ", current_container_name, "\nNow: ", new_container_name)
  } else if (sample_type == 3) {
    # need to implement
  } else {
    return_message <- "ERROR: sample type is invalid"
  }

  message(return_message)
  return(return_message)
}