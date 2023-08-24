#' Move Wetlab Containers in the EPPIcenter SampleDB database
#' 
#' @param sample_type A string specifying the type of samples that are being moved. Options include: `micronix`, `cryovial`, `rdt` and `paper`
#' @param container_name A vector specifying the name of the container being moved
#' @param freezer A list specifying the vector `name`, `level_I`, and/or`level_II`
#' @import dplyr
#' @export

MoveContainers <- function(sample_type, container_name, freezer, conn){
  
  stopifnot("Sample Type is not valid" = sample_type %in% c(1,2,3))
  eval.location <- filter(CheckTableTx(conn = conn, table = "location"), 
                          name == freezer$freezer.name & level_I == freezer$freezer.levelI & level_II == freezer$freezer.levelII)
  if (nrow(eval.location) == 0) {
    warning("Location does not exist!")
  }
  eval.location_id <- eval.location$id
  
  if(sample_type == "micronix"){
    container_id <- filter(CheckTableTx(conn = conn, "micronix_plate"), name == container_name)$id
    if (is_empty(container_id)) {
      warning("Attempt to move plate that does not exist (was it deleted after deleting all of it's samples?)")
    }

    ModifyTable(conn = conn,
                          table_name = "micronix_plate",
                          info_list = list(location_id = eval.location_id),
                          id = container_id)
    return_message <- paste0("Successfully Moved Container: \n", container_name)
  }
  else if(sample_type == "cryovial"){
    container_id <- filter(CheckTableTx(conn = conn, "cryovial_box"), name == container_name)$id
    if (is_empty(container_id)) {
      warning("Attempt to move cryovial_box that does not exist (was it deleted after deleting all of it's samples?)")
    }
    ModifyTable(conn = conn,
                          table_name = "cryovial_box",
                          info_list = list(location_id = eval.location_id),
                          id = container_id)
    return_message <- paste0("Successfully Moved Container: \n", container_name)
  }
  # else{
  #   container_id <- filter(CheckTableTx(conn = conn, "bag"), bag_name == container_name)$id
  #   if (is_empty(container_id)) {
  #     warning("Attempt to move bag that does not exist (was it deleted after deleting all of it's samples?)")
  #   }
  #   ModifyTable(conn = conn,
  #                         table_name = "bag",
  #                         info_list = list(location_id = eval.location_id),
  #                         id = container_id)
  #   return_message <- paste0("Successfully Moved Container: \n", container_name)
  # }

  message(return_message)
  return(return_message)
}
