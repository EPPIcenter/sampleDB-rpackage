#' Move Wetlab Containers in the EPPIcenter SampleDB database
#' 
#' @param sample_type A string specifying the type of samples that are being moved. Options include: `micronix`, `cryovial`, `rdt` and `paper`
#' @param container_name A vector specifying the name of the container being moved
#' @param freezer A list specifying the vector `name`, `level_I`, and/or`level_II`
#' @import dplyr
#' @export

MoveContainers <- function(sample_type, container_name, freezer, conn){

  stopifnot("ERROR: Sample Type is not valid" = sample_type %in% c("micronix", "cryovial", "dbs_sheet", "whole_blood", "bag", "box", "static_plate"))

  eval.location <- filter(CheckTableTx(conn = conn, table = "location"), 
                          location_root == freezer$freezer.name & level_I == freezer$freezer.levelI & level_II == freezer$freezer.levelII)
  if (nrow(eval.location) == 0) {
    warning("Location does not exist!")
  }
  eval.location_id <- eval.location$id

  manifest <- switch(
    sample_type,
    "micronix" = "micronix_plate",
    "cryovial" = "cryovial_box",
    "dbs_sheet" = "dbs_bag",
    "whole_blood" = "cryovial_box",
    "bag" = "bag",
    "box" = "box",
    "static_plate" = "micronix_plate"
  )

  container_id <- filter(CheckTableTx(conn = conn, manifest), name == container_name) %>% pull(id)
  if (is_empty(container_id)) {
    warning(paste("Attempt to move", sample_type, "container that does not exist."))
  }

  ModifyTable(conn = conn, table_name = manifest, info_list = list(location_id = eval.location_id), id = container_id)

  return_message <- paste0("Successfully Moved Container: \n", container_name)
  message(return_message)
  return(return_message)
}
