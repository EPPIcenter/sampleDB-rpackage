#' Rename Wetlab Containers in the EPPIcenter SampleDB database
#' 
#' @param sample_type A string specifying the type of samples that are being moved. Options include: `micronix`, `cryovial`, `rdt` and `paper`
#' @param current_container_name A vector specifying the name of the container being moved
#' @param new_container_name A vector specifying the name of the container being moved
#' @import dplyr
#' @export

RenameContainers <- function(sample_type, new_container_name, current_container_name, conn){

  stopifnot("ERROR: Sample Type is not valid" = sample_type %in% c("micronix", "cryovial", "dbs_bag", "bag", "box", "whole_blood"))

  if (new_container_name == "") {
    warning("Container name cannot be empty!")
  }

  manifest <- switch(
    sample_type,
    "micronix" = "micronix_plate",
    "cryovial" = "cryovial_box",
    "dbs_bag" = "dbs_bag",
    "whole_blood" = "whole_blood_tube",
    "box" = "box",
    "bag" = "bag"
  )

  if (!current_container_name %in% (tbl(conn, manifest) %>% pull(name))) {
    warning(paste("Container", current_container_name, "not found in the database!"))
  }

  if (new_container_name %in% (tbl(conn, manifest) %>% pull(name))) {
    warning("Container name already exists!")
  }

  container_id <- filter(tbl(conn, manifest), name == current_container_name) %>% pull(id)
  ModifyTable(conn = conn, table_name = manifest, info_list = list(name = new_container_name), id = container_id)

  return_message <- paste0("Successfully Renamed Container\nWas: ", current_container_name, "\nNow: ", new_container_name)
  message(return_message)
  return(return_message)
}
