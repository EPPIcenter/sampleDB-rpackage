#' Delete Empty Wetlab Containers in the EPPIcenter SampleDB database
#' @import dplyr
#' @export

DeleteEmptyContainer <- function(type, container_name, conn){

  stopifnot("ERROR: Sample Type is not valid" = type %in% c("micronix", "cryovial", "dbs_sheet", "whole_blood", "bag", "box", "static_plate"))

  if (type == "micronix" || type == "static_plate") {
    id.container <- filter(CheckTableTx(conn = conn, "micronix_plate"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete matrix plate that does not exist.")
    }
    if (type == "micronix" && filter(CheckTableTx(conn = conn, "micronix_tube"), manifest_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "micronix_plate", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else if (type == "static_plate" && filter(CheckTableTx(conn = conn, "static_well"), manifest_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "micronix_plate", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- "Error: Container is not empty"
    }

  } else if (type == "cryovial" || type == "whole_blood") {
    id.container <- filter(CheckTableTx(conn = conn, "cryovial_box"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete cryovial_box that does not exist.")
    }
    zero_sample_cryovials <- filter(CheckTableTx(conn = conn, "cryovial_tube"), manifest_id %in% id.container) %>% nrow() == 0
    zero_wb_cryovials <- filter(CheckTableTx(conn = conn, "whole_blood_tube"), cryovial_box_id %in% id.container) %>% nrow() == 0
    if (zero_sample_cryovials && zero_wb_cryovials) {
      DeleteFromTable(conn = conn, table_name = "cryovial_box", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- sprintf("Error: Container is not empty, WB found: %s, Sample found: %s",
        !zero_wb_cryovials, !zero_sample_cryovials)
    }

  } else if (type == "dbs_sheet") {
    id.container <- filter(CheckTableTx(conn = conn, "dbs_bag"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete dbs_bag that does not exist.")
    }
    if (filter(CheckTableTx(conn = conn, "dbs_control_sheet"), dbs_bag_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "dbs_bag", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- "Error: Container is not empty"
    }
  } else if (type == "bag") {
    id.container <- filter(CheckTableTx(conn = conn, "bag"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete bag that does not exist.")
    }
    if (filter(CheckTableTx(conn = conn, "paper"), manifest_type == "bag" & manifest_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "bag", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- "Error: Container is not empty"
    }
  } else if (type == "box") {
    id.container <- filter(CheckTableTx(conn = conn, "box"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete box that does not exist.")
    }
    if (filter(CheckTableTx(conn = conn, "paper"), manifest_type == "box" & manifest_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "box", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- "Error: Container is not empty"
    }
  }

  message(return_message)
  return(return_message)
}
