#' Delete Empty Wetlab Containers in the EPPIcenter SampleDB database
#' @import dplyr
#' @export

DeleteEmptyContainer <- function(type, container_name, conn){

  stopifnot("Sample Type is not valid" = type %in% c("micronix", "cryovial", "dbs_bag", "whole_blood", "bag", "box"))

  if (type == "micronix") {
    id.container <- filter(CheckTableTx(conn = conn, "micronix_plate"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete matrix plate that does not exist.")
    }
    if (filter(CheckTableTx(conn = conn, "micronix_tube"), manifest_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "micronix_plate", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- "Error: Container is not empty"
    }

  } else if (type == "cryovial") {
    id.container <- filter(CheckTableTx(conn = conn, "cryovial_box"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete cryovial_box that does not exist.")
    }
    if (filter(CheckTableTx(conn = conn, "cryovial_tube"), manifest_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "cryovial_box", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- "Error: Container is not empty"
    }

  } else if (type == "dbs_bag") {
    id.container <- filter(CheckTableTx(conn = conn, "dbs_bag"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete dbs_bag that does not exist.")
    }
    if (filter(CheckTableTx(conn = conn, "dbs_sample"), manifest_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "dbs_bag", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- "Error: Container is not empty"
    }

  } else if (type == "whole_blood") {
    id.container <- filter(CheckTableTx(conn = conn, "cryovial_box"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete whole_blood_tube that does not exist.")
    }
    if (filter(CheckTableTx(conn = conn, "whole_blood_tube"), manifest_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "whole_blood_tube", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- "Error: Container is not empty"
    }
  } else if (type == "bag") {
    id.container <- filter(CheckTableTx(conn = conn, "bag"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete whole_blood_tube that does not exist.")
    }
    if (filter(CheckTableTx(conn = conn, "whole_blood_tube"), manifest_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "whole_blood_tube", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- "Error: Container is not empty"
    }
  } else if (type == "box") {
    id.container <- filter(CheckTableTx(conn = conn, "box"), name == container_name) %>% pull(id)
    if (is_empty(id.container)) {
      warning("Attempt to delete whole_blood_tube that does not exist.")
    }
    if (filter(CheckTableTx(conn = conn, "whole_blood_tube"), manifest_id %in% id.container) %>% nrow() == 0) {
      DeleteFromTable(conn = conn, table_name = "whole_blood_tube", id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    } else {
      return_message <- "Error: Container is not empty"
    }
  }

  message(return_message)
  return(return_message)
}
