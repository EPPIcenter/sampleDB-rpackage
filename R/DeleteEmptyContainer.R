#' Delete Empty Wetlab Containers in the EPPIcenter SampleDB database
#' @import dplyr
#' @export

DeleteEmptyContainer <- function(type, container_name, conn){

  stopifnot("Sample Type is not valid" = type %in% c("micronix", "cryovial"))

  if(type == "micronix"){
    id.container <- filter(CheckTableTx(conn = conn, "micronix_plate"), name == container_name)$id
    if (is_empty(id.container)) {
      warning("Attempt to delete matrix plate that does not exist (was it deleted after deleting all of it's samples?)")
    }
    if(filter(CheckTableTx(conn = conn, "micronix_tube"), manifest_id %in% id.container) %>% nrow() == 0){
      DeleteFromTable(conn = conn,
                                table_name = "micronix_plate",
                                id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    }else{
      return_message <- "Error Container is not empty"
    }
  }
  else if(type == "cryovial"){
    id.container <- filter(CheckTableTx(conn = conn, "cryovial_box"), name == container_name)$id
    if (is_empty(id.container)) {
      warning("Attempt to delete cryovial_box that does not exist (was it deleted after deleting all of it's samples?)")
    }
    if(filter(CheckTableTx(conn = conn, "cryovial_tube"), manifest_id %in% id.container) %>% nrow() == 0){
      DeleteFromTable(conn = conn,
                                table_name = "cryovial_box",
                                id = as.character(id.container))
      return_message <- paste0("Successfully Deleted Container: \n", container_name)
    }else{
      return_message <- "Error Container is not empty"
    }
  }

  message(return_message)
  return(return_message)
}
