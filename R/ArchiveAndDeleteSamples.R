#' Archive and Delete Wetlab EPPIcenter Samples
#'
#' @description Archive and delete wetlab samples.
#'
#' @param operation A string specifying the type of operation to perform. Valid operations are `archive` and `delete`.
#'
#' `archive` should be used to delete all external data from the sample (cryovial_tube/paper/rdt information, container information, storage location)
#' but preserve internal data (sample id, specimen information, study subject information).
#' This operation can be used for example when a micronix cryovial_tube is emptied or rancid and needs to be discarded. \cr
#' `delete` should be used to remove all traces of the sample (internal and external data).
#' Deletions are recursive in the sense that they if sample ids associated with samples from an entire cryovial_box are deleted then the cryovial_box is automatically deleted.
#'
#' @param sample_id A numeric vector of a one or more sample IDs (also referred to as storage container IDs). Can be accessed using SearchSamples().
#'
#' @param verification A logical value. If set to TRUE user verification of the operation is required. If set to false user verification is bypassed. Default is set to TRUE.
#'
#' @examples
#' \dontrun{
#' ArchiveAndDeleteSamples(operation = "delete", sample_id = c(7:10))
#' ArchiveAndDeleteSamples(operation = "archive", sample_id = 100)
#' }
#' @import dplyr
#' @import RSQLite
#' @import lubridate
#' @export
#'

#' Expects 'control_id' and control type
ArchiveAndDeleteControls <- function(operation, control_type, data, comment, status, verification = TRUE, database = Sys.getenv("SDB_PATH")) {
  con <-  RSQLite::dbConnect(RSQLite::SQLite(), database)

  RSQLite::dbBegin(con)

  if (operation %in% "archive") {
    stopifnot("Status is not valid" = status %in% CheckTable("status")$name)
  }

  status_id <- filter(CheckTable("status"), name %in% status)$id
  state_id <- filter(CheckTable("state"), name %in% "Archived")$id

  stopifnot("Operation is not valid" = operation %in% c("archive", "delete", "unarchive"))

  # if (operation == "delete") {
  #   if (control_type == "dbs_sheet") {
  #     dbs_control_sheet_df <- tbl(con, "malaria_blood_control") %>%
  #       inner_join(tbl(con, "blood_spot_collection") %>% dplyr::rename(malaria_blood_control_id=id), by = c("malaria_blood_control_id")) %>%
  #       filter(id %in% data$control_id) %>%
  #       collect()

      




  #     tbl(con, "malaria_blood_control") %>%
  #   }

  # }

}

ArchiveAndDeleteSamples <- function(operation, data, comment, status, verification = TRUE){

  database <- Sys.getenv("SDB_PATH")
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
  RSQLite::dbBegin(conn)

  if (operation %in% "archive") {
    stopifnot("Status is not valid" = status %in% CheckTable("status")$name)
  }

  status_id <- filter(CheckTable("status"), name %in% status)$id
  state_id <- filter(CheckTable("state"), name %in% "Archived")$id

  stopifnot("Operation is not valid" = operation %in% c("archive", "delete", "unarchive"))

  # GET DATABASE TABLES
  database.tables <- .GetDatabaseTables(database)

  v <- (data$storage_container_id %in% database.tables$table.storage_container$id)

  if (!all(v)) {
    return(paste("Error: sample could not be found in the database (table is stale): ", data$barcode[isFALSE(v)]))
  }


  if(operation == "archive"){

    # VERIFY ARCHIVE
    if(verification == TRUE){
      response <- menu(c("Yes", "No"), title = paste("Are you sure you want to permanently archive selected?", length(sample_id), "item(s)?"))
    }else{
      response <- "1"
    }
    if(as.character(response) == "Yes" || as.character(response) == "1"){

      # ARCHIVE SAMPLES
      info_list <- list(last_updated = as.character(lubridate::now()),
                                               state_id = state_id, # Archived
                                               status_id = status_id)
      if (nchar(comment) > 0) {
        info_list <- append(info_list, list(comment = comment))
      }

      for(eval.id in data$storage_container_id){

        # ARCHIVE
        ModifyTable(conn = conn,
                              "storage_container",
                              info_list = info_list,
                              id = eval.id)

        .MakeExternalDataNA(eval.id, database.tables, conn)
      }

      # USER MSG
      return_message <- paste("Archived", length(data$storage_container_id), "Successfully")
      message(return_message)
    }
  }

  # else if(operation == "unarchive"){
  #   for(eval.id in sample_id){
  #     ModifyTable(database = database,
  #                           "storage_container",
  #                           info_list = list(exhausted = 0),
  #                           id = eval.id)
  #   }
  # message(paste("Un-archived", length(sample_id), "Successfully"))
  # }

 else{

    # VERIFY DELETION
    if(verification == TRUE){
     response <- menu(c("Yes", "No"), title = paste("Are you sure you want to permanently delete selected", length(sample_id), "item(s)?"))
    }else{
     response <- "1"
    }
    if(as.character(response) == "Yes" || as.character(response) == "1"){

      # DELETE SAMPLES
      for(eval.id in data$storage_container_id){

        # DELETE INTERNAL DATA
        .DeleteInternalData(eval.id, database.tables, conn)

        # DELETE EXTERNAL DATA
        .DeleteExternalData(eval.id, database.tables, conn)
      }

      # USER MSG
      return_message <- paste("Deleted", length(data$storage_container_id), "Successfully")
      message(return_message)
    }
  }
  RSQLite::dbCommit(conn)
  RSQLite::dbDisconnect(conn)
  return(return_message)
}

DeleteWholeBloodSamples <- function(cryovial_tube_ids) {
  database <- Sys.getenv("SDB_PATH")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
  RSQLite::dbBegin(conn)

  tryCatch({
    for (tube_id in cryovial_tube_ids) {
      # Check if it's the last tube for a study_subject
      study_subject_id <- RSQLite::dbGetQuery(conn, paste("SELECT study_subject_id FROM whole_blood_tube WHERE id = ", tube_id))
      tube_count <- RSQLite::dbGetQuery(conn, paste("SELECT COUNT(*) FROM whole_blood_tube WHERE study_subject_id = ", study_subject_id))

      if (tube_count == 1) {
        # Delete study_subject and associated malaria_blood_control
        RSQLite::dbExecute(conn, paste("DELETE FROM study_subject WHERE id = ", study_subject_id))
        RSQLite::dbExecute(conn, paste("DELETE FROM malaria_blood_control WHERE study_subject_id = ", study_subject_id))
      }

      # Delete the cryovial_tube
      RSQLite::dbExecute(conn, paste("DELETE FROM whole_blood_tube WHERE id = ", tube_id))
    }

    RSQLite::dbCommit(conn)
    message(paste("Deleted", length(cryovial_tube_ids), "whole blood samples successfully."))
  }, error = function(e) {
    RSQLite::dbRollback(conn)
    message("Error occurred: ", e$message)
  }, finally = {
    RSQLite::dbDisconnect(conn)
  })
}


.GetDatabaseTables <- function(database){
  database.tables <- list(table.storage_container = CheckTable(database = database, "storage_container"),
                          table.specimen = CheckTable(database = database, "specimen"),
                          table.cryovial_tube = CheckTable(database = database, "cryovial_tube"),
                          table.whole_blood_tube = CheckTable(database = database, "whole_blood_tube"),
                          table.micronix_tube = CheckTable(database = database, "micronix_tube"))
  return(database.tables)
}

.DeleteInternalData <- function(eval.id, database.tables, conn){
  tmp_table.storage_container <- filter(database.tables$table.storage_container, id %in% eval.id)
  specimen_id <- tmp_table.storage_container$specimen_id
  study_subject_id <- filter(database.tables$table.specimen, id %in% specimen_id)$study_subject_id

  # DELETE INTERNAL DATA -- storage container categorically
  DeleteFromTable(conn = conn,
                            table_name = "storage_container",
                            id = as.character(tmp_table.storage_container$id))

  # DELETE INTERNAL DATA -- delete specimen if it is no longer being reference
  if(!specimen_id %in% CheckTableTx(conn = conn, "storage_container")$specimen_id){
    DeleteFromTable(conn = conn,
                              table_name = "specimen",
                              id = as.character(specimen_id))

    # DELETE INTERNAL DATA -- delete study subject if it is no longer being reference
    if(!study_subject_id %in% CheckTableTx(conn = conn, "specimen")$study_subject_id){
      DeleteFromTable(conn = conn,
                                table_name = "study_subject",
                                id = as.character(study_subject_id))
    }
  }
}

.MakeExternalDataNA <- function(eval.id, database.tables, conn) {

  # DELETE EXTERNAL DATA -- micronix_tube & micronix_plate if deletion empties plate

  #if eval.id is not in matrix id, cryovial id, rdt id or paper id, skip over
  ids <- c(database.tables$table.micronix_tube$id,
           database.tables$table.cryovial_tube$id,
           database.tables$table.whole_blood_tube$id)

  if (eval.id %in% database.tables$table.micronix_tube$id) {

    # get container id before sample deletion
    manifest_id <- filter(database.tables$table.micronix_tube, id %in% eval.id)$manifest_id

    qry<-paste0("UPDATE `micronix_tube` SET `position` = NULL WHERE `id` = ", as.character(eval.id))
    dbExecute(conn, qry)

    # delete container if container id is no longer in micronix table
    if(!manifest_id %in% CheckTableTx(conn = conn, "micronix_tube")$manifest_id){
      ModifyTable(conn = conn,
                            table_name = "micronix_plate",
                            info_list = list(last_updated = as.character(lubridate::now()),
                                             location_id = NA,
                                             plate_name = NA,
                                             plate_barcode = NA),
                            id = as.character(manifest_id))
    }
  } else if (eval.id %in% database.tables$table.cryovial_tube$id) {

    # get container id before sample deletion
    manifest_id <- filter(database.tables$table.cryovial_tube, id %in% eval.id)$manifest_id

    qry<-paste0("UPDATE `cryovial_tube` SET `position` = NULL WHERE `id` = ", as.character(eval.id))
    dbExecute(conn, qry)

    # delete container if container id is no longer in micronix table
    if(!manifest_id %in% CheckTableTx(conn = conn, "cryovial_tube")$manifest_id) {
      ModifyTable(conn = conn,
                            table_name = "cryovial_box",
                            info_list = list(last_updated = as.character(lubridate::now()),
                                             location_id = NA,
                                             plate_name = NA,
                                             plate_barcode = NA),
                            id = as.character(manifest_id))
    }
  } else if (eval.id %in% database.tables$table.whole_blood_tube$id) {

    # get container id before sample deletion
    manifest_id <- filter(database.tables$table.whole_blood_tube, id %in% eval.id)$manifest_id

    qry<-paste0("UPDATE `whole_blood_tube` SET `position` = NULL WHERE `id` = ", as.character(eval.id))
    dbExecute(conn, qry)

    # delete container if container id is no longer in micronix table
    if(!manifest_id %in% CheckTableTx(conn = conn, "whole_blood_tube")$manifest_id) {
      ModifyTable(conn = conn,
                            table_name = "cryovial_box",
                            info_list = list(last_updated = as.character(lubridate::now()),
                                             location_id = NA,
                                             plate_name = NA,
                                             plate_barcode = NA),
                            id = as.character(manifest_id))
    }
  } 
}


.DeleteExternalData <- function(eval.id, database.tables, conn){

  # DELETE EXTERNAL DATA -- micronix_tube & micronix_plate if deletion empties plate
  #if eval.id is not in matrix id, cryovial id, rdt id or paper id, skip over
  ids <- c(database.tables$table.micronix_tube$id,
           database.tables$table.cryovial_tube$id,
           database.tables$table.whole_blood_tube$id)

  if(eval.id %in% ids){

    if(eval.id %in% database.tables$table.micronix_tube$id){

      # get container id before sample deletion
      matrix_plate_id <- filter(database.tables$table.micronix_tube, id %in% eval.id)$manifest_id

      # delete sample
      DeleteFromTable(conn = conn,
                                table_name = "micronix_tube",
                                id = as.character(eval.id))

      # delete container if container id is no longer in micronix table
      if(!matrix_plate_id %in% CheckTableTx(conn = conn, "micronix_tube")$manifest_id){
        DeleteFromTable(conn = conn,
                                  table_name = "micronix_plate",
                                  id = as.character(matrix_plate_id))
      }
    }

    # DELETE EXTERNAL DATA -- cryovial_tube & cryovial_box if deletion empties cryovial_box
    else if(eval.id %in% database.tables$table.cryovial_tube$id){

      # get container id before deletion
      manifest_id <- filter(database.tables$table.cryovial_tube, id %in% eval.id)$manifest_id

      #delete sample
      DeleteFromTable(conn = conn,
                                table_name = "cryovial_tube",
                                id = as.character(eval.id))

      # delete container if container id is no longer in cryovial_tube table
      if(!manifest_id %in% CheckTableTx(conn = conn, "cryovial_tube")$manifest_id) {

        DeleteFromTable(conn = conn,
                                  table_name = "cryovial_box",
                                  id = as.character(manifest_id))
      }
    }
    else if(eval.id %in% database.tables$table.whole_blood_tube$id){

      # get container id before deletion
      manifest_id <- filter(database.tables$table.whole_blood_tube, id %in% eval.id)$manifest_id

      #delete sample
      DeleteFromTable(conn = conn,
                                table_name = "whole_blood_tube",
                                id = as.character(eval.id))

      # delete container if container id is no longer in cryovial_tube table
      if(!manifest_id %in% CheckTableTx(conn = conn, "whole_blood_tube")$manifest_id) {

        DeleteFromTable(conn = conn,
                                  table_name = "cryovial_box",
                                  id = as.character(manifest_id))
      }
    }
  }
}
