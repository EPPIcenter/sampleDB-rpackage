#' Archive and Delete Wetlab EPPIcenter Samples
#'
#' @description
#'
#' @param operation A string specifying the type of operation to perform. Valid operations are `archive` and `delete`.
#'
#' `archive` should be used to delete all external data from the sample (tube/paper/rdt information, container information, storage location)
#' but preserve internal data (sample id, specimen information, study subject information).
#' This operation can be used for example when a micronix tube is emptied or rancid and needs to be discarded. \cr
#' `delete` should be used to remove all traces of the sample (internal and external data).
#' Deletions are recursive in the sense that they if sample ids associated with samples from an entire box are deleted then the box is automatically deleted.
#'
#' @param sample_id A numeric vector of a one or more sample IDs (also referred to as storage container IDs). Can be accessed using sampleDB::SearchSamples().
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
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export
#'


ArchiveAndDeleteSamples <- function(operation, data, comment, status, verification = TRUE){
  
  database <- sampleDB:::.GetSampleDBPath()
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
  RSQLite::dbBegin(conn)

  status_id <- filter(sampleDB::CheckTable("status"), name %in% status)$id
  state_id <- filter(sampleDB::CheckTable("state"), name %in% "Archived")$id
  
  stopifnot("Operation is not valid" = operation %in% c("archive", "delete", "unarchive"))

  # GET DATABASE TABLES
  database.tables <- .GetDatabaseTables(database)
  
  stopifnot("Not all Sample ID(s) are not present in the database" = all(data$`Sample ID` %in% database.tables$table.storage_container$id))
  
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

      for(eval.id in data$`Sample ID`){
        
        # ARCHIVE
        ModifyTable(conn = conn,
                              "storage_container",
                              info_list = info_list,
                              id = eval.id)

        .MakeExternalDataNA(eval.id, database.tables, conn)
      }

      # USER MSG
      return_message <- paste("Archived", length(data$`Sample ID`), "Successfully")
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
      for(eval.id in data$`Sample ID`){
        
        # DELETE INTERNAL DATA
        .DeleteInternalData(eval.id, database.tables, conn)

        # DELETE EXTERNAL DATA
        .DeleteExternalData(eval.id, database.tables, conn)
      }

      # USER MSG
      return_message <- paste("Deleted", length(data$`Sample ID`), "Successfully")
      message(return_message)
    }
  }
  RSQLite::dbCommit(conn)
  RSQLite::dbDisconnect(conn)
  return(return_message)
}

.GetDatabaseTables <- function(database){
  database.tables <- list(table.storage_container = sampleDB::CheckTable(database = database, "storage_container"),
                          table.specimen = sampleDB::CheckTable(database = database, "specimen"),
                          table.tube = sampleDB::CheckTable(database = database, "tube"),
                          table.rdt = sampleDB::CheckTable(database = database, "rdt"),
                          table.paper = sampleDB::CheckTable(database = database, "paper"),
                          table.matrix_tube = sampleDB::CheckTable(database = database, "matrix_tube"))
  return(database.tables)
}

.DeleteInternalData <- function(eval.id, database.tables, conn){
  tmp_table.storage_container <- filter(database.tables$table.storage_container, id %in% eval.id)
  specimen_id <- tmp_table.storage_container$specimen_id
  study_subject_id <- filter(database.tables$table.specimen, id %in% specimen_id)$study_subject_id

  # DELETE INTERNAL DATA -- storage container categorically
  sampleDB::DeleteFromTable(conn = conn,
                            table_name = "storage_container",
                            id = as.character(tmp_table.storage_container$id))

  # DELETE INTERNAL DATA -- delete specimen if it is no longer being reference
  if(!specimen_id %in% CheckTableTx(conn = conn, "storage_container")$specimen_id){
    sampleDB::DeleteFromTable(conn = conn,
                              table_name = "specimen",
                              id = as.character(specimen_id))

    # DELETE INTERNAL DATA -- delete study subject if it is no longer being reference
    if(!study_subject_id %in% CheckTableTx(conn = conn, "specimen")$study_subject_id){
      sampleDB::DeleteFromTable(conn = conn,
                                table_name = "study_subject",
                                id = as.character(study_subject_id))
    }
  }
}

.MakeExternalDataNA <- function(eval.id, database.tables, conn){

  # DELETE EXTERNAL DATA -- matrix_tube & matrix_plate if deletion empties plate

  #if eval.id is not in matrix id, cryovial id, rdt id or paper id, skip over
  ids <- c(database.tables$table.matrix_tube$id,
           database.tables$table.tube$id,
           database.tables$table.rdt$id,
           database.tables$table.paper$id)

  if(eval.id %in% ids){

    # get container id before sample deletion
    matrix_plate_id <- filter(database.tables$table.matrix_tube, id %in% eval.id)$plate_id

    ModifyTable(conn = conn,
                          table_name = "matrix_tube",
                          info_list = list(well_position = NA),
                          id = as.character(eval.id))

    # # delete sample
    # sampleDB::DeleteFromTable(database = database,
    #                           table_name = "matrix_tube",
    #                           id = as.character(eval.id))

    # delete container if container id is no longer in micronix table
    if(!matrix_plate_id %in% CheckTableTx(conn = conn, "matrix_tube")$plate_id){
      ModifyTable(conn = conn,
                            table_name = "matrix_plate",
                            info_list = list(last_updated = as.character(lubridate::now()),
                                             location_id = NA,
                                             plate_name = NA,
                                             plate_barcode = NA),
                            id = as.character(matrix_plate_id))
      # sampleDB::DeleteFromTable(database = database,
      #                           table_name = "matrix_plate",
      #                           id = as.character(matrix_plate_id))
    }

  }

}


.DeleteExternalData <- function(eval.id, database.tables, conn){

  # DELETE EXTERNAL DATA -- matrix_tube & matrix_plate if deletion empties plate

  #if eval.id is not in matrix id, cryovial id, rdt id or paper id, skip over
  ids <- c(database.tables$table.matrix_tube$id,
           database.tables$table.tube$id,
           database.tables$table.rdt$id,
           database.tables$table.paper$id)

  if(eval.id %in% ids){

    if(eval.id %in% database.tables$table.matrix_tube$id){

      # get container id before sample deletion
      matrix_plate_id <- filter(database.tables$table.matrix_tube, id %in% eval.id)$plate_id

      # delete sample
      sampleDB::DeleteFromTable(conn = conn,
                                table_name = "matrix_tube",
                                id = as.character(eval.id))

      # delete container if container id is no longer in micronix table
      if(!matrix_plate_id %in% sampleDB::CheckTableTx(conn = conn, "matrix_tube")$plate_id){
        sampleDB::DeleteFromTable(conn = conn,
                                  table_name = "matrix_plate",
                                  id = as.character(matrix_plate_id))
      }
    }

    # DELETE EXTERNAL DATA -- tube & box if deletion empties box
    else if(eval.id %in% database.tables$table.tube$id){

      # get container id before deletion
      box_id <- filter(database.tables$table.tube, id %in% eval.id)$box_id

      #delete sample
      sampleDB::DeleteFromTable(conn = conn,
                                table_name = "tube",
                                id = as.character(eval.id))

      # delete container if container id is no longer in tube table
      if(!box_id %in% CheckTableTx(conn = conn, "tube")$box_id){

        sampleDB::DeleteFromTable(conn = conn,
                                  table_name = "box",
                                  id = as.character(box_id))
      }
    }

    # DELETE EXTERNAL DATA -- rdt & bag if deletion empties bag
    else if(eval.id %in% database.tables$table.rdt$id){

      # get container id before deletion
      bag_id <- filter(database.tables$table.rdt, id %in% eval.id)$bag_id

      #delete sample
      sampleDB::DeleteFromTable(conn = conn,
                                table_name = "rdt",
                                id = as.character(eval.id))

      # delete container if container id is no longer in rdt table
      if(!bag_id %in% CheckTableTx(conn = conn, "rdt")$bag_id){
        sampleDB::DeleteFromTable(conn = conn,
                                  table_name = "bag",
                                  id = as.character(bag_id))
      }
    }

    # DELETE EXTERNAL DATA -- paper & bag if deletion empties bag
    else{

      # get container id before deletion
      bag_id <- filter(database.tables$table.paper, id %in% eval.id)$bag_id

      #delete sample
      sampleDB::DeleteFromTable(conn = conn,
                                table_name = "paper",
                                id = as.character(eval.id))

      # delete container if container id is no longer in bag table
      if(!bag_id %in% CheckTableTx(conn = conn, "paper")$bag_id){
        sampleDB::DeleteFromTable(conn = conn,
                                  table_name = "bag",
                                  id = as.character(bag_id))
      }
    }
  }

}
