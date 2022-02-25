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
#' @examples
#' 
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export
#' 


ArchiveAndDeleteSamples <- function(operation, sample_id){
  
  database <- "/databases/sampledb/v0.0.2/sampledb_database.sqlite"
  
  stopifnot("Operation is not valid" = operation %in% c("archive", "delete", "unarchive"))
  
  # GET DATABASE TABLES
  database.tables <- .GetDatabaseTables(database)
  
  stopifnot("Not all Sample ID(s) are not present in the database" = all(sample_id %in% database.tables$table.storage_container$id))
  
  if(operation == "archive"){
    
    # VERIFY ARCHIVE
    response <- menu(c("Yes", "No"), title = paste("Are you sure you want to permanently archive selected?", length(sample_id), "item(s)?"))
    if(as.character(response) == "Yes" || as.character(response) == "1"){
      
      # ARCHIVE SAMPLES
      for(eval.id in sample_id){
        
        # ARCHIVE
        sampleDB::ModifyTable(database = database,
                              "storage_container",
                              info_list = list(exhausted = 1),
                              id = eval.id)
        
        # DELETE EXTERNAL DATA
        .DeleteExternalData(eval.id, database.tables, database)
      }
      
      # USER MSG
      message(paste("Archived", length(sample_id), "Successfully"))
    }
  }
  
  # else if(operation == "unarchive"){
  #   for(eval.id in sample_id){
  #     sampleDB::ModifyTable(database = database,
  #                           "storage_container",
  #                           info_list = list(exhausted = 0),
  #                           id = eval.id)
  #   }
  # message(paste("Un-archived", length(sample_id), "Successfully"))
  # }
  
 else{
    
    # VERIFY DELETION
    response <- menu(c("Yes", "No"), title = paste("Are you sure you want to permanently delete selected", length(sample_id), "item(s)?"))
    if(as.character(response) == "Yes" || as.character(response) == "1"){
        
      # DELETE SAMPLES
      for(eval.id in sample_id){

        # DELETE INTERNAL DATA
        .DeleteInternalData(eval.id, database.tables, database)
          
        # DELETE EXTERNAL DATA
        .DeleteExternalData(eval.id, database.tables, database)
      }
      
      # USER MSG
      message(paste("Deleted", length(sample_id), "Successfully"))
    }
  }
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
.DeleteInternalData <- function(eval.id, database.tables, database){
  tmp_table.storage_container <- filter(database.tables$table.storage_container, id %in% eval.id)
  # print(tmp_table.storage_container)
  specimen_id <- tmp_table.storage_container$specimen_id
  # print(specimen_id)
  study_subject_id <- filter(database.tables$table.specimen, id %in% specimen_id)$study_subject_id
  # print(study_subject_id)
  # stop("HERE")
  
  # DELETE INTERNAL DATA -- storage container categorically
  sampleDB::DeleteFromTable(database = database, 
                            table_name = "storage_container", 
                            id = as.character(tmp_table.storage_container$id))
  # print(sampleDB::CheckTable(database = database, "storage_container"))
  
  # DELETE INTERNAL DATA -- delete specimen if it is no longer being reference
  if(!specimen_id %in% sampleDB::CheckTable(database = database, "storage_container")$specimen_id){
    sampleDB::DeleteFromTable(database = database, 
                              table_name = "specimen",
                              id = as.character(specimen_id))
    # print(sampleDB::CheckTable(database = database, "specimen"))
    
    # DELETE INTERNAL DATA -- delete study subject if it is no longer being reference
    if(!study_subject_id %in% sampleDB::CheckTable(database = database, "specimen")$study_subject_id){
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "study_subject", 
                                id = as.character(study_subject_id))
      # print(sampleDB::CheckTable(database = database, "study_subject"))
    }
  } 
}
.DeleteExternalData <- function(eval.id, database.tables, database){
  
  # DELETE EXTERNAL DATA -- matrix_tube & matrix_plate if deletion empties plate
  if(eval.id %in% database.tables$table.matrix_tube$id){
    
    # get container id before sample deletion
    matrix_plate_id <- filter(database.tables$table.matrix_tube, id %in% eval.id)$plate_id
    
    # delete sample
    sampleDB::DeleteFromTable(database = database, 
                              table_name = "matrix_tube", 
                              id = as.character(eval.id))
    
    # delete container if container id is no longer in micronix table
    if(!matrix_plate_id %in% sampleDB::CheckTable(database = database, "matrix_tube")$plate_id){
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "matrix_plate", 
                                id = as.character(matrix_plate_id))          
    }
  }
  
  # DELETE EXTERNAL DATA -- tube & box if deletion empties box
  else if(eval.id %in% database.tables$table.tube$id){
    
    # get container id before deletion
    box_id <- filter(database.tables$table.tube, id %in% eval.id)$box_id
    
    #delete sample
    sampleDB::DeleteFromTable(database = database, 
                              table_name = "tube", 
                              id = as.character(eval.id))
    
    # delete container if container id is no longer in tube table
    if(!box_id %in% sampleDB::CheckTable(database = database, "tube")$box_id){
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "box", 
                                id = as.character(box_id))
    }
  }
  
  # DELETE EXTERNAL DATA -- rdt & bag if deletion empties bag
  else if(eval.id %in% database.tables$table.rdt$id){
    
    # get container id before deletion
    bag_id <- filter(database.tables$table.rdt, id %in% eval.id)$bag_id
    
    #delete sample
    sampleDB::DeleteFromTable(database = database, 
                              table_name = "rdt", 
                              id = as.character(eval.id))
    # delete container if container id is no longer in rdt table
    if(!bag_id %in% sampleDB::CheckTable(database = database, "rdt")$bag_id){
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "bag", 
                                id = as.character(bag_id))
    }
  }
  
  # DELETE EXTERNAL DATA -- paper & bag if deletion empties bag
  else{
    # get container id before deletion
    bag_id <- filter(database.tables$table.paper, id %in% eval.id)$bag_id
    
    #delete sample
    sampleDB::DeleteFromTable(database = database, 
                              table_name = "paper", 
                              id = as.character(eval.id))
    
    # delete container if container id is no longer in bag table
    if(!bag_id %in% sampleDB::CheckTable(database = database, "paper")$bag_id){
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "bag", 
                                id = as.character(bag_id))
    }
  }
}