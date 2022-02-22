#' Archive and Delete EPPIcenter Wetlab Samples
#' 
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export
#' 


ArchiveAndDeleteSamples <- function(operation, filters = NULL, storage_container_ids = NULL){
  
  database <- "/databases/sampledb/v0.0.2/sampledb_database.sqlite"
  stopifnot("Operation is not valid" = operation %in% c("archive", "delete", "unarchive"))
  
  if(!is.null(filters)){
    storage_container_ids <- sampleDB::SearchSamples(filters)$storage_container_ids 
  }else{
    storage_container_ids <- storage_container_ids
  }
  
  if(operation == "archive"){
    for(eval.id in storage_container_ids){
      sampleDB::ModifyTable(database = database,
                            "storage_container",
                            info_list = list(exhausted == 1),
                            id = eval.id)
    }
  }
  else if(operation == "unarchive"){
    for(eval.id in storage_container_ids){
      sampleDB::ModifyTable(database = database,
                            "storage_container",
                            info_list = list(exhausted == 0),
                            id = eval.id)
    }
  }else{
    
      response <- menu(c("Yes", "No"), title = "Are you sure you want to delete selected item(s)?")
      if(response == "Yes"){
        database.tables <- list(table.storage_container = sampleDB::CheckTable(database = database, "storage_container"),
                                table.tube = sampleDB::CheckTable(database = database, "tube"),
                                table.rdt = sampleDB::CheckTable(database = database, "rdt"),
                                table.paper = sampleDB::CheckTable(database = database, "paper"),
                                table.matrix_tube = sampleDB::CheckTable(database = database, "matrix_tube"))
        
        # DELETE EXTERNAL DATA IF IT IS NOT BEING USED FOR A DIFFERENT SAMPLE
        # DELETE INTERNAL DATA
        for(eval.id in storage_container_ids){
          # DELETE INTERNAL DATA
          tmp_table.storage_container <- filter(database.tables$table.storage_container, id %in% eval.id)
          tmp.specimen_id <- tmp_table.storage_container$specimen_id
          tmp.study_subject_id <- filter(database.tables$table.specimen, id %in% tmp.specimen_id)$study_subject_id
          sampleDB::DeleteFromTable(database = database, 
                                    table_name = "storage_container", 
                                    id = as.character(tmp_table.storage_container$id))
          # specimen
          if(!tmp.specimen_id %in% sampleDB::CheckTable(database = database, "storage_container")$specimen_id){
            sampleDB::DeleteFromTable(database = database, 
                                      table_name = "specimen",
                                      id = as.character(tmp.specimen_id))
            # study subject
            if(!tmp.study_subject_id %in% sampleDB::CheckTable(database = database, "specimen")$study_subject_id){
              sampleDB::DeleteFromTable(database = database, 
                                        table_name = "study_subject", 
                                        id = as.character(tmp.study_subject_id))
            }
          }
          
          # DELETE EXTERNAL DATA
          if(eval.id %in% database.tables$table.matrix_tube$id){
            tmp.matrix_plate_id <- filter(database.tables$table.matrix_tube, id %in% eval.id)$plate_id
            sampleDB::DeleteFromTable(database = database, 
                                      table_name = "matrix_tube", 
                                      id = as.character(eval.id))
            # DELETE CONTAINER IF CONTAINER ID IS NO LONGER IN SAMPLE TYPE
            if(!tmp.matrix_plate_id %in% sampleDB::CheckTable(database = database, "matrix_tube")$plate_id){
              sampleDB::DeleteFromTable(database = database, 
                                        table_name = "matrix_plate", 
                                        id = as.character(tmp.matrix_plate_id))          
            }
          }
          else if(eval.id %in% table.tube$id){
            tmp.box_id <- filter(database.tables$table.tube, id %in% eval.id)$box_id
            sampleDB::DeleteFromTable(database = database, 
                                      table_name = "tube", 
                                      id = as.character(eval.id))
            # DELETE CONTAINER IF CONTAINER ID IS NO LONGER IN SAMPLE TYPE
            if(!tmp.box_id %in% sampleDB::CheckTable(database = database, "tube")$box_id){
              sampleDB::DeleteFromTable(database = database, 
                                        table_name = "box", 
                                        id = as.character(tmp.box_id))
            }
          }
          else if(eval.id %in% table.rdt$id){
            tmp.bag_id <- filter(database.tables$table.rdt, id %in% eval.id)$bag_id
            sampleDB::DeleteFromTable(database = database, 
                                      table_name = "rdt", 
                                      id = as.character(eval.id))
            # DELETE CONTAINER IF CONTAINER ID IS NO LONGER IN SAMPLE TYPE
            if(!tmp.bag_id %in% sampleDB::CheckTable(database = database, "rdt")$bag_id){
              sampleDB::DeleteFromTable(database = database, 
                                        table_name = "bag", 
                                        id = as.character(tmp.bag_id))
            }
          }
          else{
            tmp.bag_id <- filter(database.tables$table.paper, id %in% eval.id)$bag_id
            sampleDB::DeleteFromTable(database = database, 
                                      table_name = "paper", 
                                      id = as.character(eval.id))
            # DELETE CONTAINER IF CONTAINER ID IS NO LONGER IN SAMPLE TYPE
            if(!tmp.bag_id %in% sampleDB::CheckTable(database = database, "paper")$bag_id){
              sampleDB::DeleteFromTable(database = database, 
                                        table_name = "bag", 
                                        id = as.character(tmp.bag_id))
            }
          }
        }
      }else{
        message("NULL")
      }
  }
}
