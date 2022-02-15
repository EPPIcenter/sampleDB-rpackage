#' Update References in the EPPIcenter sampleDB database
#' 
#' @param reference One of the following strings: freezer, specimen_type, study
#' @param operation One of the following strings: add, modify, delete
#' @param information A key value pair that matches the specified reference and operation
#' @examples
#' 
#' UpdateReferences(reference = "study", operation = "modify", information = list(OldStudyShortCode = "C", NewStudyTitle = "AA", NewLeadPerson = "EE"))
#' UpdateReferences(reference = "study", operation = "delete", information = list(DeleteStudyShortCode = "MNP"))
#' @import dplyr
#' @import purrr
#' @export

#NEED TO ADD CHECKS AGAINST DUPLICATE NAMING --
#CAN GET ERR CODE FROM SQLITE EXECUTION?
UpdateReferences <- function(reference, operation, information){
  
  database <- "/databases/new.sampleDB.db"
    
  references <- c("study", "freezer","specimen_type")
  stopifnot(reference %in% references)
  
  operations <- c("add", "modify", "delete")
  stopifnot(operation %in% operations)
  
  if(reference == "freezer"){
    if(operation == "add"){
      
      #ADD FREEZER
      stopifnot(setequal(names(information), c("NewFreezerName", "NewFreezerLocationType", "NewFreezerLevelI", "NewFreezerLevelII")))
      out <- sampleDB::AddToTable(database = database, 
                           table_name = "location",
                           list(created = as.character(lubridate::now("UTC")),
                                last_updated = as.character(lubridate::now("UTC")),
                                location_name = information$NewFreezerName,
                                location_type = information$NewFreezerLocationType,
                                level_I = information$NewFreezerLevelI,
                                level_II = information$NewFreezerLevelII,
                                level_III = "NA"))
      message(
        paste0("Added New Freezer:\n",
               "\tName: \"", information$NewFreezerName, "\"\n",
               "\tType: \"", information$NewFreezerLocationType, "\"\n",
               "\tLevel I: \"", information$NewFreezerLevelI, "\"\n",
               "\tLevel II: \"", level_II = information$NewFreezerLevelII, "\""))
    }
    if(operation == "modify"){
      
      # MODIFY FREEZER
      stopifnot(setequal(names(information), c("OldFreezerName", "OldFreezerLevelI", "OldFreezerLevelII")))
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == information$OldFreezerName & level_I == information$OldFreezerLevelI & level_II == information$OldFreezerLevelII)
      id.OldFreezerName <- as.character(tmp_table.location$id)
      eval.created <- as.character(tmp_table.location$created)
      
      eval.info_list <- list(created = eval.created,
                             last_updated = as.character(lubridate::now("UTC")),
                             location_name = information$NewFreezerName,
                             location_type = information$NewFreezerLocationType,
                             level_I = information$NewFreezerLevelI,
                             level_II = information$NewFreezerLevelII,
                             level_III = "NA") %>% purrr::discard(function(x){is.null(x) || x == ""})
      
      sampleDB::ModifyTable(database = database,
                            table_name = "location",
                            info_list = eval.info_list,
                            id = id.OldFreezerName)
      
      tmp_table2.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == information$OldFreezerName & level_I == information$OldFreezerLevelI & level_II == information$OldFreezerLevelII)

      message(
        paste0("Modified Freezer:\n",
               "\tPrevious Name: \"", tmp_table.location$location_name, "\"\n",
               "\tPrevious Type: \"", tmp_table.location$location_type, "\"\n",
               "\tPrevious Level I: \"", tmp_table.location$level_I, "\"\n",
               "\tPrevious Level II: \"", tmp_table.location$level_II, "\"",
               "New Freezer:\n",
               "\tCurrent Name: \"", tmp_table2.location$location_name, "\"\n",
               "\tCurrent Type: \"", tmp_table2.location$location_type, "\"\n",
               "\tCurrent Level I: \"", tmp_table2.location$level_I, "\"\n",
               "\tCurrent Level II: \"", tmp_table2.location$level_II, "\""))
    }
    
    if(operation == "delete"){
      
      # DELETE FREEZER
      stopifnot(names(information) == c("DeleteFreezerName", "DeleteFreezerLevelI", "DeleteFreezerLevelII"))
      tmp_table.location <- filter(sampleDB::CheckTable(database = database, "location"), location_name == information$DeleteFreezerName & level_I == information$DeleteFreezerLevelI & level_II == information$DeleteFreezerLevelII)
      id.DeleteFreezerName <- as.character(tmp_table.location$id)
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "location",
                                id = id.DeleteFreezerName)
      message(
        paste0("Deleted Freezer:\n",
                "\tName: \"",information$DeleteFreezerName, "\"\n",
                "\tType: \"", tmp_table.location$location_type, "\"\n",
                "\tLevel I: \"", tmp_table.location$level_I, "\"\n",
                "\tLevel II: \"", tmp_table.location$level_II, "\""))
    }
  }
  if(reference == "specimen_type"){
    if(operation == "add"){
      
      #ADD SPECIMEN TYPE
      stopifnot(names(information) == "NewSpecimenTypeName")
      sampleDB::AddToTable(database = database, 
                           table_name = "specimen_type",
                           list(created = as.character(lubridate::now("UTC")),
                                last_updated = as.character(lubridate::now("UTC")),
                                label = information$NewSpecimenTypeName))
      message(
        paste0("Added New Specimen Type:\n",
               "\tName: \"", information$NewSpecimenTypeName, "\""))
    }
    if(operation == "modify"){
      
      # MODIFY SPECIMEN TYPE
      stopifnot(setequal(names(information), c("NewSpecimenTypeName","OldSpecimenTypeName")))
      id.OldSpecimenTypeName <- as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == information$OldSpecimenTypeName)$id)
      eval.created <- as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == information$OldSpecimenTypeName)$created)
      sampleDB::ModifyTable(database = database,
                            table_name = "specimen_type",
                            info_list = list(created = eval.created,
                                             last_updated = as.character(lubridate::now("UTC")),
                                             label = information$NewSpecimenTypeName),
                            id = id.OldSpecimenTypeName)
      message(
        paste0("Modified Specimen Type with:\n",
               "\tPrevious Name: \"", information$OldSpecimenTypeName, "\"\n",
               "\tCurrent Name: \"", information$NewSpecimenTypeName, "\""))
    }
    
    if(operation == "delete"){
      
      # DELETE SPECIMEN TYPE
      stopifnot(names(information) == c("DeleteSpecimenTypeName"))
      id.DeleteSpecimenTypeName <- as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == information$DeleteSpecimenTypeName)$id)
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "specimen_type",
                                id = id.DeleteSpecimenTypeName)
      message(
        paste0("Deleted Specimen Type:\n",
               "\tName: \"", DeleteSpecimenTypeName, "\""))
    }
  }
  if(reference == "study"){
    if(operation == "add"){

      #ADD STUDY
      stopifnot(setequal(names(information), c("NewStudyTitle", "NewStudyDescription", "NewStudyShortCode", "NewStudyLongitudinal", "NewStudyLeadPerson")))
      sampleDB::AddToTable(database = database, 
                           table_name = "study", 
                           info_list = list(created = as.character(lubridate::now("UTC")),
                                            last_updated = as.character(lubridate::now("UTC")),
                                            title = information$NewStudyTitle,
                                            description = information$NewStudyDescription,
                                            short_code = information$NewStudyShortCode,
                                            is_longitudinal = information$NewStudyLongitudinal,
                                            lead_person = information$NewStudyLeadPerson))
      message(
        paste0("Added New Study:\n",
               "\tTitle: \"", information$NewStudyTitle, "\"\n",
               "\tDescription: \"", information$NewStudyDescription, "\"\n",
               "\tShort Code: \"", information$NewStudyShortCode, "\"\n",
               "\tLead Person: \"", information$NewStudyLeadPerson, "\"\n",
               "\tLongitudinal: \"", information$NewStudyLongitudinal, "\""))

    }
    if(operation == "modify"){
      
      stopifnot(all(names(information) %in% c("OldStudyShortCode")))
      tmp_table.study <- filter(sampleDB::CheckTable(database = database, "study"), short_code == information$OldStudyShortCode)
      eval.created <- as.character(tmp_table.study$created)
      id.OldStudyShortCode <- as.character(tmp_table.study$id)
      
      eval.list <- list(created = eval.created,
                             last_updated = as.character(lubridate::now("UTC")),
                             title = information$NewStudyTitle,
                             description = information$NewStudyDescription,
                             short_code = information$NewStudyShortCode,
                             is_longitudinal = information$NewStudyLongitudinal,
                             lead_person = information$NewStudyLeadPerson,
                             hidden = information$NewStudyHidden) %>% purrr::discard(function(x) is.null(x) || x == "")
      
      sampleDB::ModifyTable(database = database,
                            table_name = "study",
                            info_list = eval.list,
                            id = id.OldStudyShortCode)
      tmp_table2.study <- filter(sampleDB::CheckTable(database = database, "study"), short_code == information$OldStudyShortCode)
      
      message(
        paste0("Modified Study with:\n",
               "\tPrevious Title: \"", tmp_table.study$title, "\"\n",
               "\tPrevious Description: \"", tmp_table.study$description, "\"\n",
               "\tPrevious Short Code: \"", tmp_table.study$short_code, "\"\n",
               "\tPrevious Lead Person: \"", tmp_table.study$lead_person, "\"\n",
               "\tPrevious Longitudinal: \"", tmp_table.study$is_longitudinal, "\"",
               "New Study:\n",
               "\tCurrent Title: \"", tmp_table2.study$title, "\"\n",
               "\tCurrent Description: \"", tmp_table2.study$description, "\"\n",
               "\tCurrent Short Code: \"", tmp_table2.study$short_code, "\"\n",
               "\tCurrent Lead Person: \"", tmp_table2.study$lead_person, "\"\n",
               "\tCurrent Longitudinal: \"", tmp_table2.study$is_longitudinal, "\""))
    }
    
    if(operation == "delete"){
      stopifnot(names(information) == c("DeleteStudyShortCode"))
      tmp_table.study <- filter(sampleDB::CheckTable(database = database, "study"), short_code == information$DeleteStudyShortCode)
      id.DeleteStudyShortCode <- as.character(tmp_table.study$id)
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "study", 
                                id = id.DeleteStudyShortCode)
      
      message(
        paste0("Deleted Study:\n",
               "\tTitle: \"", tmp_table.study$title, "\"\n",
               "\tDescription: \"", tmp_table.study$description, "\"\n",
               "\tShort Code: \"", tmp_table.study$short_code, "\"\n",
               "\tLead Person: \"", tmp_table.study$lead_person, "\"\n",
               "\tLongitudinal: \"", tmp_table.study$is_longitudinal, "\""))
    }
  }
}
