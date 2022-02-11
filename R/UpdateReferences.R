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
      sampleDB::AddToTable(database = database, 
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
      stopifnot(setequal(names(information), c("OldFreezerName", "NewFreezerName", "NewFreezerLocationType", "NewFreezerLevelI", "NewFreezerLevelII", "NewFreezerLevelIII")))
      id.OldFreezerName <- as.character(filter(sampleDB::CheckTable(database = database, "location"), location_name == information$OldFreezerName)$id)
      eval.created <- as.character(filter(sampleDB::CheckTable(database = database, "location"), location_name == information$OldFreezerName)$created)
      sampleDB::ModifyTable(database = database,
                            table_name = "location",
                            info_list = list(created = eval.created,
                                             last_updated = as.character(lubridate::now("UTC")),
                                             location_name = information$NewFreezerName,
                                             location_type = information$NewFreezerLocationType,
                                             level_I = information$NewFreezerLevelI,
                                             level_II = information$NewFreezerLevelII,
                                             level_III = information$NewFreezerLevelIII),
                            id = id.OldFreezerName)
      message(
        paste0("Modified Freezer with:\n",
               "\tName: \"", information$NewFreezerName, "\"\n",
               "\tType: \"", "\"\n",
               "\tLevel I: \"", "\"\n",
               "\tLevel II: \"", "\"",
               "New Freezer:\n",
               "\tName: \"", information$NewFreezerName, "\"\n",
               "\tType: \"", information$NewFreezerLocationType, "\"\n",
               "\tLevel I: \"", information$NewFreezerLevelI, "\"\n",
               "\tLevel II: \"", level_II = information$NewFreezerLevelII, "\""))
    }
    
    if(operation == "delete"){
      
      # DELETE FREEZER
      stopifnot(names(information) == c("DeleteFreezerName"))
      id.DeleteFreezerName <- as.character(filter(sampleDB::CheckTable(database = database, "location"), location_name == information$DeleteFreezerName)$id)
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "location",
                                id = id.DeleteFreezerName)
      message(
        paste0("Deleted Freezer with:\n",
                "\tName: \"", "\"\n",
                "\tType: \"", "\"\n",
                "\tLevel I: \"", "\"\n",
                "\tLevel II: \"", "\""))
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
      # message(
      #   paste0("Added New Freezer:\n",
      #          "\tName: \"", information$NewFreezerName, "\"\n",
      #          "\tType: \"", information$NewFreezerLocationType, "\"\n",
      #          "\tLevel I: \"", information$NewFreezerLevelI, "\"\n",
      #          "\tLevel II: \"", level_II = information$NewFreezerLevelII, "\""))
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
      # message(
      #   paste0("Modified Freezer with:\n",
      #          "\tName: \"", information$NewFreezerName, "\"\n",
      #          "\tType: \"", "\"\n",
      #          "\tLevel I: \"", "\"\n",
      #          "\tLevel II: \"", "\"",
      #          "New Freezer:\n",
      #          "\tName: \"", information$NewFreezerName, "\"\n",
      #          "\tType: \"", information$NewFreezerLocationType, "\"\n",
      #          "\tLevel I: \"", information$NewFreezerLevelI, "\"\n",
      #          "\tLevel II: \"", level_II = information$NewFreezerLevelII, "\""))
    }
    
    if(operation == "delete"){
      
      # DELETE SPECIMEN TYPE
      stopifnot(names(information) == c("DeleteSpecimenTypeName"))
      id.DeleteSpecimenTypeName <- as.character(filter(sampleDB::CheckTable(database = database, "specimen_type"), label == information$DeleteSpecimenTypeName)$id)
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "specimen_type",
                                id = id.DeleteSpecimenTypeName)
      # message(
      #   paste0("Deleted Freezer with:\n",
      #          "\tName: \"", "\"\n",
      #          "\tType: \"", "\"\n",
      #          "\tLevel I: \"", "\"\n",
      #          "\tLevel II: \"", "\""))
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
      # message(
      #   paste0("Added New Freezer:\n",
      #          "\tName: \"", information$NewFreezerName, "\"\n",
      #          "\tType: \"", information$NewFreezerLocationType, "\"\n",
      #          "\tLevel I: \"", information$NewFreezerLevelI, "\"\n",
      #          "\tLevel II: \"", level_II = information$NewFreezerLevelII, "\""))

    }
    if(operation == "modify"){
      
      stopifnot(all(names(information) %in% c("OldStudyShortCode", "NewStudyTitle", "NewStudyDescription", "NewStudyShortCode", "NewStudyLongitudinal", "NewStudyLeadPerson")))
      eval.created <- as.character(filter(sampleDB::CheckTable(database = database, "study"), short_code == information$OldStudyShortCode)$created)
      id.OldStudyShortCode <- as.character(filter(sampleDB::CheckTable(database = database, "study"), short_code == information$OldStudyShortCode)$id)
      
      sampleDB::ModifyTable(database = database,
                            table_name = "study",
                            info_list = list(created = eval.created,
                                             last_updated = as.character(lubridate::now("UTC")),
                                             title = information$NewStudyTitle,
                                             description = information$NewStudyDescription,
                                             short_code = information$NewStudyShortCode,
                                             is_longitudinal = information$NewStudyLongitudinal,
                                             lead_person = information$NewStudyLeadPerson,
                                             hidden = information$NewStudyHidden) %>% purrr::discard(function(x) is.null(x) || x == ""),
                            id = id.OldStudyShortCode)
      # message(
      #   paste0("Modified Freezer with:\n",
      #          "\tName: \"", information$NewFreezerName, "\"\n",
      #          "\tType: \"", "\"\n",
      #          "\tLevel I: \"", "\"\n",
      #          "\tLevel II: \"", "\"",
      #          "New Freezer:\n",
      #          "\tName: \"", information$NewFreezerName, "\"\n",
      #          "\tType: \"", information$NewFreezerLocationType, "\"\n",
      #          "\tLevel I: \"", information$NewFreezerLevelI, "\"\n",
      #          "\tLevel II: \"", level_II = information$NewFreezerLevelII, "\""))
    }
    
    if(operation == "delete"){
      stopifnot(names(information) == c("DeleteStudyShortCode"))
      id.DeleteStudyShortCode <- as.character(filter(sampleDB::CheckTable(database = database, "study"), short_code == information$DeleteStudyShortCode)$id)
      #sampleDB::DeleteFromTable will not delete if study is in use
      sampleDB::DeleteFromTable(database = database, 
                                table_name = "study", 
                                id = id.DeleteStudyShortCode)
      # message(
      #   paste0("Deleted Freezer with:\n",
      #          "\tName: \"", "\"\n",
      #          "\tType: \"", "\"\n",
      #          "\tLevel I: \"", "\"\n",
      #          "\tLevel II: \"", "\""))
    }
  }
}

# Notes:
# What if usr only wants to update part of a study?
# There are no checks currently in place
