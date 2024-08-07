#' Update References in the EPPIcenter SampleDB database
#' 
#' @param reference A string specifying a type of EPPIcenter reference (`freezer`, `specimen_type`, `study`).
#' @param operation A string specifying the type of update operation to perform (`add`, `modify`, `delete`).
#' @param identifier A reference-specific list that identifies the reference item that is being updated.
#' 
#' **If the `reference` is `freezer` the identifier list must include:** \cr
#' `freezer_name`: A string specifying the freezer name \cr
#' `freezer_levelI`: A string specifying the freezer levelI name \cr
#' `freezer_levelII`: A string specifying the freezer levelII name \cr
#' 
#' **If the `reference` is `specimen_type` the identifier list must include:** \cr
#' `specimen_type_name`: A string specifying the specimen type name \cr
#' 
#' **If the `reference` is `study` the identifier list must include:** \cr
#' `study_short_code`: A string specifying the study short code \cr
#' 
#' @param update A reference-specific list that contains the reference update information
#' 
#' **If the `reference` is `freezer` the update, for `add` operations the list must include:** \cr
#' `freezer_name`: A string specifying the freezer name \cr
#' `freezer_type`: A string specifying the freezer type \cr
#' `freezer_levelI`: A string specifying the freezer levelI name \cr
#' `freezer_levelII`: A string specifying the freezer levelII name \cr
#' 
#' **If the `reference` is `specimen_type` the update, for `add` operations the list must include:** \cr
#' `specimen_type_name`: A string specifying the specimen type name \cr
#' 
#' **If the `reference` is `study` the update, for `add` operations the list must include:** \cr
#' `study_title`: A string specifying the study title \cr
#' `study_description`: A string specifying the study description \cr
#' `study_short_code`: A string specifying the study short code \cr
#' `study_lead_person`: A string specifying the study lead person \cr
#' `study_longitudinal`: A logical value. `TRUE` if the study is longitudinal and `FALSE` if the study is not longitudinal \cr
#' 
#' **For `modify` operations all update items are optional**
#' 
#' @examples
#' \dontrun{
#' UpdateReferences(reference = "freezer", operation = "add", update = list(freezer_name = "Ruth", freezer_type = "-80", freezer_levelI = "rack1", freezer_levelII = "position_C1"))
#' }
#' @import dplyr
#' @import purrr
#' @export

# for now rely on sqlite for protection from duplications
# DeleteFromTable function protects against deleting ids in use

UpdateReferences <- function(reference, operation, identifier = NULL, update = NULL){
  
  database <- Sys.getenv("SDB_PATH")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
  RSQLite::dbBegin(conn)
    
  references <- c("study", "freezer","specimen_type")
  stopifnot("invalid reference\n vaid options are: study, freezer and specimen_type" = 
              reference %in% references)
  
  operations <- c("add", "modify", "delete")
  stopifnot("invalid operation\n vaid options are: add, modify and delete" = 
              operation %in% operations)
  
  if(reference == "freezer"){
    if(operation == "add"){
      
      stopifnot("update list must include: freezer_name, freezer_type, freezer_levelI and freezer_levelII" = 
                  setequal(names(update), c("freezer_name", "freezer_type", "freezer_levelI", "freezer_levelII")))
      AddToTable(conn = conn, 
                           table_name = "location",
                           list(created = as.character(lubridate::now()),
                                last_updated = as.character(lubridate::now()),
                                location_root = update$freezer_name,
                                storage_type_id = update$freezer_type,
                                level_I = update$freezer_levelI,
                                level_II = update$freezer_levelII,
                                level_III = NA))
      
      return_message <- paste0("Added New Freezer:\n",
                               "\tName: \"", update$freezer_name, "\"\n",
                               "\tType: \"", update$freezer_type, "\"\n",
                               "\tLevel I: \"", update$freezer_levelI, "\"\n",
                               "\tLevel II: \"", level_II = update$freezer_levelII, "\"")
    }
    if(operation == "modify"){
      
      stopifnot("identifier list must include: freezer_name, freezer_levelI and freezer_levelII"= 
                  all(c("freezer_name", "freezer_levelI", "freezer_levelII") %in% names(identifier)))

      tmp_table.location <- filter(CheckTableTx(conn = conn, "location"), location_root == identifier$freezer_name & level_I == identifier$freezer_levelI & level_II == identifier$freezer_levelII)
      stopifnot("freezer could not be identified" = nrow(tmp_table.location) != 0)
      id.ref_freezer_space <- as.character(tmp_table.location$id)
      eval.created <- as.character(tmp_table.location$created)

      eval.info_list <- list(created = eval.created,
                             last_updated = as.character(lubridate::now()),
                             location_root = update$freezer_name,
                             storage_type_id = update$freezer_type,
                             level_I = update$freezer_levelI,
                             level_II = update$freezer_levelII,
                             level_III = NA) %>% 
        purrr::discard(function(x){is.null(x) || x == "" || is.na(x)})

      ModifyTable(conn = conn,
                            table_name = "location",
                            info_list = eval.info_list,
                            id = id.ref_freezer_space)
      
      tmp_table2.location <- filter(CheckTableTx(conn = conn, "location"), location_root == update$freezer_name & level_I == update$freezer_levelI & level_II == update$freezer_levelII)

      return_message <- paste0("Modified Freezer:\n",
                               "\tPrevious Name: \"", tmp_table.location$location_root, "\"\n",
                               "\tPrevious Level I: \"", tmp_table.location$level_I, "\"\n",
                               "\tPrevious Level II: \"", tmp_table.location$level_II, "\"\n",
                               "New Freezer:\n",
                               "\tCurrent Name: \"", tmp_table2.location$location_root, "\"\n",
                               "\tCurrent Level I: \"", tmp_table2.location$level_I, "\"\n",
                               "\tCurrent Level II: \"", tmp_table2.location$level_II, "\"")
    }
    
    if(operation == "delete"){
      
      stopifnot("identifier list must include: freezer_name, freezer_levelI and freezer_levelII" = 
                  all(c("freezer_name", "freezer_levelI", "freezer_levelII") %in% names(identifier)))
      tmp_table.location <- filter(CheckTableTx(conn = conn, "location"), location_root == identifier$freezer_name & level_I == identifier$freezer_levelI & level_II == identifier$freezer_levelII)
      stopifnot("freezer could not be identified" = nrow(tmp_table.location) != 0)
      id.ref_freezer_space <- as.character(tmp_table.location$id)
      DeleteFromTable(conn = conn, 
                                table_name = "location",
                                id = id.ref_freezer_space)
      
      return_message <- paste0("Deleted Freezer:\n",
                               "\tName: \"",identifier$freezer_name, "\"\n",
                               "\tLevel I: \"", tmp_table.location$level_I, "\"\n",
                               "\tLevel II: \"", tmp_table.location$level_II, "\"")
    }
  }
  if(reference == "specimen_type"){
    if(operation == "add"){
      
      stopifnot("update list must include: specimen_type_name" = 
                  names(update) == "specimen_type_name")
      AddToTable(conn = conn, 
                           table_name = "specimen_type",
                           list(created = as.character(lubridate::now()),
                                last_updated = as.character(lubridate::now()),
                                name = update$specimen_type_name))
      return_message <- paste0("Added New Specimen Type:\n",
                               "\tName: \"", update$specimen_type_name, "\"")
    }
    if(operation == "modify"){
      
      stopifnot("identifier list must include: specimen_type_name" = 
                  names(identifier) == "specimen_type_name")
      tmp_table.specimen_type <- filter(CheckTable(database = database, "specimen_type"), name == identifier$specimen_type_name)
      stopifnot("specimen type could not be identified" = nrow(tmp_table.specimen_type) != 0)
      id.ref_specimen_type <- as.character(tmp_table.specimen_type$id)
      eval.created <- as.character(tmp_table.specimen_type$created)
      ModifyTable(conn = conn,
                            table_name = "specimen_type",
                            info_list = list(created = eval.created,
                                             last_updated = as.character(lubridate::now()),
                                             name = update$specimen_type_name),
                            id = id.ref_specimen_type)
      
      return_message <- paste0("Modified Specimen Type with:\n",
                               "\tPrevious Name: \"", identifier$specimen_type_name, "\"\n",
                               "\tCurrent Name: \"", update$specimen_type_name, "\"")
    }
    
    if(operation == "delete"){
      
      stopifnot("identifier list must include: specimen_type_name" = 
                  names(identifier) == c("specimen_type_name"))
      tmp_table.specimen_type <- filter(CheckTableTx(conn = conn, "specimen_type"), name == identifier$specimen_type_name)
      stopifnot("specimen type could not be identified" = nrow(tmp_table.specimen_type) != 0)
      id.ref_specimen_type <- as.character(tmp_table.specimen_type$id)
      DeleteFromTable(conn = conn, 
                                table_name = "specimen_type",
                                id = id.ref_specimen_type)
      
      return_message <- paste0("Deleted Specimen Type:\n",
                               "\tName: \"", identifier$specimen_type_name, "\"")
    }
  }
  if(reference == "study"){
    if(operation == "add"){

      stopifnot(setequal(names(update), c("study_title", "study_description", "study_short_code", "study_longitudinal", "study_lead_person")))
      AddToTable(conn = conn, 
                           table_name = "study", 
                           info_list = list(created = as.character(lubridate::now()),
                                            last_updated = as.character(lubridate::now()),
                                            title = update$study_title,
                                            description = update$study_description,
                                            short_code = update$study_short_code,
                                            is_longitudinal = as.numeric(update$study_longitudinal),
                                            lead_person = update$study_lead_person))
      
      return_message <- paste0("Added New Study:\n",
                               "\tTitle: \"", update$study_title, "\"\n",
                               "\tDescription: \"", update$study_description, "\"\n",
                               "\tShort Code: \"", update$study_short_code, "\"\n",
                               "\tLead Person: \"", update$study_lead_person, "\"\n",
                               "\tLongitudinal: \"", update$study_longitudinal, "\"")

    }
    if(operation == "modify"){
      
      stopifnot(all(c("study_short_code") %in% names(identifier)))
      tmp_table.study <- filter(CheckTableTx(conn = conn, "study"), short_code == identifier$study_short_code)
      stopifnot("study could not be identified" = nrow(tmp_table.study) != 0)
      eval.created <- as.character(tmp_table.study$created)
      id.ref_study <- as.character(tmp_table.study$id)
      
      eval.list <- list(created = eval.created,
                             last_updated = as.character(lubridate::now()),
                             title = update$study_title,
                             description = update$study_description,
                             short_code = update$study_short_code,
                             is_longitudinal = as.numeric(update$study_longitudinal),
                             lead_person = update$study_lead_person) %>% 
        purrr::discard(function(x) is.null(x) || x == "")
      
      ModifyTable(conn = conn,
                            table_name = "study",
                            info_list = eval.list,
                            id = id.ref_study)
      if(update$study_short_code == "" | is.na(update$study_short_code)){
        tmp_table2.study <- filter(CheckTableTx(conn = conn, "study"), short_code == identifier$study_short_code)
      }else{
        tmp_table2.study <- filter(CheckTableTx(conn = conn, "study"), short_code == update$study_short_code) 
      }
      
      
      return_message <- paste0("Modified Study:\n",
                               "\tPrevious Title: \"", tmp_table.study$title, "\"\n",
                               "\tPrevious Description: \"", tmp_table.study$description, "\"\n",
                               "\tPrevious Short Code: \"", tmp_table.study$short_code, "\"\n",
                               "\tPrevious Lead Person: \"", tmp_table.study$lead_person, "\"\n",
                               "\tPrevious Longitudinal: \"", tmp_table.study$is_longitudinal, "\"\n",
                               "New Study:\n",
                               "\tCurrent Title: \"", tmp_table2.study$title, "\"\n",
                               "\tCurrent Description: \"", tmp_table2.study$description, "\"\n",
                               "\tCurrent Short Code: \"", tmp_table2.study$short_code, "\"\n",
                               "\tCurrent Lead Person: \"", tmp_table2.study$lead_person, "\"\n",
                               "\tCurrent Longitudinal: \"", tmp_table2.study$is_longitudinal, "\"")
    }
    
    if(operation == "delete"){
      stopifnot(names(identifier) == c("study_short_code"))
      tmp_table.study <- filter(CheckTableTx(conn = conn, "study"), short_code == identifier$study_short_code)
      stopifnot("study could not be identified" = nrow(tmp_table.study) != 0)
      id.ref_study <- as.character(tmp_table.study$id)
      DeleteFromTable(conn = conn, 
                                table_name = "study", 
                                id = id.ref_study)
      
      return_message <- paste0("Deleted Study:\n",
                               "\tTitle: \"", tmp_table.study$title, "\"\n",
                               "\tDescription: \"", tmp_table.study$description, "\"\n",
                               "\tShort Code: \"", tmp_table.study$short_code, "\"\n",
                               "\tLead Person: \"", tmp_table.study$lead_person, "\"\n",
                               "\tLongitudinal: \"", tmp_table.study$is_longitudinal, "\"")
    }
  }

  RSQLite::dbCommit(conn)
  RSQLite::dbDisconnect(conn)
  message(return_message)
  return(return_message)
}


#' Append Strains to the Database Table
#' 
#' This function appends new strains to the 'strain' table in the database.
#' 
#' @param user_file A data frame containing a column named 'strain'.
#' @param database The path to the SQLite database.
#' 
#' @return A boolean indicating success (TRUE) or failure (FALSE).
#' @export
#' 
#' @examples
#' # Assuming you have a data frame named user_data with a strain column and a valid SQLite database at "/path/to/db"
#' result <- append_strains_to_db(user_data, "/path/to/db")
#' 
append_strains_to_db <- function(user_file, database = Sys.getenv("SDB_PATH")) {
  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  on.exit(DBI::dbDisconnect(con), add = TRUE)  # Ensure connection is closed even if there's an error
  
  res <- FALSE
  
  tryCatch({
    DBI::dbBegin(con)
    res <- DBI::dbAppendTable(con, "strain", user_file %>% select(Strains) %>% dplyr::rename(name = Strains))
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    message("Error appending strains to database: ", e$message)
  })
  
  return(res)
}

#' Append Studies to the Database Table
#' 
#' This function appends new strains to the 'strain' table in the database.
#' 
#' @param user_file A data frame containing a column named 'strain'.
#' @param database The path to the SQLite database. 
#' 
#' @return A boolean indicating success (TRUE) or failure (FALSE).
#' @export
#' 
#' @examples
#' # Assuming you have a data frame named user_data with a strain column and a valid SQLite database at "/path/to/db"
#' result <- append_strains_to_db(user_data, "/path/to/db")
#' 
# Appends a study record to the database
append_study_to_db <- function(title, short_code, description, lead_person, is_longitudinal, database) {

  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  on.exit(DBI::dbDisconnect(con), add = TRUE)  # Ensure connection is closed even if there's an error
  
  # Begin transaction
  dbBegin(con)
  
  res <- NULL
  
  tryCatch({
    now <- as.character(lubridate::now())
    df.payload <- data.frame(
      created = now,
      last_updated = now,
      title = title,
      short_code = short_code,
      description = description,
      lead_person = lead_person,
      is_longitudinal = is_longitudinal
    )

    res <- dbAppendTable(con, "study", df.payload)
    dbCommit(con)  # Commit the transaction if everything went fine

  }, error = function(e) {
    # On error, rollback and propagate the error to the caller
    dbRollback(con)
    message("Error appending study to database: ", e$message)
  })
  
  return(res)
}