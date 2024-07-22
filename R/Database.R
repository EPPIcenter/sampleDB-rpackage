#' Initialize Database Connection and Copy User Data
#'
#' This function initializes a connection to a specified database and copies user data to a predefined table in the database.
#'
#' @param database The path or name of the SQLite database to connect to.
#' @param user_data A dataframe containing the user data to be copied to the database.
#'
#' @return A connection object representing the active database connection.
#' @examples
#' \dontrun{
#'   database_path <- Sys.getenv("DB_PATH")
#'   user_df <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
#'   conn <- init_and_copy_to_db(database_path, user_df)
#' }
#' @export
init_and_copy_to_db <- function(database, user_data) {
  con <- init_db_conn(database)
  copy_to(con, user_data) 
  return(con)
}

#' Get all strains from the database
#' 
#' @param con A database connection
#' @return A character vector of all strains in the database
#' @export
#' @examples
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' get_strains(con)
#' dbDisconnect(con)
#' @keywords utility database
#' @return A character vector of all strains in the database
get_strains <- function(con) {
  tbl(con, "strain") %>%
    select(name) %>%
    pull()
}

#' Get all studies from the database
#' 
#' @param con A database connection
#' @return A character vector of all studies in the database
#' @export
#' @examples
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' get_studies(con)
#' dbDisconnect(con)
#' @keywords utility database
#' @return A character vector of all studies in the database
get_studies <- function(con) {
  tbl(con, "study") %>%
    select(short_code) %>%
    pull()
}

#' Get all batches from the database
#' 
#' @param con A database connection
#' @return A character vector of all batches in the database
#' @export
#' @examples
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' get_batches(con)
#' dbDisconnect(con)
#' @keywords utility database
#' @return A character vector of all studies in the database
get_batches <- function(con) {

  ## Temporary way to identify a batch from a study
  dbReadTable(con, "study") %>%
    dplyr::mutate(is_batch = !is.na(lubridate::parse_date_time(short_code, "%Y-%m-%d", quiet = TRUE, exact = TRUE))) %>%
    filter(is_batch) %>%
    select(short_code) %>%
    pull()
}

#' Get all densities from the database
#' 
#' @param con A database connection
#' @return A character vector of all batches in the database
#' @export
#' @examples
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' get_batches(con)
#' dbDisconnect(con)
#' @keywords utility database
#' @return A character vector of all studies in the database
get_densities <- function(con) {

  ## Temporary way to identify a batch from a study
  dbReadTable(con, "malaria_blood_control") %>%
    distinct(density) %>%
    arrange(density) %>%
    pull()
}

#' Get all batches from the database
#' 
#' @param con A database connection
#' @return A character vector of all batches in the database
#' @export
#' @examples
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' get_batches(con)
#' dbDisconnect(con)
#' @keywords utility database
#' @return A character vector of all studies in the database
get_percentages <- function(con) {
  tbl(con, "composition_strain") %>%
    select(percentage) %>%
    distinct() %>%
    arrange(percentage) %>%
    pull()
}


#' Retrieve compositions from the database by label
#'
#' @param con A database connection object.
#' @param labels A character vector of labels to be matched.
#'
#' @return A dataframe of matched compositions
retrieve_compositions_by_label <- function(con, labels) {
    tbl(con, "composition") %>%
        dplyr::filter(label %in% !!labels) %>%
        collect()
}

#' Extract unique compositions from the database
#'
#' This function queries a database to collect and process data, generating a unique composition key based on sorted strains and percentages.
#'
#' @param con A database connection object.
#'
#' @return A data frame containing sorted_strains_key for each composition in the database with associated strains and percentages.
#' @seealso \code{\link{format_labels}}
#' @export
get_unique_compositions_from_database <- function(con) {
  local_data <- tbl(con, "composition_strain") %>%
    dplyr::left_join(tbl(con, "strain") %>% dplyr::rename(strain = name), by = c(strain_id = "id")) %>%
    dplyr::collect()

  local_data %>%
    group_by(composition_id) %>%
    summarise(
      combined_strains = paste(strain, collapse = ";"),
      combined_percentages = paste(percentage, collapse = ";")
    ) %>%
    mutate(
      split_data = map2(combined_strains, combined_percentages, ~split_and_sort(.x, .y)),
      sorted_strains_key = map_chr(split_data, ~paste(.x$sorted_strains, collapse = "-")),
      sorted_percentages = map(split_data, ~.x$sorted_percentages)
    ) %>%
    inner_join(dbReadTable(con, "composition"), by = c("composition_id" = "id")) %>%
    select(composition_id, sorted_strains_key, sorted_percentages, index, label)
}

#' Get all composition type from the database
#' 
#' @param con A database connection
#' @export
#' @return A character vector of all monoclonal and polyclonal types in the database
get_composition_types <- function(con) {
    
  tbl(con, "composition_strain") %>%
      add_count(composition_id, name = "strain_count") %>%
      select(strain_count) %>%
      distinct() %>%
      collect() %>%
      mutate(composition_label = format_composition_types(strain_count)) %>%
      arrange(strain_count) %>%
      pull(name = strain_count, composition_label)
} 

#' Format composition types
#' 
#' @param n A numeric vector that represents the number of strains found in a composition
#' @export
#' @return A character vector of all monoclonal and polyclonal types in the database (1-strain, 2-strain, etc.)

format_composition_types <- function(n) {
  sprintf("%d-strain", n)
}

#' Retrieve Current Database Version
#'
#' @param db_or_conn Either a connection string to the SQLite database or an active connection.
#'
#' @return Character string indicating the current version of the database.
#' @import DBI
#' @export
get_db_version <- function(db_or_conn = Sys.getenv("SDB_PATH")) {
  
  # Check if the input is a connection string or an active connection
  is_conn <- inherits(db_or_conn, "DBIConnection")
  
  # If it's a connection string and the file exists
  if (!is_conn && file.exists(db_or_conn)) {
    con <- init_db_conn(db_or_conn)
    on.exit(DBI::dbDisconnect(con), add = TRUE) # Ensure disconnection once done
  } else if (is_conn) {
    con <- db_or_conn
  } else {
    return(NA)
  }
  
  # Fetch the version
  version <- DBI::dbGetQuery(con, "SELECT name FROM version ORDER BY name DESC LIMIT 1") %>% dplyr::pull(name)
  return(version)
}

#' Execute SQL Queries on a Database
#'
#' @param con Database connection.
#' @param sql List of SQL queries to execute.
#' @export
execute_sql <- function(con, sql) {
  lapply(sql, function(s) { DBI::dbExecute(con, s) })
}

#' Lock a SQLite Database
#'
#' This function locks an existing SQLite database to prevent further modifications while performing operations.
#'
#' @param database Path to the SQLite database to be locked.
#' 
#' @return Database connection object after the locking operation.
#' @examples
#' \dontrun{
#'   con <- lock_database("path_to_original.db")
#'   # Do other operations...
#'   DBI::dbDisconnect(con)  # Remember to disconnect when done.
#' }
#' @keywords internal setup
lock_database <- function(database) {
  con <- DBI::dbConnect(SQLite(), database)
  lapply(c("PRAGMA locking_mode = EXCLUSIVE;", "BEGIN EXCLUSIVE;"), function(s) { DBI::dbExecute(con, s) })
  return(con)
}

#' Copy SQLite Database
#'
#' This function copies an existing SQLite database to a new location.
#'
#' @param database Path to the original SQLite database.
#' @param new_database Path to the destination for the SQLite database copy.
#' 
#' @return TRUE if the copy is successful, otherwise FALSE.
#' @examples
#' \dontrun{
#'   success <- copy_database("path_to_original.db", "path_to_new.db")
#'   if (!success) {
#'     stop("Failed to copy the database.")
#'   }
#' }
copy_database <- function(database, new_database) {
  stopifnot("Database could not be copied. Source does not exist." = file.exists(database))
  return(file.copy(database, new_database))
}

#' Initialize a Database Connection
#'
#' This function establishes and returns a connection to an SQLite database.
#'
#' @param db_path The path to the SQLite database (default is "SDB_PATH" env variable).
#'
#' @return A connection object to the SQLite database.
#' @examples
#' \dontrun{
#'   conn <- init_db_conn(Sys.getenv("SDB_PATH"))
#' }
#' @export
init_db_conn <- function(db_path = Sys.getenv("SDB_PATH")) {
  # Load necessary packages
  requireNamespace("RSQLite", quietly = TRUE)

  # Establish a connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  
  if (is.null(con)) {
    stop("Failed to establish a connection to the database.")
  }

  return(con)
}


#' Flatten all database tables by sample type
#' 
#' This function is a utility function to pull all samples from the database.
#'
#' @param sample_storage_type Samples to view by type. Options are 'micronix' and 'cryovial'.
#' @param database The path to the database.
#' 
#' @import RSQLite
#' @import dplyr
#' @import tidyr
#' @export
create_flat_view_by_sample_type <- function(sample_storage_type, database = Sys.getenv("SDB_PATH")) {
  
  stopifnot("Sample storage type is not available for this function." = sample_storage_type %in% c('micronix', 'cryovial'))
  
  con <- dbConnect(SQLite(), database)
  
  join_by_sample_metadata <- function(sample_sql) {
    study_tbl <- tbl(con, "study") %>% select(study_id = id, short_code)
    study_subject_tbl <- tbl(con, "study_subject") %>% select(study_subject_id = id, study_subject = name, study_id)
    specimen_tbl <- tbl(con, "specimen") %>% select(specimen_type_id, specimen_id = id, study_subject_id, collection_date)
    specimen_type_tbl <- tbl(con, "specimen_type") %>% select(specimen_type = name, specimen_type_id = id)
    storage_container_tbl <- tbl(con, "storage_container") %>% select(id, specimen_id, state_id, status_id)
    
    sample_sql %>%
      inner_join(storage_container_tbl, by = "id") %>%
      inner_join(specimen_tbl, by = "specimen_id") %>%
      inner_join(specimen_type_tbl, by = "specimen_type_id") %>%
      inner_join(study_subject_tbl, by = "study_subject_id") %>%
      inner_join(study_tbl, by = "study_id")
  }
  
  location_tbl <- tbl(con, "location") %>% select(location_id = id, location_root, level_I, level_II)
  state_tbl <- tbl(con, "state") %>% select(state_id = id, state = name)
  status_tbl <- tbl(con, "status") %>% select(status_id = id, status = name)
  
  if (sample_storage_type == "micronix") {
    micronix_tbl <- tbl(con, "micronix_tube") %>% select(id, barcode, position, manifest_id)
    micronix_plate_tbl <- tbl(con, "micronix_plate") %>% select(manifest_id = id, plate_name = name, plate_barcode = barcode, location_id)
    sql <- micronix_tbl %>% 
      inner_join(micronix_plate_tbl, by = "manifest_id")
    sql <- join_by_sample_metadata(sql)
  } else if (sample_storage_type == "cryovial") {
    cryovial_tbl <- tbl(con, "cryovial_tube") %>% select(id, barcode, position, manifest_id)
    cryovial_box_tbl <- tbl(con, "cryovial_box") %>% select(manifest_id = id, box_name = name, box_barcode = barcode, location_id)
    sql <- cryovial_tbl %>%
      inner_join(cryovial_box_tbl, by = "manifest_id")
    sql <- join_by_sample_metadata(sql)
  } else {
    stop("Unimplemented sample storage type!!!")
  }
  
  # Join the remaining sample data tables and return the results
  sql %>% 
    inner_join(location_tbl, by = "location_id") %>%
    inner_join(state_tbl, by = "state_id") %>%
    inner_join(status_tbl, by = "status_id") %>%
    select(!ends_with("_id")) %>%
    collect()

}

#' View all extractions in the database 
#' 
#' This function is a utility function to pull all extractions from the database.
#'
#' @param database The path to the database.
#' 
#' @import RSQLite
#' @import dplyr
#' @import tidyr
#' @export
create_flat_view_of_extractions <- function(database = Sys.getenv("SDB_PATH")) {

  con <- init_db_conn(db_path = database)

  study_subject <- tbl(con, "study_subject") %>% dplyr::select(study_subject_id = id, study_id, ControlUID = name)
  batches <- tbl(con, "study") %>% dplyr::select(study_id = id, Batch = short_code)
  malaria_blood_controls <- tbl(con, "malaria_blood_control") %>% dplyr::select(study_subject_id)
  specimens <- tbl(con, "specimen") %>% dplyr::select(SpecimenID = id, study_subject_id)
  storage_containers <- tbl(con, "storage_container") %>% select(ExtractionID=id, SpecimenID = specimen_id)
  micronix_tubes <- tbl(con, "micronix_tube") %>% select(Barcode = barcode, ExtractionID = id)

  study_subject %>%
    dplyr::inner_join(batches, by = join_by(study_id)) %>%
    dplyr::inner_join(malaria_blood_controls, by = join_by(study_subject_id)) %>%
    dplyr::inner_join(specimens, by = join_by(study_subject_id)) %>%
    dplyr::inner_join(storage_containers, by = join_by(SpecimenID)) %>%
    dplyr::inner_join(micronix_tubes, by = join_by(ExtractionID)) %>%
    dplyr::select(ExtractionID, Barcode, ControlUID, Batch) %>%
    collect()
}