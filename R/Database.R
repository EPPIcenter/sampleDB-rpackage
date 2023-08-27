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

#' Extract identifiers from the database
#'
#' This function queries a database to collect and process data, generating a unique identifier based on strains and percentages.
#'
#' @param con A database connection object.
#'
#' @keywords utilities database
#' @return A data frame containing unique identifiers for each composition in the database with associated strains and percentages.
#' @seealso \code{\link{format_labels}}
#' @export
get_identifiers_from_database <- function(con) {
    local_data <- tbl(con, "composition_strain") %>%
        dplyr::left_join(tbl(con, "strain") %>% dplyr::rename(strain = name), by = c(strain_id = "id")) %>%
        dplyr::collect()

    local_data %>%
        group_by(composition_id) %>%
        reframe(
            combined_strains = paste(strain, collapse = ";"),
            combined_percentages = paste(percentage, collapse = ";")
        ) %>%
        mutate(
            split_data = map2(combined_strains, combined_percentages, ~split_and_sort(.x, .y)),
            unique_id = map2_chr(split_data, split_data,
                                 ~create_unique_id_from_sorted(.x$sorted_strains, .x$sorted_percentages))
        ) %>%
        inner_join(dbReadTable(con, "composition"), by = c("composition_id" = "id")) %>%
        mutate(legacy = as.logical(legacy)) %>%  # Convert legacy to logical
        select(composition_id, unique_id, index, label, legacy)
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