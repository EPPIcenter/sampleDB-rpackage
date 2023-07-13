#' Read a user CSV file
#'
#' This function reads a user CSV file.
#'
#' @param user_csv The path to the user's csv file.
#' @return A data frame containing the user's CSV file.
#' @noRd
#' @examples
#' \dontrun{
#' read_user_csv("path/to/file.csv")
#' }
#' @export
read_user_csv <- function(user_csv) {
  read.csv(file = user_csv, header = FALSE, na.strings = "", blank.lines.skip = TRUE)
}


#' Preprocess a user CSV file
#'
#' This function preprocesses a user CSV file.
#'
#' @param user_file The user's CSV file.
#' @param user_action The action the user is performing.
#' @return A data frame containing the user's CSV file.
#' @noRd
#' @examples
#' \dontrun{
#' preprocess_csv(user_file, "upload")
#' }
#' @export
preprocess_csv <- function(user_file, user_action) {

  user_file[user_file == ""] <- NA
  user_file[] <- lapply(user_file, function(x) as.character(gsub("[\n\t,]", "", x)))

  if (user_action %in% c("move", "upload")) {
    empty_rows <- rowSums(user_file == "" | is.na(user_file) | is.null(user_file)) == ncol(user_file)
    empty_cols <- colSums(user_file == "" | is.na(user_file) | is.null(user_file)) == nrow(user_file)
    user_file <- user_file[!empty_rows, !empty_cols]
  }

  return(user_file)
}

#' Identify the row in a CSV file that matches the header based on required column names.
#'
#' @param user_file A data frame representing the CSV file.
#' @param required_user_column_names A character vector of required column names.
#' @param valid_header_rows A numeric vector of row numbers to consider as potential header rows.
#'                          By default, it considers the first two rows.
#' @return An integer representing the row number where the header was found, or NULL if not found.
#' @examples
#' \dontrun{
#' user_data <- read.csv("path_to_user_file.csv", header = FALSE)
#' find_header(user_data, c("Name", "Age"))
#' }
#' @export
find_header <- function(user_file, required_user_column_names, valid_header_rows = 1:2) {

  # Sanity check
  if (nrow(user_file) <= 1) {
    stop("File is empty or contains only one row.")
  }

  # Check each potential header row for the presence of required column names
  for (colname_ridx in valid_header_rows) {
    row <- user_file[colname_ridx, ]
    if (all(required_user_column_names %in% row)) {
      return(colname_ridx)
    }
  }

  # Return NULL if no valid header row found
  return(NULL)
}



#' Check if the collection date column should be required
#'
#' This function checks if the collection date column should be required.
#'
#' @param con A database connection.
#' @param user_file The user's CSV file.
#' @param study_data The study data from the database.
#' @return A data frame containing the formatting errors.
#' @import DBI
#' @import dplyr
#' @noRd
#' @keywords internal
check_collection_date <- function(con, user_file, study_data = NULL) {
  df_error <- data.frame(column = character(), reason = character(), trigger = character())

  if (is.null(study_data)) {
    study_data <- DBI::dbReadTable(con, "study")
  }

  relevant_studies <- study_data %>%
    filter(short_code %in% user_file$StudyCode) %>%
    inner_join(user_file, by = c("short_code" = "StudyCode"))

  # Collection date column must exist for all samples that are part of a longitudinal study
  if (!"CollectionDate" %in% colnames(user_file) && nrow(filter(relevant_studies, is_longitudinal == 1)) > 0) {
    df_error <- data.frame(
      column = "CollectionDate",
      reason = "Collection date is required for samples of longitudinal studies.",
      trigger = filter(relevant_studies, is_longitudinal == 1) %>% pull(short_code)
    )
  }

  return(df_error)
}


#' Set a Specific Row as the Header
#'
#' This function updates the header of the user file based on the specified row number.
#'
#' @param user_file A data frame representing the user file.
#' @param header_row A numeric value indicating which row to set as the header.
#' @return A data frame with the updated header.
#' @examples
#' \dontrun{
#' user_file <- read.csv("path_to_user_file.csv", header = FALSE)
#' set_header_row(user_file, 2)
#' }
#' @export
set_header_row <- function(user_file, header_row) {
  colnames(user_file) <- user_file[header_row,]
  user_file <- user_file %>% dplyr::slice(-c(1:header_row))
  return(user_file)
}

#' Create Formatting Error Data Frame
#'
#' This function creates a data frame detailing the formatting errors for required columns that are missing.
#'
#' @param required A character vector of required column names that were not detected.
#' @return A data frame detailing the formatting errors.
#' @examples
#' \dontrun{
#' format_error(c("Name", "Age"))
#' }
#' @export
format_error <- function(required) {
  df.error.formatting <- data.frame(
    column = required,
    reason = c("Always Required"),
    trigger = c("Not detected in file")
  )
  return(df.error.formatting)
}


#' Set the header row for the user CSV file
#'
#' This function identifies the valid header row in the user file and sets it as the header.
#' If no valid header row is found, a formatting error is thrown.
#'
#' @param user_file The user's CSV file as a data frame.
#' @param required_columns A character vector of required column names.
#' @return A data frame with the appropriate header row set.
#' @export
set_user_file_header <- function(user_file, required_columns) {
  header_row <- find_header(user_file, required_columns, valid_header_rows = 1:2)
  if (is.null(header_row)) {
    stop_formatting_error("Could not find required columns", format_error(required_columns))
  }
  return(set_header_row(user_file, header_row))
}


#' Detect Missing Columns in User's CSV File
#'
#' This function checks for any required columns that are missing from the user's CSV file.
#' Additionally, it checks special conditions such as the presence of "CollectionDate" for
#' specific studies that are longitudinal.
#'
#' @param user_file A data frame representing the user's CSV file.
#' @param column_attributes An object containing attributes for columns like required, conditional, and action.
#' @param database A character string specifying the path to the SQLite database.
#'
#' @return A character vector of missing columns from the user's CSV file.
#' 
#' @import DBI dplyr
#' @export
#' @examples
#' \dontrun{
#' user_data <- data.frame(StudyCode = c("ST001"), StudySubject = c("SS001"))
#' column_attrs <- FileColumnAttributes$new()
#' database_path <- "path_to_database.sqlite"
#' detect_missing_columns(user_data, column_attrs, database_path)
#' }
#' @keywords internal
detect_missing_columns <- function(user_file, column_attributes, database = Sys.getenv("SDB_PATH")) {
  
  # Retrieve 'required_columns' from 'column_attributes'.
  required_columns <- column_attributes$required
  
  # Detect any columns from 'required_columns' that aren't present in 'user_file'.
  missing_columns <- setdiff(required_columns, colnames(user_file))
  
  # Check for StudyCode in user_file and if 'upload' is part of the attributes.
  if (!"StudyCode" %in% colnames(user_file) && "CollectionDate" %in% column_attributes$conditional) {
    
    # Retrieve relevant records from the study table based on StudyCode.
    matched_studies <- get_matched_studies(user_file, database)
    
    # Conditionally check the CollectionDate column based on longitudinal studies and attributes.
    if (nrow(filter(matched_studies, is_longitudinal == 1)) > 0) {
      stop_formatting_error("Samples of longitudinal studies must include a CollectionDate column.", format_error("CollectionDate"))
    }
  }

  return(missing_columns)
}

#' Retrieve Matched Studies from the Database
#'
#' This function connects to the specified SQLite database, retrieves study records that match
#' the `StudyCode` present in the user's file, and returns these matched studies.
#'
#' @param user_file A data frame representing the user's CSV file.
#' @param database A character string specifying the path to the SQLite database.
#'
#' @return A data frame containing matched study records.
#' 
#' @import DBI dplyr RSQLite
#' @export
#' @examples
#' \dontrun{
#' user_file <- data.frame(StudyCode = c("ST001", "ST002"))
#' database_path <- "path_to_database.sqlite"
#' get_matched_studies(user_file, database_path)
#' }
#' @keywords internal
get_matched_studies <- function(user_file, database) {
  con <- NULL
  matched_studies <- data.frame() # Initialize as empty dataframe
  error <- FALSE
  
  tryCatch({
    print(user_file)
    con <- dbConnect(RSQLite::SQLite(), database = database)
    print(con)
    # matched_studies <- DBI::dbReadTable(con, "study") %>%
    #   filter(study_short_code %in% user_file$StudyCode) %>%
    #   inner_join(user_file, by = c("short_code" = "StudyCode"))
    matched_studies <- DBI::dbReadTable(con, "study")
    print(matched_studies)
  },
  error = function(e) {
    error <- TRUE
  },
  finally = {
    if (!is.null(con)) {
      dbDisconnect(con)
    }

    if (error) {
      stop("Could not connect to database.")
    }
  })
  
  return(matched_studies)
}


#' Select relevant columns from the user CSV file
#'
#' This function selects the required, conditional, and optional columns from the user file
#' based on provided file column attributes.
#'
#' @param user_file The user's CSV file as a data frame.
#' @param file_column_attr A list containing attributes of file columns such as required, conditional, and optional columns.
#' @return A data frame with only the relevant columns selected.
#' @export
select_relevant_columns <- function(user_file, file_column_attr) {
  user_file <- select(
    user_file,
    all_of(file_column_attr$required),
    any_of(file_column_attr$conditional),
    any_of(file_column_attr$optional)
  )
  return(user_file)
}


#' Handle special columns in the user CSV file
#'
#' This function handles special cases for columns like `container_name` and `freezer_address`
#' which might be added by users.
#'
#' @param user_file The user's CSV file as a data frame.
#' @param container_name The name of the container column if provided by the user.
#' @param freezer_address The address of the freezer column if provided by the user.
#' @param location_params A character vector of location parameters/columns related to the freezer address.
#' @return A list containing the modified user_file and an updated vector of missing columns.
#' @export
handle_special_columns <- function(user_file, container_name, freezer_address, location_params) {
  if (!is.null(container_name) && !container_name %in% colnames(user_file)) {
    user_file[[container_name]] <- rep(container_name, nrow(user_file)) 
  }

  if (!is.null(freezer_address) && !all(location_params %in% colnames(user_file))) {
    for (param in location_params) {
      if (param %in% names(freezer_address)) {
        user_file[[param]] <- rep(freezer_address[[param]], nrow(user_file))
      }
    }
  }

  return(user_file)
}




#' Check if the user CSV file meets the requirements
#'
#' This function checks if the user CSV file meets specific upload requirements,
#' including necessary columns and formatting checks. Errors will be thrown if requirements are not met.
#'
#' @param user_file The user's CSV file as a data frame.
#' @param sample_type The sample type associated with the user's CSV file.
#' @param user_action The action the user is performing, which may have specific checks associated with it.
#' @param file_type The file format.
#' @param container_name The name of the container.
#' @param freezer_address The address of the freezer.
#' @return A cleaned and checked user_file ready for further processing.
#' @export
validate_and_format_specimen_file <- function(user_file, sample_type, user_action, file_type, container_name = NULL, freezer_address = NULL) {

  # 1. Retrieve column information.
  file_column_attr <- get_sample_file_columns(sample_type, user_action, file_type)

  # 2. Handle header.
  user_file <- set_user_file_header(user_file, file_column_attr$required)

  # 3. Handle special columns. These are columns that should be included but may be added by the user.
  user_file <- handle_special_columns(user_file, container_name, freezer_address, file_column_attr$location)

  # 4. Detect missing columns.
  missing_columns <- detect_missing_columns(user_file, file_column_attr)

  # If there are any missing columns still, throw!
  if (length(missing_columns) > 0) {
    stop_formatting_error("Missing required columns", format_error(missing_columns))
  }

  # 5. Select relevant columns.
  user_file <- select_relevant_columns(user_file, file_column_attr)

  # Notify success.
  message("Required columns detected.")

  return(user_file)
}


ProcessSpecimenCSV <- function(user_csv, user_action, sample_type, container_name = NULL, freezer_address = NULL, file_type = "na", validate = TRUE, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG")) {

  if (is.null(user_csv) || user_csv == "") {
    stop("No csv file was provided.")
  }

  # Read and preprocess user CSV
  user_file <- read_user_csv(user_csv)
  user_file <- preprocess_csv(user_file, user_action)
  user_file <- validate_and_format_specimen_file(user_file, user_action, sample_type)

}

