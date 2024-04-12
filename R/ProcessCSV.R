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
#' @return A data frame containing the user's CSV file.
#' @noRd
#' @examples
#' \dontrun{
#' preprocess_csv(user_file, "upload")
#' }
#' @export
preprocess_csv <- function(user_file) {

  user_file[user_file == ""] <- NA
  
  # Remove unwanted characters and trim whitespace
  user_file[] <- lapply(user_file, function(x) {
    x <- as.character(x)
    x <- gsub("[\n\t,]", "", x)  # Remove newline, tab, and commas
    trimws(x)  # Trim leading and trailing whitespaces
  })

  empty_rows <- rowSums(user_file == "" | is.na(user_file) | is.null(user_file)) == ncol(user_file)
  empty_cols <- colSums(user_file == "" | is.na(user_file) | is.null(user_file)) == nrow(user_file)
  user_file <- user_file[!empty_rows, !empty_cols, drop = FALSE]

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
    if (any(required_user_column_names %in% row)) {
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
format_error <- function(required, reason = "Always Required", trigger = "Not detected in file") {
  df.error.formatting <- data.frame(
    column = required,  # 'required' basically dictates the number of rows
    reason = c(reason),
    trigger = c(trigger)
  )
  return(df.error.formatting)
}

#' Remove columns that have NA in a row
#' 
#' This is a helper function for set_user_file_header. Before set_header_row
#' can be called, we need to make sure there are no NAs in the row because
#' we should not have NAs as column headers. At this point we know what 
#' the header row should be so we can simply remove columns at this point. 
#' This helps resolve bugs - if he user has a random character in a row,
#' it will create a new column. This potentially could be addressed during 
#' preprocessing but the advantage of handling this in set_user_file_header
#' is that we have have the columns that we want and we can confidently remove
#' anything else.
#' 
#' @param data The data frame
#' @param row_num The row to look for NAs in
remove_na_columns_in_row <- function(data, row_num) {
  valid_columns <- !is.na(data[row_num, ])
  return(data[ , valid_columns, drop = FALSE])
}

#' Set the header row for the user CSV file
#'
#' This function identifies the valid header row in the user file and sets it as the header.
#' If no valid header row is found, a formatting error is thrown.
#'
#' @param user_file The user's CSV file as a data frame.
#' @param file_column_attr A colum attribute object containing required columns.
#' @importFrom purrr is_empty
#' @return A data frame with the appropriate header row set.
#' @export
set_user_file_header <- function(user_file, file_column_attr) {
  header_row <- find_header(user_file, file_column_attr$required, valid_header_rows = 1:2)
  if (is.null(header_row)) {
    stop_formatting_error("Could not find required columns", format_error(file_column_attr$required))
  }

  # Remove columns with NA in the target header row
  user_file <- remove_na_columns_in_row(user_file, header_row)

  duplicated_column_names <- check_for_duplicates_in_a_row(user_file, header_row)
  if (!is_empty(duplicated_column_names)) {
    stop_formatting_error(
      "Duplicate column names detected, please check your file",
      format_error(
        duplicated_column_names,
        "Duplicated",
        "Found more than once in your file header"
      )
    )
  }

  return(set_header_row(user_file, header_row))
}


#' Check for duplicates in row
#' 
#' This function will check if there are duplicated values in a row. This function
#' is used to identify a if a users file has duplicated column headers before
#' setting the column names.
#' 
#' @param user_data The user data data.frame.
#' @param row_number Numerical value indicating which row to check for duplicates
#' @importFrom tidyr pivot_longer
#' @return A list with the column names that were found more than once.
#' @keywords internal
check_for_duplicates_in_a_row <- function(user_data, row_number) {
  user_data %>% 
    slice(row_number) %>%
    pivot_longer(cols = everything(), names_to = "column", values_to = "value") %>%
    count(value) %>%
    filter(n > 1) %>%
    pull(value) %>%
    unique()
}


#' Detect Missing Specimen Columns in User's CSV File
#'
#' This function examines the user's specimen CSV file for any essential specimen columns that may be absent.
#' Additionally, it verifies specific conditions such as the existence of "CollectionDate" for specimens
#' that belong to longitudinal studies.
#'
#' @param specimen_file A data frame representing the user's specimen CSV file.
#' @param specimen_column_attributes An object containing attributes for specimen columns, including required, conditional, and action.
#' @param database A character string indicating the path to the SQLite database for specimen information.
#'
#' @return A character vector of specimen columns absent from the user's CSV file.
#'
#' @import DBI dplyr
#' @export
#' @examples
#' \dontrun{
#' specimen_data <- data.frame(StudyCode = c("ST001"), SpecimenID = c("SP001"))
#' specimen_column_attrs <- SpecimenColumnAttributes$new()
#' database_path <- "path_to_database.sqlite"
#' detect_missing_specimen_columns(specimen_data, specimen_column_attrs, database_path)
#' }
#' @keywords internal
detect_missing_specimen_columns <- function(specimen_file, specimen_column_attributes, database = Sys.getenv("SDB_PATH")) {

  # Identify any columns from 'required_columns' that are not present in 'specimen_file'.
  missing_columns <- setdiff(specimen_column_attributes$required, colnames(specimen_file))

  # Check for StudyCode in specimen_file and if 'CollectionDate' is part of the specimen column attributes.
  if (!"StudyCode" %in% colnames(specimen_file) && "CollectionDate" %in% specimen_column_attributes$conditional) {

    # Retrieve relevant records from the study table based on StudyCode.
    matched_studies <- get_matched_studies(specimen_file, database)

    # Conditionally verify the CollectionDate column based on the nature of longitudinal specimen studies.
    if (is.null(matched_studies) || nrow(filter(matched_studies, is_longitudinal == 1)) > 0) {
      stop_formatting_error("Specimens from longitudinal studies must include a CollectionDate column.", format_error("CollectionDate"))
    }
  }

  return(missing_columns)
}

#' Detect Missing Columns in Reference CSV File
#'
#' This function examines the user's control CSV file to identify any essential control columns that may be absent.
#' It checks against the given control column attributes to ensure all required columns are present in the CSV.
#'
#' @param reference_file A data frame representing the user's control CSV file.
#' @param reference_column_attributes An object containing attributes for control columns, specifically required columns.
#'
#' @return A character vector of control columns missing from the user's CSV file.
#'
#' @examples
#' \dontrun{
#' control_data <- data.frame(ControlCode = c("CT001"), ControlName = c("CTRL_A"))
#' control_column_attrs <- ControlColumnAttributes$new() # Assuming such an object exists
#' detect_missing_control_columns(control_data, control_column_attrs)
#' }
#' @export
#' @keywords internal
detect_missing_reference_columns <- function(control_file, reference_column_attributes) {

  missing_columns <- setdiff(reference_column_attributes$required, colnames(control_file))
  return(missing_columns)
}

#' Detect Missing Columns in Control CSV File
#'
#' This function examines the user's control CSV file to identify any essential control columns that may be absent.
#' It checks against the given control column attributes to ensure all required columns are present in the CSV.
#'
#' @param control_file A data frame representing the user's control CSV file.
#' @param control_column_attributes An object containing attributes for control columns, specifically required columns.
#'
#' @return A character vector of control columns missing from the user's CSV file.
#'
#' @examples
#' \dontrun{
#' control_data <- data.frame(ControlCode = c("CT001"), ControlName = c("CTRL_A"))
#' control_column_attrs <- ControlColumnAttributes$new() # Assuming such an object exists
#' detect_missing_control_columns(control_data, control_column_attrs)
#' }
#' @export
#' @keywords internal
detect_missing_control_columns <- function(control_file, control_column_attributes, database = Sys.getenv("SDB_PATH")) {


  # Retrieve 'required_columns' from 'specimen_column_attributes'.
  required_columns <- c(
    control_column_attributes$required,
    control_column_attributes$conditional
  )

  missing_columns <- setdiff(required_columns, colnames(control_file))
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
  matched_studies <- NULL
  error <- FALSE

  tryCatch({
    con <- init_db_conn(database)
    matched_studies <- dbReadTable(con, "study") %>%
      inner_join(user_file, by = c("short_code" = "StudyCode"))
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
#' @param file_column_attr An instance of ColumnData containing attributes of file columns.
#' @param bind_data Data that has been bound to the data frame that should be included in the selection.
#' @return A data frame with only the relevant columns selected.
#' @export
select_relevant_columns <- function(user_file, file_column_attr, bind_data) {

  # Note: For now, use the values and not denormalized names.
  user_file <- user_file %>%
    select(
      all_of(file_column_attr$required),
      any_of(file_column_attr$conditional),
      any_of(file_column_attr$optional),
      all_of(names(bind_data))
    )

  return(user_file)
}


#' Check if the user CSV file meets the requirements
#'
#' This function checks if the user CSV file meets specific upload requirements,
#' including necessary columns and formatting checks. Errors will be thrown if requirements are not met.
#'
#' @param user_file The user's CSV file as a data frame.
#' @param sample_type The sample type associated with the user's CSV file.
#' @param file_column_attr The file column attributes.
#' @return A cleaned and checked user_file ready for further processing.
#' @export
validate_and_format_specimen_file <- function(user_file, sample_type, user_action, file_column_attr, bind_data = NULL) {

  # 1. Handle header.
  user_file <- set_user_file_header(user_file, file_column_attr)

  # 2. Add new column and data to the file

  if (!is.null(bind_data)) {
    user_file <- bind_new_data(user_file, bind_data)
  }

  missing_columns <- detect_missing_specimen_columns(user_file, file_column_attr)

  # If there are any missing columns still, throw!
  if (length(missing_columns) > 0) {
    stop_formatting_error("Missing required columns", format_error(missing_columns))
  }

  # 5. Select relevant columns.
  user_file <- select_relevant_columns(user_file, file_column_attr, bind_data)

  # Notify success.
  message("Required columns detected.")

  return(user_file)
}


#' Check if the user CSV file for control meets the requirements
#'
#' This function checks if the user CSV file for control meets specific upload requirements,
#' including necessary columns and formatting checks. Errors will be thrown if requirements are not met.
#'
#' @param user_file The user's CSV file as a data frame.
#' @param file_column_attr A list containing attributes of file columns such as required, conditional, and optional columns.
#' @param bind_data Data that should be added to the data frame.
#' @return A cleaned and checked user_file ready for further processing.
#' @export
validate_and_format_control_file <- function(user_file, file_column_attr, bind_data = NULL) {

  # 1. Handle header.
  user_file <- set_user_file_header(user_file, file_column_attr)

  # 2. Add new column and data to the file
  if (!is.null(bind_data)) {
    user_file <- bind_new_data(user_file, bind_data)
  }

  # 3. Detect missing columns.
  missing_columns <- detect_missing_control_columns(user_file, file_column_attr)

  # If there are any missing columns still, throw!
  if (length(missing_columns) > 0) {
    stop_formatting_error("Missing required columns", format_error(missing_columns))
  }

  # 4. Select relevant columns.
  user_file <- select_relevant_columns(user_file, file_column_attr, bind_data)

  # Notify success.
  message("Required columns detected.")

  return(user_file)
}

#' Check if the user CSV file for control meets the requirements
#'
#' This function checks if the user CSV file for control meets specific upload requirements,
#' including necessary columns and formatting checks. Errors will be thrown if requirements are not met.
#'
#' @param user_file The user's CSV file as a data frame.
#' @param file_column_attr A list containing attributes of file columns such as required, conditional, and optional columns.
#' @return A cleaned and checked user_file ready for further processing.
#' @export
validate_and_format_reference_file <- function(user_file, file_column_attr, bind_data = NULL) {

  # 2. Handle header.
  user_file <- set_user_file_header(user_file, file_column_attr)

  # 3. Detect missing columns.
  missing_columns <- detect_missing_reference_columns(user_file, file_column_attr)

  # If there are any missing columns still, throw!
  if (length(missing_columns) > 0) {
    stop_formatting_error("Missing required columns", format_error(missing_columns))
  }

  # 4. Select relevant columns.
  user_file <- select_relevant_columns(user_file, file_column_attr, bind_data)

  if ("LegacyLabel" %in% file_column_attr$optional && is.null(user_file$LegacyLabel)) {
    user_file$LegacyLabel <- c(NA)
  }

  # Notify success.
  message("Required columns detected.")

  return(user_file)
}

#' Process and Validate Specimen User CSV File
#'
#' This function processes and validates a user's specimen CSV file to ensure it meets
#' the requirements for a specific action. The process includes reading the file, preprocessing,
#' validation, and formatting of the specimens. If the file doesn't meet the specified requirements
#' for specimens, an error is thrown.
#'
#' @param user_csv The path to the user's specimen CSV file. This will be read into a data frame.
#' @param user_action A character string indicating the action the user is taking with the specimen data (e.g., 'upload').
#' @param sample_type A character string specifying the type of specimens in the user CSV.
#' @param file_type A character string indicating the type of file (default is "na").
#' @param bind_data A named list of data to be added to the user file. The list names will be the new columns and the values to their respective column. Default is NULL, which means that no data will be binded.
#' @param database A character string indicating the path to the SQLite database used for specimen validation checks. Defaults to the system environment variable 'SDB_PATH'.
#' @param config_yml A character string indicating the path to a configuration YAML file for specimens. Defaults to the system environment variable 'SDB_CONFIG'.
#'
#' @return A processed and validated user specimen CSV as a data frame.
#'
#' @seealso
#' \code{\link{read_user_csv}}, \code{\link{preprocess_csv}},
#' \code{\link{validate_and_format_specimen_file}}, \code{\link{validate_specimens}}
#' @export
#' @examples
#' \dontrun{
#' # Provide the path to the user specimen CSV file and action details
#' user_file_path <- "path/to/user/specimen/file.csv"
#' action <- "upload"
#' type <- "blood_sample"
#'
#' # Process and validate the user specimen CSV
#' processed_csv <- process_specimen_csv(user_file_path, action, type)
#' }
process_specimen_csv <- function(user_csv, user_action, sample_type, file_type = "na", bind_data = NULL, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG")) {
  if (is.null(user_csv) || user_csv == "") {
    stop("No csv file was provided.")
  }

  # Read and preprocess user CSV file
  # Steps:
  # 1. Read the CSV file.
  # 2. Preprocess to remove empty rows or columns.
  # 3. Prepare for validation by checking for empty data points, renaming columns and adding position column if necessary.
  # 4. Validate and format based on requirements.
  user_data <- read_and_preprocess_csv(user_csv)

  file_column_attr <- get_sample_file_columns(sample_type, user_action, file_type, config_yml)
  user_data <- validate_and_format_specimen_file(user_data, user_action, sample_type, file_column_attr, bind_data)

  user_data <- prepare_specimen_data_for_validation(sample_type, user_data, file_type, file_column_attr) # note: duplicate information, see fn for details

  user_data <- validate_specimens(user_data, sample_type, user_action, file_type, database)

  return(user_data)
}


#' Process and Validate Control CSV File
#'
#' This function processes and validates a user-provided control CSV file.
#' The process includes reading the file, preprocessing it to remove empty rows or columns,
#' and then validating and formatting based on the given control type and action.
#' If the file doesn't meet the specific requirements or if any validation rules are violated,
#' appropriate errors or warnings will be raised.
#'
#' @param user_csv The path to the user's control CSV file. This will be read into a data frame.
#' @param user_action A character string indicating the action the user is taking (e.g., 'upload').
#' @param control_type A character string specifying the type of controls in the user CSV.
#' @param file_type A character string indicating the type of file (default is "na").
#' @param bind_data A named list of data to be added to the user file. The list names will be the new columns and the values to their respective column. Default is NULL, which means that no data will be binded.
#' @param database A character string indicating the path to the SQLite database
#'        used for validation checks. Defaults to the system environment variable 'SDB_PATH'.
#' @param config_yml A character string indicating the path to a configuration YAML file.
#'        Defaults to the system environment variable 'SDB_CONFIG'.
#'
#' @return A processed and validated control CSV as a data frame.
#'
#' @seealso
#' \code{\link{read_and_preprocess_csv}}, \code{\link{validate_and_format_control_file}}, \code{\link{validate_controls}}
#' @export
#' @examples
#' \dontrun{
#' # Provide the path to the user CSV file and action details
#' user_file_path <- "path/to/user/control_file.csv"
#' action <- "upload"
#' type <- "quality_control"
#'
#' # Process and validate the user CSV
#' processed_data <- process_control_csv(user_file_path, action, type)
#' }
process_control_csv <- function(user_csv, user_action, control_type, file_type = "na", bind_data = NULL, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG")) {
  
  # Ensure a valid CSV is provided
  if (is.null(user_csv) || user_csv == "") {
    stop("No csv file was provided.")
  }

  # Read and preprocess user CSV file
  user_data <- read_and_preprocess_csv(user_csv)

  # Validate and format the user CSV data
  file_column_attr <- get_control_file_columns(control_type, user_action)
  user_data <- validate_and_format_control_file(user_data, file_column_attr, bind_data)

  # Further validate controls - check for empty data points, rename columns and add position column if necessary
  user_data <- prepare_control_data_for_validation(control_type, user_data, user_action, file_column_attr)
  user_data <- validate_controls(database, user_data, control_type, user_action)

  return(user_data)
}

#' Process and Validate Reference CSV File
#'
#' This function processes and validates a user-provided reference CSV file.
#' The process includes reading the file, preprocessing it to remove empty rows or columns,
#' and then validating and formatting based on the given reference type and action.
#' If the file doesn't meet the specific requirements or if any validation rules are violated,
#' appropriate errors or warnings will be raised.
#'
#' @param user_csv The path to the user's reference CSV file. This will be read into a data frame.
#' @param user_action A character string indicating the action the user is taking (e.g., 'upload').
#' @param reference_type A character string specifying the type of references in the user CSV. 
#'        Valid types are 'compositions' of 'strains'.
#' @param database A character string indicating the path to the SQLite database
#'        used for validation checks. Defaults to the system environment variable 'SDB_PATH'.
#' @param config_yml A character string indicating the path to a configuration YAML file.
#'        Defaults to the system environment variable 'SDB_CONFIG'.
#'
#' @return A processed and validated reference CSV as a data frame.
#'
#' @seealso
#' \code{\link{read_and_preprocess_csv}}, \code{\link{validate_and_format_reference_file}}, \code{\link{validate_references}}
#' @export
#' @examples
#' \dontrun{
#' # Provide the path to the user CSV file and action details
#' user_file_path <- "path/to/user/reference_file.csv"
#' action <- "upload"
#' type <- "compositions"
#'
#' # Process and validate the user CSV
#' processed_data <- process_reference_csv(user_file_path, action, type)
#' }
process_reference_csv <- function(user_csv, user_action, reference_type, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG")) {

  # Ensure a valid CSV is provided
  if (is.null(user_csv) || user_csv == "") {
    stop("No csv file was provided.")
  }

  # Read and preprocess user CSV file
  user_data <- read_and_preprocess_csv(user_csv)

  # Validate and format the user CSV data
  file_column_attr <- get_reference_file_columns(reference_type)
  user_data <- validate_and_format_reference_file(user_data, file_column_attr)

  # Further validate references
  validation_data <- prepare_reference_data_for_validation(user_data, reference_type, file_column_attr)
  
  validate_references(database, validation_data, reference_type, user_action)

  # Return the validated data
  return(validation_data)
}

#' Add Data to User Data
#'
#' This function takes a named list and appends its values as a new column to a dataframe.
#' The name of the named list is matched with the dataframe's column names.
#'
#' @param df The dataframe to which the data will be added.
#' @param named_list The named list whose elements should be added as a new column to the dataframe.
#' 
#' @return The dataframe with the added column.
#' @examples
#' df <- data.frame(Name = c("John", "Doe"), Age = c(25, 30))
#' new_data <- list(Designation = "Manager")
#' df <- bind_new_data(df, new_data)
#'
#' @export
bind_new_data <- function(df, named_list) {

  # Check if column name already exists in the dataframe
  if(any(names(named_list) %in% colnames(df))) {
    duplicate_column_names <- named_list[names(named_list) %in% colnames(df)]
    stop_formatting_error(
      "Column name from the new data already exists in the dataframe.", 
      format_error(
        duplicate_column_names,
        "Cannot have duplicated columns when binding new data",
        "Duplicate found"
      )
    )
  }
  
  # Repeat the value of the named list to match the length of the dataframe
  repeated_values <- lapply(named_list, function(x) { return(rep(x, nrow(df))) })
  
  # Convert named list to dataframe column and append
  df_new_column <- as.data.frame(repeated_values)
  df <- cbind(df, df_new_column)
  
  return(df)
}


#' Read and Preprocess User CSV File
#'
#' This function reads the provided user CSV file and then preprocesses it based on the given user action.
#'
#' @param user_file The path to the user's CSV file. This will be read into a data frame.
#'
#' @return A preprocessed user CSV as a data frame.
#'
#' @seealso
#' \code{\link{read_user_csv}}, \code{\link{preprocess_csv}}
#' @export
#' @examples
#' \dontrun{
#' user_file_path <- "path/to/user/file.csv"
#' action <- "upload"
#'
#' preprocessed_data <- read_and_preprocess_csv(user_file_path, action)
#' }
read_and_preprocess_csv <- function(user_csv) {
  user_data <- read_user_csv(user_csv)
  user_data <- preprocess_csv(user_data)

  return(user_data)
}

#' Modify the user column for validation purposes
#' 
#' @param user_data The users uploaded and formatted data.
#' @param dimensions Dimensions of the matrix to check against.
#' @param expected_position_column Expected position column names(s)
#' @param position_col The name of the concatenated position to be used with validation
#' 
#' @return The users updated data frame.
#' @importFrom stringr str_pad
#' @import dplyr
#' @keywords internal validation
prepare_matrix_position_column <- function(user_data, dimensions, expected_position_column, position_col) {

  if (is.null(expected_position_column)) {
    stop("Parameter `expected_position_column` is NULL")
  }

  position_columns <- NULL
  if (is.list(expected_position_column)) {
    position_columns <- unlist(expected_position_column)
  } else if (is.character(expected_position_column)) {
    position_columns <- expected_position_column
  } else {
    stop("Cannot handle `expected_position_column`")
  }

  if (!is.null(position_columns) && length(position_columns) == 2) {

    ROW_IDX <- 1
    COL_IDX <- 2

    rows <- user_data[[position_columns[ROW_IDX]]]
    cols <- as.integer(user_data[[position_columns[COL_IDX]]])

    # Conduct grandular checks of the position coordinates here
    # There are position checks for row and columns
    
    invalid_rows <- unique(rows[!rows %in% LETTERS[1:dimensions[ROW_IDX]]]) 
    if (is.null(invalid_rows) || !purrr::is_empty(invalid_rows)) {
      stop_formatting_error(
        sprintf("Invalid row coordinates detected"),
        format_error(
          position_columns[ROW_IDX],
          sprintf("Rows should always be letters [%s-%s]", LETTERS[1], LETTERS[dimensions[ROW_IDX]]),
          paste0("Invalid entries found: ", paste(invalid_rows, collapse = ", "))
        )
      )
    }

    invalid_cols <- unique(cols[!cols %in% 1:dimensions[COL_IDX]])
    if (is.null(invalid_cols) || !purrr::is_empty(invalid_cols)) {
      stop_formatting_error(
        sprintf("Invalid rows detected. Rows should always be integers [%d-%d]", 1, dimensions[COL_IDX]),
        format_error(
          position_columns[COL_IDX],
          sprintf("Columns should always be integers [%s-%s]", 1, dimensions[COL_IDX]),
          paste0("Invalid entries found: ", paste(invalid_cols, collapse = ", "))
        )
      )
    }

    user_data[[position_col]] <- paste0(
      rows, 
      str_pad(cols, width = 2, pad = "0")
    )
      
    user_data <- user_data[, !(names(user_data) %in% position_columns)]
  } 
  # Check for a single position column and its format
  else if (!is.null(position_columns) && length(position_columns) == 1) {
    incorrect_format <- which(!grepl("^[A-Z][0-9]{2}$", user_data[[position_col]]))
    if (length(incorrect_format) > 0) {
      stop_formatting_error(
        sprintf("Incorrectly formatted positions detected in column %s", position_columns),
        format_error(
          position_columns,
          "Expected format: A01",
          paste0("Incorrect entries found at row(s): ", paste(incorrect_format, collapse = ", "))
        )
      )
    }
  }

  return(user_data)
}

#' Convert Density Representations to Real Numbers
#'
#' This function processes a dataframe to convert density values that might be in various 
#' representations (e.g., "10K", "10k") into their real number equivalents. 
#' For instance, "10K" or "10k" will be converted to 10000.
#'
#' If the density value is "neg" or "unk", it will be converted to 0 and -1, respectively.
#'
#' @param user_data A dataframe containing user data with density values.
#' @param density_col A string specifying the column name of the density values in `user_data`.
#' 
#' @return A dataframe with the converted density values. Special values are 0 ('Negative Control') and -1 ('Value was not known'). All other values are returned as numerics. In cases where 'K' or 'k' is appended, the numeric value in front is multiplied by 1000. 
#'
#' @examples
#' user_df <- data.frame(Density = c("1", "10k", "100K"))
#' convert_density_representations(user_df, "Density")
#'
#' @export
convert_density_representations <- function(user_data, density_col) {
  user_data[[density_col]] <- sapply(user_data[[density_col]], function(x) {
    # Capture both 19K and 1.9K
    if (grepl("^[0-9]+\\.?[0-9]*k$", ignore.case = TRUE, x)) {
      as.numeric(sub("k$", "", ignore.case = TRUE, x)) * 1000
    } else if (grepl("^neg$", ignore.case = TRUE, x)) {
      as.numeric(0.0)
    # if unknown, return (-1). This will be a special value to mean that the data could not be found
    } else if (grepl("^unk$", ignore.case = TRUE, x)) {
      as.numeric(-1.0)
    } else {
      as.numeric(x)
    }
  })
  
  return(user_data)
}


#' Prepare DataFrame for Specimen Validation
#'
#' This function pre-processes the input data frame based on given column mappings.
#' It renames columns and also applies column-specific transformations where needed.
#'
#' @param user_data A data frame with columns to be renamed.
#' @param file_type Parameter needed for internal function. (NOTE: should make this information available in a different way since we need it for file_column_attr)
#' @param file_column_attr A ColumnData object containing the column mappings.
#'
#' @return A pre-processed data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' user_data <- data.frame(
#'   StudyCode = c(1,2),
#'   StudySubject = c('X', 'Y'),
#'   Row = c(1, 2),
#'   Column = c('A', 'A')
#' )
#'
#' # Assuming file_column_attr is an instantiated ColumnData object
#'
#' new_df <- prepare_specimen_data_for_validation(user_data, file_column_attr)
#' }
prepare_specimen_data_for_validation <- function(sample_type, user_data, file_type, file_column_attr) {
  # Pre-processing the user data up front for the following reasons:
  # 1. The transformed data aids in various validation functions.
  # 2. Transformed data is directly copied to the SQL database for validation.
  # 
  # By transforming data up front, we reduce the need for in-memory table loading 
  # and leverage SQL for efficient validations. There also limitations using 
  # dplyr for database manipulation and querying, so doing these modification upfront
  # makes it easier to prepare the data frame for validation.

  # Check if file_column_attr is of class ColumnData
  if (!inherits(file_column_attr, "ColumnData")) {
    stop("file_column_attr must be an object of class 'ColumnData'")
  }

  # 1. Check for missing data in required positions

  ## In cases where the CSV file was generated by certain software platforms (ie. traxcer),
  ## empty barcodes show be allowed and simply filtered out
  if (sample_type == "micronix" && file_type == "traxcer") {
    message(sprintf("Removed %d rows with no barcode entries.", sum(is.na(user_data$`Tube ID`))))
    user_data <- user_data[!is.na(user_data$`Tube ID`), ]
  }

  error <- check_missing_data(user_data, file_column_attr)

  if (!is.null(error)) {
    stop_validation_error("There are missing data in required fields.", error)
  }

  # Extract column names from file_column_attr
  required_columns <- file_column_attr$required
  conditional_columns <- file_column_attr$conditional
  optional_columns <- file_column_attr$optional

  # Combine all expected column names
  all_expected_columns <- c(required_columns, conditional_columns, optional_columns)

  # Identify columns that are missing in user_data
  missing_columns <- setdiff(all_expected_columns, colnames(user_data))

  # Add missing columns to user_data and fill with NA
  for (col_name in missing_columns) {
    user_data[[col_name]] <- NA
  }

  # 2. Check for duplicated rows
  error <- check_duplicated_rows(user_data)

  if (!is.null(error)) {
    stop_validation_error("There are missing data in required fields.", error)
  }

  expected_position_column <- get_position_column_by_sample(sample_type, file_type)

  dimensions <- c(ifelse(sample_type == "micronix", 8, 10), ifelse(sample_type == "micronix", 12, 10))
  user_data <- prepare_matrix_position_column(user_data, dimensions, expected_position_column, "Position")
  user_data <- add_row_numbers(user_data)

  return(user_data)
}

#' Prepare DataFrame for Control Validation
#'
#' This function pre-processes the input data frame based on given column mappings.
#' It renames columns and also applies column-specific transformations where needed.
#'
#' @param user_data A data frame with columns to be renamed.
#' @param file_column_attr A ColumnData object containing the column mappings.
#'
#' @return A pre-processed data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' user_data <- data.frame(
#'   StudyCode = c(1,2),
#'   StudySubject = c('X', 'Y'),
#'   Row = c(1, 2),
#'   Column = c('A', 'A')
#' )
#'
#' # Assuming file_column_attr is an instantiated ColumnData object
#'
#' new_df <- prepare_control_data_for_validation(user_data, file_column_attr)
#' }
prepare_control_data_for_validation <- function(control_type, user_data, action, file_column_attr) {

  # Pre-processing the user data up front for the following reasons:
  # 1. The transformed data aids in various validation functions.
  # 2. Transformed data is directly copied to the SQL database for validation.
  # 
  # By transforming data up front, we reduce the need for in-memory table loading 
  # and leverage SQL for efficient validations. There also limitations using 
  # dplyr for database manipulation and querying, so doing these modification upfront
  # makes it easier to prepare the data frame for validation.

  # Check if file_column_attr is of class ColumnData
  if (!inherits(file_column_attr, "ColumnData")) {
    stop("file_column_attr must be an object of class 'ColumnData'")
  }

  # 1. Check for missing data in required positions
  error <- check_missing_data(user_data, file_column_attr)
  if (!is.null(error)) {
    stop_validation_error("There are missing data in required fields.", error)
  }

  # 2. Check for duplicated rows
  error <- check_duplicated_rows(user_data)
  if (!is.null(error)) {
    stop_validation_error("There are missing data in required fields.", error)
  }

  # get the destination container positions

  ## JSON UPDATE
  ## update this after json modification
  storage_container <- get_storage_container_by_control(control_type)

  # If we have whole blood, then prepare the position column
  if (control_type == "whole_blood") {
    dimensions <- c(9, 9)  # creations go into cryovials

    # If there are position values, prepare the column that will be used by the database (`ControlOriginPosition`)
    if (is.list(storage_container$position_keys) && !is.null(storage_container$position_keys[[1]])) {
      position_columns <- storage_container$position_keys
      user_data <- prepare_matrix_position_column(user_data, dimensions, position_columns, "ControlOriginPosition")
    }
  }
  
  # NOTE: If these checks grow, then make into functions
  if (action == "extraction") {
    destination_container <- get_destination_container_by_control(control_type)
    position_columns <- destination_container$position_keys
    dimensions <- c(8, 12)  # Extractions go into micronix
    user_data <- prepare_matrix_position_column(user_data, dimensions, position_columns, "ExtractedDNAPosition")

    # TODO: make this a function and add to the validation checks
    # TODO: make the data external and customizable (preferences?)
    user_data[["SpecimenType"]] <- ifelse(control_type == "whole_blood", "DNA (WB)", "DNA (DBS)")

  } else if (action == "create") {
    # Convert Density Representations to Real numbers
    user_data <- convert_density_representations(user_data, "Density")
  } else {
    stop("Invalid action!")
  }

  # Extract the label, legacy, and index from the composition ID provided by the user
  user_data <- normalize_composition_ids(user_data, "CompositionID", "Label", "Index", "LegacyLabel")
  user_data <- add_row_numbers(user_data)

  # Add conditional and optional columns with NA as default values if they do not exist
  conditional_columns <- file_column_attr$conditional
  optional_columns <- file_column_attr$optional
  
  for (col in conditional_columns) {
    if (!col %in% colnames(user_data)) {
      user_data[[col]] <- NA
    }
  }
  
  for (col in optional_columns) {
    if (!col %in% colnames(user_data)) {
      user_data[[col]] <- NA
    }
  }

  return(user_data)
}

#' Normalize Composition IDs from User Data
#'
#' This function processes a dataframe to extract and split the composition IDs
#' into separate columns (Label, Index, and LegacyLabel) based on their format.
#'
#' @param user_data A dataframe containing user data with composition IDs.
#' @param in_composition_id_col A string specifying the column name of the composition IDs in `user_data`.
#' @param out_label_col A string specifying the name of the new column where the labels will be stored. Default is "Label".
#' @param out_index_col A string specifying the name of the new column where the indexes will be stored. Default is "Index".
#' @param out_legacy_col A string specifying the name of the new column where the legacy status (TRUE or FALSE) will be stored. Default is "LegacyLabel".
#'
#' @return A dataframe with new columns (specified by label_col, index_col, and legacy_col)
#' containing the split composition ID components.
#'
#' @export
#' @importFrom purrr map map_chr map_dbl map_lgl

normalize_composition_ids <- function(user_data, in_composition_id_col, out_label_col, out_index_col, out_legacy_col) {

  if (in_composition_id_col %in% colnames(user_data)) {
    composition_data <- purrr::map(user_data[[in_composition_id_col]], split_composition_id)
    
    user_data[[out_label_col]] <- purrr::map_chr(composition_data, "label")
    user_data[[out_index_col]] <- purrr::map_dbl(composition_data, "index")
    user_data[[out_legacy_col]] <- purrr::map_lgl(composition_data, "legacy")
  }

  return(user_data)
}

#' Denormalize Composition IDs from User Data
#'
#' This function processes a dataframe to combine the separate columns (Label, Index, and LegacyLabel)
#' back into a single composition ID column based on their format.
#'
#' @param user_data A dataframe containing user data with split composition IDs.
#' @param in_label_col A string specifying the column name of the labels in `user_data`. Default is "Label".
#' @param in_index_col A string specifying the column name of the indexes in `user_data`. Default is "Index".
#' @param in_legacy_col A string specifying the column name indicating if the composition ID is legacy in `user_data`. Default is "LegacyLabel".
#' @param out_composition_id_col A string specifying the name of the new column where the combined composition IDs will be stored.
#'
#' @return A dataframe with a new column (specified by composition_id_col) containing the combined composition ID.
#'
#' @export
denormalize_composition_ids <- function(user_data, in_label_col, in_index_col, in_legacy_col, out_composition_id_col) {
  
  user_data[[out_composition_id_col]] <- ifelse(
    user_data[[in_legacy_col]], 
    user_data[[in_label_col]], 
    paste0(user_data[[in_label_col]], "_", user_data[[in_index_col]])
  )

  return(user_data)
}

#' Prepare DataFrame for Reference Validation
#'
#' @export
prepare_reference_data_for_validation <- function(user_data, reference_type, file_column_attr) {

  # Check if file_column_attr is of class ColumnData
  if (!inherits(file_column_attr, "ColumnData")) {
    stop("file_column_attr must be an object of class 'ColumnData'")
  }

  # 1. Check for missing data in required positions
  error <- check_missing_data(user_data, file_column_attr)
  if (!is.null(error)) {
    stop_validation_error("There are missing data in required fields.", error)
  }

  # 2. Check for duplicated rows
  error <- check_duplicated_rows(user_data)
  if (!is.null(error)) {
    stop_validation_error("There are missing data in required fields.", error)
  }

  # Assuming rename_columns renames based on given mapping
  user_data <- add_row_numbers(user_data)

  # Unravel the data
  if (reference_type %in% c("compositions")) {

    error <- check_strain_percentage_match(user_data, "Percentages", "Strains")
    if (!is.null(error)) {
      stop_validation_error("Percentages and strains found with different lengths.", error)
    }

    user_data <- split_and_unnest_columns(user_data, "Strains", "Percentages", append = "Long")
  }

  # make sure strains are capitalized
  if (reference_type %in% c("strains")) {
    user_data[["Strains"]] <- toupper(user_data[["Strains"]])
  }

  return(user_data)
}


#' Validate that the strain and percentage columns have matching semicolon counts
#'
#' @param data The users data.
#' @param percentage_col The name of the column containing percentages.
#' @param strain_col The name of the column containing strains.
#'
#' @keywords pre-validation strains compositions
#' @return NULL or ErrorData.
check_strain_percentage_match <- function(data, percentage_col, strain_col) {
  
  # Find discrepancies in number of splits
  discrepancies <- sapply(1:nrow(data), function(i) {
    length(unlist(strsplit(as.character(data[[strain_col]][i]), ";"))) != length(unlist(strsplit(as.character(data[[percentage_col]][i]), ";")))
  })

  if (any(discrepancies)) {
    error_rows <- which(discrepancies)
    error_data <- ErrorData$new(
      description = "Mismatched count of semicolons between strain and percentage columns",
      columns = c(strain_col, percentage_col),
      rows = error_rows
    )
    return(list(error = error_data))
  }

  return(NULL)
}


#' Split and Unnest Strain and Percentage Columns
#'
#' This function takes in a data frame and splits the provided strain and percentage columns by a semicolon (;). 
#' The resulting two lists within each row are then unnested into separate rows. This is commonly used for 
#' validating strains and their corresponding compositions.
#'
#' @param user_data A data frame with the specified strain and percentage columns.
#' @param strains_col The column name in the data frame containing strain data.
#' @param percentage_col The column name in the data frame containing percentage data.
#'
#' @return A data frame with unnested strain and percentage columns.
#' 
#' @examples
#' \dontrun{
#' result <- split_and_unnest_columns(user_data, "strain_column_name", "percentage_column_name")
#' }
#' 
#' @export
#' @keywords pre-validation strains compositions
split_and_unnest_columns <- function(user_data, strains_col, percentage_col, append = NULL) {
    df <- user_data %>%
      dplyr::mutate(
          strain = strsplit(!!sym(strains_col), ";"),
          percentage = strsplit(!!sym(percentage_col), ";")
      ) %>%
      tidyr::unnest(cols = c("strain", "percentage")) %>%
      dplyr::mutate(percentage = as.double(percentage)) %>%
      dplyr::rename_with(
          ~ if_else(. == "strain", paste0(strains_col, append), 
                    if_else(. == "percentage", paste0(percentage_col, append), .)), 
          .cols = c("strain", "percentage")
      )
      
    return(df)
}

#' Collapse and Nest Strain and Percentage Columns
#'
#' This function takes in a data frame and collapses the provided strain and percentage columns using a semicolon (;). 
#' The individual strains and percentages in each row are combined into single, semicolon-separated strings. 
#' This is commonly used after manipulating or analyzing strains and their corresponding compositions.
#'
#' @param user_data A data frame with the specified strain and percentage columns.
#' @param strains_col The column name in the data frame containing strain data.
#' @param percentage_col The column name in the data frame containing percentage data.
#'
#' @return A data frame with nested strain and percentage columns.
#' 
#' @examples
#' \dontrun{
#' result <- collapse_and_nest_columns(df, "strain_column_name", "percentage_column_name")
#' }
#' 
#' @export
#' @keywords post-processing strains compositions
collapse_and_nest_columns <- function(user_data, row_number_col, strains_col, percentage_col) {
    df <- user_data %>%
      group_by(!!sym(row_number_col)) %>%
      dplyr::mutate(
          strain = paste(!!sym(strains_col), collapse = ";"),
          percentage = paste(!!sym(percentage_col), collapse = ";")
      ) %>%
      select(-c(!!sym(strains_col), !!sym(percentage_col))) %>%
      dplyr::rename_with(~ if_else(. == "strain", strains_col, if_else(. == "percentage", percentage_col, .))) %>%
      ungroup()

    return(df)
}


# Helper function to split composition IDs
split_composition_id <- function(composition_id) {
  if (grepl("^S\\d+_\\d+$", composition_id)) {
    split_string <- strsplit(composition_id, "_")[[1]]
    index <- as.numeric(split_string[2])
    label <- split_string[1]
    legacy <- FALSE
  } else {
    index <- NA_integer_
    label <- composition_id
    legacy <- TRUE
  }
  
  list(label = label, index = index, legacy = legacy)
}
