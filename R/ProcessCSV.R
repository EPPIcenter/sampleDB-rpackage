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
#' @param file_column_attr A colum attribute object containing required columns.
#' @return A data frame with the appropriate header row set.
#' @export
set_user_file_header <- function(user_file, file_column_attr) {

  header_row <- find_header(user_file, file_column_attr$required, valid_header_rows = 1:2)
  if (is.null(header_row)) {
    stop_formatting_error("Could not find required columns", format_error(file_column_attr$required))
  }

  return(set_header_row(user_file, header_row))
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
  
  # Retrieve 'required_columns' from 'specimen_column_attributes'.
  required_columns <- specimen_column_attributes$get_required_colnames()
  
  # Identify any columns from 'required_columns' that are not present in 'specimen_file'.
  missing_columns <- setdiff(required_columns, colnames(specimen_file))
  
  # Check for StudyCode in specimen_file and if 'CollectionDate' is part of the specimen column attributes.
  if (!"StudyCode" %in% colnames(specimen_file) && "CollectionDate" %in% specimen_column_attributes$conditional) {
    
    # Retrieve relevant records from the study table based on StudyCode.
    matched_studies <- get_matched_studies(specimen_file, database)
    
    # Conditionally verify the CollectionDate column based on the nature of longitudinal specimen studies.
    if (nrow(filter(matched_studies, is_longitudinal == 1)) > 0) {
      stop_formatting_error("Specimens from longitudinal studies must include a CollectionDate column.", format_error("CollectionDate"))
    }
  }

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
detect_missing_control_columns <- function(control_file, control_column_attributes) {
  required_columns <- control_column_attributes$get_required_colnames()
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
  matched_studies <- data.frame() # Initialize as empty dataframe
  error <- FALSE
  
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), database = database)
    matched_studies <- DBI::dbReadTable(con, "study") %>%
      filter(study_short_code %in% user_file$StudyCode) %>%
      inner_join(user_file, by = c("short_code" = "StudyCode"))
    matched_studies <- DBI::dbReadTable(con, "study")
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
#' @param file_column_attr An instance of FileColumnAttributes containing attributes of file columns.
#' @return A data frame with only the relevant columns selected.
#' @export
select_relevant_columns <- function(user_file, file_column_attr) {

  # Note: For now, use the values and not denormalized names.
  user_file <- user_file %>%
    select(
      all_of(file_column_attr$get_required_colnames()),
      all_of(file_column_attr$get_location_colnames()),
      all_of(file_column_attr$get_container_colnames()),
      any_of(file_column_attr$get_conditional_colnames()),
      any_of(file_column_attr$get_optional_colnames())
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
#' @param file_column_attr The file column attributes.
#' @param container_name The name of the container.
#' @param freezer_address The address of the freezer.
#' @return A cleaned and checked user_file ready for further processing.
#' @export
validate_and_format_specimen_file <- function(user_file, sample_type, user_action, file_column_attr, container_name = NULL, freezer_address = NULL) {

  # 2. Handle header.
  user_file <- set_user_file_header(user_file, file_column_attr)

  # 3. Handle special columns. These are columns that should be included but may be added by the user.
  user_file <- handle_special_columns(user_file, container_name, freezer_address, file_column_attr$location)

  # 4. Detect missing columns.
  missing_columns <- detect_missing_specimen_columns(user_file, file_column_attr)

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

#' Check if the user CSV file for control meets the requirements
#'
#' This function checks if the user CSV file for control meets specific upload requirements,
#' including necessary columns and formatting checks. Errors will be thrown if requirements are not met.
#'
#' @param user_file The user's CSV file as a data frame.
#' @param file_column_attr A list containing attributes of file columns such as required, conditional, and optional columns.
#' @param container_name The name of the container (optional).
#' @param freezer_address The address of the freezer (optional).
#' @return A cleaned and checked user_file ready for further processing.
#' @export
validate_and_format_control_file <- function(user_file, file_column_attr, container_name = NULL, freezer_address = NULL) {

  # 2. Handle header.
  user_file <- set_user_file_header(user_file, file_column_attr)

  # 3. Detect missing columns.
  missing_columns <- detect_missing_control_columns(user_file, file_column_attr)

  # If there are any missing columns still, throw!
  if (length(missing_columns) > 0) {
    stop_formatting_error("Missing required columns", format_error(missing_columns))
  }

  # 4. Select relevant columns.
  user_file <- select_relevant_columns(user_file, file_column_attr)

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
#' @param container_name (Optional) A character string specifying the name of the specimen container. If provided and not present in the CSV, it will be added to each row.
#' @param freezer_address (Optional) A list of attributes for the specimen freezer. If provided and any attributes are missing from the CSV, they will be added to each row.
#' @param file_type A character string indicating the type of file (default is "na").
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
process_specimen_csv <- function(user_csv, user_action, sample_type, container_name = NULL, freezer_address = NULL, file_type = "na", database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG")) {
  if (is.null(user_csv) || user_csv == "") {
    stop("No csv file was provided.")
  }

  # Read and preprocess user CSV file
  # Steps:
  # 1. Read the CSV file.
  # 2. Preprocess to remove empty rows or columns.
  # 3. Prepare for validation by checking for empty data points, renaming columns and adding position column if necessary.
  # 4. Validate and format based on requirements.
  
  user_data <- read_and_preprocess_csv(user_csv, user_action)

  file_column_attr <- get_sample_file_columns(sample_type, user_action, file_type, config_yml)
  user_data <- validate_and_format_specimen_file(user_data, user_action, sample_type, file_column_attr, container_name, freezer_address)

  user_data <- prepare_specimen_data_for_validation(user_data, file_column_attr)
  user_data <- validate_specimens(user_data, sample_type, user_action, database)
  
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
#' @param container_name (Optional) A character string specifying the name of the container.
#'        If provided and not present in the CSV, it will be added to each row.
#' @param freezer_address (Optional) A list of attributes for the freezer.
#'        If provided and any attributes are missing from the CSV, they will be added to each row.
#' @param file_type A character string indicating the type of file (default is "na").
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
process_control_csv <- function(user_csv, user_action, control_type, container_name = NULL, freezer_address = NULL, file_type = "na", database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG")) {
  
  # Ensure a valid CSV is provided
  if (is.null(user_csv) || user_csv == "") {
    stop("No csv file was provided.")
  }

  # Read and preprocess user CSV file
  user_data <- read_and_preprocess_csv(user_csv, user_action)
  
  # Validate and format the user CSV data
  file_column_attr <- get_control_file_columns(control_type, user_action)
  user_data <- validate_and_format_control_file(user_data, file_column_attr, container_name, freezer_address)
  
  # Further validate controls
  user_data <- prepare_control_data_for_validation(user_data, file_column_attr)
  user_data <- validate_controls(database, user_data, control_type, user_action)

  return(user_data)
}

#' Read and Preprocess User CSV File
#' 
#' This function reads the provided user CSV file and then preprocesses it based on the given user action.
#' 
#' @param user_file The path to the user's CSV file. This will be read into a data frame.
#' @param user_action A character string indicating the action the user is taking (e.g., 'upload').
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
read_and_preprocess_csv <- function(user_csv, user_action) {
  user_data <- read_user_csv(user_csv)
  user_data <- preprocess_csv(user_data, user_action)

  return(user_data)
}


#' Prepare DataFrame for Specimen Validation
#'
#' This function pre-processes the input data frame based on given column mappings.
#' It renames columns and also applies column-specific transformations where needed.
#'
#' @param user_data A data frame with columns to be renamed.
#' @param file_column_attr A FileColumnAttributes object containing the column mappings.
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
#' # Assuming file_column_attr is an instantiated FileColumnAttributes object
#'
#' new_df <- prepare_specimen_data_for_validation(user_data, file_column_attr)
#' }
prepare_specimen_data_for_validation <- function(user_data, file_column_attr) {

  # Check if file_column_attr is of class FileColumnAttributes
  if (!inherits(file_column_attr, "FileColumnAttributes")) {
    stop("file_column_attr must be an object of class 'FileColumnAttributes'")
  }

  # 1. Check for missing data in required positions
  error <- check_missing_data(user_data, file_column_attr)

  if (!is.null(error)) {
    stop_validation_error("There are missing data in required fields.", error)
  }

  # Get column mappings using all_fields method
  column_mappings <- file_column_attr$all_fields()

  if ("position" %in% names(column_mappings)) {    

    position_columns <- unlist(column_mappings$position)

    row <- user_data[[position_columns[1]]]
    col <- as.integer(user_data[[position_columns[2]]]) # ensure that this is an integer

    user_data$Position <- sprintf("%s%02d", row, col)

    # Drop the original Row and Column columns
    user_data <- user_data[ , !(names(user_data) %in% position_columns)]
  }

  user_data <- add_row_numbers(user_data)

  return(user_data)
}

#' Prepare DataFrame for Control Validation
#'
#' This function pre-processes the input data frame based on given column mappings.
#' It renames columns and also applies column-specific transformations where needed.
#'
#' @param user_data A data frame with columns to be renamed.
#' @param file_column_attr A FileColumnAttributes object containing the column mappings.
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
#' # Assuming file_column_attr is an instantiated FileColumnAttributes object
#'
#' new_df <- prepare_control_data_for_validation(user_data, file_column_attr)
#' }
prepare_control_data_for_validation <- function(user_data, file_column_attr) {
  # Check if file_column_attr is of class FileColumnAttributes
  if (!inherits(file_column_attr, "FileColumnAttributes")) {
    stop("file_column_attr must be an object of class 'FileColumnAttributes'")
  }

  # 2. Apply position concatenation for source container

  if ("position" %in% names(file_column_attr$container$container_src)) {
    position_columns <- unlist(file_column_attr$container_src$position)
    row <- user_data[[position_columns[1]]]
    col <- user_data[[position_columns[2]]]
    user_data$SourcePosition <- sprintf("%s%02d", row, col)
    user_data <- user_data[, !(names(user_data) %in% position_columns)]
  }

  # 3. Apply position concatenation for destination container
  if ("position" %in% names(file_column_attr$container$container_dest)) {
    position_columns <- unlist(file_column_attr$container_dest$position)
    row <- user_data[[position_columns[1]]]
    col <- user_data[[position_columns[2]]]
    user_data$DestinationPosition <- sprintf("%s%02d", row, col)
    user_data <- user_data[, !(names(user_data) %in% position_columns)]
  }

  # 1. Check for missing data in required positions
  error <- check_missing_data(user_data, file_column_attr)
  if (!is.null(error)) {
    stop_validation_error("There are missing data in required fields.", error)
  }

  # Assuming rename_columns renames based on given mapping
  user_data <- add_row_numbers(user_data)

  return(user_data)
}
