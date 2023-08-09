#' Check for Control Existence
#'
#' This function checks for unmatched rows based on the control IDs 
#' in a specified table when compared to the `study_subject` table. 
#' It returns an error data if there are unmatched rows.
#' 
#' @param con A database connection.
#' @param table_name The name of the table to check.
#' @param row_number_col The name of the row number column.
#' @param control_col The name of the control column.
#' @param batch_col The name of the batch column.
#' @param error_if_exists If TRUE, returns an error if the control ID exists. If FALSE, 
#'   returns an error if the control ID does not exist.
#' 
#' @return NULL if there is no error, otherwise an ErrorData object with 
#'   appropriate description and data frame.

check_control_exists <- function(con, table_name, row_number_col, control_col, batch_col, error_if_exists = FALSE) {
  
  df <- tbl(con, table_name) %>%
    left_join(tbl(con, "study_subject") %>% 
              dplyr::rename(control_id = id), by = control_col) %>%
    group_by(all_of(c(batch_col, control_col)))
  
  if (error_if_exists) {
    df <- df %>%
      filter(!is.na(control_id)) %>%
      ungroup() %>%
      select(all_of(c(row_number_col, control_col, batch_col))) %>%
      collect()
    
    if (nrow(df) > 0) {
      return(ErrorData$new(description = "Control IDs already exist in the database.", data_frame = df))
    }
    
  } else {
    df <- df %>%
      filter(is.na(control_id)) %>%
      ungroup() %>%
      select(all_of(c(row_number_col, control_col, batch_col))) %>%
      collect()
    
    if (nrow(df) > 0) {
      return(ErrorData$new(description = "Control IDs are not found in the database.", data_frame = df))
    }
  }
  
  return(NULL)
}



check_composition_id_exists <- function(con, table_name, row_number_col, composition_id_col, error_if_exists = FALSE) {
  
  # Find rows where the composition_id doesn't exist in the composition table
  df <- tbl(con, table_name) %>%
    anti_join(tbl(con, "composition"), by = setNames("label", composition_id_col)) %>%
    select(all_of(c(row_number_col, composition_id_col))) %>%
    collect()
  
  # If any rows are found and error_if_exists is TRUE, throw an error
  if (error_if_exists && nrow(df) > 0) {
    stop("Composition IDs are not found in the database.")
  }

  # If any rows are found, return an ErrorData object
  if (nrow(df) > 0) {
    return(ErrorData$new(description = "Composition IDs are not found in the database.", data_frame = df))
  }
  
  return(NULL)
}

#' Validate DBS Sheet Control Data
#'
#' This function conducts a series of validation checks on DBS sheet control data.
#' The function will return any errors encountered during validation.
#'
#' @param user_data A data frame containing control data to validate.
#' @param action The action being performed, either "create" or "extraction".
#' @param database The database connection or specification to use for validation.
#' @return A list containing validation errors, if any.
#' @keywords validation, dbs_sheet
validate_dbs_sheet <- function(user_data, action, database) {
  perform_dbs_sheet_db_validations(database, user_data, action)
}

#' Perform Database-Related Validations for DBS Sheets
#'
#' This function conducts database-related validation checks for DBS sheet control data.
#' It initiates a database connection, then performs create or extraction validations based on the action provided.
#'
#' @param database The database connection or specification for validation.
#' @param user_data The users data.
#' @param action The action being performed, either "create" or "extraction".
#' @return A list containing validation errors, if any.
#' @keywords validation, dbs_sheet
perform_dbs_sheet_db_validations <- function(database, user_data, action) {
  con <- init_and_copy_to_db(database, user_data)
  on.exit(dbDisconnect(con), add = TRUE)
  errors <- list()

  # Utility function to simplify repetitive operations
  dbs_sheet_test <- function(validation_func, ...) {
    func_name <- deparse(substitute(validation_func))
    cat("Executing function:", func_name, "\n")
    result <- do.call(validation_func, list(con, "user_data", "RowNumber", ...))
    errors <<- add_to_errors(errors, result)
  }

  if (action == "create") {
    validate_dbs_sheet_create(dbs_sheet_test)
  } else if (action == "extraction") {
    validate_dbs_sheet_extraction(dbs_sheet_test)
  }

  return(errors)
}

#' Validate DBS Sheet Create
#'
#' Conducts specific validation checks for creating DBS sheet controls.
#'
#' @param dbs_sheet_test The utility function for performing validation checks.
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_dbs_sheet_create <- function(dbs_sheet_test) {
  # References check
  dbs_sheet_test(check_composition_id_exists, "CompositionID", error_if_exists = FALSE)
  dbs_sheet_test(validate_study_reference, "Batch")
  dbs_sheet_test(validate_location_reference, "Minus20Freezer", "ShelfName", "BasketName")
}

#' Validate DBS Sheet Extraction
#'
#' Conducts specific validation checks for extracting DBS sheet controls.
#'
#' @param dbs_sheet_test The utility function for performing validation checks.
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_dbs_sheet_extraction <- function(dbs_sheet_test) {
  dbs_sheet_test(check_micronix_barcodes_exist, "Barcode", error_if_exists = TRUE)
  dbs_sheet_test(validate_empty_micronix_well_upload, "destination_position", "PlateName", "PlateBarcode")

  # References check
  dbs_sheet_test(check_control_exists, "ControlID", "Batch", error_if_exists = FALSE)
  dbs_sheet_test(validate_study_reference, "Batch")

  # Validate source location
  dbs_sheet_test(validate_location_reference, "Minus20Freezer", "RackName", "RackPosition")

  # Validate destination location
  dbs_sheet_test(validate_location_reference, "Minus20Freezer", "ShelfName", "BasketName")
}

#' Validate Whole Blood Control Data
#'
#' This function conducts a series of validation checks on whole blood control data.
#' The function will return any errors encountered during validation.
#'
#' @param user_data A data frame containing control data to validate.
#' @param action The action being performed, either "create" or "extraction".
#' @param database The database connection or specification to use for validation.
#' @return A list containing validation errors, if any.
#' @keywords validation, whole_blood
validate_whole_blood <- function(user_data, action, database) {
  perform_whole_blood_db_validations(database, user_data, action)
}

#' Perform Database-Related Validations for Whole Blood Controls
#'
#' This function conducts database-related validation checks for whole blood control data.
#' It initiates a database connection, then performs create or extraction validations based on the action provided.
#'
#' @param database The database connection or specification for validation.
#' @param user_data The users data.
#' @param action The action being performed, either "create" or "extraction".
#' @return A list containing validation errors, if any.
#' @keywords validation, whole_blood
perform_whole_blood_db_validations <- function(database, user_data, action) {
  con <- init_and_copy_to_db(database, user_data)
  on.exit(dbDisconnect(con), add = TRUE)
  errors <- list()

  # Utility function to simplify repetitive operations
  whole_blood_test <- function(validation_func, ...) {
    func_name <- deparse(substitute(validation_func))
    cat("Executing function:", func_name, "\n")
    result <- do.call(validation_func, list(con, "user_data", "RowNumber", ...))
    errors <<- add_to_errors(errors, result)
  }

  if (action == "create") {
    validate_whole_blood_create(whole_blood_test)
  } else if (action == "extraction") {
    validate_whole_blood_extraction(whole_blood_test)
  }

  return(errors)
}

#' Validate Whole Blood Create
#'
#' Conducts specific validation checks for creating whole blood controls.
#'
#' @param whole_blood_test The utility function for performing validation checks.
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_whole_blood_create <- function(whole_blood_test) {

  # Whole blood is stored in cryovials so reuse cryovial tests
  whole_blood_test(check_cryovial_barcodes_exist, "Barcode", error_if_exists = TRUE)
  whole_blood_test(validate_empty_cryovial_well_upload, "destination_position", "BoxName", "BoxBarcode")

  # References check
  whole_blood_test(check_composition_id_exists, "CompositionID")
  whole_blood_test(validate_study_reference, "Batch")
  whole_blood_test(validate_location_reference, "Minus80Freezer", "RackName", "RackPosition")
}

#' Validate Whole Blood Extraction
#'
#' Conducts specific validation checks for extracting whole blood controls.
#'
#' @param whole_blood_test The utility function for performing validation checks.
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_whole_blood_extraction <- function(whole_blood_test) {

  # Add your validation function calls here
  whole_blood_test(check_micronix_barcodes_exist, "Barcode", error_if_exists = TRUE)
  whole_blood_test(validate_empty_micronix_well_upload, "destination_position", "PlateName", "PlateBarcode")
  whole_blood_test(validate_cryovial_tube_exists, "source_position", "BoxName", "BoxBarcode")

  # References check
  whole_blood_test(validate_study_reference, "Batch")
  whole_blood_test(check_control_exists, "ControlID", "Batch", error_if_exists = TRUE)

  # Validate source location
  whole_blood_test(validate_location_reference, "Minus80Freezer", "RackName", "RackPosition")

  # Validate destination location
  whole_blood_test(validate_location_reference, "Minus20Freezer", "ShelfName", "BasketName")

}

#' Validate Control Specimens
#'
#' @param database A database connection or reference.
#' @param formatted_csv The CSV data formatted for validation.
#' @param control_type The type of control specimen ('dbs_sheet' or 'whole_blood').
#' @param user_action The user action taken on the control specimen ('extraction' or 'create').
#' 
#' @return Returns the validated user data if no errors are found; stops with validation errors otherwise.
validate_controls <- function(database, user_data, control_type, user_action) {

  errors <- list()

  if (control_type == "dbs_sheet") {
    errors <- validate_dbs_sheet(user_data, user_action, database)
  } else if (control_type == "whole_blood") {
    errors <- validate_whole_blood(user_data, user_action, database)
  }

  if (user_action == "created") {
    validation_result <- validate_dates(user_data, "Created", "%Y-%m-%d")

    if (!is.null(validation_result)) {
      errors <- add_to_errors(errors, validation_result)
    }
  }

  if (length(errors) > 0) {
    stop_validation_error("Validation error", errors)
  }

  return(user_data)
}