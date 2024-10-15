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

  study_subject <- tbl(con, "study_subject") %>% dplyr::select(study_subject_id = id, study_id, ControlUID = name)
  batches <- tbl(con, "study") %>% dplyr::select(study_id = id, Batch = short_code)

  batch_uid_joined <- study_subject %>%
    inner_join(batches, by = join_by(study_id))

  df <- tbl(con, table_name) %>%
    left_join(batch_uid_joined, by = join_by(Batch, ControlUID))
  
  if (error_if_exists) {
    df <- df %>%
      filter(!is.na(study_subject_id)) %>%
      select(all_of(c(row_number_col, control_col, batch_col))) %>%
      collect()
    
    if (nrow(df) > 0) {
      return(ErrorData$new(description = "Control IDs already exist in the database.", data_frame = df))
    }
    
  } else {
    df <- df %>%
      filter(is.na(study_subject_id)) %>%
      select(all_of(c(row_number_col, control_col, batch_col))) %>%
      collect()
    
    if (nrow(df) > 0) {
      return(ErrorData$new(description = "Control IDs are not found in the database.", data_frame = df))
    }
  }
  
  return(NULL)
}


check_composition_id_exists <- function(con, table_name, row_number_col, label_col, index_col, legacy_col, error_if_exists = FALSE) { 

  # Remove NAs so that we can join
  na_removal <- setNames(
    list(0, 0),
    list(index_col, "index") # user file col, database col
  )

  # Composition join columns
  composition_joins = setNames(
    c("label", "index", "legacy"),
    c(label_col, index_col, legacy_col)
  )

  # Find rows where the label doesn't exist in the composition table
  df <- tbl(con, table_name) %>%
    tidyr::replace_na(na_removal) %>%
    left_join(tbl(con, "composition") %>% tidyr::replace_na(na_removal), by = composition_joins)
  
  # If any rows are found and error_if_exists is TRUE, throw an error
  if (error_if_exists) {
    df <- df %>% 
      filter(!is.na(id)) %>%
      select(row_number_col, "CompositionID") %>%
      collect()

    description <- "Composition IDs already exist in the database."
  } else {
    df <- df %>% 
      filter(is.na(id)) %>%
      select(row_number_col, "CompositionID") %>%
      collect()

    description <- "Composition IDs are not found in the database."
  }

  # If any rows are found, return an ErrorData object
  if (nrow(df) > 0) {
    return(ErrorData$new(description = description, data_frame = df))
  }
  
  return(NULL)
}

#' Validate Extraction Counts Against Blood Spot Collection Totals
#'
#' This function checks if the sum of new extractions for each blood spot collection
#' in the provided user data does not exceed the total count available for each collection.
#' It ensures that extractions are within the limits of available blood spots by dynamically
#' joining user data with related tables to identify the relevant blood_spot_collection_id for each record.
#'
#' @param con A database connection object.
#' @param user_data A dataframe containing the new extractions to be validated.
#'   It must contain columns that can be used to derive blood_spot_collection_id indirectly.
#' @param row_number_col The name of the column in `user_data` that provides a unique row identifier.
#' @param control_uid_col The name of the column in `user_data` that corresponds to the control UID.
#' @param batch_col The name of the column in `user_data` related to the batch information.
#' @param sheet_name_col The name of the column in `user_data` for the sheet name.
#' @param sheet_label_col The name of the column in `user_data` for the sheet label, which may contain NA values.
#'
#' @return An `ErrorData` object if any blood spot collection's new total extractions exceed its total counts,
#'   detailing the violations. Returns `NULL` if all new extraction counts are within the allowed limits.
#'
#' @import dplyr
#' @import DBI
#' @export
validate_extraction_counts_with_totals <- function(con, user_data, row_number_col, control_uid_col, batch_col, sheet_name_col, sheet_barcode_col) {
  
  # Prepare user_data with a join key combining sheet_name_col and sheet_barcode_col when both exist
  sql <- tbl(con, user_data) %>%
    mutate(join_key = ifelse(is.na(!!sym(sheet_barcode_col)), 
                             !!sym(sheet_name_col), 
                             paste(!!sym(sheet_name_col), !!sym(sheet_barcode_col), sep = "_")))

  # Join user_data with necessary tables to get blood_spot_collection_id
  # The specific joins and the conditions will depend on your database schema
  df_payload <- sql %>%
    inner_join(dplyr::tbl(con, "study") %>% dplyr::rename(study_id = id), by = setNames("short_code", batch_col)) %>%
    inner_join(dplyr::tbl(con, "study_subject") %>% dplyr::rename(study_subject_id = id, control_uid=name), by = setNames("control_uid", control_uid_col)) %>%
    inner_join(dplyr::tbl(con, "malaria_blood_control") %>% dplyr::rename(malaria_blood_control_id = id), by = "study_subject_id") %>%
    inner_join(dplyr::tbl(con, "blood_spot_collection") %>% dplyr::rename(blood_spot_collection_id = id), by = "malaria_blood_control_id") %>%
    select(row_number_col, blood_spot_collection_id, !!sym(row_number_col), !!sym(control_uid_col), !!sym(batch_col), !!sym(sheet_name_col)) %>%
    group_by(blood_spot_collection_id) %>%
    mutate(new_exhausted = n()) %>%
    ungroup()

  # Fetch current exhausted and total counts for the related blood_spot_collection_id(s)
  df_payload <- df_payload %>%
    dplyr::left_join(dplyr::tbl(con, "blood_spot_collection") %>%
                dplyr::rename(blood_spot_collection_id = id) %>%
                dplyr::select(blood_spot_collection_id, exhausted, total), by = "blood_spot_collection_id") %>%
    dplyr::mutate(new_total_exhausted = exhausted + new_exhausted) %>%
    dplyr::filter(new_total_exhausted > total) %>%
    collect()

  # Check if any collection exceeds the total
  if (nrow(df_payload) > 0) {

    error_rows <- df_payload %>%
      select(!!sym(row_number_col), !!sym(control_uid_col), !!sym(batch_col), !!sym(sheet_name_col))

    return(ErrorData$new(
      description = "Extraction counts exceed totals for some blood spot collections",
      data_frame = error_rows
    ))
  }

  # If no violations, return NULL to indicate validation passed
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
  browser()
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
  } else if (action == "move") {
    validate_dbs_sheet_move(dbs_sheet_test)
  } else {
    stop("No validation implemented for this action!!!")
  }

  return(errors)
}

#' Validate DBS Bag Label is unique at a location
#'
#' This function checks that the DBS bag label is unique at a location.
#'
#' @param con A database connection.
#' @param table_name The name of the table to check.
#' @param row_number_col The name of the row number column.
#' @param bag_label_col The name of the bag label column.
#' @param freezer_col The name of the freezer column.
#' @param shelf_col The name of the shelf column.
#' @param basket_col The name of the basket column.
#' @param error_if_exists If TRUE, returns an error if the bag label exists. If FALSE, 
#'   returns an error if the bag label does not exist.
#' 
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_dbs_bag_label_is_unique <- function(con, table_name, row_number_col, bag_label_col, freezer_col, shelf_col, basket_col, error_if_exists = FALSE) {
  
  dbs_bag_tbl <- tbl(con, "dbs_bag") %>% dplyr::rename(dbs_bag_id=id, bag_label=name)
  location_tbl <- tbl(con, "location") %>% dplyr::rename(location_id=id)
  
  # Join tables
  dbs_bag_location_tbl <- location_tbl %>%
    inner_join(dbs_bag_tbl, "location_id")

  # Columns to join user table and bag-location table
  dbs_bag_location_joins <- setNames(
    c("bag_label", "location_root", "level_I", "level_II"),
    c(bag_label_col, shelf_col, freezer_col, basket_col)
  )

  df <- tbl(con, table_name) %>%
    dplyr::inner_join(dbs_bag_location_tbl, by = dbs_bag_location_joins) %>%
    dplyr::select(all_of(c(row_number_col, bag_label_col, freezer_col, shelf_col, basket_col))) %>%
    dplyr::collect()

  if (error_if_exists) {
    if (nrow(df) > 0) {
      error_message <- sprintf("DBS bag label is not unique at location %s.", basket_col)
      return(ErrorData$new(description = error_message, data_frame = df))
    }
  } else {
    if (nrow(df) == 0) {
      error_message <- sprintf("DBS bag label could not be found at location %s.", basket_col)
      return(ErrorData$new(description = error_message, data_frame = df))
    }
  }
  
  return(NULL)  
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
  dbs_sheet_test(check_composition_id_exists, "Label", "Index", "LegacyLabel", error_if_exists = FALSE)
  dbs_sheet_test(validate_study_reference_db, "Batch", controls = TRUE)
  dbs_sheet_test(validate_location_reference_db, "DBS_FreezerName", "DBS_ShelfName", "DBS_BasketName")
  dbs_sheet_test(validate_dbs_bag_label_is_unique, "BagName", "DBS_FreezerName", "DBS_ShelfName", "DBS_BasketName", error_if_exists = TRUE)
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
  dbs_sheet_test(validate_empty_micronix_well_upload, "ExtractedDNAPosition", "PlateName", "PlateBarcode")
  dbs_sheet_test(validate_extraction_counts_with_totals, "ControlUID", "Batch", "SheetName", "SheetBarcode")

  # References check
  dbs_sheet_test(check_control_exists, "ControlUID", "Batch", error_if_exists = FALSE)
  dbs_sheet_test(validate_study_reference_db, "Batch")

  # Validate source location
  dbs_sheet_test(validate_location_reference_db, "DBS_FreezerName", "DBS_ShelfName", "DBS_BasketName")

  # Validate destination location
  dbs_sheet_test(validate_location_reference_db, "FreezerName", "ShelfName", "BasketName")
}

#' Validate DBS Sheet Move
#'
#' Conducts specific validation checks for moving DBS sheet controls.
#'
#' @param dbs_sheet_test The utility function for performing validation checks.
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_dbs_sheet_move <- function(dbs_sheet_test) {
  # References check
  # dbs_sheet_test(validate_dbs_bag_label_is_unique, "BagName", "DBS_FreezerName", "DBS_ShelfName", "DBS_BasketName", error_if_exists = TRUE)
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
  } else if (action == "move") {
    validate_whole_blood_move(whole_blood_test)
  } else {
    stop("No implementation for this action!!!")
  }

  return(errors)
}

#' Validate Cryovial Tube Exists
#'
#' This function checks that a cryovial exists by position, boxname, and barcode and box barcode, if provided.
#'
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_whole_blood_tube_exists <- function(con, table_name, row_number_col, position_col, box_name_col, box_barcode_col, control_uid_col, batch_col) {
  
  df <- tbl(con, table_name) %>%
    dplyr::left_join(tbl(con, "whole_blood_tube") %>% dplyr::rename(whole_blood_tube_id=id), by = setNames("position", position_col)) %>%
    dplyr::left_join(tbl(con, "cryovial_box") %>% dplyr::rename(cryovial_box_id=id, box_barcode=barcode), by = "cryovial_box_id") %>%
    dplyr::filter(name == !!sym(box_name_col) | box_barcode == !!sym(box_barcode_col)) %>%
    dplyr::left_join(tbl(con, "malaria_blood_control") %>% dplyr::rename(malaria_blood_control_id=id), by = "malaria_blood_control_id") %>%
    dplyr::left_join(tbl(con, "study_subject") %>% dplyr::rename(study_subject_id=id, control_uid=name), by = "study_subject_id") %>%
    dplyr::left_join(tbl(con, "study") %>% dplyr::rename(study_id=id), by = "study_id") %>%
    dplyr::filter(control_uid == !!sym(control_uid_col) & short_code == !!sym(batch_col)) %>%
    dplyr::filter(is.na(whole_blood_tube_id) | is.na(cryovial_box_id) | is.na(malaria_blood_control_id) | is.na(study_subject_id) | is.na(study_id)) %>%
    dplyr::select(all_of(c(row_number_col, position_col, box_name_col, box_barcode_col))) %>%
    dplyr::collect()

  if (nrow(df) > 0) {
    error_message <- "Cryovial tube could not be found."
    return(ErrorData$new(description = error_message, data_frame = df))
  }
  
  return(NULL)  
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

  # References check
  whole_blood_test(check_composition_id_exists, "Label", "Index", "LegacyLabel")
  whole_blood_test(validate_study_reference_db, "Batch", controls = TRUE)
  whole_blood_test(validate_location_reference_db, "WB_FreezerName", "WB_RackName", "WB_RackPosition")

  # Whole blood is stored in cryovials so reuse cryovial tests
  whole_blood_test(validate_empty_cryovial_well_upload, "ControlOriginPosition", "BoxName", "BoxBarcode")
}

#' Validate Whole Blood Move
#'
#' Conducts specific validation checks for creating whole blood controls.
#'
#' @param whole_blood_test The utility function for performing validation checks.
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_whole_blood_move <- function(whole_blood_test) {

  # Whole blood is stored in cryovials so reuse cryovial tests
  whole_blood_test(validate_empty_cryovial_well_upload, "Position", "BoxName", "BoxBarcode")
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

  # Add validation function calls here
  whole_blood_test(check_micronix_barcodes_exist, "Barcode", error_if_exists = TRUE)
  whole_blood_test(validate_empty_micronix_well_upload, "ExtractedDNAPosition", "PlateName", "PlateBarcode")

  # References check
  whole_blood_test(validate_study_reference_db, "Batch", controls = TRUE)
  whole_blood_test(check_control_exists, "ControlUID", "Batch", error_if_exists = FALSE)
  whole_blood_test(validate_whole_blood_tube_exists, "ControlOriginPosition", "BoxName", "BoxBarcode", "ControlUID", "Batch")

  # Validate source location
  whole_blood_test(validate_location_reference_db, "WB_FreezerName", "WB_RackName", "WB_RackPosition")

  # Validate destination location
  whole_blood_test(validate_location_reference_db, "FreezerName", "ShelfName", "BasketName")

}

#' Validate Control Specimens
#'
#' @param database A database connection or reference.
#' @param formatted_csv The CSV data formatted for validation.
#' @param control_type The type of control specimen ('dbs_sheet' or 'whole_blood').
#' @param user_action The user action taken on the control specimen ('extraction' or 'create').
#' 
#' @return Nothing
validate_controls <- function(database, user_data, control_type, user_action) {

  errors <- list()

  if (control_type == "dbs_sheet") {
    errors <- validate_dbs_sheet(user_data, user_action, database)
  } else if (control_type == "whole_blood") {
    errors <- validate_whole_blood(user_data, user_action, database)
  }

  validation_result <- NULL
  if (user_action == "created") {
    validation_result <- validate_date_format(user_data, "Batch", "%Y-%m-%d")

    if (inherits(validation_result, "ErrorData")) {
      errors <- add_to_errors(errors, validation_result)
    }
  }

  if (user_action == "extraction") {
    validation_result <- validate_date_format(user_data, "ExtractedOn", "%Y-%m-%d")

    if (inherits(validation_result, "ErrorData")) {
      errors <- add_to_errors(errors, validation_result)
    }
  }

  # Initialize the ValidationErrorCollection with the accumulated errors and the user_data
  error_collection <- ValidationErrorCollection$new(errors, user_data)

  if (error_collection$length() > 0) {
    stop_validation_error("Validation error", error_collection)
  }

  # update here
  if (user_action == "created") {
    user_data <- handle_unknown_date_tokens(user_data, "Batch", validation_result$parsed_dates, validation_result$token_mask)
  }

  return(user_data)
}