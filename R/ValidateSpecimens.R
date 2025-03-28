#' Validate Micronix-specific position rules
#'
#' @param data A dataframe containing the data to be validated.
#' @param position_column A character string specifying the column name for 'position'.
#' @param row_number_column A character string specifying the column name for 'RowNumber'.
#'
#' @return An ErrorData object if Micronix position rules are violated, or NULL otherwise.
validate_micronix_position <- function(data,
                                      position_column,
                                      row_number_column) {

  rn_letter <- data %>%
    group_by(!!sym(row_number_column)) %>%
    dplyr::mutate(
      letter_check = substr(!!sym(position_column), 1, 1) %in% LETTERS
    ) %>%
    select(!!sym(row_number_column), letter_check) %>%
    filter(!letter_check) %>%
    pull(!!sym(row_number_column))

  if (length(rn_letter) > 0) {
    df_err <- data[data[[row_number_column]] %in% rn_letter, c(row_number_column, position_column)]
    return(ErrorData$new(description = "Rows must use letters",
                         columns = names(df_err),
                         rows = df_err[[row_number_column]]))
  }

  return(NULL)
}

#' Validate Matrix Container position
#'
#' @param con A database connection object.
#' @param user_data The name of the table where the user data is temporarily stored in the database.
#' @param row_number_col The column with the row number in the `user_data`.
#' @param container_name_col The column with container name in `user_data`.
#' @param container_position_col The column with container positions in the `user_data`.
#' @param matrix_tablename The tablename in the database that should be checked.
#' @param container_tablename The sample container tablename. This is used for position checks.
#' @param error_if_exists Logical. If TRUE, an error is returned if the barcode exists in the database.
#'
#' @return An ErrorData object if Micronix position rules are violated, or NULL otherwise.
validate_matrix_container <- function(con, user_data, row_number_col, container_name_col, container_position_col, matrix_tablename, container_tablename, error_if_exists) {

  # Directly define the join conditions using named vectors
  user_table_joins <- setNames(
    c("name", "position"),
    c(container_name_col, container_position_col)
  )

  matrix_container_tbl <- tbl(con, matrix_tablename) %>%
    dplyr::rename(manifest_id = id) %>%
    dplyr::inner_join(tbl(con, container_tablename), by = join_by(manifest_id))

  df <- tbl(con, user_data) %>%
    dplyr::left_join(matrix_container_tbl, by = user_table_joins) %>%
    filter(!is.na(id)) %>%
    select(all_of(c(row_number_col, container_name_col, container_position_col))) %>%
    collect()

  if (error_if_exists && nrow(df) > 0) {
    return(ErrorData$new(
      description = "A sample already exists in the specified position.",
      columns = c(row_number_col, container_name_col, container_position_col),
      rows = df[[row_number_col]]
    ))
  } else if (!error_if_exists && nrow(df) == 0) {
    return(ErrorData$new(
      description = "A sample does not exist in the specified position.",
      columns = c(row_number_col, container_name_col, container_position_col),
      rows = df[[row_number_col]]
    ))
  }

  return(NULL)
}

#' Validate Micronix-specific barcode length rules
#'
#' @param data A dataframe containing the data to be validated.
#' @param barcode_column A character string specifying the column name for 'barcode'.
#' @param row_number_column A character string specifying the column name for 'RowNumber'.
#'
#' @return An ErrorData object if Micronix barcode rules are violated, or NULL otherwise.
validate_micronix_barcode_length <- function(data,
                                            barcode_column,
                                            row_number_column) {
  rn_barcode <- data %>%
    group_by(!!sym(row_number_column)) %>%
    dplyr::mutate(
      barcode_check = nchar(!!sym(barcode_column)) == 10
    ) %>%
    select(!!sym(row_number_column), barcode_check) %>%
    filter(!barcode_check) %>%
    pull(!!sym(row_number_column))

  if (length(rn_barcode) > 0) {
    df_err <- data[data[[row_number_column]] %in% rn_barcode, c(row_number_column, barcode_column)]
    return(ErrorData$new(description = "Micronix Barcodes must be 10 digits long",
                         columns = names(df_err),
                         rows = df_err[[row_number_column]]))
  }

  return(NULL)
}

#' Validate Micronix Barcode is specific in the file
#'
#' @param con A database connection object.
#' @param table_name The name of the table containing user-uploaded data.
#' @param row_number_col The name of the column in the user-uploaded table that contains row numbers.
#' @param barcode_col The name of the column in the user-uploaded table that contains the Micronix barcodes.
#'
#' @return An instance of the ErrorData class if errors are found, or NULL if there are no errors.
#' @keywords validation, micronix
validate_micronix_barcodes_are_unique_in_file <- function(data,
                                            barcode_column,
                                            row_number_column,
                                            plate_name_column) {
  df <- data %>%
    dplyr::group_by(!!sym(barcode_column)) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(!!sym(row_number_column), !!sym(barcode_column), !!sym(plate_name_column))

  if (nrow(df) > 0) {
    return(ErrorData$new(description = "Duplicate Micronix Barcodes found in your file",
                         data = df))
  }

  return(NULL)
}


#' Validate Cryovial-specific position rules
#'
#' @param data A dataframe containing the data to be validated.
#' @param position_column A character string specifying the column name for 'position'.
#' @param row_number_column A character string specifying the column name for 'RowNumber'.
#'
#' @return An ErrorData object if Cryovial position rules are violated, or NULL otherwise.
validate_cryovial_position <- function(data,
                                      position_column,
                                      row_number_column) {

  rn_letter <- data %>%
    group_by(!!sym(row_number_column)) %>%
    dplyr::mutate(
      letter_check = substr(!!sym(position_column), 1, 1) %in% LETTERS
    ) %>%
    select(!!sym(row_number_column), letter_check) %>%
    filter(!letter_check) %>%
    pull(!!sym(row_number_column))

  if (length(rn_letter) > 0) {
    df_err <- data[data[[row_number_column]] %in% rn_letter, c(row_number_column, position_column)]
    return(ErrorData$new(description = "Rows for Cryovial must use letters",
                         columns = names(df_err),
                         rows = df_err[[row_number_column]]))
  }

  return(NULL)
}

#' Check if collection dates are missing for samples in a longitudinal study
#'
#' @param con A database connection object.
#' @param table_name The name of the table where the user data is temporarily stored in the database.
#' @param row_number_col The name of the column containing the row number.
#' @param study_short_code_col The name of the column containing the study short code.
#' @param collection_date_col The name of the column containing the collection date.
#'
#' @keywords validation
#' @return An instance of the ErrorData class or NULL.
check_longitudinal_study_dates <- function(con, table_name, row_number_col, study_short_code_col, collection_date_col) {

  df <- tbl(con, table_name) %>%
    dplyr::inner_join(tbl(con, "study"), by = setNames("short_code", study_short_code_col)) %>% # Join on study short code
    filter(is_longitudinal == 1 & is.na(!!sym(collection_date_col))) %>%
    select(all_of(c(row_number_col, study_short_code_col, collection_date_col))) %>%
    collect()

  df[[collection_date_col]] <- as.character(lubridate::as_date(df[[collection_date_col]]))

  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = "Missing collection date found for sample in longitudinal study",
      columns = c(row_number_col, study_short_code_col, collection_date_col),
      rows = df[[row_number_col]]
    ))
  }
  return(NULL)
}

#' Check If Micronix Plate Exists in the Database
#'
#' This function checks whether the Micronix plates provided in a user-uploaded table
#' exist in the main micronix plate database.
#'
#' @param con A database connection object.
#' @param table_name The name of the table containing user-uploaded data.
#' @param row_number_col The name of the column in the user-uploaded table that contains row numbers.
#' @param plate_name_col The name of the column in the user-uploaded table that contains the Micronix container names.
#' @param plate_barcode_col The name of the column in the user-uploaded table that contains the Micronix container barcodes.
#'
#' @return An instance of the ErrorData class if errors are found, or NULL if there are no errors.
#' @keywords validation, micronix
#' @export
check_micronix_plate_exists <- function(con, table_name, row_number_col, plate_name_col, plate_barcode_col = NULL) {
  
  if (!is.null(plate_barcode_col)) {
    user_table_joins <- setNames(
      c("barcode", "name"),
      c(plate_barcode_col, plate_name_col)
    )
  } else {
    user_table_joins <- setNames(
      c("name"),
      c(plate_name_col)
    ) 
  }

  df <- tbl(con, table_name) %>%
    left_join(tbl(con, "micronix_plate"), by = user_table_joins) %>%
    filter(is.na(id)) %>%
    select(all_of(c(row_number_col, plate_name_col))) %>%
    collect()

  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = "Micronix plate not found",
      columns = c(row_number_col, plate_name_col, plate_barcode_col),
      rows = df[[row_number_col]]
    ))
  }
  return(NULL)
}

#' Check Each Cryovial Move is Unique
#'
#' Ensures that each cryovial move can be uniquely identified based on one or more
#' of the valid column names: "Barcode", "StudySubject", and "CollectionDate".
#'
#' @param con The database connection.
#' @param table_name The name of the table containing the cryovial move data.
#' @param row_number_col The name of the column in the table containing row numbers.
#' @param valid_columns A character vector of valid column names (default: c("Barcode", "StudySubject", "CollectionDate")).
#'
#' @return An instance of the ErrorData class if any rows are not uniquely identifiable, or NULL if all rows are unique.
#' @keywords validation, cryovial
check_each_row_cryovial_move_is_unique <- function(con, table_name, row_number_col, valid_columns = c("Barcode", "StudySubject", "CollectionDate", "StudyCode", "SpecimenType")) {
  
  # Check if at least one valid column exists in the table
  column_names <- dbListFields(con, table_name)
  matching_columns <- intersect(valid_columns, column_names)
  
  if (length(matching_columns) == 0) {
    stop("At least one valid column (Barcode, StudySubject, CollectionDate) must exist in the table.")
  }
  
  # Pull data for matching columns and row numbers
  df <- tbl(con, table_name) %>%
    select(all_of(c(row_number_col, matching_columns))) %>%
    collect()
  
  # Check for uniqueness of rows based on the matching columns
  duplicated_rows <- df %>%
    group_by(across(all_of(matching_columns))) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    select(!!sym(row_number_col)) %>%
    distinct()
  
  if (nrow(duplicated_rows) > 0) {
    return(ErrorData$new(
      description = "Each Cryovial must be uniquely identifiable using any of the provided columns below. Please check for duplicates or empty rows.",
      columns = matching_columns,
      rows = duplicated_rows[[row_number_col]]
    ))
  }
  
  return(NULL)
}

#' Check Each Cryovial Move is Identifiable
#'
#' Ensures that each cryovial move contains enough data to be identified by checking related database tables.
#'
#' @param con The database connection.
#' @param table_name The name of the table containing the cryovial move data.
#' @param row_number_col The name of the column in the table containing row numbers.
#' @param valid_columns A character vector of valid column names (default: c("Barcode", "StudySubject", "CollectionDate", "StudyCode", "SpecimenType")).
#'
#' @return An instance of the ErrorData class if any rows are not identifiable, or NULL if all rows are identifiable.
#' @keywords validation, cryovial
check_each_row_cryovial_move_is_identifiable <- function(con, table_name, row_number_col, valid_columns = c("Barcode", "StudySubject", "CollectionDate", "StudyCode", "SpecimenType")) {
  
  # Check if at least one valid column exists in the table
  column_names <- dbListFields(con, table_name)
  matching_columns <- intersect(valid_columns, column_names)
  
  if (length(matching_columns) == 0) {
    stop("At least one valid column (Barcode, StudySubject, CollectionDate, StudyCode, SpecimenType) must exist in the table.")
  }
  
  # Pull data for user columns and row numbers
  user_data <- tbl(con, table_name) %>%
    select(all_of(c(row_number_col, matching_columns))) %>%
    collect()
  
  # Create resolved_data by joining database tables
  resolved_data <- tbl(con, "specimen") %>%
    select(specimen_id = id, study_subject_id, specimen_type_id, CollectionDate = collection_date) %>%
    left_join(tbl(con, "study_subject") %>% select(study_subject_id = id, study_id, StudySubject = name), by = "study_subject_id") %>%
    left_join(tbl(con, "study") %>% select(study_id = id, StudyCode = short_code), by = "study_id") %>%
    left_join(tbl(con, "specimen_type") %>% select(specimen_type_id = id, SpecimenType = name), by = "specimen_type_id") %>%
    left_join(tbl(con, "storage_container") %>% select(storage_id = id, specimen_id, state_id, status_id), by = "specimen_id") %>%
    left_join(tbl(con, "cryovial_tube") %>% select(storage_id = id, Barcode = barcode), by = "storage_id") %>%
    # left_join(tbl(con, "state") %>% select(state_id = id, State = name), by = "state_id") %>%
    # left_join(tbl(con, "status") %>% select(status_id = id, Status = name), by = "status_id") %>%
    collect()

  state_table <- tbl(con, "state") %>%
    select(state_id = id, State = name) %>%
    collect()

  status_table <- tbl(con, "status") %>%
    select(status_id = id, Status = name) %>%
    collect()

  # Initialize a vector to track rows that cannot be identified
  non_identifiable_rows <- integer()
  inactive_rows <- integer()
  inactive_tib <- tibble()
  
  # Iterate through each row in user_data
  for (i in seq_len(nrow(user_data))) {
    row <- user_data[i, ]
    
    # Check for "UNK", "unk", "UNKNOWN", or "unknown" in CollectionDate and replace with lubridate::origin
    if ("CollectionDate" %in% matching_columns) {
      if (tolower(row$CollectionDate) %in% c("unk", "unknown")) {
        row$CollectionDate <- as.character(lubridate::origin)
      }
    }
    
    # Find non-empty columns for the current row
    non_empty_columns <- matching_columns[!is.na(row[matching_columns]) & row[matching_columns] != ""]
    
    # Skip if no columns have data
    if (length(non_empty_columns) == 0) {
      non_identifiable_rows <- c(non_identifiable_rows, row[[row_number_col]])
      next
    }
    
    # Construct filter conditions dynamically for the non-empty columns
    conditions <- purrr::map2(non_empty_columns, row[non_empty_columns], ~rlang::expr(!!sym(.x) == !!.y))
    
    # Check if the row can be matched in resolved_data
    match_found <- resolved_data %>%
      filter(!!!conditions) %>%  # Splice the conditions into the filter call
      summarise(match = n()) %>%
      pull(match) > 0
    
    if (!match_found) {
      non_identifiable_rows <- c(non_identifiable_rows, row[[row_number_col]])
    } else {
      resolved_with_state <- resolved_data %>%
        filter(!!!conditions) %>%  # Splice the conditions into the filter call
        left_join(state_table, by = "state_id") %>%
        left_join(status_table, by = "status_id") %>%
        select(State, Status)

      if (resolved_with_state[["State"]] != "Active") {
        inactive_rows <- c(inactive_rows, row[[row_number_col]])
        inactive_tib <- bind_rows(inactive_tib, resolved_with_state)
      }
    }
  }
  
  # Return errors if any rows are not identifiable
  if (length(non_identifiable_rows) > 0) {
    return(ErrorData$new(
      description = "Cryovial could not be found in the database. Please check for data entry errors, or include additional metadata to help with the search.",
      columns = matching_columns,
      rows = non_identifiable_rows
    ))
  }

  if (length(inactive_rows) > 0) {
    return(ErrorData$new(
      description = "Cryovial move file has inactive samples! These should be reviewed. Moving will cause them to activate!",
      columns = matching_columns,
      rows = inactive_rows,
      addtl_data = inactive_tib,
      error_level = "Warning"
    ))
  }
  
  return(NULL)
}



#' Check If Cryovial Box Exists in the Database
#'
#' This function checks whether the cryovial containers provided in a user-uploaded table
#' exist in the main cryovial tube database.
#'
#' @param con A database connection object.
#' @param table_name The name of the table containing user-uploaded data.
#' @param row_number_col The name of the column in the user-uploaded table that contains row numbers.
#' @param box_name_col The name of the column in the user-uploaded table that contains the cryovial container names.
#'
#' @return An instance of the ErrorData class if errors are found, or NULL if there are no errors.
#' @keywords validation, cryovial
#' @export
check_cryovial_box_exists <- function(con, table_name, row_number_col, box_name_col) {
  
  # Directly define the join conditions using named vectors
  user_table_joins <- setNames(
    c("name"),
    c(box_name_col)
  )
  
  df <- tbl(con, table_name) %>%
    left_join(tbl(con, "cryovial_box"), by = user_table_joins) %>%
    filter(is.na(id)) %>%
    select(all_of(c(row_number_col, box_name_col))) %>%
    collect()

  if (nrow(df) > 0) {
    return(
      ErrorData$new(
      description = "Cryovial Box not found",
      columns = c(row_number_col, box_name_col),
      rows = df[[row_number_col]]
    )
  )}

  return(NULL)
}

#' Check Micronix Barcodes in Database
#'
#' This function checks whether Micronix barcodes provided in a user-uploaded table
#' exist or don't exist (based on `error_if_exists` parameter) in the main database.
#'
#' @param con A database connection object.
#' @param user_data The name of the table containing user-uploaded data.
#' @param row_number_col The name of the column in the user-uploaded table that contains row numbers.
#' @param micronix_col The name of the column in the user-uploaded table that contains the Micronix barcodes.
#' @param error_if_exists A logical value. If TRUE, function returns error if barcode exists in the database, 
#' if FALSE, function returns error if barcode doesn't exist.
#'
#' @return An instance of the ErrorData class if errors are found, or NULL if there are no errors.
#' @keywords validation, micronix
#' @export
check_micronix_barcodes_exist <- function(con, user_data, row_number_col, micronix_col, error_if_exists=TRUE) {
  
  df <- tbl(con, user_data) %>%
    filter(!is.na(!!sym(micronix_col))) %>%
    left_join(tbl(con, "micronix_tube"), by = setNames("barcode", micronix_col))
  
  if (error_if_exists) {
    df <- df %>% filter(!is.na(id))
    error_desc <- "Micronix barcode already exists in database"
  } else {
    df <- df %>% filter(is.na(id))
    error_desc <- "Micronix barcode not found in database"
  }
  
  df <- df %>% select(!!sym(row_number_col), !!sym(micronix_col)) %>% collect()

  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = error_desc,
      columns = c(row_number_col, micronix_col),
      rows = df[[row_number_col]]
    ))
  }

  return(NULL)
}

#' Check Micronix Barcodes in Database
#'
#' This function checks whether Micronix barcodes provided in a user-uploaded table
#' exist or don't exist (based on `error_if_exists` parameter) in the main database.
#'
#' @param con A database connection object.
#' @param user_data The name of the table containing user-uploaded data.
#' @param row_number_col The name of the column in the user-uploaded table that contains row numbers.
#' @param micronix_col The name of the column in the user-uploaded table that contains the Micronix barcodes.
#' @param error_if_exists A logical value. If TRUE, function returns error if barcode exists in the database, 
#' if FALSE, function returns error if barcode doesn't exist.
#'
#' @return An instance of the ErrorData class if errors are found, or NULL if there are no errors.
#' @keywords validation, micronix
#' @export
check_if_sample_is_archived <- function(con, user_data, row_number_col, micronix_col) {
  
  df <- tbl(con, user_data) %>%
    left_join(tbl(con, "micronix_tube"), by = setNames("barcode", micronix_col)) %>%
    left_join(tbl(con, "storage_container"), by = "id") %>%
    left_join(tbl(con, "state") %>% dplyr::rename(State = "name"), by = c("state_id" = "id")) %>%
    left_join(tbl(con, "status") %>% dplyr::rename(Status = "name"), by = c("status_id" = "id")) %>%
    filter(State != "Active")

  # add addtional columns
  addtl_data <- df %>% select(State, Status, Comment = comment) %>% collect()
  df <- df %>% select(!!sym(row_number_col), !!sym(micronix_col)) %>% collect()

  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = "ARCHIVED samples found in scan! Please check your plate and confirm that this is your intention!",
      data_frame = df,
      addtl_data = addtl_data,
      error_level = "Warning"
    ))
  }

  return(NULL)
}

#' Check Cryovial Barcodes in Database
#'
#' This function will check for the prescence of active cyrovial barcodes in the database.
#' In other words, it will ignore any cryovials that are in a state other than 'Active'.
#'
#' @param con A database connection object.
#' @param user_data The name of the table where the user data is temporarily stored in the database.
#' @param row_number_col The column with the row number in the `user_data`.
#' @param cryovial_col The column with Cryovial barcodes in the `user_data`.
#' @param cryovial_box_col The column with Cryovial box IDs in the `user_data`.
#' @param error_if_exists Logical. If TRUE, an error is returned if the barcode exists in the database.
#'
#' @keywords validation, cryovial
#' @return An instance of the ErrorData class or NULL.
check_cryovial_barcodes_exist <- function(con, user_data, row_number_col, cryovial_col, cryovial_box_col, error_if_exists = TRUE) {
  
  # Start by pulling data from user_data table
  df <- tbl(con, user_data) %>%
    filter(!is.na(!!sym(cryovial_col))) %>%
    select(!!sym(row_number_col), !!sym(cryovial_col), !!sym(cryovial_box_col))

  storage_container_tbl <- tbl(con, "storage_container") %>% dplyr::select(cryovial_id = id, state_id)
  cryovial_tube <- tbl(con, "cryovial_tube") %>% dplyr::select(cryovial_id = id, barcode, manifest_id)
  cryovial_box <- tbl(con, "cryovial_box") %>% dplyr::select(manifest_id = id, box_name = name)

  container_df <- cryovial_tube %>%
    inner_join(storage_container_tbl, by = c("cryovial_id")) %>%
    filter(state_id == 1) %>%
    left_join(cryovial_box, by = c("manifest_id"))

  # Join with cryovial_tube based on cryovial barcode
  df <- df %>%
    left_join(container_df, by = setNames(c("barcode"), c(cryovial_col)))

  # Error handling based on existence and duplication criteria
  if (error_if_exists) {
    df <- df %>% filter(!is.na(cryovial_id))
    error_desc <- "Cryovial is duplicated within a box"
  } else {
    df <- df %>% filter(is.na(cryovial_id))
    error_desc <- "Cryovial barcode not found in database"
  }

  # Collect the final set of data and check for errors
  df <- df %>% select(!!sym(row_number_col), !!sym(cryovial_col)) %>% collect()

  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = error_desc,
      columns = c(row_number_col, cryovial_col, cryovial_box_col),
      rows = df[[row_number_col]]
    ))
  }

  return(NULL)
}

#' Check if boxes have enough unique cryovial barcodes to be easily identifiable.
#'
#' @description This function is meant to check whether a box of cryovials is overly similar to another
#' box in that it could potentially be confused to the end user. This can also be used to quickly
#' check if a cryovial box has already been uploaded to the database (ie. prevent duplicated uploads). 
#' This is a problem for cryovials because barcodes must only be unique within a box; they are not universally unique. 
#'
#' @param con A database connection object.
#' @param user_data The name of the table where the user data is temporarily stored in the database.
#' @param row_number_col The column with the row number in the `user_data`.
#' @param cryovial_col The column with Cryovial barcodes in the `user_data`.
#' @param cryovial_box_col The column with Cryovial box IDs in the `user_data`.
#' @param similarity_tolerance The percent similarity that is tolerated.
#'
#' @keywords validation, cryovial
#' @return An instance of the ErrorData class or NULL.
validate_box_uniqueness <- function(con, user_data, row_number_col, cryovial_col, cryovial_box_col, similarity_tolerance = 10) {

  # Retrieve user data for cryovial barcodes and box names
  user_df <- tbl(con, user_data) %>%
    select(!!sym(row_number_col), !!sym(cryovial_col), !!sym(cryovial_box_col)) %>%
    collect()

  # Retrieve existing cryovial data from the database
  storage_container_tbl <- tbl(con, "storage_container") %>%
    select(id, state_id) %>%
    filter(state_id == 1)

  existing_cryovials <- tbl(con, "cryovial_tube") %>%
    inner_join(storage_container_tbl, by = "id") %>%
    select(cryovial_id = id, barcode, manifest_id) %>%
    collect()

  # Retrieve existing box data from the database
  existing_boxes <- tbl(con, "cryovial_box") %>%
    select(manifest_id = id, existing_box_name = name) %>%
    collect()

  # Join user data with existing cryovial data to find matches
  matches <- user_df %>%
    inner_join(existing_cryovials, by = setNames(c("barcode"), c(cryovial_col))) %>%
    left_join(existing_boxes, by = "manifest_id")

  # Make sure to keep the names of boxes from user data and existing data separate
  matches <- matches %>%
    select(!!sym(row_number_col), !!sym(cryovial_col), user_box_name = !!sym(cryovial_box_col), existing_box_name)

  # Calculate the percentage of barcodes from each user's box found in each existing box
  comparison <- matches %>%
    group_by(user_box_name, existing_box_name) %>%
    summarise(count = n(), .groups = "drop") %>%
    left_join(user_df %>% count(!!sym(cryovial_box_col)), by = c("user_box_name" = cryovial_box_col)) %>%
    mutate(similarity = count / n * 100) %>%
    filter(similarity >= similarity_tolerance) %>%
    ungroup() %>%
    select(user_box_name, existing_box_name, similarity) %>%
    left_join(matches, by = c("user_box_name", "existing_box_name")) %>%
    select(!!sym(row_number_col), !!sym(cryovial_col), user_box_name, existing_box_name) %>%
    collect() %>%
    rename(!!sym(cryovial_box_col) := user_box_name) %>%
    filter(!is.na(Barcode))

  # Return results or NULL if no issues are found
  if (nrow(comparison) > 0) {
    error_desc <- sprintf("Similarity threshold (%s%%) exceeded between uploaded and existing boxes: %s", similarity_tolerance, paste(unique(comparison$existing_box_name), collapse = ", "))
    return(ErrorData$new(
      description = error_desc,
      columns = c(row_number_col, cryovial_col, cryovial_box_col),
      rows = unique(comparison[[row_number_col]]),
      error_level = "Warning"
    ))
  }

  return(NULL)
}

#' Validate if barcodes already exist for a given study
#'
#' @param con A database connection object.
#' @param user_data A data frame containing the user data.
#' @param row_number_col The name of the column containing the row number.
#' @param barcode_col The column name for barcodes in user_data.
#' @param study_short_code_col The column name for study short codes in user_data.
#'
#' @keywords validation, cryovial
#' @return An instance of the ErrorData class or NULL.
validate_existing_barcodes_by_study <- function(con, user_data, row_number_col, barcode_col, study_short_code_col) {

  df <- tbl(con, user_data) %>%
    filter(!is.na(!!sym(barcode_col))) %>%
    inner_join(tbl(con, "container"), by = c(barcode_col = "barcode")) %>%
    inner_join(tbl(con, "storage_container"), by = c("id" = "storage_container_id")) %>%
    inner_join(tbl(con, "specimen"), by = c("id" = "specimen_id")) %>%
    inner_join(tbl(con, "study_subject"), by = c("id" = "study_subject_id")) %>%
    inner_join(tbl(con, "study"), by = c("id" = "study_id", study_short_code_col = "short_code")) %>%
    select(all_of(c(row_number_col, barcode_col, study_short_code_col))) %>%
    collect()

  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = "Barcodes found that already exist with current study",
      columns = c(row_number_col, barcode_col, study_short_code_col),
      rows = df[[row_number_col]]
    ))
  }

  return(NULL)
}

#' Check uniqueness of StudySubject within non-longitudinal studies.
#'
#' @param con The database connection.
#' @param table_name The table name in the database that contains the data.
#' @param row_number_col The column name for row numbers.
#' @param study_short_code_col The column name for study short codes.
#' @param study_subject_col The column name for study subjects.
#' @param specimen_type_col The column name for the speciment type.
#'
#' @return ErrorData object or NULL if no errors found.
#' @keywords validation, cryovial
validate_non_longitudinal_study_subjects <- function(con, table_name, row_number_col, study_short_code_col, study_subject_col, specimen_type_col) {

  # Setup joins

  study_joins <- setNames(c("short_code"), c(study_short_code_col))
  study_subject_joins <- setNames(c("name", "study_id"), c(study_subject_col, "study_id"))
  specimen_type_joins <- setNames(c("name", "specimen_type_id"), c(specimen_type_col, "specimen_type_id"))

  study_tbl <- tbl(con, "study") %>% dplyr::rename(study_id = id)
  study_subject_tbl <- tbl(con, "study_subject") %>% dplyr::rename(study_subject_id = id)
  specimen_type_tbl <- tbl(con, "specimen_type") %>% dplyr::rename(specimen_type_id = id)
  specimen_tbl <- tbl(con, "specimen") %>% dplyr::rename(specimen_id = id)

  # Check for duplicates in the database as well as duplicates
  # in the file.
  df <- tbl(con, table_name) %>%
    dplyr::inner_join(study_tbl, by = study_joins) %>%
    dplyr::inner_join(study_subject_tbl, by = study_subject_joins) %>%
    dplyr::inner_join(specimen_tbl, by = c("study_subject_id")) %>%
    dplyr::inner_join(specimen_type_tbl, by = specimen_type_joins) %>%
    dplyr::filter(is_longitudinal == 0) %>%
    select(all_of(c(row_number_col, study_subject_col, study_short_code_col, specimen_type_col))) %>%
    collect()

  if(nrow(df) > 0) {
    return(ErrorData$new(data_frame = df, description = "Specimen types must be unique for study subjects in non-longitudinal studies"))
  }
  return(NULL)
}

#' Ensure uniqueness of study subjects and collection dates within longitudinal studies.
#'
#' @param con The database connection.
#' @param table_name The table name in the database that contains the data.
#' @param row_number_col The column name for row numbers.
#' @param study_short_code_col The column name for study short codes.
#' @param study_subject_col The column name for study subjects.
#' @param collection_date_col The column name for collection dates.
#'
#' @return ErrorData object or NULL if no errors found.
#' @keywords validation, studies
validate_longitudinal_study <- function(con, table_name, row_number_col, study_short_code_col, study_subject_col, collection_date_col) {

  # Setup joins
  study_joins <- setNames(c("short_code"), c(study_short_code_col))
  study_subject_joins <- setNames(c("name", "study_id"), c(study_subject_col, "id"))
  specimen_joins <- setNames(c("study_subject_id", "collection_date"), c("id_study_subject", collection_date_col))

  df <- tbl(con, table_name) %>%
    inner_join(tbl(con, "study"), by = study_joins, suffix = c("", "_study")) %>%
    filter(is_longitudinal == 1) %>% # Only check longitudinal studies
    inner_join(tbl(con, "study_subject"), by = study_subject_joins, suffix = c("", "_study_subject")) %>%
    inner_join(tbl(con, "specimen"), by = specimen_joins, suffix = c("", "_specimen")) %>%
    filter(is.na(id_specimen)) %>% # Only check samples that don't already exist in the database
    select(all_of(c(row_number_col, study_subject_col, collection_date_col, study_short_code_col))) %>%
    collect()

  if(nrow(df) > 0) {
    return(ErrorData$new(data_frame = df, description = "Study subject and collection date must be unique within a longitudinal study"))
  }
  return(NULL)
}

#' Confirm that Cryovials with no barcodes have collection dates if there's already a sample from that study subject.
#'
#' @param con The database connection.
#' @param table_name The table name in the database that contains the data.
#' @param row_number_col The column name for row numbers.
#' @param study_short_code_col The column name for study short codes.
#' @param study_subject_col The column name for study subjects.
#' @param barcode_col The column name for barcodes.
#' @param collection_date_col The column name for collection dates.
#'
#' @return ErrorData object or NULL if no errors found.
#' @keywords validation, dates
validate_cryovial_collection_dates <- function(con, table_name, row_number_col, study_short_code_col, study_subject_col, barcode_col, collection_date_col) {
  # Setup joins
  study_joins <- setNames(c("short_code"), c(study_short_code_col))
  study_subject_joins <- setNames(c("name", "study_id"), c(study_subject_col, "id"))
  specimen_joins <- setNames(c("study_subject_id", "collection_date"), c("id_study_subject", collection_date_col))
  
  df <- tbl(con, table_name) %>%
    filter(is.na(!!sym(barcode_col)) & is.na(!!sym(collection_date_col))) %>% # Only check samples without barcodes and collection_dates
    inner_join(tbl(con, "study"), by = study_joins, suffix = c("", "_study")) %>%
    inner_join(tbl(con, "study_subject"), by = study_subject_joins, suffix = c("", "_study_subject")) %>%
    inner_join(tbl(con, "specimen"), by = specimen_joins, suffix = c("", "_specimen")) %>%
    select(all_of(c(row_number_col, study_subject_col, study_short_code_col, collection_date_col))) %>%
    collect()

  if(nrow(df) > 0) {
    return(ErrorData$new(data_frame = df, description = "Sample must have a collection date if there is no barcode provided and there is already a sample from this study subject."))
  }
  return(NULL)
}

#' Validate that Cryovial barcodes are unique by Study short_code and SpecimenType.
#' Barcodes are not required but can be added as additional information. If 
#' we have a barcode, we need to ensure that it is unique within the study.
#'
#' @param con The database connection.
#' @param table_name The table name in the database that contains the data.
#' @param row_number_col The column name for row numbers.
#' @param barcode_col The column name for barcodes.
#' @param study_short_code_col The column name for study short codes.
#' @param specimen_type_col The column name for study subjects.
#'
#' @return ErrorData object or NULL if no errors found.
#' @keywords validation, dates
validate_cryovial_barcodes <- function(con, table_name, row_number_col, barcode_col, study_short_code_col, specimen_type_col) {

  # Setup joins
  study_joins <- setNames(c("short_code"), c(study_short_code_col))
  specimen_type_joins <- setNames(c("name", "specimen_type_id"), c(specimen_type_col, "specimen_type_id"))

  # Tbls
  study_tbl <- tbl(con, "study") %>% dplyr::rename(study_id = id)
  study_subject_tbl <- tbl(con, "study_subject") %>% dplyr::rename(study_subject_id = id)
  specimen_type_tbl <- tbl(con, "specimen_type") %>% dplyr::rename(specimen_type_id = id)
  specimen_tbl <- tbl(con, "specimen") %>% dplyr::rename(specimen_id = id)
  storage_container_tbl <- tbl(con, "storage_container") %>% dplyr::select(cryovial_id = id, state_id, specimen_id)
  cryovial_tube_tbl <- tbl(con, "cryovial_tube") %>%
    dplyr::select(cryovial_id = id, !!sym(barcode_col) := barcode, manifest_id)
  cryovial_box_tbl <- tbl(con, "cryovial_box") %>% dplyr::select(manifest_id = id, box_name = name)

  # Validate that Cryovial barcodes are unique by Study short_code and SpecimenType if 
  # we have barcodes in the user data.
  df <- tbl(con, table_name) %>%
    filter(!is.na(!!sym(barcode_col))) %>%
    inner_join(study_tbl, by = study_joins) %>%
    inner_join(study_subject_tbl, by = "study_id") %>%
    inner_join(specimen_tbl, by = "study_subject_id") %>%
    inner_join(specimen_type_tbl, by = specimen_type_joins) %>%
    inner_join(storage_container_tbl, by = "specimen_id") %>%
    filter(state_id == 1) %>%
    inner_join(cryovial_tube_tbl, by = c("cryovial_id", barcode_col)) %>%
    inner_join(cryovial_box_tbl, by = "manifest_id") %>%
    select(all_of(c(row_number_col, barcode_col, study_short_code_col, specimen_type_col))) %>%
    collect()

  if (nrow(df) > 0) {
    return(
      ErrorData$new(
        data_frame = df,
        description = "Barcodes must be unique by Study and SpecimenType.",
        error_level = "Warning"
      )
    )
  }

  return(NULL)
}

#' Micronix Empty Well Validation
#'
#' This function checks if the provided sample in the Micronix dataset is being uploaded to an empty well.
#'
#' @param con A database connection object.
#' @param table_name The name of the formatted CSV table in the database.
#' @param row_number_col The column name representing the row number in `table_name`.
#' @param position_col The column name representing the position in `table_name`.
#' @param container_name_col The column name representing the Micronix container name in `table_name`.
#' @param container_barcode col The column name representing the Micronix container barcode in `table_name`.
#'
#' @return An object of class ErrorData. If there are no errors, NULL is returned.
#' @export
#' @keywords validation, micronix
validate_empty_micronix_well_upload <- function(con, table_name, row_number_col, position_col, container_name_col, container_barcode_col) {
  
  # Directly define the join conditions using named vectors
  user_table_joins <- setNames(
    c("container_barcode", "container_name", "position"),
    c(container_barcode_col, container_name_col, position_col)
  )

  df <- tbl(con, table_name) %>%
    inner_join(
      tbl(con, "storage_container") %>%
        select(status_id, id) %>%
        filter(status_id == 1) %>% 
        inner_join(tbl(con, "micronix_tube"), by = c("id" = "id")) %>%
        inner_join(tbl(con, "micronix_plate") %>%
          dplyr::rename(
            container_barcode = barcode,
            container_name = name
          ), by = c("manifest_id" = "id")
        ), by = user_table_joins) %>%
    select(all_of(c(row_number_col, position_col, container_name_col))) %>%
    collect()

  if (nrow(df) > 0) {
    error_message <- "Uploading sample to Micronix well location that already has an active sample"
    return(ErrorData$new(description = error_message, data = df))
  }

  return(NULL)
}

#' Check DBS Specimen Box and Bag exist
#'
#' This function checks if the Box or Bag exists in the database.
#'
#' @param con A database connection object.
#' @param user_data The name of the formatted CSV table in the database.
#' @param row_number_col The column name representing the row number in `table_name`.
#' @param label_col The column name representing the position in `table_name`.
#' @param container_name_col The column name representing the Cryovial container name in `table_name`.
#'
#' @return An object of class ErrorData. If there are no errors, NULL is returned.
#' @keywords validation, dbs-specimen
#' @export
validate_dbs_box_bag_exists <- function(con, user_data, row_number_col, container_name_col, container_type_col) {
  # Directly define the join conditions using named vectors

  user_table_joins <- setNames(
    c("name", "manifest_type"),
    c(container_name_col, container_type_col)
  )

  errors <- NULL

  paper_box_tbl <- tbl(con, "paper") %>%
    filter(manifest_type == "box")

  paper_bag_tbl <- tbl(con, "paper") %>%
    filter(manifest_type == "bag")

  box_tbl <- tbl(con, "box") %>%
    dplyr::rename(box_id = id) %>%
    dplyr::left_join(paper_box_tbl, by = c("box_id"="manifest_id")) %>%
    dplyr::mutate(manifest_type = "box")

  bag_tbl <- tbl(con, "bag") %>%
    dplyr::rename(bag_id = id) %>%
    dplyr::left_join(paper_bag_tbl, by = c("bag_id"="manifest_id")) %>%
    dplyr::mutate(manifest_type = "bag")

  user_data_box_tbl <- tbl(con, "user_data") %>%
    filter(!!rlang::sym(container_type_col) == "Box")

  user_data_bag_tbl <- tbl(con, "user_data") %>%
    filter(!!rlang::sym(container_type_col) == "Bag")

  df_box_user_data_joined <- user_data_box_tbl %>%
    dplyr::mutate(!!sym(container_type_col) := tolower(!!sym(container_type_col))) %>%
    dplyr::left_join(box_tbl, by = user_table_joins)

  df_bag_user_data_joined <- user_data_bag_tbl %>%
    dplyr::mutate(!!sym(container_type_col) := tolower(!!sym(container_type_col))) %>%
    dplyr::left_join(bag_tbl, by = user_table_joins) 

  # Check that the dbs samples
  bag_id_na <- df_bag_user_data_joined %>%
    filter(is.na(bag_id)) %>%
    select(all_of(c(row_number_col, container_name_col))) %>%
    collect()

  box_id_na <- df_box_user_data_joined %>%
    filter(is.na(box_id)) %>%
    select(all_of(c(row_number_col, container_name_col))) %>%
    collect()

  if (nrow(bag_id_na) > 0) {
    error.data <- ErrorData$new(data_frame = bag_id_na, description = "Bag could not be found.")
    errors <- c(errors, error.data)
  }

  if (nrow(box_id_na) > 0) {
    error.data <- ErrorData$new(data_frame = box_id_na, description = "Box could not be found.")
    errors <- c(errors, error.data)
  }

  return(errors)
}

#' DBS Specimen paper identifier uniqueness check
#'
#' This function checks if the provided paper in the DBS specimen dataset is unique by position.
#'
#' @param con A database connection object.
#' @param user_data The name of the formatted CSV table in the database.
#' @param row_number_col The column name representing the row number in `table_name`.
#' @param label_col The column name representing the position in `table_name`.
#' @param container_name_col The column name representing the Cryovial container name in `table_name`.
#'
#' @return An object of class ErrorData. If there are no errors, NULL is returned.
#' @keywords validation, dbs-specimen
validate_dbs_sample_label_uniqueness <- function(con, user_data, row_number_col, label_col, container_name_col, container_type_col, error_if_exists) {
  # Directly define the join conditions using named vectors

  user_table_joins <- setNames(
    c("label", "name", "manifest_type"),
    c(label_col, container_name_col, container_type_col)
  )

  errors <- NULL

  paper_box_tbl <- tbl(con, "paper") %>%
    filter(manifest_type == "box")

  paper_bag_tbl <- tbl(con, "paper") %>%
    filter(manifest_type == "bag")

  box_tbl <- tbl(con, "box") %>%
    dplyr::rename(box_id = id) %>%
    dplyr::left_join(paper_box_tbl, by = c("box_id"="manifest_id"))

  bag_tbl <- tbl(con, "bag") %>%
    dplyr::rename(bag_id = id) %>%
    dplyr::left_join(paper_bag_tbl, by = c("bag_id"="manifest_id"))

  df_bag <- tbl(con, user_data) %>%
    dplyr::mutate(!!sym(container_type_col) := tolower(!!sym(container_type_col))) %>%
    dplyr::left_join(bag_tbl, by = user_table_joins) %>%
    dplyr::group_by(!!rlang::sym(container_name_col), !!rlang::sym(label_col)) %>%
    dplyr::mutate(n = n()) %>%
    ungroup() %>%
    filter(n > 1 | !is.na(bag_id)) %>%
    select(all_of(c(row_number_col, label_col, container_name_col))) %>%
    collect()

  df_box <- tbl(con, user_data) %>%
    dplyr::mutate(!!sym(container_type_col) := tolower(!!sym(container_type_col))) %>%
    dplyr::left_join(box_tbl, by = user_table_joins) %>%
    dplyr::group_by(!!rlang::sym(container_name_col), !!rlang::sym(label_col)) %>%
    dplyr::mutate(n = n()) %>%
    ungroup() %>%
    filter(n > 1 | !is.na(box_id)) %>%
    select(all_of(c(row_number_col, label_col, container_name_col))) %>%
    collect()

  if(nrow(df_bag) > 0) {
    error.data <- ErrorData$new(data_frame = df_bag, description = "Labels must be unique on papers with DBS specimens (Bag).")
    errors <- c(errors, error.data)
  }
  if (nrow(df_box) > 0) {
    error.data <- ErrorData$new(data_frame = df_box, description = "Labels must be unique on papers with DBS specimens (Box).")
    errors <- c(errors, error.data)
  }
  return(errors)

}

#' Cryovial Empty Well Validation
#'
#' This function checks if the provided sample in the Cryovial dataset is being uploaded to an empty well.
#'
#' @param con A database connection object.
#' @param table_name The name of the formatted CSV table in the database.
#' @param row_number_col The column name representing the row number in `table_name`.
#' @param position_col The column name representing the position in `table_name`.
#' @param container_name_col The column name representing the Cryovial container name in `table_name`.
#'
#' @return An object of class ErrorData. If there are no errors, NULL is returned.
#' @export
#' @keywords validation, cryovial
validate_empty_cryovial_well_upload <- function(con, table_name, row_number_col, position_col, container_name_col) {

  # Directly define the join conditions using named vectors
  user_table_joins <- setNames(
    c(container_name_col, position_col),
    c("container_name", "position") # some of these are from renames!!!
  )

  # Check empty wells for Cryovial
  df <- tbl(con, "storage_container") %>%
    select(status_id, id) %>%
    filter(status_id == 1) %>% 
    inner_join(tbl(con, "cryovial_tube"), by = c("id" = "id")) %>%
    inner_join(tbl(con, "cryovial_box") %>%
      dplyr::rename(
        container_barcode = barcode,
        container_name = name
      ), by = c("manifest_id" = "id")
    ) %>%
    inner_join(tbl(con, table_name), by = user_table_joins) %>% 
    select(!!sym(row_number_col), setNames(names(user_table_joins), user_table_joins)) %>%
    collect()

  if (nrow(df) > 0) {
    error_message <- "Uploading sample to Cryovial well location that already has an active sample"
    return(ErrorData$new(description = error_message, data = df))
  }

  return(NULL)
}

#' Empty Whole Blood Well Validation
#'
#' This function checks if the provided sample in the Cryovial dataset is being uploaded to an empty well.
#'
#' @param con A database connection object.
#' @param table_name The name of the formatted CSV table in the database.
#' @param row_number_col The column name representing the row number in `table_name`.
#' @param position_col The column name representing the position in `table_name`.
#' @param container_name_col The column name representing the Cryovial container name in `table_name`.
#'
#' @return An object of class ErrorData. If there are no errors, NULL is returned.
#' @export
#' @keywords validation, cryovial
validate_empty_wb_well_upload <- function(con, table_name, row_number_col, position_col, container_name_col) {

  # Directly define the join conditions using named vectors
  user_table_joins <- setNames(
    c(container_name_col, position_col),
    c("container_name", "position") # some of these are from renames!!!
  )

  # Check empty wells for Cryovial
  df <- tbl(con, "whole_blood_tube") %>%
    select(status_id, id, cryovial_box_id, position) %>%
    filter(status_id == 1) %>% 
    inner_join(tbl(con, "cryovial_box") %>%
      dplyr::rename(
        container_barcode = barcode,
        container_name = name
      ), by = c("cryovial_box_id" = "id")
    ) %>%
    inner_join(tbl(con, table_name), by = user_table_joins) %>% 
    select(!!sym(row_number_col), setNames(names(user_table_joins), user_table_joins)) %>%
    collect()

  if (nrow(df) > 0) {
    error_message <- "Uploading Whole Blood to well location that already has an active sample"
    return(ErrorData$new(description = error_message, data = df))
  }

  return(NULL)
}

#' Check Unique Positions
#' 
#' @param user_data A data frame contianer the user data
#' @param position_col A position column
#' @param container_name_col The container name column
#' @param container_barcode_col The container barcode column
check_unique_positions <- function(user_data, position_col, container_name_col, container_barcode_col) {
  duplicates <- user_data %>%
    group_by(!!sym(container_name_col), !!sym(container_barcode_col), !!sym(position_col)) %>%
    filter(n() > 1) %>%
    ungroup()

  if (nrow(duplicates) > 0) {
    return(ErrorData$new(description = sprintf("Found duplicate positions for given %s and %s", container_name_col, container_barcode_col),
                         columns = names(duplicates),
                         rows = duplicates[["RowNumber"]]))
  }
}


#' Validate Micronix Specimen Data
#'
#' This function conducts a series of validation checks on Micronix specimen data.
#' The function will return any errors encountered during validation.
#'
#' @param user_data A data frame containing specimen data to validate.
#' @param action The action being performed, e.g. "upload" or "move".
#' @param database The database connection or specification to use for validation.
#' @return An ErrorDataList object containing validation errors, if any.
#' @keywords validation, micronix
validate_micronix <- function(user_data, action, file_type, database) {
  errors <- list()

  variable_colnames <- list()
  variable_colnames[['barcode_col']] <- find_column_name(user_data, c("Barcode", "Tube ID", "TubeCode"))

  result <- validate_micronix_barcode_length(user_data, variable_colnames[['barcode_col']], "RowNumber")
  if (!is.null(result)) {
    errors <- add_to_errors(errors, result)
  }

  result <- validate_micronix_barcodes_are_unique_in_file(user_data, variable_colnames[['barcode_col']], "RowNumber", "PlateName")
  if (!is.null(result)) {
    errors <- add_to_errors(errors, result)
  }

  result <- validate_micronix_position(user_data, "Position", "RowNumber")
  if (!is.null(result)) {
    errors <- add_to_errors(errors, result)
  }

  result <- check_unique_positions(user_data, "Position", "PlateName", "PlateBarcode")
  if (!is.null(result)) {
    errors <- add_to_errors(errors, result)
  }

  errors <- c(
    errors,
    perform_micronix_db_validations(database, user_data, action, variable_colnames)
  )

  return(errors)
}

#' Validate Static Plate Data
#'
#' This function conducts a series of validation checks on Static Plate specimen data.
#' The function will return any errors encountered during validation.
#'
#' @param user_data A data frame containing specimen data to validate.
#' @param action The action being performed, e.g. "upload" or "move".
#' @param database The database connection or specification to use for validation.
#' @return An ErrorDataList object containing validation errors, if any.
#' @keywords validation, micronix
validate_static_plate <- function(user_data, action, file_type, database) {
  errors <- list()

  result <- validate_micronix_position(user_data, "Position", "RowNumber")
  if (!is.null(result)) {
    errors <- add_to_errors(errors, result)
  }

  result <- check_unique_positions(user_data, "Position", "PlateName", "PlateBarcode")
  if (!is.null(result)) {
    errors <- add_to_errors(errors, result)
  }

  errors <- c(
    errors,
    perform_static_plate_db_validations(database, user_data, action, variable_colnames)
  )

  return(errors)
}

#' Perform Database-Related Validations for Static Plate
#'
#' This function conducts database-related validation checks for Static Plate specimen data.
#' It initiates a database connection, then performs upload or move validations based on the action provided.
#'
#' @param database The database connection or specification for validation.
#' @param user_data The users data.
#' @param action The action being performed, either "upload" or "move".
#' @return A list containing validation errors, if any.
#' @keywords validation, micronix
perform_static_plate_db_validations <- function(database, user_data, action, variable_colnames) {
  con <- init_and_copy_to_db(database, user_data)
  on.exit(dbDisconnect(con), add = TRUE)
  errors <- list()

  # Utility function to simplify repetitive operations
  static_plate_test <- function(validation_func, ...) {
    func_name <- deparse(substitute(validation_func))
    cat("Executing function:", func_name, "\n")
    result <- do.call(validation_func, list(con, "user_data", "RowNumber", ...))
    errors <<- add_to_errors(errors, result)
  }

  if (action == "upload") {
    validate_static_plate_uploads(static_plate_test)
  } else {
    stop("Invalid action for static plate db validations!!!")
  }

  return(errors)
}

#' Perform Database-Related Validations for Micronix
#'
#' This function conducts database-related validation checks for Micronix specimen data.
#' It initiates a database connection, then performs upload or move validations based on the action provided.
#'
#' @param database The database connection or specification for validation.
#' @param user_data The users data.
#' @param action The action being performed, either "upload" or "move".
#' @return A list containing validation errors, if any.
#' @keywords validation, micronix
perform_micronix_db_validations <- function(database, user_data, action, variable_colnames) {
  con <- init_and_copy_to_db(database, user_data)
  on.exit(dbDisconnect(con), add = TRUE)
  errors <- list()

  # Utility function to simplify repetitive operations
  micronix_test <- function(validation_func, ...) {
    func_name <- deparse(substitute(validation_func))
    cat("Executing function:", func_name, "\n")
    result <- do.call(validation_func, list(con, "user_data", "RowNumber", ...))
    errors <<- add_to_errors(errors, result)
  }

  if (action == "upload") {
    validate_micronix_uploads(micronix_test, variable_colnames)
  } else if (action == "move") {
    validate_micronix_moves(micronix_test, variable_colnames)
  }

  return(errors)
}

#' Validate Cryovial Specimen Data
#'
#' This function conducts a series of validation checks on Cryovial specimen data.
#' The function will return any errors encountered during validation.
#'
#' @param user_data A data frame containing specimen data to validate.
#' @param action The action being performed, e.g. "upload" or "move".
#' @param database The database connection or specification to use for validation.
#' @return A list containing validation errors, if any.
#' @keywords validation, cryovial
validate_cryovial <- function(user_data, action, database) {
  errors <- list()
  result <- check_unique_positions(user_data, "Position", "BoxName", "BoxBarcode")
  if (!is.null(result)) {
    errors <- add_to_errors(errors, result)
  }

  errors <- c(
    errors,
    perform_cryovial_db_validations(database, user_data, action)
  )

  return(errors)
}

#' Validate DBS Specimen Data
#'
#' This function conducts a series of validation checks on DBS specimen data.
#' The function will return any errors encountered during validation.
#'
#' @param user_data A data frame containing specimen data to validate.
#' @param action The action being performed, e.g. "upload" or "move".
#' @param database The database connection or specification to use for validation.
#' @return A list containing validation errors, if any.
#' @keywords validation, cryovial
validate_dbs_sample <- function(user_data, action, database) {

  user.container.types <- NULL
  valid.container.types <- c("Bag", "Box")

  if (action == "upload") {
    user.container.types <- unique(user_data$ContainerType)
    if (!"ContainerType" %in% colnames(user_data)) {
      stop("Implementation error: ContainerType must exist in users dbs sample upload.")
    }

    if (!all(user.container.types %in% valid.container.types)) {
      invalids <- !user_data$ContainerType %in% valid.container.types
      invalid.subset <- user_data[invalids, ]
      description <- sprintf("Rows must contain valid container types: %s", paste(valid.container.types, collapse = ", "))
      error.data <- ErrorData$new(description = description, columns = invalid.subset$ContainerType, rows = invalid.subset$RowNumber)
      stop_validation_error("Invalid Container Types detected in your upload.", error.data)
    }
  } else if (action == "move") {
    user.container.types <- unique(user_data$NewContainerType, user_data$OldContainerType)
    if (!"NewContainerType" %in% colnames(user_data) && !"OldContainerType" %in% colnames(user_data)) {
      stop("Implementation error: NewContainerType and OldContainerType must exist in users dbs sample move.")
    }

    # Check new container types for valid entries
    if (!all(user_data$NewContainerType %in% valid.container.types)) {
      invalids <- !user_data$NewContainerType %in% valid.container.types
      invalid.subset <- user_data[invalids, ]
      description <- sprintf("Rows must contain valid container types: %s", paste(valid.container.types, collapse = ", "))
      error.data <- ErrorData$new(description = description, columns = invalid.subset$NewContainerType, rows = invalid.subset$RowNumber)
      stop_validation_error("Invalid Container Types detected in your upload.", error.data)
    }

    # Check old container types for valid entries
    if (!all(user_data$OldContainerType %in% valid.container.types)) {
      invalids <- !user_data$OldContainerType %in% valid.container.types
      invalid.subset <- user_data[invalids, ]
      description <- sprintf("Rows must contain valid container types: %s", paste(valid.container.types, collapse = ", "))
      error.data <- ErrorData$new(description = description, columns = invalid.subset$OldContainerType, rows = invalid.subset$RowNumber)
      stop_validation_error("Invalid Container Types detected in your upload.", error.data)
    }
  } else {
    stop("Validation halted. Unrecognized action.")
  }

  if (!all(user.container.types %in% valid.container.types)) {
    invalids <- !user_data$ContainerType %in% valid.container.types
    invalid.subset <- user_data[invalids, ]
    description <- sprintf("Rows must contain valid container types: %s", paste(valid.container.types, collapse = ", "))
    error.data <- ErrorData$new(description = description, columns = invalid.subset$ContainerType, rows = invalid.subset$RowNumber)
    stop_validation_error("Invalid Container Types detected in your upload.", error.data)
  }

  errors <- list()
  errors <- c(
    errors,
    perform_dbs_sample_db_validations(database, user_data, action)
  )

  return(errors)
}

#' Perform Database-Related Validations for Cryovials
#'
#' This function conducts database-related validation checks for Cryovial specimen data.
#' It initiates a database connection, then performs upload or move validations based on the action provided.
#'
#' @param database The database connection or specification for validation.
#' @param user_data The users data.
#' @param action The action being performed, either "upload" or "move".
#' @return A list containing validation errors, if any.
#' @keywords validation, cryovial
perform_cryovial_db_validations <- function(database, user_data, action) {
  con <- init_and_copy_to_db(database, user_data)
  on.exit(dbDisconnect(con), add = TRUE)
  errors <- list()

  # Utility function to simplify repetitive operations
  cryovial_test <- function(validation_func, ...) {
    func_name <- deparse(substitute(validation_func))
    cat("Executing function:", func_name, "\n")
    result <- do.call(validation_func, list(con, "user_data", "RowNumber", ...))
    errors <<- add_to_errors(errors, result)
  }

  if (action == "upload") {
    validate_cryovial_uploads(cryovial_test)
  } else if (action == "move") {
    validate_cryovial_moves(cryovial_test)
  }

  return(errors)
}

#' Perform Database-Related Validations for DBS Samples
#'
#' This function conducts database-related validation checks for DBS specimen data.
#' It initiates a database connection, then performs upload or move validations based on the action provided.
#'
#' @param database The database connection or specification for validation.
#' @param user_data The users data.
#' @param action The action being performed, either "upload" or "move".
#' @return A list containing validation errors, if any.
#' @keywords validation, cryovial
perform_dbs_sample_db_validations <- function(database, user_data, action) {
  con <- init_and_copy_to_db(database, user_data)
  on.exit(dbDisconnect(con), add = TRUE)
  errors <- list()

  # Utility function to simplify repetitive operations
  dbs_sample_test <- function(validation_func, ...) {
    func_name <- deparse(substitute(validation_func))
    cat("Executing function:", func_name, "\n")
    result <- do.call(validation_func, list(con, "user_data", "RowNumber", ...))
    errors <<- add_to_errors(errors, result)
  }

  if (action == "upload") {
    validate_dbs_sample_uploads(dbs_sample_test)
  } else if (action == "move") {
    validate_dbs_sample_moves(dbs_sample_test)
  } else {
    stop("Invalid action!!!")
  }

  return(errors)
}

#' Validate Micronix Uploads
#'
#' Conducts specific validation checks for uploading Micronix specimens.
#'
#' @param micronix_test The utility function for performing validation checks.
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_micronix_uploads <- function(micronix_test, variable_colnames) {
  micronix_test(validate_matrix_container, "PlateName", "Position", "micronix_plate", "micronix_tube", error_if_exists = TRUE)
  micronix_test(check_micronix_barcodes_exist, variable_colnames[['barcode_col']], error_if_exists = TRUE)
  micronix_test(validate_study_reference_db, "StudyCode")
  micronix_test(validate_specimen_type_db, "SpecimenType")
  micronix_test(validate_location_reference_db, "FreezerName", "ShelfName", "BasketName")
  micronix_test(check_longitudinal_study_dates, "StudyCode", "CollectionDate")
}

#' Validate Micronix Moves
#'
#' Conducts specific validation checks for moving Micronix specimens.
#'
#' @param micronix_test The utility function for performing validation checks.
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_micronix_moves <- function(micronix_test, variable_colnames) {

  # todo: pass this information in
  micronix_test(check_micronix_barcodes_exist, variable_colnames[['barcode_col']], error_if_exists = FALSE)
  micronix_test(check_micronix_plate_exists, "PlateName")
  micronix_test(check_if_sample_is_archived, variable_colnames[['barcode_col']])

}

#' Validate Cryovial Uploads
#'
#' Conducts specific validation checks for uploading Cryovial specimens.
#'
#' @param con The database connection.
#' @param cryovial_test The utility function for performing tests.
#' @return A list object containing validation errors, if any.
#' @keywords validation
validate_cryovial_uploads <- function(cryovial_test) {
  cryovial_test(validate_matrix_container, "BoxName", "Position", "cryovial_box", "cryovial_tube", error_if_exists = TRUE)
  cryovial_test(check_longitudinal_study_dates, "StudyCode", "CollectionDate")
  cryovial_test(validate_longitudinal_study, "StudyCode", "StudySubject", "CollectionDate")
  cryovial_test(validate_cryovial_barcodes, "Barcode", "StudyCode", "SpecimenType")
  cryovial_test(validate_study_reference_db, "StudyCode")
  cryovial_test(validate_specimen_type_db, "SpecimenType")
  cryovial_test(validate_location_reference_db, "FreezerName", "RackName", "RackPosition")
}

#' Validate Cryovial Moves
#'
#' Conducts specific validation checks for moving Cryovial specimens.
#'
#' @param con The database connection.
#' @param cryovial_upload_test The utility function for performing tests.
#' @return A list object containing validation errors, if any.
#' @keywords validation
validate_cryovial_moves <- function(cryovial_test) {
  cryovial_test(check_each_row_cryovial_move_is_identifiable) # Checks if the cryovial can be found
  cryovial_test(check_each_row_cryovial_move_is_unique) # Checks if the cryovial is duplicated
  cryovial_test(check_cryovial_box_exists, "BoxName")
}

#' Validate Static Plate
#'
#' Conducts specific validation checks for uploading specimens in static plate wells.
#'
#' @param con The database connection.
#' @param static_plate_test The utility function for performing tests.
#' @return A list object containing validation errors, if any.
#' @keywords validation
validate_static_plate_uploads <- function(static_plate_test) {
  static_plate_test(validate_matrix_container, "BoxName", "Position", "static_plate", "static_well", error_if_exists = TRUE)
  static_plate_test(check_longitudinal_study_dates, "StudyCode", "CollectionDate")
  static_plate_test(validate_longitudinal_study, "StudyCode", "StudySubject", "CollectionDate")
  static_plate_test(validate_study_reference_db, "StudyCode")
  static_plate_test(validate_specimen_type_db, "SpecimenType")
  static_plate_test(validate_location_reference_db, "FreezerName", "RackName", "RackPosition")
}

#' Validate Micronix Uploads
#'
#' Conducts specific validation checks for uploading Micronix specimens.
#'
#' @param micronix_test The utility function for performing validation checks.
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_dbs_sample_uploads <- function(dbs_sample_test) {
  dbs_sample_test(validate_study_reference_db, "StudyCode")
  dbs_sample_test(validate_specimen_type_db, "SpecimenType")
  dbs_sample_test(validate_location_reference_db, "FreezerName", "ShelfName", "BasketName")
  dbs_sample_test(check_longitudinal_study_dates, "StudyCode", "CollectionDate")
  dbs_sample_test(validate_dbs_sample_label_uniqueness, "Label", "ContainerName", "ContainerType", error_if_exists = TRUE)
}

#' Validate DBS Specimen Moves
#'
#' Conducts specific validation checks for moving DBS Specimen specimens.
#'
#' @param con The database connection.
#' @param cryovial_upload_test The utility function for performing tests.
#' @return A list object containing validation errors, if any.
#' @keywords validation
validate_dbs_sample_moves <- function(dbs_sample_test) {
  dbs_sample_test(validate_dbs_box_bag_exists, "NewContainer", "NewContainerType")
  dbs_sample_test(validate_dbs_sample_label_uniqueness, "Label", "NewContainer", "NewContainerType", error_if_exists = TRUE)
}

#' Validate Specimens Main Function
#'
#' The `validate_specimens` function serves as a main driver for validating specimens
#' data, both for Micronix and Cryovial specimens. The process involves several key steps:
#' 
#' 1. Formatting the provided user CSV based on the sample type and action with `format_user_csv`.
#' 2. Validation based on the sample type using either `validate_micronix` or `validate_cryovial`.
#' 3. If the user action is "upload", dates in the data will be further validated using `validate_dates_with_tokens`.
#' 4. Any errors during the validation steps are accumulated and, if present, will trigger an 
#'    error message, stopping the process.
#' 5. If unknown date tokens are found during validation, these are handled using the 
#'    `handle_unknown_date_tokens` function.
#'
#' At the end of the validation process, the user data will be either ready for upload to the
#' database or returned to the user with errors.
#'
#' @param user_data Character string. The path to the user-provided CSV file to be validated.
#' @param sample_type Character string. The type of sample being validated. Expected values 
#'                    are "micronix" or "cryovial".
#' @param user_action Character string. The action being performed by the user, 
#'                    e.g. "upload".
#' @param database Character string. The database connection or specification for validation.
#'                 Default is the system environment variable "SDB_PATH".
#' 
#' @return A data frame. The validated and potentially modified user data if validation 
#'         passes. If validation does not pass, an error will be raised.
#'
#' @keywords validation
#' @seealso \code{\link{format_user_csv}}, \code{\link{validate_micronix}},
#'          \code{\link{validate_cryovial}}, \code{\link{validate_dates_with_tokens}},
#'          \code{\link{handle_unknown_date_tokens}}
#' @export
validate_specimens <- function(user_data, sample_type, user_action, file_type, database = Sys.getenv("SDB_PATH")) {

  errors <- list()
  if (sample_type == "micronix") {
    errors <- validate_micronix(user_data, user_action, file_type, database)
  } else if (sample_type == "cryovial") {
    errors <- validate_cryovial(user_data, user_action, database)
  } else if (sample_type == "dbs_sample") {
    errors <- validate_dbs_sample(user_data, user_action, database)
  } else if (sample_type == "static_plate") {
    errors <- validate_static_plate(user_data, user_action, database)
  } else {
    stop("Invalid sample type!!!")
  }

  validation_result <- NULL
  if (user_action == "upload") {
    validation_result <- validate_dates_with_tokens(user_data, "CollectionDate", "%Y-%m-%d",  c("unk", "UNK", "unknown", "UNKNOWN"))

    if (inherits(validation_result, "ErrorData")) {
      errors <- add_to_errors(errors, validation_result)
    }
  }

  # Initialize the ValidationErrorCollection with the accumulated errors and the user_data
  error_collection <- ValidationErrorCollection$new(errors, user_data)

  # If there are any errors, execute the stop route
  if (error_collection$count_errors("Error") > 0) {
    stop_validation_error("Validation error", error_collection)
  }

  # update here
  if (user_action == "upload") {
    user_data <- handle_unknown_date_tokens(user_data, "CollectionDate", validation_result$parsed_dates, validation_result$token_mask)
  }

  # If there are only warnings, return the user data with the warnings
  if (error_collection$count_errors("Warning") > 0) {
    return (list(data = user_data, warnings = error_collection))
  } else {
    return (user_data)
  }
}
