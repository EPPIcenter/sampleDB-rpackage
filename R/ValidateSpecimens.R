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
check_micronix_plate_exists <- function(con, table_name, row_number_col, plate_name_col, plate_barcode_col) {
  
  user_table_joins <- setNames(
    c("barcode", "name"),
    c(plate_barcode_col, plate_name_col)
  )

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


#' Check If Cryovial Box Exists in the Database
#'
#' This function checks whether the cryovial containers provided in a user-uploaded table
#' exist in the main cryovial tube database.
#'
#' @param con A database connection object.
#' @param table_name The name of the table containing user-uploaded data.
#' @param row_number_col The name of the column in the user-uploaded table that contains row numbers.
#' @param box_name_col The name of the column in the user-uploaded table that contains the cryovial container names.
#' @param box_barcode_col The name of the column in the user-uploaded table that contains the manifest barcodes linked with the cryovial.
#'
#' @return An instance of the ErrorData class if errors are found, or NULL if there are no errors.
#' @keywords validation, cryovial
#' @export
check_cryovial_box_exists <- function(con, table_name, row_number_col, box_name_col, box_barcode_col) {
  
  # Directly define the join conditions using named vectors
  user_table_joins <- setNames(
    c("barcode", "name"),
    c(box_barcode_col, box_name_col)
  )
  
  df <- tbl(con, table_name) %>%
    left_join(tbl(con, "cryovial_box"), by = user_table_joins) %>%
    filter(is.na(id)) %>%
    select(all_of(c(row_number_col, box_name_col))) %>%
    collect()

  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = "Cryovial container not found",
      columns = c(row_number_col, box_name_col, box_barcode_col),
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


#' Check Cryovial Barcodes in Database
#'
#' This function checks whether Cryovial barcodes provided in a user-uploaded table
#' exist or don't exist (based on `error_if_exists` parameter) in the main database.
#'
#' @param con A database connection object.
#' @param user_data The name of the table containing user-uploaded data.
#' @param row_number_col The name of the column in the user-uploaded table that contains row numbers.
#' @param cryovial_col The name of the column in the user-uploaded table that contains the Cryovial barcodes.
#' @param error_if_exists A logical value. If TRUE, function returns error if barcode exists in the database, 
#' if FALSE, function returns error if barcode doesn't exist.
#'
#' @return An instance of the ErrorData class if errors are found, or NULL if there are no errors.
#' @keywords validation, cryovial
#' @export
check_cryovial_barcodes_exist <- function(con, user_data, row_number_col, cryovial_col, error_if_exists=TRUE) {
  
  df <- tbl(con, user_data) %>%
    left_join(tbl(con, "cryovial_tube"), by = setNames("barcode", cryovial_col))
  
  if (error_if_exists) {
    df <- df %>% filter(!is.na(id))
    error_desc <- "Cryovial barcode already exists in database"
  } else {
    df <- df %>% filter(is.na(id))
    error_desc <- "Cryovial barcode not found in database"
  }
  
  df <- df %>% select(all_of(c(row_number_col, cryovial_col))) %>% collect()

  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = error_desc,
      columns = c(row_number_col, cryovial_col),
      rows = df[[row_number_col]]
    ))
  }

  return(NULL)
}

#' Check if Cryovial barcodes in the move file exist in the database
#'
#' @param con A database connection object.
#' @param user_data The name of the table where the user data is temporarily stored in the database.
#' @param row_number_col The column with the row number in the `user_data`.
#' @param cryovial_col The column with Cryovial barcodes in the `user_data`.
#' @param error_if_exists Logical. If TRUE, an error is returned if the barcode exists in the database.
#'
#' @keywords validation, cryovial
#' @return An instance of the ErrorData class or NULL.
check_cryovial_barcodes_exist <- function(con, user_data, row_number_col, cryovial_col, error_if_exists=TRUE) {
  df <- tbl(con, user_data) %>%
    filter(!is.na(!!sym(cryovial_col))) %>%
    left_join(tbl(con, "cryovial_tube"), by = setNames("barcode", cryovial_col))
  
  if (error_if_exists) {
    df <- df %>% filter(!is.na(id))
    error_desc <- "Cryovial barcode already exists in database"
  } else {
    df <- df %>% filter(is.na(id))
    error_desc <- "Cryovial barcode not found in database"
  }
  
  df <- df %>% select(!!sym(row_number_col), !!sym(cryovial_col)) %>% collect()

  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = error_desc,
      columns = c(row_number_col, cryovial_col),
      rows = df[[row_number_col]]
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
#'
#' @return ErrorData object or NULL if no errors found.
#' @keywords validation, cryovial
validate_non_longitudinal_study_subjects <- function(con, table_name, row_number_col, study_short_code_col, study_subject_col) {
  
  # Setup joins
  study_joins <- setNames(c("short_code"), c(study_short_code_col))
  study_subject_joins <- setNames(c("name"), c(study_subject_col))

  df <- tbl(con, table_name) %>%
    inner_join(tbl(con, "study"), by = study_joins, suffix = c("", "_study")) %>%
    inner_join(tbl(con, "study_subject"), by = study_subject_joins, suffix = c("", "_study_subject")) %>%
    filter(is_longitudinal == 0 & !is.na(id_study_subject)) %>%
    collect()

  if(nrow(df) > 0) {
    return(ErrorData$new(data_frame = df, description = "Study subjects must be unique in studies that are not longitudinal"))
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

#' Micronix Occupied Well Validation
#'
#' This function checks if the provided sample in the Micronix dataset is being uploaded to a well with an existing sample.
#'
#' @param con A database connection object.
#' @param table_name The name of the formatted CSV table in the database.
#' @param row_number_col The column name representing the row number in `table_name`.
#' @param position_col The column name representing the position in `table_name`.
#' @param container_name_col The column name representing the Micronix container name in `table_name`.
#' @param container_barcode_col The column name representing the Micronix container barcode in `table_name`.
#'
#' @return An object of class ErrorData. If there are errors, NULL is returned.
#' @export
#' @keywords validation, micronix

validate_micronix_tube_exists <- function(con, table_name, row_number_col, position_col, container_name_col, container_barcode_col) {
  
  # Directly define the join conditions using named vectors
  user_table_joins <- setNames(
    c("container_barcode", "container_name", "position"),
    c(container_barcode_col, container_name_col, position_col)
  )

  df <- tbl(con, "storage_container") %>%
    select(status_id, id) %>%
    filter(status_id != 1) %>%  # Changed this condition to find occupied wells
    inner_join(tbl(con, "micronix_tube"), by = c("id" = "id")) %>%
    inner_join(tbl(con, "micronix_plate") %>%
      dplyr::rename(
        container_barcode = barcode,
        container_name = name
      ), by = c("manifest_id" = "id")
    ) %>%
    inner_join(tbl(con, table_name), by = user_table_joins) %>% 
    select(all_of(c(row_number_col, position_col, container_name_col))) %>%
    collect()

  if (nrow(df) > 0) {
    error_message <- "The Micronix sample you specified does not exist in the database."
    return(ErrorData$new(description = error_message, data = df))
  }

  return(NULL)
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
validate_empty_cryovial_well_upload <- function(con, table_name, row_number_col, position_col, container_name_col, container_barcode_col) {

  # Directly define the join conditions using named vectors
  user_table_joins <- setNames(
    c(container_barcode_col, container_name_col, position_col),
    c("container_barcode", "container_name", "position") # some of these are from renames!!!
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

#' Cryovial Occupied Well Validation
#'
#' This function checks if the provided sample in the Cryovial dataset is being uploaded to a well with an existing sample.
#'
#' @param con A database connection object.
#' @param table_name The name of the formatted CSV table in the database.
#' @param row_number_col The column name representing the row number in `table_name`.
#' @param position_col The column name representing the position in `table_name`.
#' @param container_name_col The column name representing the Cryovial container name in `table_name`.
#' @param container_barcode_col The column name representing the Cryovial container barcode in `table_name`.
#'
#' @return An object of class ErrorData. If there are errors, NULL is returned.
#' @export
#' @keywords validation, cryovial

validate_cryovial_tube_exists <- function(con, table_name, row_number_col, position_col, container_name_col, container_barcode_col) {
  
  # Directly define the join conditions using named vectors
  user_table_joins <- setNames(
    c("container_barcode", "container_name", "position"),
    c(container_barcode_col, container_name_col, position_col)
  )

  df <- tbl(con, "cryovial_container") %>% # Assuming the table name is cryovial_container
    select(status_id, id) %>%
    filter(status_id != 1) %>%  # Finding occupied wells
    inner_join(tbl(con, "cryovial_tube"), by = c("id" = "id")) %>%  # Assuming the table name is cryovial_tube
    inner_join(tbl(con, "cryovial_plate") %>%  # Assuming the table name is cryovial_plate
      dplyr::rename(
        container_barcode = barcode,
        container_name = name
      ), by = c("manifest_id" = "id")
    ) %>%
    inner_join(tbl(con, table_name), by = user_table_joins) %>% 
    select(all_of(c(row_number_col, position_col, container_name_col))) %>%
    collect()

  if (nrow(df) > 0) {
    error_message <- "The Cryovial sample you specified does not exist in the database."
    return(ErrorData$new(description = error_message, data = df))
  }

  return(NULL)
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

  result <- validate_micronix_position(user_data, "Position", "RowNumber")
  if (!is.null(result)) {
    errors <- add_to_errors(errors, result)
  }

  errors <- c(
    errors,
    perform_micronix_db_validations(database, user_data, action, variable_colnames)
  )

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
  perform_cryovial_db_validations(database, user_data, action)
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

#' Validate Micronix Uploads
#'
#' Conducts specific validation checks for uploading Micronix specimens.
#'
#' @param micronix_test The utility function for performing validation checks.
#' @return A list containing validation errors, if any.
#' @export
#' @keywords validation
validate_micronix_uploads <- function(micronix_test, variable_colnames) {

  micronix_test(check_micronix_barcodes_exist, variable_colnames[['barcode_col']], error_if_exists = TRUE)
  micronix_test(validate_study_reference_db, "StudyCode")
  micronix_test(validate_specimen_type_db, "SpecimenType")
  micronix_test(validate_location_reference_db, "FreezerName", "ShelfName", "BasketName")

  micronix_test(check_longitudinal_study_dates, "StudyCode", "CollectionDate")
  micronix_test(validate_empty_micronix_well_upload,  "Position", "PlateName", "PlateBarcode")
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
  micronix_test(check_micronix_plate_exists, "PlateName", "PlateBarcode")
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
  cryovial_test(check_longitudinal_study_dates, "StudyCode", "CollectionDate")
  cryovial_test(validate_non_longitudinal_study_subjects, "StudyCode", "StudySubject")
  cryovial_test(validate_longitudinal_study, "StudyCode", "StudySubject", "CollectionDate")
  cryovial_test(validate_cryovial_collection_dates, "StudyCode", "StudySubject", "Barcode", "CollectionDate")
  cryovial_test(validate_study_reference_db, "StudyCode")
  cryovial_test(validate_specimen_type_db, "SpecimenType")
  cryovial_test(validate_location_reference_db, "FreezerName", "RackName", "RackPosition")
  cryovial_test(validate_empty_cryovial_well_upload, "Position", "BoxName", "BoxBarcode")
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
  cryovial_test(check_cryovial_barcodes_exist, "Barcode", error_if_exists = FALSE)
  cryovial_test(check_cryovial_box_exists, "BoxName", "BoxBarcode")
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

  if (error_collection$length() > 0) {
    stop_validation_error("Validation error", error_collection)
  }

  # update here
  user_data <- handle_unknown_date_tokens(user_data, "CollectionDate", validation_result$parsed_dates, validation_result$token_mask)

  return(user_data)
}
