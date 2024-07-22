#' Concatenate Position from Column and Row Information
#'
#' This function takes a column (character) and a row (numeric) and returns a formatted string by concatenating the column and row values.
#'
#' @param col A character vector indicating the column.
#' @param row A numeric vector indicating the row.
#'
#' @return A character vector of the concatenated position.
#' @examples
#' concat_position("A", 5) # "A05"
#' concat_position("B", 1) # "B01"
#' concat_position("AA", 23) # "AA23"
#' @export
concat_position <- function(col, row) {
  sprintf("%s%02d", col, row)
}


#' Safely get a value from a list of possible values
#' 
#' This function will return the first column that it finds from a list of columns
#' @keywords internal
safe_extract <- function(data_row, ...) {
  potential_cols <- c(...)
  for (col in potential_cols) {
    if (col %in% names(data_row) && !is.na(data_row[[col]])) {
      return(data_row[[col]])
    }
  }
  return(NA_character_)
}

#' Validate study references (Database Version)
#'
#' This function checks if the study reference provided in the dataset exists in the database.
#' Note: This function assumes that the data is already present in the database.
#'
#' @param con A database connection object.
#' @param table_name The name of the formatted CSV table in the database.
#' @param row_number_col The name of the row number column.
#' @param study_short_code_col The name of the study short code column.
#' @param controls Whether are not we are validating controls. Controls have 'Batches' instead of studies.
#' @param error_if_exists Whether to indicate an error if a study / batch is already in the database.
#'
#' @keywords validation
#' @return ErrorData object indicating any studies not found.
validate_study_reference_db <- function(con, table_name, row_number_col, study_short_code_col, controls = FALSE, error_if_exists = FALSE) {

  # Left join with the study table
  df <- tbl(con, table_name) %>%
    left_join(tbl(con, "study"), by = setNames("short_code", study_short_code_col)) %>%
    select(all_of(c(row_number_col, study_short_code_col, "id")))
    
  # Depending on error_if_exists flag, filter the results
  if (error_if_exists) {
    df <- df %>%
      filter(!is.na(id))
    description <- ifelse(controls, "Batch already exists in the database", "Study already exists in the database")
  } else {
    df <- df %>%
      filter(is.na(id))
    description <- ifelse(controls, "Batch could not be found in the database", "Study could not be found in the database")
  }

  # Drop the id column as it's not needed for the output
  df <- df %>%
    select(-id) %>%
    collect()

  # Check if there are any resulting rows and return appropriate ErrorData
  if (nrow(df) > 0) {
    return(ErrorData$new(description = description, data_frame = df))
  }

  return(NULL)
}

#' Validate that study does not have a date for it's short code 
#' 
#' This function helps to validate that a study does not have a date for it's short code, which is reserved for control batches.
#' 
#' @param con A database connection object.
#' @param table_name The name of the formatted CSV table in the database.
#' @param row_number_col The name of the row number column.
#' @param study_short_code_col The name of the study short code column.
validate_study_short_code <- function(con, table_name, row_number, study_short_code_col) {
  df <- tbl(con, table_name) %>%
    filter(!is.na(!!sym(study_short_code_col))) %>%
    filter(grepl("\\d{4}-\\d{2}-\\d{2}", !!sym(study_short_code_col))) %>%
    select(all_of(c(row_number_col, study_short_code_col))) %>%
    collect()

  if (nrow(df) > 0) {
    return(ErrorData$new(description = "Study short code cannot contain a date", data_frame = df))
  }
  return(NULL)
} 


#' Validate specimen type references (Database Version)
#'
#' This function checks if the specimen type reference provided in the dataset exists in the database.
#' Note: This function assumes that the data is already present in the database.
#'
#' @param con A database connection object.
#' @param table_name The name of the formatted CSV table in the database.
#' @param row_number_col The name of the row number column.
#' @param specimen_type_col The name of the specimen type column.
#'
#' @keywords validation
#' @return ErrorData object indicating any specimen types not found.
validate_specimen_type_db <- function(con, table_name, row_number_col, specimen_type_col) {

  df <- tbl(con, table_name) %>%
    left_join(tbl(con, "specimen_type"), by = setNames("name", specimen_type_col)) %>%
    filter(is.na(id)) %>%
    select(all_of(c(row_number_col, specimen_type_col))) %>%
    collect()

  if (nrow(df) > 0) {
    return(ErrorData$new(description = "Specimen Type Reference Validation", data_frame = df))
  }

  return(NULL)
}

#' Validate location references (Database Version)
#'
#' This function checks if the location reference provided in the dataset exists in the database.
#' Note: This function assumes that the data is already present in the database.
#'
#' @param con A database connection object.
#' @param table_name The name of the formatted CSV table in the database.
#' @param row_number_col The name of the row number column.
#' @param name_col The name of the name column for locations.
#' @param level_I_col The name of the Level I column for locations.
#' @param level_II_col The name of the Level II column for locations.
#'
#' @keywords validation
#' @return ErrorData object indicating any locations not found.
validate_location_reference_db <- function(con, table_name, row_number_col, name_col, level_I_col, level_II_col) {

  # Directly define the join conditions using named vectors
  user_table_joins <- setNames(
    c("location_root", "level_I", "level_II"),
    c(name_col, level_I_col, level_II_col)
  )

  df <- tbl(con, table_name) %>%
    left_join(tbl(con, "location") %>%
                dplyr::rename(location_id = id),
              by = user_table_joins) %>%
    filter(is.na(location_id)) %>%
    select(all_of(c(row_number_col, name_col, level_I_col, level_II_col))) %>%
    collect()

  if (nrow(df) > 0) {
    errstring <- sprintf("The following %s, %s, and/or %s are not found in the database", name_col, level_I_col, level_II_col)
    return(ErrorData$new(description = errstring, data = df))
  }

  return(NULL)
}

#' Validate Date Format
#'
#' @description Validates the date column in the provided data frame and ensures the dates are in a recognized format.
#' @param data A data frame.
#' @param date_col The name of the date column.
#' @param allowed_date_formats A character vector of allowed date formats.
#' @return An ErrorData object or NULL if no errors.
validate_date_format <- function(data, date_col, allowed_date_formats) {

  # Parse the collection dates
  parsed_dates <- lubridate::parse_date_time(data[[date_col]], allowed_date_formats, quiet = TRUE, exact = TRUE)
  
  # Find rows with invalid dates
  invalid_rows <- which(!is.na(data[[date_col]]) & is.na(parsed_dates))
  
  if (length(invalid_rows) > 0) {
    return(ErrorData$new(description = sprintf("Unrecognized strings found in collection date column. Use recognized date formats: %s.", paste(allowed_date_formats, collapse = ", ")), columns = date_col, rows = invalid_rows))
  }
  return(NULL) 
}

#' Validate Dates with Tokens
#'
#' @description Validates the date column in the provided data frame. It ensures the dates are in a recognized format or a set of allowed tokens indicating the date is unknown.
#' @param data A data frame.
#' @param date_col The name of the date column.
#' @param allowed_date_formats A character vector of allowed date formats.
#' @param tokens Recognized tokens indicating unknown dates.
#' @return A list with elements `error_data` (an ErrorData object or NULL if no errors), `parsed_dates` (a vector of parsed dates), and `token_mask` (a logical mask identifying rows with recognized tokens indicating unknown dates).
validate_dates_with_tokens <- function(data, date_col, allowed_date_formats, tokens) {
  
  # Parse the collection dates
  parsed_dates <- lubridate::parse_date_time(data[[date_col]], allowed_date_formats, quiet = TRUE, exact = TRUE)
  
  # Mask for recognized tokens
  token_mask <- !data[[date_col]] %in% tokens

  # Find rows with invalid dates
  invalid_rows <- which(!is.na(data[[date_col]]) & is.na(parsed_dates) & token_mask)
  
  error_data <- NULL
  if (length(invalid_rows) > 0) {
    string <- paste("Unrecognized strings found in collection date column. Use recognized date formats or add any of the following if the collection date is unknown:", paste(tokens, collapse=", "))
    return(ErrorData$new(description = string, columns = date_col, rows = invalid_rows))
  }

  return(list(parsed_dates = parsed_dates, token_mask = token_mask))
}


#' Handle Unknown Date Tokens
#'
#' @description Adjusts the date column for rows where the user signified with tokens that the dates are unknown.
#' @param data A data frame.
#' @param date_col The name of the date column.
#' @param parsed_dates A vector of parsed dates.
#' @param token_mask A logical mask identifying rows with recognized tokens indicating unknown dates.
#' @keywords utils
#' @return The adjusted data frame.
handle_unknown_date_tokens <- function(data, date_col, parsed_dates, token_mask) {
  data[[date_col]] <- parsed_dates
  data[[date_col]][!token_mask] <- rep(lubridate::origin, sum(!token_mask))
  data[[date_col]] <- as.character(lubridate::as_date(data[[date_col]]))

  return(data)
}

#' Check for missing data in required fields
#'
#' @param formatted_csv The user provided dataframe to check.
#' @param col_attributes A named list of expected column attributes.
#' @return An ErrorData object if any required data is missing, otherwise NULL.
check_missing_data <- function(formatted_csv, col_attributes) {

  # Ensure that data frame remains intact with the 'drop = FALSE' argument
  subset_data <- formatted_csv[, col_attributes$required, drop = FALSE]
  missing_rows <- which(rowSums(is.na(subset_data)) > 0)
  missing_cols <- colnames(subset_data)[colSums(is.na(subset_data)) > 0]

  if (length(missing_cols) > 0) {
    description <- "Rows found with missing data"
    return(ErrorData$new(description = description, columns = missing_cols, rows = missing_rows))
  }

  return(NULL)
}

#' Check for duplicated rows in a data frame
#'
#' @param formatted_csv The user-provided data frame to check for duplicated rows.
#' @param col_attributes A named list of expected column attributes. If you want to check only specific columns for duplicates, include their names here. Otherwise, it will check all columns.
#' @return An ErrorData object if any duplicated rows are found, otherwise NULL.
check_duplicated_rows <- function(formatted_csv, col_attributes = NULL) {

  # If col_attributes is not NULL, only check duplicates based on those columns
  if (!is.null(col_attributes)) {
    subset_data <- formatted_csv[, col_attributes, drop = FALSE]
  } else {
    subset_data <- formatted_csv
  }

  # Find duplicates
  duplicate_rows <- which(duplicated(subset_data) | duplicated(subset_data, fromLast = TRUE))

  if (length(duplicate_rows) > 0) {
    description <- "Rows found with duplicate data"
    return(ErrorData$new(description = description, columns = colnames(subset_data), rows = duplicate_rows))
  }

  return(NULL)
}


#' Add row numbers to a dataframe
#'
#' @param df A dataframe.
#' @return A dataframe with an additional 'RowNumber' column.
#' @keywords utility
#' @export
add_row_numbers <- function(df, row_number_col = "RowNumber") {
  df %>% dplyr::mutate(!!sym(row_number_col) := row_number())
}

#' Add a position column based on column attributes
#'
#' @param df A dataframe to modify.
#' @param col_attributes A named list of column attributes for exceptions.
#' @return A dataframe, potentially with a new 'position' column.
#' @keywords utility
#' @export
add_position_column <- function(df, col_attributes) {
  position_keys <- col_attributes$position

  # No position specified
  if (is.null(position_keys)) {
    return(df)
  }

  # When position is a single column
  if (is.character(position_keys)) {
    df %>%
      dplyr::mutate(position = df[[position_keys]])
  }
  # When position is split across two columns
  else if (length(position_keys) == 2) {
    position1_key <- position_keys[1]
    position2_key <- position_keys[2]

    df %>%
      dplyr::mutate(
        position = sprintf(
          "%s%02d",
          df[[position1_key]],
          as.integer(df[[position2_key]])
        )
      )
  } else {
    stop("Unexpected structure for position keys.")
  }
}

#' Add an Error to the Error List
#'
#' This utility function checks if an error exists and, if so, appends it to the provided error list.
#'
#' @param errors A list object that contains the current errors.
#' @param err An error object to be added to the error list. It can be NULL, in which case no action is taken.
#'
#' @return The updated errors list.
#' @examples
#' \dontrun{
#'   errors <- list()  
#'   new_error <- list(description = "Some error")  # Hypothetical error structure.
#'   errors <- add_to_errors(errors, new_error)
#' }
#' @export
add_to_errors <- function(errors, err) {

  if (is.null(err)) {
    return(errors)
  }

  if (!is.null(err) && is.list(err)) {
    for (err.obj in err) {
      if (!is.null(err.obj) && is.null(err.obj$description)) {
        stop("Error object must have a description field.")
      }

      if (!is.null(err.obj) && !is.null(err.obj$description)) {  # Check that error data is present
        errors[[length(errors) + 1]] <- err.obj
      }
    }
  } else {
    if (!is.null(err) && is.null(err$description)) {
      stop("Error object must have a description field.")
    }

    if (!is.null(err) && !is.null(err$description)) {  # Check that error data is present
      errors[[length(errors) + 1]] <- err
    }
  }

  return(errors)
}

#' Get the Relevant Table Name Based on Container Class
#'
#' This function returns the name of the relevant database table
#' based on the provided container class.
#'
#' @param container_class A string representing the container class. 
#'   Currently supports "micronix_tube" and "cryovial_tube".
#'
#' @return A string representing the name of the relevant table.
#'   Will stop and throw an error for invalid or unsupported container classes.
#'
#' @examples
#' get_container_table_name("micronix_tube") # Should return "micronix_plate"
#' get_container_table_name("cryovial_tube") # Should return "cryovial_plate"
#'
#' @export
get_container_table_name <- function(container_class) {
  switch(container_class,
         micronix_tube = "micronix_plate", # replace with actual table name
         cryovial_tube = "cryovial_plate", # replace with actual table name
         stop("Invalid container_class")
  )
}

#' Find First Matching Column Name
#'
#' This function checks for the presence of potential column names within a data frame 
#' and returns the first one that matches, or NA_character_ if none is found.
#'
#' @param data A data frame or tibble.
#' @param potential_names A character vector containing potential column names to search for.
#'
#' @return A string representing the first matching column name, or NA_character_ if none is found.
#' @examples
#' df <- data.frame(
#'   Barcode = 1:5,
#'   FreezerName = 6:10
#' )
#' find_column_name(df, c("Barcode", "Tube ID", "TubeCode"))
#' find_column_name(df, c("FreezerName", "FreezerName"))
#'
#' @export
find_column_name <- function(data, potential_names) {
  intersect(colnames(data), potential_names)[1] %||% NA_character_
}

#' Get normalized path
#'
#' @param site_install Logical indicating whether installation is site-wide.
#' @param pkgname Character string of the package name.
#' @param dir_type Character string specifying the type of directory. Valid values are "config" and "data".
#' @param filename File to add.
#'
#' @return A character string of the normalized path.
#' @export
get_normalized_path <- function(site_install, pkgname, dir_type, filename) {
  stopifnot(dir_type %in% c("config", "data"))
  
  base_dir <- switch(
    dir_type,
    config = ifelse(site_install, rappdirs::site_config_dir(), rappdirs::user_config_dir()),
    data = ifelse(site_install, rappdirs::site_data_dir(), rappdirs::user_data_dir())
  )
  
  suppressWarnings(normalizePath(file.path(base_dir, pkgname, filename)))
}


#' Get environ file path
#'
#' @param site_install Logical indicating whether installation is site-wide.
#'
#' @return A character string of the environment file path.
get_environ_file_path <- function(site_install) {
  suppressWarnings(
    normalizePath(
      ifelse(site_install,
             file.path(Sys.getenv("R_HOME"), "etc", "Renviron.site"),
             file.path(Sys.getenv("HOME"), ".Renviron")
      )
    )
  )
}


#' Find duplicate rows in a data frame
#'
#' @param data A data frame.
#' @param cols A character vector of column names to check for duplicates.
get_duplicated_rows <- function(data) {
  data[duplicated(data), ]
}

#' Normalize Percentages by Group
#' 
#' Normalize percentage column by groups so that 


#' Extract unique compositions from user data
#'
#' This function processes user data to generate a unique composition key based on sorted strains and percentages.
#'
#' @param user_data A data frame with at least two columns: 'Strains' and 'Percentages', each containing semicolon-separated values (for polyclonal controls).
#'
#' @return A data frame containing sorted_strains_key for each row of user data.
#' @export
get_unique_compositions_from_user_data <- function(user_data) {
  user_data %>%
    rowwise() %>%
    mutate(split_data = list(split_and_sort(Strains, Percentages))) %>%
    ungroup() %>%
    mutate(
      sorted_strains_key = map_chr(split_data, ~paste(.x$sorted_strains, collapse = "-")),
      sorted_percentages = map(split_data, ~.x$sorted_percentages),
      composition_num = row_number()
    ) %>%
    select(sorted_strains_key, sorted_percentages, Strains, Percentages, LegacyLabel) %>%
    distinct()
}
