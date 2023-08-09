#' Convert string values with 'k' or 'K' to numeric values
#'
#' @param data A data frame with a column named 'density'.
#'
#' @keywords compositions
#' @return A list with the processed data and potential ErrorData.
convert_density_to_numeric <- function(data) {
  vb <- grepl('k|K', data$density)
  data$density[vb] <- str_replace(data$density[vb], "k|K", "")

  if (any(!grepl("^\\d+$", data$density))) {
    error_rows <- which(!grepl("^\\d+$", data$density))
    error_data <- ErrorData$new(
      description = "Invalid format in density column",
      columns = "density",
      rows = error_rows
    )
    return(list(data = NULL, error = error_data))
  }

  data$density <- as.integer(data$density)
  data$density[vb] <- as.integer(data$density[vb] * 1000)

  return(list(data = data, error = NULL))
}

#' Split and unnest the strain and percentage columns
#'
#' @param data A data frame with columns named 'strain' and 'percentage'.
#'
#' @keywords compositions
#' @return A list with the processed data and potential ErrorData.
split_and_unnest_data <- function(data) {
  # Find discrepancies in number of splits
  discrepancies <- sapply(1:nrow(data), function(i) {
    length(unlist(strsplit(as.character(data$strain[i]), ";"))) != length(unlist(strsplit(as.character(data$percentage[i]), ";")))
  })

  if (any(discrepancies)) {
    error_rows <- which(discrepancies)
    error_data <- ErrorData$new(
      description = "Mismatched count of semicolons between strain and percentage columns",
      columns = c("strain", "percentage"),
      rows = error_rows
    )
    return(list(data = NULL, error = error_data))
  }

  processed_data <- data %>%
    dplyr::mutate(
      strain2 = strsplit(strain, ";"),
      percentage2 = strsplit(percentage, ";")
    ) %>%
    tidyr::unnest(cols = c(strain2, percentage2))

  return(list(data = processed_data, error = NULL))
}

#' Validate strains recorded in the database
#'
#' @param con A database connection object.
#' @param table_name The name of the table in the database that contains the user data.
#' @param strain_col The column name for strains.
#' @param row_number_col The column name for row numbers.
#' @param check_existence Logical. If TRUE, checks if the strain exists in the database. If FALSE, checks if it's absent.
#'
#' @keywords validation
#' @return An instance of the ErrorData class or NULL.
validate_strains_in_database <- function(con, table_name, strain_col, row_number_col, check_existence = TRUE) {
  # Create a joined table between user data and strain data
  df <- tbl(con, table_name) %>%
      left_join(tbl(con, "strain"), by = setNames(strain_col, "name"))

  # Check if strain exists or not based on check_existence parameter
  if (check_existence) {
    df <- df %>%
      filter(is.na(id)) %>%
      select(!!sym(row_number_col), !!sym(strain_col)) %>%
      distinct() %>%
      collect()

    error_description <- "Strain found that is not recorded in the database"
  } else {
    df <- df %>%
      filter(!is.na(id)) %>%
      select(!!sym(row_number_col), !!sym(strain_col)) %>%
      distinct() %>%
      collect()

    error_description <- "Strain found that is already recorded in the database"
  }

  # Return an ErrorData object if issues are found, otherwise return NULL
  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = error_description,
      columns = c(row_number_col, strain_col),
      rows = df[[row_number_col]]
    ))
  }

  return(NULL)
}

#' Validate if control compositions sum correctly
#'
#' @param user_data A data frame containing the user data.
#' @param percentage_col The column name for percentage in user_data.
#' @param row_number_col The column name for row numbers in user_data.
#' @param tolerance Tolerance for sum of control compositions.
#'
#' @keywords validation
#' @return An instance of the ErrorData class or NULL.
validate_composition_sum <- function(user_data, percentage_col, row_number_col, tolerance = 0.02) {
  df = user_data %>%
      select(!!sym(row_number_col), !!sym(percentage_col)) %>%
      group_by(!!sym(row_number_col)) %>%
      dplyr::mutate(percentage=as.numeric(!!sym(percentage_col))) %>%
      dplyr::mutate(percentage=ifelse(is.na(percentage), 0, percentage)) %>%
      dplyr::mutate(
        perc_sum = sum(percentage, na.rm=TRUE),
        equals_1 = as.logical(perc_sum >= 1.0 - tolerance && perc_sum <= 1.0) # tolerance added
      ) %>%
      filter(equals_1 == FALSE) %>%
      ungroup() %>%
      select(!!sym(row_number_col), !!sym(percentage_col)) %>%
      distinct()

  if (nrow(df) > 0) {
    errmsg <- sprintf("Sum composition sums are not within the permissable range[%d, %d]", 1.0 - tolerance, 1.0)
    return(ErrorData$new(
      description = errmsg,
      columns = c(row_number_col, percentage_col),
      rows = df[[row_number_col]]
    ))
  }

  return(NULL)
}

#' Validate References
#'
#' The main function to validate references. It can handle different types of references.
#' After validation, it will either return the user data or stop the process if errors are found.
#'
#' @param database The database connection or specification for validation.
#' @param formatted_csv The user-provided CSV file to be validated.
#' @param reference_type The type of reference being validated: "strains" or "compositions".
#' @param user_action The action being performed, e.g. "upload".
#' @return The user data if validation passes or an error if not.
#' @export
#' @keywords validation
validate_references <- function(database, formatted_csv, reference_type, user_action) {
  user_data <- add_row_numbers(formatted_csv)
  errors <- ErrorDataList$new(user_data)

  if (reference_type == "strains") {

    # Check for required columns
    columns <- get_reference_file_columns(reference_type, user_action)
    add_to_errors(errors, check_missing_data(user_data, columns))

    # Do database validation
    con <- init_and_copy_to_db(database, user_data)
    errors$merge(validate_strains(con, "user_data"))

  } else if (reference_type == "compositions") {

    # Check for required columns
    columns <- get_reference_file_columns(reference_type, user_action)
    add_to_errors(errors, check_missing_data(user_data, columns))

    # Make sure the composition percentage sum is within the tolerance threshold
    err_composition <- validate_composition_sum(formatted_csv, "percentage", "RowNumber", tolerance = 0.02)
    if (!is.null(err_composition)) {
      errors$add_error(err_composition)
    }

    # Do database validation
    con <- init_and_copy_to_db(database, user_data)
    errors$merge(validate_compositions(con, "user_data"))
  }

  if (errors$length() > 0) {
    stop_validation_error("Validation error", errors)
  }

  return(user_data)
}

#' Validate Strains Data
#'
#' This function conducts a series of validation checks on strain data.
#' The function will return any errors encountered during validation.
#'
#' @param con The database connection.
#' @param user_data A data frame containing strain data to validate.
#' @return An ErrorDataList object containing validation errors, if any.
#' @export
#' @keywords validation
validate_strains <- function(con, user_data) {
  errors <- ErrorDataList$new(user_data)

  # Check that the strain does not exist
  err_strain_name <- validate_strains_in_database(con, "strains", strain_col, row_number_col, check_existence = FALSE)
  if (!is.null(err_strain_name)) {
    errors$add_error(err_strain_name)
  }

  # Add more strain-specific validations as needed

  return(errors)
}

#' Validate Compositions Data
#'
#' This function conducts a series of validation checks on composition data.
#' The function will return any errors encountered during validation.
#'
#' @param con The database connection.
#' @param user_data A data frame containing composition data to validate.
#' @return An ErrorDataList object containing validation errors, if any.
#' @export
#' @keywords validation
validate_compositions <- function(con, user_data) {
  errors <- ErrorDataList$new(user_data)

  # Check that the strain does exist
  err_strain_name <- validate_strains_in_database(con, "strains", strain_col, row_number_col, check_existence = TRUE)
  if (!is.null(err_strain_name)) {
    errors$add_error(err_strain_name)
  }


  # Add more composition-specific validations as needed

  return(errors)
}
