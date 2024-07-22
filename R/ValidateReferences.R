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

#' Validate strains recorded in the database
#'
#' @param con A database connection object.
#' @param table_name The name of the table in the database that contains the user data.
#' @param strain_col The column name for strains.
#' @param row_number_col The column name for row numbers.
#' @param error_select Select which column to report with errors. The column used here will depend on what data you want the user to see regarding strains.
#' @param error_if_exists Logical. If TRUE, checks if the strain exists in the database. If FALSE, checks if it's absent.
#'
#' @keywords validation
#' @return An instance of the ErrorData class or NULL.
validate_strains_in_database <- function(con, table_name, row_number_col, strain_col, error_select, error_if_exists = TRUE) {

  df <- tbl(con, table_name) %>%
    left_join(tbl(con, "strain"), by = setNames("name", strain_col))

  if (error_if_exists) {
    df <- df %>%
      filter(!is.na(id)) %>%
      select(!!sym(row_number_col), !!sym(error_select)) %>%
      distinct() %>%
      collect()

    error_description <- "Strain(s) found that are already recorded in the database"
  } else {
    df <- df %>%
      filter(is.na(id)) %>%
      select(!!sym(row_number_col), !!sym(error_select)) %>%
      distinct() %>%
      collect()

    error_description <- "Strain(s) found that are not recorded in the database"
  }

  # Return an ErrorData object if issues are found, otherwise return NULL
  if (nrow(df) > 0) {
    return(ErrorData$new(
      description = error_description,
      columns = c(row_number_col, error_select),
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
validate_composition_sum <- function(user_data, row_number_col, strains_col, percentage_col, tolerance = 2.0) {
   df = user_data %>%
      select(!!sym(row_number_col), !!sym(strains_col), !!sym(percentage_col)) %>%
      group_by(!!sym(row_number_col)) %>%
      dplyr::mutate(
        perc_sum = sum(!!sym(percentage_col)),
        equals_100 = as.logical(perc_sum >= 100.0 - tolerance & perc_sum <= 100.0) # tolerance added
      ) %>%
      filter(equals_100 == FALSE) %>%
      ungroup() %>%
      select(all_of(c(row_number_col, strains_col, percentage_col))) %>%
      distinct()

  if (nrow(df) > 0) {
    errmsg <- sprintf("Sum composition sums are not within the permissable range ([%.2f, %.2f])", 100.0 - tolerance, 100.0)
    return(ErrorData$new(
      description = errmsg,
      columns = c(row_number_col, percentage_col),
      rows = df[[row_number_col]]
    ))
  }

  return(NULL)
}

#' Validate Strains Data
#'
#' This function conducts a series of validation checks on Strains data.
#' The function will return any errors encountered during validation.
#'
#' @param user_data A data frame containing control data to validate.
#' @param action The action being performed, either "create" or "extraction".
#' @param database The database connection or specification to use for validation.
#' @return A list containing validation errors, if any.
#' @keywords validation, strain
validate_strain <- function(user_data, action, database) {
  return(perform_strain_db_validations(database, user_data, action))
}

#' Perform Database-Related Validations for Strains
#'
#' This function conducts database-related validation checks for Strains data.
#' It initiates a database connection, then performs create or extraction validations based on the action provided.
#'
#' @param database The database connection or specification for validation.
#' @param user_data The users data.
#' @param action The action being performed, either "create" or "extraction".
#' @return A list containing validation errors, if any.
#' @keywords validation, strain
perform_strain_db_validations <- function(database, user_data, action) {

  con <- init_and_copy_to_db(database, user_data)
  on.exit(dbDisconnect(con), add = TRUE)
  errors <- list()

  # Utility function to simplify repetitive operations
  strain_test <- function(validation_func, ...) {
    func_name <- deparse(substitute(validation_func))
    cat("Executing function:", func_name, "\n")
    result <- do.call(validation_func, list(con, "user_data", "RowNumber", ...))
    errors <<- add_to_errors(errors, result)
  }

  strain_test(validate_strains_in_database, "Strains", error_select = "Strains", error_if_exists = TRUE)

  return(errors)
}

#' Validate Compositions Data
#'
#' This function conducts a series of validation checks on Compositions data.
#' The function will return any errors encountered during validation.
#'
#' @param user_data A data frame containing control data to validate.
#' @param action The action being performed, either "create" or "extraction".
#' @param database The database connection or specification to use for validation.
#' @return A list containing validation errors, if any.
#' @keywords validation, composition
validate_composition <- function(user_data, action, database) {

  errors <- list()
  #todo: expose this
  tolerance <- 2.0

  result <- validate_composition_sum(user_data, "RowNumber", "StrainsLong", "PercentagesLong", tolerance)

  if (!is.null(result)) {
    errors <- add_to_errors(errors, result)
  }

  errors <- c(
    errors,
    perform_composition_db_validations(database, user_data, action)
  )

  return(errors)
}

#' Perform Database-Related Validations for Compositions
#'
#' This function conducts database-related validation checks for Compositions data.
#' It initiates a database connection, then performs create or extraction validations based on the action provided.
#'
#' @param database The database connection or specification for validation.
#' @param user_data The users data.
#' @param action The action being performed, either "create" or "extraction".
#' @return A list containing validation errors, if any.
#' @keywords validation, composition
perform_composition_db_validations <- function(database, user_data, action) {
  con <- init_and_copy_to_db(database, user_data)
  on.exit(dbDisconnect(con), add = TRUE)
  errors <- list()
  # Utility function to simplify repetitive operations
  composition_test <- function(validation_func, ...) {
    func_name <- deparse(substitute(validation_func))
    cat("Executing function:", func_name, "\n")
    result <- do.call(validation_func, list(con, "user_data", "RowNumber", ...))
    errors <<- add_to_errors(errors, result)
  }

  composition_test(validate_strains_in_database, "StrainsLong", error_select = "Strains", error_if_exists = FALSE)

  return(errors)
}

#' Validate References
#'
#' @param database A database connection or reference.
#' @param formatted_csv The CSV data formatted for validation.
#' @param reference_type The type of reference ('strains' or 'compositions').
#' @param user_action The user action taken on the control specimen ('extraction' or 'create').
#'
#' @return nothing
#' @export
validate_references <- function(database, user_data, reference_type, user_action) {
  errors <- NULL

  if (reference_type == "strains") {
    errors <- validate_strain(user_data, user_action, database)
  } else if (reference_type == "compositions") {
    errors <- validate_composition(user_data, user_action, database)
  } else if (reference_type == "batch") {
    errors <- validate_batch(user_data, user_action, database)
  }

  # Initialize the ValidationErrorCollection with the accumulated errors and the user_data
  error_collection <- ValidationErrorCollection$new(errors, user_data)

  if (error_collection$length() > 0) {
    stop_validation_error("Validation error", error_collection)
  }
}


#' Validate Batch Data
#'
#' This function conducts a series of validation checks on Strains data.
#' The function will return any errors encountered during validation.
#'
#' @param user_data A data frame containing control data to validate.
#' @param action The action being performed, either "create" or "extraction".
#' @param database The database connection or specification to use for validation.
#' @return A list containing validation errors, if any.
#' @keywords validation, strain
validate_batch <- function(user_data, action, database) {
  errors <- list()
  validation_result <- validate_date_format(user_data, "Batch", "%Y-%m-%d")

  if (!is.null(validation_result$error_data)) {
    errors <- add_to_errors(errors, validation_result$error_data)
  }

  errors <- c(errors, perform_batch_db_validations(database, user_data, action))

  return(errors)
}

#' Perform Database-Related Validations for Batches
#'
#' This function conducts database-related validation checks for Strains data.
#' It initiates a database connection, then performs create or extraction validations based on the action provided.
#'
#' @param database The database connection or specification for validation.
#' @param user_data The users data.
#' @param action The action being performed, either "create" or "extraction".
#' @return A list containing validation errors, if any.
#' @keywords validation, strain
#' @export
perform_batch_db_validations <- function(database, user_data, action) {
  con <- init_and_copy_to_db(database, user_data)
  on.exit(dbDisconnect(con), add = TRUE)
  errors <- list()

  # Utility function to simplify repetitive operations
  batch_test <- function(validation_func, ...) {
    func_name <- deparse(substitute(validation_func))
    cat("Executing function:", func_name, "\n")
    result <- do.call(validation_func, list(con, "user_data", "RowNumber", ...))
    errors <<- add_to_errors(errors, result)
  }

  batch_test(validate_study_reference_db, "Batch", controls = TRUE, error_if_exists = TRUE)

  return(errors)
}
