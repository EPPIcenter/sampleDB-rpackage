# R/app_errors.R

#' Stop with an application error
#'
#' This function throws an application error of the given type,
#' with the provided message and any additional data.
#'
#' @param type The type of the error. This should be a character string.
#' @param message The error message. This should be a character string.
#' @param data Additional data associated with the error.
#'
#' @return Never returns, because it signals an error.
#'
#' @examples
#' \dontrun{
#' stop_app_error("my_error", "This is an error!", data.frame(x = 1:5))
#' }
#'
#' @export
stop_app_error <- function(type, message, data = NULL, call = NULL) {
  rlang::abort(type, message = message, data = data, call = call)
}

#' Stop with a formatting error
#'
#' This function throws a formatting error with the provided message and data.
#'
#' @param message The error message. This should be a character string.
#' @param data Additional data associated with the error.
#'
#' @return Never returns, because it signals an error.
#'
#' @examples
#' \dontrun{
#' stop_formatting_error("This is a formatting error!", data.frame(x = 1:5))
#' }
#'
#' @export
#' @keywords error-handling
stop_formatting_error <- function(message, data, call = rlang::caller_env()) {
  stop_app_error("formatting_error", message, data, call)
}

#' Stop with a validation error
#'
#' This function throws a validation error with the provided message and data.
#'
#' @param message The error message. This should be a character string.
#' @param data Additional data associated with the error.
#'
#' @return Never returns, because it signals an error.
#'
#' @examples
#' \dontrun{
#' stop_formatting_error("This is a validation error!", data.frame(x = 1:5))
#' }
#'
#' @export
#' @keywords error-handling
stop_validation_error <- function(message, data, call = rlang::caller_env()) {
  stop_app_error("validation_error", message, data, call)
}

#' ErrorData class
#'
#' @description This class represents an individual error found in the data.
#' It includes the error description and the row numbers and column names where the error was found.
#'
#' @field description A character string describing the error.
#' @field columns A character vector of column names where the error was found.
#' @field rows An integer vector of row numbers where the error was found.
#' @export
#' @keywords error-handling
ErrorData <- R6::R6Class(
  "ErrorData",
  public = list(
    description = NULL,
    columns = NULL,
    rows = NULL,

    #' @description Initialize method for the ErrorData class.
    #' @details This method sets up a new ErrorData object. If a data.frame is provided, it will use its column names
    #' and the 'RowNumber' column for columns and rows, respectively. If not, the columns and rows arguments
    #' should be used. It will stop with an error if neither a data.frame is provided nor both columns and rows.
    #' @param description A character string describing the error. Default is NULL.
    #' @param columns A character vector of column names where the error was found. Default is NULL.
    #' @param rows An integer vector of row numbers where the error was found. Default is NULL.
    #' @param data_frame A data.frame with all of the invalid data. Can be used instead of providing rows and columns.
    #' Default is NULL.
    #' @return An initialized ErrorData object.
    initialize = function(description = NULL, columns = NULL, rows = NULL, data_frame = NULL) {
      if (!is.null(data_frame)) {
        self$columns = colnames(data_frame)
        self$rows = data_frame$RowNumber
      } else if (!is.null(columns) && !is.null(rows)) {
        self$columns <- columns
        self$rows <- rows
      } else {
        stop("Either provide a data.frame or both columns and rows.")
      }
      self$description <- description
    }
  )
)

#' ValidationErrorCollection class
#'
#' @description This class manages a collection of ErrorData objects.
#' It can be used to add new ErrorData objects, and to export the collection of errors to a CSV file.
#'
#' @field error_data_list A list of ErrorData objects representing the collection of errors.
#' @field user_data The original data frame where the errors were found.
#' @export
#' @keywords error-handling
ValidationErrorCollection <- R6::R6Class(
  "ValidationErrorCollection",
  public = list(
    error_data_list = list(),
    user_data = NULL,

    #' @method ValidationErrorCollection initialize
    #' @description This method receives a list of ErrorData objects.
    #' @param errors The list of ErrorData objects to be added.
    #' @param user_data The user data where the error data came from.
    initialize = function(errors = list(), user_data = NULL) {
      self$error_data_list = errors
      self$user_data = user_data
    },

    #' @method ErrorDataList add_error
    #' @description This method adds an ErrorData object.
    #' @param error_data The ErrorData object to be added.
    add_error = function(error_data) {
      if (!is(error_data, "ErrorData")) {
        stop("Input must be an ErrorData object.")
      }
      self$error_data_list[[length(self$error_data_list) + 1]] = error_data
    },

    #' @method ErrorDataList get_error_details_by_index
    #' @description Gets an error by index.
    #' @param index The index to specify which error.
    get_error_details_by_index = function(index) {

      if (index < 1 || index > self$length()) {
        stop("Invalid index.")
      }

      specific_error <- self$error_data_list[[index]]

      error_details <- data.frame(
        RowNumber = specific_error$rows,
        self$user_data[specific_error$rows, specific_error$columns, drop = FALSE],
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      error_details <- error_details[, specific_error$columns] # reorder columns
      
      return(error_details)
    },

    #' @method ErrorDataList to_csv
    #' @description This method is used to export the errors to a CSV file.
    #' @param filename The name of the CSV file.
    to_csv = function(filename) {
        if (length(self$error_data_list) == 0) {
            stop("No errors have been added.")
        }
        # Here, we're aggregating all the errors from the list into a single data frame.
        error_df <- do.call(rbind, lapply(self$error_data_list, function(error) {
            data.frame(
                row = error$rows,
                column = error$columns,
                description = rep(error$description, length(error$rows)),
                stringsAsFactors = FALSE
            )
        }))

        # Group by row and collapse descriptions
        error_df <- aggregate(description ~ row + column, data = error_df, FUN = function(x) paste(x, collapse = ";"))

        # Now, we're aggregating descriptions by row number. This is in case there are multiple
        # descriptions for a single row.
        extracted_data <- mapply(function(row, col) {
            # Directly access the column using the `[[]]` operator and then access the specific row.
            return(self$user_data[[col]][row])
        }, error_df$row, error_df$column, SIMPLIFY = TRUE)

        # The extracted goes in the `error_df`.
        error_df[["data"]] <- as.character(extracted_data)

        # Finally, write the csv.
        write.csv(error_df, file = filename, row.names = FALSE)
    },

    #' @method ErrorDataList length
    #' @description This method returns the number of ErrorData objects in the list.
    length = function() {
      return(length(self$error_data_list))
    }
  )
)
