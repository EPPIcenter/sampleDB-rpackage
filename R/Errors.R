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
stop_app_error <- function(type, message, data = NULL) {
  rlang::abort(type, message = message, data = data)
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
stop_formatting_error <- function(message, data) {
  stop_app_error("formatting_error", message, data)
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
stop_validation_error <- function(message, data) {
  stop_app_error("validation_error", message, data)
}

#' ErrorData class
#'
#' @description This class represents an individual error found in the data.
#' It includes the error description and the row numbers and column names where the error was found.
#'
#' @field description A character string describing the error.
#' @field columns A character vector of column names where the error was found.
#' @field rows An integer vector of row numbers where the error was found.
#'
ErrorData <- R6::R6Class(
  "ErrorData",
  public = list(
    description = NULL,
    columns = NULL,
    rows = NULL,

    #' @method ErrorData initialize
    #' @description This method is used to initialize the ErrorData object.
    #' @param description The description of the error.
    #' @param columns The columns where the error was found.
    #' @param rows The rows where the error was found.
    #' @return An ErrorData object
    initialize = function(description = NULL, columns = NULL, rows = NULL) {
      self$description <- description
      self$columns <- columns
      self$rows <- rows
    }
  )
)


#' ErrorDataList class
#'
#' @description This class manages a collection of ErrorData objects.
#' It can be used to add new ErrorData objects, and to export the collection of errors to a CSV file.
#'
#' @field error_data_list A list of ErrorData objects representing the collection of errors.
#' @field user_data The original data frame where the errors were found.
#'
ErrorDataList <- R6::R6Class(
  "ErrorDataList",
  public = list(
    error_data_list = list(),
    user_data = NULL,

    #' @method ErrorDataList initialize
    #' @description This method is used to initialize the ErrorDataList object.
    #' @param user_data The original data where the errors were found.
    initialize = function(user_data = NULL) {
      self$error_data_list = list()
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

    #' @method ErrorDataList merge_data_list
    #' @description This method is used to merge the error data from another ErrorDataList into this one.
    #' @param other The other ErrorDataList object.
    merge_data_list = function(other) {
      if (!is(other, "ErrorDataList")) {
        stop("Input must be an ErrorDataList object.")
      }
      self$error_data_list <- c(self$error_data_list, other$error_data_list)
      if(!is.null(other$user_data)){
        self$user_data <- rbind(self$user_data, other$user_data)
      }
    },

    #' @method ErrorDataList to_csv
    #' @description This method is used to export the errors to a CSV file.
    #' @param filename The name of the CSV file.
    to_csv = function(filename) {
      if (length(self$error_data_list) == 0) {
        stop("No errors have been added.")
      }

      error_df <- do.call(rbind, lapply(self$error_data_list, function(error) {
        data.frame(
          row = error$rows,
          column = error$columns,
          description = rep(error$description, length(error$rows)),
          stringsAsFactors = FALSE
        )
      }))

      # Group by row and collapse descriptions
      error_df <- aggregate(description ~ row, data = error_df, FUN = function(x) paste(x, collapse = ";"))

      error_df$data <- mapply(function(row, col) {
        self$user_data[row, col]
      }, error_df$row, error_df$column, SIMPLIFY = FALSE)

      write.csv(error_df, file = filename, row.names = FALSE)
    }
  )
)




