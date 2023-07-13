#' parse_and_validate_dates function
#'
#' @description This function parses and validates dates in the 'collection_date' column of a dataframe. It also checks for tokens representing unknown dates.
#' Errors found in the date format are returned as an ErrorData object.
#'
#' @param user_data The dataframe containing the data to be checked
#' 
#' @return A list containing parsed_dates, token_mask, and an ErrorData object if any
#' 
parse_and_validate_dates <- function(user_data) {
  
  allowed_date_formats = c("%Y-%m-%d")
  tokens = c("unk", "UNK", "unknown", "UNKNOWN")
  
  parsed_dates <- lubridate::parse_date_time(user_data$collection_date, allowed_date_formats, quiet = TRUE, exact = TRUE)
  
  token_mask <- !user_data$collection_date %in% tokens
  
  error_rows <- user_data[!is.na(user_data$collection_date) & is.na(parsed_dates) & token_mask,]$row_number
  error_df <- user_data[user_data$row_number %in% error_rows, c("row_number", "collection_date")]
  
  description <- paste("Unrecognized strings found in collection date column. Add any of the following if the collection date is unknown:", paste(tokens, collapse=", "))
  
  error_data <- ErrorData$new(description, "collection_date", error_df$row_number)
  
  list(parsed_dates = parsed_dates, token_mask = token_mask, error_data = error_data)
}

#' format_dates function
#'
#' @description This function formats the 'collection_date' column of a dataframe using the parsed_dates and token_mask.
#'
#' @param user_data The dataframe containing the data to be formatted
#' @param parsed_dates The parsed dates obtained from parse_and_validate_dates function
#' @param token_mask The token mask obtained from parse_and_validate_dates function
#' 
#' @return The formatted user_data
#' 
format_dates <- function(user_data, parsed_dates, token_mask) {
  
  user_data$collection_date <- parsed_dates
  user_data$collection_date[!token_mask] <- rep(lubridate::origin, sum(!token_mask))
  user_data$collection_date = as.character(lubridate::as_date(user_data$collection_date))
  
  user_data
}

#' validate_and_copy_dates function
#'
#' @description This function serves as a pipeline that parses, validates, and formats dates in the 'collection_date' column of a dataframe.
#' It checks for tokens representing unknown dates. Errors found in the date format are returned as an ErrorDataList object.
#' 
#' @param user_data The dataframe containing the data to be checked
#' @param user_action The action performed by the user, currently only 'upload' is supported
#' 
#' @return A list containing user_data and an ErrorDataList object if any
#' 
validate_and_copy_dates <- function(user_data, user_action) {
  
  if (user_action %in% c("upload")) {
    
    parsed_dates <- NULL
    token_mask <- NULL
    error_data <- NULL
    
    # Parsing and validating dates
    result <- parse_and_validate_dates(user_data)
    parsed_dates <- result$parsed_dates
    token_mask <- result$token_mask
    error_data <- result$error_data
    
    # Formatting dates
    if (length(user_data$collection_date[!is.na(user_data$collection_date) & is.na(parsed_dates) & token_mask]) == 0) {
      user_data <- format_dates(user_data, parsed_dates, token_mask)
    }

    # Create an ErrorDataList object and add the error
    error_data_list <- ErrorDataList$new(user_data)
    error_data_list$add_error(error_data)
  }
  
  list(user_data = user_data, error_data_list = error_data_list)
}#' parse_and_validate_dates function
#'
#' @description This function parses and validates dates in the 'collection_date' column of a dataframe. It also checks for tokens representing unknown dates.
#' Errors found in the date format are returned as an ErrorData object.
#'
#' @param user_data The dataframe containing the data to be checked
#' 
#' @return A list containing parsed_dates, token_mask, and an ErrorData object if any
#' 
parse_and_validate_dates <- function(user_data) {
  
  allowed_date_formats = c("%Y-%m-%d")
  tokens = c("unk", "UNK", "unknown", "UNKNOWN")
  
  parsed_dates <- lubridate::parse_date_time(user_data$collection_date, allowed_date_formats, quiet = TRUE, exact = TRUE)
  
  token_mask <- !user_data$collection_date %in% tokens
  
  error_rows <- user_data[!is.na(user_data$collection_date) & is.na(parsed_dates) & token_mask,]$row_number
  error_df <- user_data[user_data$row_number %in% error_rows, c("row_number", "collection_date")]
  
  description <- paste("Unrecognized strings found in collection date column. Add any of the following if the collection date is unknown:", paste(tokens, collapse=", "))
  
  error_data <- ErrorData$new(description, "collection_date", error_df$row_number)
  
  list(parsed_dates = parsed_dates, token_mask = token_mask, error_data = error_data)
}

#' format_dates function
#'
#' @description This function formats the 'collection_date' column of a dataframe using the parsed_dates and token_mask.
#'
#' @param user_data The dataframe containing the data to be formatted
#' @param parsed_dates The parsed dates obtained from parse_and_validate_dates function
#' @param token_mask The token mask obtained from parse_and_validate_dates function
#' 
#' @return The formatted user_data
#' 
format_dates <- function(user_data, parsed_dates, token_mask) {
  
  user_data$collection_date <- parsed_dates
  user_data$collection_date[!token_mask] <- rep(lubridate::origin, sum(!token_mask))
  user_data$collection_date = as.character(lubridate::as_date(user_data$collection_date))
  
  user_data
}

#' validate_and_copy_dates function
#'
#' @description This function serves as a pipeline that parses, validates, and formats dates in the 'collection_date' column of a dataframe.
#' It checks for tokens representing unknown dates. Errors found in the date format are returned as an ErrorDataList object.
#' 
#' @param user_data The dataframe containing the data to be checked
#' @param user_action The action performed by the user, currently only 'upload' is supported
#' 
#' @return A list containing user_data and an ErrorDataList object if any
#' 
validate_and_copy_dates <- function(user_data, user_action) {
  
  if (user_action %in% c("upload")) {
    
    parsed_dates <- NULL
    token_mask <- NULL
    error_data <- NULL
    
    # Parsing and validating dates
    result <- parse_and_validate_dates(user_data)
    parsed_dates <- result$parsed_dates
    token_mask <- result$token_mask
    error_data <- result$error_data
    
    # Formatting dates
    if (length(user_data$collection_date[!is.na(user_data$collection_date) & is.na(parsed_dates) & token_mask]) == 0) {
      user_data <- format_dates(user_data, parsed_dates, token_mask)
    }

    # Create an ErrorDataList object and add the error
    error_data_list <- ErrorDataList$new(user_data)
    error_data_list$add_error(error_data)
  }
  
  list(user_data = user_data, error_data_list = error_data_list)
}


