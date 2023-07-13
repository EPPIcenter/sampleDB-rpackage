#'  ValidateWholeBloodUpload
#' 
#' This function is used to validate a users whole blood upload
#'  
#' @param database Path to the sampleDB database
#' @param user_data Columns that require data
#' @param file_type The type of file that is being uploaded (default=na)
#' @param dbmap Map connecting users file columns to database tables and columns. This is used to build the list of errors with the user provided column names.
#' @noRd
#' @export
ValidateWholeBloodUpload <- function(database, user_data, file_type="na", dbmap) {
  requires_data <- c("barcode", "position1", "position2", "study_short_code", "collection_date", "manifest_name", "location_root", "level_I", "level_II")
  con <- NULL
  bError <- FALSE
  err <- errmsg <- NULL
  tryCatch({
    user_data <- ConvertToDatabaseNames(user_data, dbmap)
    err <- CheckForMissingDataInRequiredColumns(user_data, requires_data, err, dbmap)

    ## detect `k` or `K` and convert to numeric
    vb <- grepl('k|K', user_data$density)
    user_data$density[vb] <- str_replace(user_data$density[vb], "k|K", "")
    user_data$density <- as.integer(user_data$density)
    user_data$density[vb] <- as.integer(user_data$density[vb] * 1000) # had to explicitly make the result integer to prevent the result from turning to 1e+04

    user_data.1 = user_data %>%
      dplyr::mutate(
        strain2 = strsplit(strain, ";"),
        percentage2 = strsplit(percentage, ";")
      ) %>%
      tidyr::unnest(cols = c(strain2, percentage2))

    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    copy_to(con, user_data.1)

    # Make sure composition IDs exist
    err <- CheckThatCompositionIDExists(con, user_data_tbl="user_data.1", err=err, dbmap=dbmap)

    # Find columns that are missing data
    err <- CheckForMissingDataInRequiredColumns(user_data, requires_data=requires_data, err=err, dbmap=dbmap)

    # Find Positions that are not unique in the file.
    err <- CheckUploadPositionsUniqueInFile(user_data, file_type=file_type, err=err, dbmap=dbmap) # hardcoded "na"

    # position formatting should follow cryovial formating
    err <- CheckPositionColumnFormatting(user_data, sample_storage_type=2, file_type=file_type, err=err, dbmap=dbmap) # `1` is Micronix, which is the default for now

    # make sure the spot is open in the cryovial box
    err <- CheckIfWholeBloodPositionIsOccupied(con, user_data_tbl = "user_data.1", err=err, dbmap=dbmap)
  },
  warning = function(w) {
    bError <<- TRUE
    errmsg <<- w$message
  },
  error = function(e) {
    bError <<- TRUE
    errmsg <<- e$message
  },
  finally = {

    if (!is.null(con)) {
      dbDisconnect(con)
    }

    if (bError) {
      stop(errmsg)
    }

    ## throw if bad data found
    if (!is.null(err) && length(err) > 0) {
      stop_validation_error("There was a problem with the content of your file", err)
    }
  })

  return(user_data)
}


#' ValidateDBSSheetUpload
#' 
#' This function is used to validate a users whole blood upload
#'  
#' @param database Path to the sampleDB database
#' @param user_data Columns that require data
#' @param dbmap Map connecting users file columns to database tables and columns. This is used to build the list of errors with the user provided column names.
#' @noRd
#' @export
ValidateDBSSheetUpload <- function(database, user_data, dbmap) {
  requires_data <- c("strain", "percentage", "density", "study_short_code", "manifest_name", "location_root", "level_I", "level_II")

  bError <- FALSE
  err <- errmsg <- NULL
  con <- NULL
  tryCatch({

    user_data <- ConvertToDatabaseNames(user_data, dbmap)
    err <- CheckForMissingDataInRequiredColumns(user_data, requires_data, err, dbmap)

    ## detect `k` or `K` and convert to numeric
    vb <- grepl('k|K', user_data$density)
    user_data$density[vb] <- str_replace(user_data$density[vb], "k|K", "")
    user_data$density <- as.numeric(user_data$density)
    user_data$density[vb] <- as.numeric(user_data$density[vb] * 1000) # had to explicitly make the result integer to prevent the result from turning to 1e+04

    user_data.1 = user_data %>%
      dplyr::mutate(
        strain2 = strsplit(strain, ";"),
        percentage2 = strsplit(percentage, ";")
      ) %>%
      tidyr::unnest(cols = c(strain2, percentage2))


    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    copy_to(con, user_data.1)

    # Make sure composition IDs exist
    err <- CheckThatCompositionIDExists(con, user_data_tbl="user_data", err=err, dbmap=dbmap)

    df <- tbl(con, "user_data.1") %>%
      left_join(tbl(con, "location") %>%
        dplyr::rename(location_id = id), by = c('location_root', 'level_I', 'level_II')) %>%
      filter(is.na(location_id)) %>%
      select(row_number, location_root, level_I, level_II) %>%
      collect()

    errstring = sprintf("The following %s, %s and / or %s are not found in the database", dbmap["location_root"], dbmap["level_I"], dbmap["level_II"])
    err <- .maybe_add_err(err, df, errstring, dbmap)
  },
  warning = function(w) {
    bError <<- TRUE
    errmsg <<- w$message
  },
  error = function(e) {
    bError <<- TRUE
    errmsg <<- e$message
  },
  finally = {

    if (!is.null(con)) {
      dbDisconnect(con)
    }

    if (bError) {
      stop(errmsg)
    }

    ## throw if bad data found
    if (!is.null(err) && length(err) > 0) {
      stop_validation_error("There was a problem with the content of your file", err)
    }
  })

  return(user_data)
}

#' ValidateExtractedDNA
#' 
#' This function is used to validate a users whole blood upload
#'  
#' @param database Path to the sampleDB database
#' @param user_data Columns that require data
#' @param dbmap Map connecting users file columns to database tables and columns. This is used to build the list of errors with the user provided column names.
#' @noRd
#' @export
ValidateExtractedDNA <- function(database, user_data, dbmap) {
  requires_data <- c("barcode", "position1", "position2", "study_short_code", "study_subject", "collection_date", "tube_barcode", "plate_name", "box_name", "location_root", "level_I", "level_II", "specimen_type")
  con <- NULL
  bError <- FALSE
  err <- errmsg <- NULL

  tryCatch({
    user_data <- ConvertToDatabaseNames(user_data, dbmap)
    err <- CheckForMissingDataInRequiredColumns(user_data, requires_data, err, dbmap)

    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    copy_to(con, user_data)

    ## Check the dependencies required to record an extraction
    err <- CheckReferences(con, user_data_tbl = "user_data", type="control", action="extraction", err=err, dbmap=dbmap)

    # Find Positions that are not unique in the file.
    err <- CheckExtactionPositionsUniqueInFile(user_data, err=err, dbmap=dbmap) # hardcoded "na"

    # Make sure there isn't an active sample in the position already
    err <- CheckIfExtractionDestinationIsOccupied(con, user_data_tbl = "user_data", err=err, dbmap=dbmap)
  },
  warning = function(w) {
    bError <<- TRUE
    errmsg <<- w$message
  },
  error = function(e) {
    bError <<- TRUE
    errmsg <<- e$message
  },
  finally = {

    if (!is.null(con)) {
      dbDisconnect(con)
    }

    if (bError) {
      stop(errmsg)
    }

    ## throw if bad data found
    if (!is.null(err) && length(err) > 0) {
      stop_validation_error("There was a problem with the content of your file", err)
    }
  })

  return(user_data)
}
