ValidateReferenceStrains <- function(database, user_data, dbmap) {

  required_names <- requires_data <- NULL

  required_names <- c("strain")

  if (is.null(required_names)) {
    stop("Invalid reference detected")
  }

  requires_data <- c(requires_data, required_names)

  bError <- FALSE
  err <- errmsg <- NULL

  con <- NULL
  tryCatch({

    for (nm in names(dbmap)) {
      if (all(dbmap[[nm]] %in% colnames(user_data))) {
        user_data = dplyr::rename(user_data, unlist(dbmap[nm]))
      } else {
        user_data[[nm]] = c(NA)
      }
    }

    user_data = dplyr::mutate(user_data, row_number = row_number())

    stopifnot("Required database column names not implemented" = !is.null(required_names))

    matched_columns <- match(required_names, names(user_data))
    if (any(is.na(matched_columns))) {
      errmsg <- paste("Required database column is missing:", paste(required_names[is.na(matched_columns)], collapse = ", "))
      stop(errmsg)
    }

    ## broad sweep check if missing any data in required fields
    rs <- sum(is.na(user_data[, requires_data])) > 0
    rn <- user_data[rs, ] %>% pull(row_number)

    cs <- sum(is.na(user_data[, requires_data])) > 0
    cols <- colnames(user_data[, requires_data])[cs]

    if (length(cols) > 0) {
      df <- user_data[rn, c("row_number", cols)]
      err <- .maybe_add_err(err, df, "Rows found with missing data", dbmap)
    }

    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    copy_to(con, user_data)

    ## 1) Ensure that the strain has not already been added
    df = tbl(con, "user_data") %>%
      dplyr::mutate(strain = toupper(strain)) %>%
      left_join(
        tbl(con, "strain") %>%
          dplyr::rename(strain = name) %>%
          dplyr::mutate(strain = toupper(strain))
        , by = c("strain")
      ) %>%
      filter(!is.na(id)) %>%
      select(all_of(colnames(user_data))) %>%
      collect()

    err <- .maybe_add_err(err, df, "Strain detected", dbmap)

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
      stop_validation_error("There was a problem with the content of your file.", err)
    }
  })

  return(user_data)
}


#' This function is internally used to check a formatted csv file for duplicated positions. Note that this should accept the formatted file produced by ProcessCSV().
#'
#' @param database Path to the sampleDB database
#' @param user_data Columns that require data
#' @param dbmap Map connecting users file columns to database tables and columns. This is used to build the list of errors with the user provided column names.
#' @param tolerance A threshold for the allowed sum of percentages. Default is 0.02 (sum of percentages >= 0.98% is okay)
#' @noRd
#' @export
ValidateControlCompositions <- function(database, user_data, dbmap, tolerance = 0.02) {
	browser()
  requires_data <- c("strain", "percentage")

  bError <- FALSE
  err <- errmsg <- NULL

	tryCatch({

    user_data = ConvertToDatabaseNames(user_data, dbmap)
    err = CheckForMissingDataInRequiredColumns(user_data, requires_data, err, dbmap)

		# modify the user data so that it can be comapared with strains
		user_data.1 = user_data %>%
    	dplyr::mutate(
    		strain=strsplit(strain, ";"),
    		percentage=strsplit(percentage, ";")
    	) %>%
    	tidyr::unnest(cols=c("strain", "percentage")) 

    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    copy_to(con, user_data.1)

    ## check that all strains are found
    df = tbl(con, "user_data.1") %>%
      left_join(tbl(con, "strain"), by = c("strain"="name")) %>%
      filter(is.na(id)) %>%
      select(row_number, strain) %>%
      distinct() %>%
      dplyr::rename(strain = strain) %>%
      distinct() %>%
      collect()

    err <- .maybe_add_err(err, df, "Strain found that is not recorded in the database", dbmap)

    # inclusive tolerance for strain composition sum
    tolerance = 0.02 # note: could add this as a setting

    df = tbl(con, "user_data.1") %>%
      select(row_number, percentage) %>%
      group_by(row_number) %>%
      dplyr::mutate(percentage=as.numeric(percentage)) %>%
      dplyr::mutate(percentage=ifelse(is.na(percentage), 0, percentage)) %>%
      dplyr::mutate(
        perc_sum = sum(percentage, na.rm=TRUE),
        equals_1 = as.logical(perc_sum >= 1.0 - tolerance && perc_sum <= 1.0) # tolerance added
      ) %>%
      filter(equals_1 == FALSE) %>%
      ungroup() %>%
      select(row_number, percentage) %>%
      distinct() %>%
      collect()

    err <- .maybe_add_err(err, df, "Some controls do not sum to 100", dbmap)

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
      stop_validation_error("There was a problem with the content of your file.", err)
    }
  })

  return (user_data)
}
