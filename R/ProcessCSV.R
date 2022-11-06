#' @import lubridate
#' @import dplyr
#' @export


ProcessCSV <- function(user_csv, user_action, sample_storage_type, container_name = NULL, file_type = NULL, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG")) {

  if (!require(dplyr)) {
    stop("Function requires dplyr for database access!")
  }

  user_file <- read.csv(file = user_csv, header = FALSE)

  valid_actions = c("upload", "move")
  errmsg <- paste("Action is not valid. Valid actions are: ", paste(valid_actions, collapse = ", "))
  stopifnot(errmsg = (user_action %in% valid_actions))

  ## Depending on the csv tool, there may be empty strings or other special characters
  user_file[user_file == ""] <- NA
  user_file[] <- lapply(user_file, function(x) as.character(gsub("[\n\t,]", "", x)))

  ## remove empty rows and columns
  ## we do select columns below, so removing columns is technically duplicate work
  empty_rows <- rowSums(user_file == "" | is.na(user_file) | is.null(user_file)) == ncol(user_file)
  empty_cols <- colSums(user_file == "" | is.na(user_file) | is.null(user_file)) == nrow(user_file)
  user_file <- user_file[!empty_rows, !empty_cols]

  ## Read Configuration File
  config <- yaml::read_yaml(config_yml)
  traxcer_position <- ifelse(
    is.na(config$traxcer_position$override),
    config$traxcer_position$default,
    config$traxcer_position$override
  )

  ## Required Column Names
  # todo: add cryovials here

  optional_user_column_names <- conditional_user_column_names <- required_user_column_names <- NULL
  if (user_action %in% c("upload", "move")) {
    required_user_column_names <- switch(sample_storage_type,
      "micronix" = switch(file_type,
        "visionmate" = c(
          "LocationRow",
          "LocationColumn",
          "TubeCode"
        ),
        "traxcer" = c(
          traxcer_position, # definable in user preferences
          "Tube ID"
        ),
        "na" = c(
          "MicronixBarcode",
          "Row",
          "Column"
        )
      ),
      "cryovial" = c(
        "Barcode",
        "BoxRow",
        "BoxColumn"
      )
    )

    if (is.null(required_user_column_names)) {
      stop(paste("The expected column names for sample type", sample_storage_type, "and file type", file_type, "are not implemented (yet)."))
    }
  }

  # Additional user action driven columns below
  # - upload: metadata is required, collection date is conditional as requirement depends on the study, and comments are optional
  # - move: no need to add additional columns
  # - search: all searchable fields are optional


  # applies for all upload storage types
  if (user_action %in% c("upload")) {
    required_user_column_names <- c(required_user_column_names, c("Participant", "SpecimenType", "StudyCode"))
    conditional_user_column_names <- c("CollectionDate")
    optional_user_column_names <- c("Comment")
  } else if (user_action %in% c("search")) {
    optional_user_column_names <- c("CollectionDate", "StudyCode", "Participant", "SpecimenType")
  }

  ## second row is valid because traxcer will have "plate_label:" in the first row
  valid_header_rows <- c(1, 2)

  header_ridx <- .FindValidHeader(user_file = user_file, required_user_column_names = required_user_column_names, valid_header_rows = valid_header_rows)

  if (!header_ridx %in% valid_header_rows) {
    errmsg <- paste("Valid header rows could not be found in your file. Please check that the following column names are present:", paste(required_user_column_names, collapse = ", "))
    stop(errmsg)
  }

  ## format the file
  user_file <- user_file %>% setNames(.[header_ridx, ]) %>% .[-c(1, header_ridx), ]

  ## conditional and optional columns are only for uploads right now
  if ("upload" == user_action) {

    tmp <- sampleDB:::CheckTable("study") %>%
      filter(short_code %in% user_file$StudyCode) %>%
      inner_join(user_file, by = c("short_code" = "StudyCode"))

    # check this first
    if (!"collection_date" %in% colnames(user_file) && nrow(filter(tmp, is_longitudinal == 1)) > 0) {
      stop("Collection date is required for samples of longitudinal studies.")
    } else if ("collection_date" %in% colnames(user_file)) {
      df_invalid <- filter(tmp, is_longitudinal == 1 & is.na(collection_date))
      if (nrow(df_invalid) > 0) {
        errmsg <- paste("Missing collection date for following sample barcode(s):", paste(df_invalid$index, collapse = " "))
        stop(errmsg)
      }
    }
  }

  if (user_action %in% c("upload", "move")) {
    user_file <- select(user_file, all_of(required_user_column_names), contains(conditional_user_column_names), contains(optional_user_column_names))
  }

  ## Now convert to sampleDB formatting

  # this will contain the processed, validated file that can be used for the associated action
  processed_file <- NULL

  if (user_action %in% c("upload", "move")) {
    ## Micronix
    if (sample_storage_type == "micronix" && file_type == "na") {
      processed_file$barcode <- user_file$MicronixBarcode
      processed_file$index <- paste0(user_file$Row, user_file$Column)
    } else if (sample_storage_type == "micronix" && file_type == "traxcer") {
      processed_file$barcode <- user_file$`Tube ID`
      processed_file$index <- user_file %>% pull(all_of(traxcer_position))
    } else if (sample_storage_type == "micronix" && file_type == "visionmate") {
      processed_file$barcode = user_file$TubeCode
      processed_file$index  = paste0(user_file$LocationRow, user_file$LocationColumn) 
    }

    ## Cryovial
    else if (sample_storage_type == "cryovial") {
      processed_file$barcode <- user_file$Barcode
      processed_file$index <-  paste0(user_file$BoxRow, ":", user_file$BoxColumn)
    }
  }

  # conditional and optional columns only apply for uploads.
  # search files only contain optional columns.
  # metadata only applies for uploads.

  if ("upload" %in% c(user_action)) {
    processed_file$study_short_code <- user_file$StudyCode
    processed_file$study_subject <- user_file$StudySubject
    processed_file$specimen_type <- user_file$SpecimenType

    if ("CollectionDate" %in% colnames(user_file)) {
      processed_file$collection_date <- user_file$CollectionDate
    } else {
      processed_file$collection_date <- rep(NA, nrow(user_file))
    }

    if ("Comment" %in% colnames(user_file)) {
      processed_file$comment <- user_file$Comment
    } else {
      processed_file$comment <- rep(NA, nrow(user_file))
    }
  }

  processed_file <- as.data.frame(processed_file)
  print(processed_file)
  ### Quality check the data now

  print("Formatting complete.")
  .CheckFormattedFileData(
    database = database,
    formatted_csv = processed_file,
    sample_storage_type = sample_storage_type,
    user_action = user_action,
    required_user_column_names = required_user_column_names,
    conditional_user_column_names = conditional_user_column_names,
    optional_user_column_names = optional_user_column_names,
    container_name = container_name
  )

  return(processed_file)

}

.CheckFormattedFileData <- function(database, formatted_csv, sample_storage_type, user_action, required_user_column_names, conditional_user_column_names, optional_user_column_names, container_name) {

  # this is an internal mapping to the database that should not be exposed to the user
  required_names <- requires_data <- container_metadata <- NULL
  if (user_action %in% c("upload", "move")) {
    required_names <- switch(sample_storage_type,
        "micronix" = c(
          "index",
          "barcode"
        ),
        "cryovial" = c(
          "index",
          "barcode"
        )
      )

    requires_data <- c(requires_data, required_names)
  }

  if (user_action %in% c("upload")) {
    required_names <- c(required_names,
      c(
        "study_short_code",
        "study_subject",
        "specimen_type",
        "collection_date",
        "comment"
    ))

    requires_data <- c(requires_data,
      c(
        "study_short_code",
        "study_subject",
        "specimen_type"

    ))
  }

  barcode_length_constraint <- switch(sample_storage_type,
    "micronix" = 10,
    "cryovial" = 6
  )

  bError <- FALSE
  errmsg <- NULL

  if (user_action %in% c("upload", "move")) {

    con <- NULL
    tryCatch({

      stopifnot("Required database column names not implemented" = !is.null(required_names))

      stopifnot("Storage container name must be defined for uploads and moves" = !is.null(container_name))

      stopifnot("All collection dates are not in YMD format" = .CheckDateFormat(formatted_csv))

      .CheckIndexIsValid(formatted_csv, sample_storage_type)

      # check that there are no empty cells besides collection_date (checked later) and comment (may or may not exist)


      errmsg <- paste0(sample_storage_type, " barcodes must be ", barcode_length_constraint, " long.")
      stopifnot(errmsg = all(nchar(formatted_csv$barcode) == barcode_length_constraint))

      matched_columns <- match(required_names, names(formatted_csv))
      if (any(is.na(matched_columns))) {
        errmsg <- paste("Required database column is missing:", paste(required_names[is.na(matched_columns)], collapse = ", "))
        stop(errmsg)
      }

      missing_data <- is.na(formatted_csv[, requires_data])
      if (any(missing_data)) {
        errmsg <- paste("The upload file is missing data in required columns. Please check that your file contains the following column names and have data entries for each sample.", paste(requires_data, collapse = ", "))
        stop(errmsg)
      }

      ## Deeper validation using database

      container_tables <- list(
        "manifest" = switch(sample_storage_type,
          "micronix" = "micronix_plate",
          "cryovial" = "cryovial_box",
        ),
        "container_class" = switch(sample_storage_type,
          "micronix" = "micronix_tube",
          "cryovial" = "cryovial_tube"
        )
      )

      # make sure not uploading to well positions with active samples
      con <- DBI::dbConnect(RSQLite::SQLite(), database)
      stopifnot("Uploading sample to well location that already has an active sample" = tbl(con, "storage_container") %>%
        select(status_id, id) %>%
        filter(status_id == 1) %>%
        inner_join(tbl(con, container_tables[["container_class"]]), by = c("id" = "id")) %>%
        inner_join(tbl(con, container_tables[["manifest"]]), by = c("manifest_id" = "id")) %>%
        filter(name == container_name) %>%
        select(manifest_id, index, status_id) %>%
        collect() %>%
        nrow(.) == 0)

      if ("upload" == user_action) {
        study_codes <- formatted_csv %>% pull(study_short_code) %>% unique(.)
        specimen_types <- formatted_csv %>% pull(specimen_type) %>% unique(.)
        stopifnot("Study code not found" = tbl(con, "study") %>%
          filter(short_code %in% study_codes) %>%
          collect() %>%
          nrow(.) > 0)

        stopifnot("Specimen type not found" = tbl(con, "specimen_type") %>%
          filter(barcode %in% specimen_types) %>%
          collect() %>%
          nrow(.) > 0)
      }

      print("Validation Complete.")
    },
    error = function(e) {
      bError <<- TRUE
      errmsg <<- e$message
    },
    finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
      if (bError) {
        formatted_csv <- NULL
        stop(errmsg)
      }
    }) # end tryCatch
  }
}

# Logistical Checks
.FindValidHeader <- function(user_file, required_user_column_names, valid_header_rows = valid_header_rows) {

  # sanity check
  stopifnot("File is empty" = nrow(user_file) > 1)

  # this variable will be set with the valid header row index (if it exists)
  header_ridx <- 0

  for (colname_ridx in valid_header_rows) {
    row <- user_file[colname_ridx, ]
    if(all(required_user_column_names %in% row)) {
      header_ridx <- colname_ridx
      break
    }
  }

  return(header_ridx)
}

.CheckIndexIsValid <- function(formatted_csv, sample_storage_type) {

  indexes <- formatted_csv %>% pull(index)
  if ("micronix" == sample_storage_type) {

    # check row letters
    row_letter_check <- substr(indexes, 1, 1) %in% LETTERS

    # make sure the columns are numbers and above zero
    col_numbers <- substr(indexes, 2, nchar(indexes))
    col_number_indices <- which(col_numbers %>%
      as.numeric() %>%
      suppressWarnings() > 0)

    # check for duplicates
    well_dups_check <- !duplicated(indexes)

    stopifnot("Invalid micronix index" = all(row_letter_check) && length(col_number_indices) == length(indexes) && all(well_dups_check))
  } else if ("cryovial" == sample_storage_type) {
    valid <- lapply(strsplit(indexes, ":"), function(x) { return(nchar(x[1]) == 1 & !is.na(as.integer(x[1])) & nchar(x[2]) == 1 & !is.na(as.integer(x[2]))) })
    stopifnot("Invalid cryovial index" = all(unlist(valid)))
  } else {
    stop("Index validation for sample_storage_type is not implemented.")
  }
}

.CheckDateFormat <- function(formatted_csv){
  if (require(lubridate)) {
    if("collection_date" %in% names(formatted_csv)){
      collection_dates <- formatted_csv %>% pull(collection_date)
      collection_dates <- collection_dates[!is.na(collection_dates)]
      out <- all(!is.na(parse_date_time(collection_dates, orders = "ymd")) == TRUE)
    }else{
      out <- TRUE
    }
    return(out)
  }
}
