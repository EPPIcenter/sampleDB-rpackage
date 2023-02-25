#' @import lubridate
#' @import dplyr
#' @import glue
#' @importFrom rlang abort
#' @import rjson
#' @export


ProcessCSV <- function(user_csv, user_action, sample_storage_type, container_name = NULL, freezer_address = NULL, file_type = "na", validate = TRUE, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG")) {
  df.error.formatting <- data.frame(column = NULL, reason = NULL, trigger = NULL)

  if (!require(dplyr)) {
    stop("Function requires dplyr for database access!")
  }

  if(!is.null(container_name) && container_name == "") {
    container_name <- NULL
  }

  if (!is.null(freezer_address) && all(freezer_address == "")) {
    freezer_address <- NULL
  }

  user_file <- read.csv(file = user_csv, header = FALSE)

  valid_actions = c("upload", "move")
  if (!user_action %in% valid_actions) {
    errmsg <- paste("Action is not valid. Valid actions are:", paste(valid_actions, collapse = ", "))
    stop_usage_error(errmsg)
  }

  ## Depending on the csv tool, there may be empty strings or other special characters
  user_file[user_file == ""] <- NA
  user_file[] <- lapply(user_file, function(x) as.character(gsub("[\n\t,]", "", x)))

  ## remove empty rows and columns
  ## we do select columns below, so removing columns is technically duplicate work
  empty_rows <- rowSums(user_file == "" | is.na(user_file) | is.null(user_file)) == ncol(user_file)
  empty_cols <- colSums(user_file == "" | is.na(user_file) | is.null(user_file)) == nrow(user_file)
  user_file <- user_file[!empty_rows, !empty_cols]

  ## Read File Specification File
  file_specs_json <- rjson::fromJSON(file = system.file(
    "extdata", "file_specifications.json", package = .sampleDB$pkgname))

  ## Read Configuration File
  config <- yaml::read_yaml(config_yml)
  traxcer_position <- ifelse(
    is.na(config$traxcer_position$override),
    config$traxcer_position$default,
    config$traxcer_position$override
  )

  ## Required Column Names

  file_index <- which(lapply(file_specs_json$file_types, function(x) x$id) == file_type)
  sample_storage_type_index <- which(lapply(file_specs_json$file_types[[file_index]]$sample_type, function(x) x$id) == sample_storage_type)

  if (length(sample_storage_type_index) == 0) {
    stop("Unimplemented file specifications for this sample storage type")
  }

  actions <- file_specs_json$file_types[[file_index]]$sample_type[[sample_storage_type_index]]$actions[[user_action]]
  required_user_column_names <- actions[['required']]
  conditional_user_column_names <- actions[['conditional']]
  optional_user_column_names <- actions[['optional']]

  ## Shared fields

  sample_type_index <- which(lapply(file_specs_json$shared$sample_type, function(x) x$id) == sample_storage_type)

  required_user_column_names <- c(required_user_column_names, file_specs_json$shared$upload[['required']])
  conditional_user_column_names <- c(conditional_user_column_names, file_specs_json$shared$upload[['conditional']])
  optional_user_column_names <- c(optional_user_column_names, file_specs_json$shared$upload[['optional']])

  manifest_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$name
  manifest_barcode_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$barcode
  location_parameters <- file_specs_json$shared$sample_type[[sample_type_index]]$location
  location_parameters <- unlist(location_parameters[c("name", "level_I", "level_II")])

  required_user_column_names <- c(required_user_column_names, c(manifest_name, unname(location_parameters)))
  optional_user_column_names <- c(optional_user_column_names, c(manifest_barcode_name))

  if (is.null(required_user_column_names)) {
    stop(paste("The expected column names for sample type", sample_storage_type, "and file type", file_type, "are not implemented (yet)."))
  }

  ## second row is valid because traxcer will have "plate_label:" in the first row
  valid_header_rows <- 1:2

  header_row <- .FindHeader(user_file = user_file, required_user_column_names = required_user_column_names, valid_header_rows = valid_header_rows)

  if (is.null(header_row)) {
    errmsg <- paste("Valid header rows could not be found in your file. Please check that the following required column names are present:", paste(required_user_column_names, collapse = ", "))
    stop(errmsg)
  }

  user_file <- user_file %>% setNames(.[header_row, ]) %>% .[-c(1, header_row), ]

  # todo: check the parameters of the function and see if some of the data points are there (in case called from R package)
  # then, filter out the columns that could not be resolved, and add to the data frame. This will be a usage error.
  # as a side note: formatting error should probably be renamed.

  missing_columns <- required_user_column_names[!required_user_column_names %in% colnames(user_file)]

  if (!is.null(container_name) && manifest_name %in% missing_columns) {
    missing_columns <- missing_columns[missing_columns != manifest_name]
    user_file[manifest_name] <- container_name
  }
  if (!is.null(freezer_address) && all(location_parameters %in% missing_columns)) {
    missing_columns <- missing_columns[!location_parameters %in% missing_columns]
    user_file[location_parameters] <- freezer_address
  }

  if (length(missing_columns) > 0) {

    df.error.formatting <- rbind(
      df.error.formatting,
      data.frame(
            column = missing_columns,
            reason = "Always Required",
            trigger = c(NA)
          )
      )
  }

  if ("StudyCode" %in% colnames(user_file) && "upload" %in% user_action) {
    tmp <- sampleDB:::CheckTable("study") %>%
      filter(short_code %in% user_file$StudyCode) %>%
      inner_join(user_file, by = c("short_code" = "StudyCode"))

    # collection date must exist for all samples that are part of a longitudinal study
    if (!"CollectionDate" %in% colnames(user_file) && nrow(filter(tmp, is_longitudinal == 1)) > 0) {
      df.error.formatting <- rbind(
        df.error.formatting,
        data.frame(
          column = "CollectionDate",
          reason = "Collection date is required for samples of longitudinal studies.",
          trigger = data.frame(
            name = filter(tmp, is_longitudinal) %>% pull(name)
          )
        )
      )
    }
  }

  ## Throw if any of the required columns are missing
  # since the application is retrofitting the already released shiny application, there is only a subset of fields checked. This
  # should be expanded upon.
  if (nrow(df.error.formatting)) {
    stop_formatting_error(df = df.error.formatting)
  }


  user_file <- select(user_file, all_of(required_user_column_names), contains(conditional_user_column_names), contains(optional_user_column_names))

  ## use this chunk to validate conditional parameters

  if ("upload" %in% user_action) {

   if ("CollectionDate" %in% colnames(user_file)) {
      df_invalid <- filter(tmp, is_longitudinal == 1 & is.na(CollectionDate))
      if (nrow(df_invalid) > 0) {
        stop_validation_error("Missing collection date found for sample in longitudinal study", 1)
      }
    }
  }

  ## Now convert to sampleDB formatting

  # this will contain the processed, validated file that can be used for the associated action
  processed_file <- NULL

  if (user_action %in% c("upload", "move")) {

    ## Micronix
    if (sample_storage_type == 1 && file_type == "na") {
      processed_file$barcode <- user_file$Barcode
      processed_file$position <- paste0(user_file$Row, user_file$Column)
    } else if (sample_storage_type == 1 && file_type == "traxcer") {
      processed_file$barcode <- user_file$`Tube ID`
      processed_file$position <- user_file %>% pull(all_of(traxcer_position))
    } else if (sample_storage_type == 1 && file_type == "visionmate") {
      processed_file$barcode = user_file$TubeCode
      processed_file$position  = paste0(user_file$LocationRow, user_file$LocationColumn)
    }

    ## Cryovial
    else if (sample_storage_type == 2) {
      processed_file$barcode <- user_file$Barcode
      processed_file$position <-  paste0(user_file$BoxRow, user_file$BoxColumn)

    ## DBS
    } else if (sample_storage_type == 3) {
      processed_file$position <-  paste0(user_file$Row, user_file$Column)
    } else {
      stop("Unimplemented position formatting code for this sample type.")
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

    if (all(location_parameters %in% colnames(user_file)) && is.null(freezer_address)) {
      processed_file$name <- user_file %>% pull(all_of(unlist(location_parameters['name'])))
      processed_file$level_I <- user_file %>% pull(all_of(unlist(location_parameters['level_I'])))
      processed_file$level_II <- user_file %>% pull(all_of(unlist(location_parameters['level_II'])))
    } else {
      processed_file$name <- rep(freezer_address[['name']], nrow(user_file))
      processed_file$level_I <- rep(freezer_address[['level_I']], nrow(user_file))
      processed_file$level_II <- rep(freezer_address[['level_II']], nrow(user_file))
    }

    if (manifest_barcode_name %in% colnames(user_file)) {
      processed_file$manifest_barcode <- user_file %>% pull(all_of(manifest_barcode_name))
    } else {
      processed_file$manifest_barcode <- rep(NA, nrow(user_file))
    }

    if (manifest_name %in% colnames(user_file) && is.null(container_name)) {
      processed_file$manifest_name <- user_file %>% pull(all_of(manifest_name))
    } else {
      processed_file$manifest_name <- rep(container_name, nrow(user_file))
    }
  }

  if ("upload" %in% c(user_action) && sample_storage_type == 3) {
    processed_file$`0.05` <- user_file$`0.05`
    processed_file$`0.1` <- user_file$`0.1`
    processed_file$`1` <- user_file$`1`
    processed_file$`10` <- user_file$`10`
    processed_file$`100` <- user_file$`100`
    processed_file$`1k` <- user_file$`1k`
    processed_file$`10k` <- user_file$`10k`
    processed_file$strain <- user_file$Strain
  }

  # need check.names FALSE to prevent prepending `X` to numeric colnames
  processed_file <- as.data.frame(processed_file, check.names=FALSE)
  ### Quality check the data now

  message("Formatting complete.")

  if (validate) {
    .CheckFormattedFileData(
      database = database,
      formatted_csv = processed_file,
      sample_storage_type = sample_storage_type,
      user_action = user_action,
      required_user_column_names = required_user_column_names,
      conditional_user_column_names = conditional_user_column_names,
      optional_user_column_names = optional_user_column_names
    )
  }

  return(processed_file)

}

.CheckFormattedFileData <- function(database, formatted_csv, sample_storage_type, user_action, required_user_column_names, conditional_user_column_names, optional_user_column_names) {


  # this is an internal mapping to the database that should not be exposed to the user
  required_names <- requires_data <- container_metadata <- NULL
  if (user_action %in% c("upload", "move")) {
    required_names <- switch(sample_storage_type,
        "1" = c(
          "position",
          "barcode"
        ),
        "2" = c(
          "position",
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
    "1" = 10,
    "2" = 6
  )

  bError <- FALSE
  values <- errmsg <- NULL

  if (user_action %in% c("upload", "move")) {

    con <- NULL
    tryCatch({

      stopifnot("Required database column names not implemented" = !is.null(required_names))

      stopifnot("All collection dates are not in YMD format" = .CheckDateFormat(formatted_csv))

      .CheckPositionIsValid(formatted_csv, sample_storage_type, user_action)

      # check that there are no empty cells besides collection_date (checked later) and comment (may or may not exist)

      # errmsg <- paste0(sample_storage_type, " barcodes must be ", barcode_length_constraint, " long.")

      # barcode_length <- sum(nchar(unlist(strsplit(formatted_csv$barcode, "-"))))

      # stopifnot(errmsg = all(barcode_length == barcode_length_constraint))

      matched_columns <- match(required_names, names(formatted_csv))
      if (any(is.na(matched_columns))) {
        errmsg <- paste("Required database column is missing:", paste(required_names[is.na(matched_columns)], collapse = ", "))
        stop_validation_error(errmsg, 1)
      }

      missing_data <- is.na(formatted_csv[, requires_data])
      if (any(missing_data)) {
        errmsg <- paste("The upload file is missing data in required columns. Please check that your file contains the following column names and have data entries for each sample.", paste(requires_data, collapse = ", "))
        stop_validation_error(errmsg, 1)
      }

      ## Deeper validation using database

      container_tables <- list(
        "manifest" = switch(sample_storage_type,
          "1" = "micronix_plate",
          "2" = "cryovial_box",
          "3" = "dbs_paper"
        ),
        "container_class" = switch(sample_storage_type,
          "1" = "micronix_tube",
          "2" = "cryovial_tube",
          "3" = "dbs_spot"
        )
      )

      # make sure not uploading to well positions with active samples
      # note: potentially could increase speed by using copy_to if upload files are large

      con <- DBI::dbConnect(RSQLite::SQLite(), database)
      active_positions <- tbl(con, "storage_container") %>%
        select(status_id, id) %>%
        filter(status_id == 1) %>%
        inner_join(tbl(con, container_tables[["container_class"]]), by = c("id" = "id")) %>%
        inner_join(tbl(con, container_tables[["manifest"]]) %>% dplyr::rename(manifest_name = name), by = c("manifest_id" = "id")) %>%
        collect() %>%
        select(position, manifest_name) %>%
        inner_join(formatted_csv, by = c("manifest_name", "position")) %>%
        nrow(.)
      if (active_positions > 0) {
        stop_validation_error("Uploading sample to well location that already has an active sample", 1)
      }

      if ("upload" == user_action) {
        file_study_codes <- formatted_csv %>% pull(study_short_code) %>% unique(.)
        db_study_codes <- dbReadTable(con, "study") %>% pull(short_code)
        if (!all(file_study_codes %in% db_study_codes)) {
          stop_validation_error("Study not found", file_study_codes[!file_study_codes %in% db_study_codes])
        }

        file_specimen_types <- formatted_csv %>% pull(specimen_type) %>% unique(.)
        db_specimen_types <- dbReadTable(con, "specimen_type") %>% pull(name)

        if (!all(file_specimen_types %in% db_specimen_types)) {
          stop_validation_error("Specimen Type not found", file_specimen_types[!file_specimen_types %in% db_specimen_types])
        }

        stopifnot("Location does not exist" = formatted_csv %>%
          left_join(DBI::dbReadTable(con, "location") %>%
                      dplyr::rename(location_id = id), by = c('name', 'level_I', 'level_II')) %>%
          filter(is.na(location_id)) %>%
          nrow(.) == 0)
      }

      message("Validation complete.")
    },
    error_validation = function(e) {
      bError <<- TRUE
      errmsg <<- e$message
      values <<- e$values
    },
    finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
      if (bError) {
        formatted_csv <- NULL
        stop_validation_error(errmsg, values)
      }
    }) # end tryCatch
  }
}

# Logistical Checks
.FindHeader <- function(user_file, required_user_column_names, valid_header_rows = valid_header_rows) {

  # sanity check
  stopifnot("File is empty" = nrow(user_file) > 1)

  # this variable will be set with the valid header row position (if it exists)

  for (colname_ridx in valid_header_rows) {
    row <- user_file[colname_ridx, ]
    if (any(required_user_column_names %in% row)) {
      return(colname_ridx)
    }
  }

  return(NULL)
}

.CheckPositionIsValid <- function(formatted_csv, sample_storage_type, user_action) {

  # exit for now
  if (3 %in% sample_storage_type) {
    return()
  }

  positions <- formatted_csv %>% pull(position)

  if (any(c(1, 2) %in% sample_storage_type)) { # micronix, cryovial

    # check row letters
    row_letter_check <- substr(positions, 1, 1) %in% LETTERS

    # make sure the columns are numbers and above zero
    col_numbers <- substr(positions, 2, nchar(positions))
    col_number_indices <- which(col_numbers %>%
      as.numeric() %>%
      suppressWarnings() > 0)


    if ("1" %in% sample_storage_type && !all(nchar(col_numbers) == 2)) {
      stop("Numbers should be in ## format. Fill with zero if less than 10 (e.g \"05\")")
    }

    duplicates <- data.frame()
    if (user_action %in% "upload") {
      duplicates <- formatted_csv %>%
        group_by(manifest_name, position) %>%
        filter(n() > 1)
    }

    stopifnot("Invalid row letter detected" = all(row_letter_check))
    stopifnot("Invalid col number detected" = length(col_number_indices) == length(positions))
    stopifnot("Duplicated position detected" = nrow(duplicates) == 0)
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

stop_usage_error <- function(message) {
  rlang::abort("usage_error", message = message)
}

stop_formatting_error <- function(df) {
  message <- "There was an issue with the format of the file."
  rlang::abort("formatting_error", message = message, df = df)
}

stop_validation_error <- function(message, values) {
  rlang::abort("validation_error", message = message, values = values)
}
