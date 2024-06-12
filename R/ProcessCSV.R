#' Checks the format of a csv file and validates the data given sample type and file type parameters
#'
#' `ProcessCSV()` creates an intermediate dataframe from a user provided csv that can then be passed to UploadSamples or MoveSamples:
#'
#' @param user_csv The path to the user file on disk
#' @param user_action  The action that will be taken. This can be 'upload', 'move' or 'search'.
#' @param sample_storage_type The type of storage the samples are in. This can be '1', '2' or '3', which identify 'Micronix', 'Cryovial' or 'DBS', respectively.
#' @param file_type The file type. This usually will be 'na'. The default value is 'na'.
#' @param container_name Optional parameter to specify the container the samples are being added to. This is not required if the container name is specifed in your file.
#' @param freezer_address The location of the container as a named list `list(name=NULL, level_I=NULL, level_II=NULL)`. This is not required in the location is specified in file.
#' @param validate Whether to validate the data. Setting to `FALSE` will skip the validation step and will only check the file format. Default is `TRUE`.
#' @param database Path to the sampleDB database. Default is `Sys.getenv("SDB_PATH")`.
#' @param config_yml Path to the user configuration file. Default is `Sys.getenv("SDB_CONFIG")`.
#'#'
#' @examples
#' \dontrun{
#'  # Format a sample datasheet with micronix samples using the 'na' micronix format that will be added to sampleDB. Add the container name and container location as parameters.
#'  formatted_csv <- ProcessCSV(
#'    user_csv = "/path/to/sample_data.csv",
#'    user_action = "upload",
#'    file_type = "na",
#'    sample_storage_type = "micronix",
#'    container_name="LN2_XXXX",
#'    freezer_address=list(name="FreezerA", level_I="Shelf1", level_II="BasketC")
#'   )
#' }
#' @import lubridate
#' @import dplyr
#' @importFrom rlang abort
#' @import rjson
#' @import stringr
#' @export ProcessCSV


ProcessCSV <- function(user_csv, user_action, sample_storage_type, search_type = NULL, container_name = NULL, freezer_address = NULL, file_type = "na", validate = TRUE, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG")) {

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

  if (is.null(user_csv) || user_csv == "") {
    stop("No csv file was provided.")
  }

  # no header because the header will be identified by the storage type and the expected columns, and the traxcer header can be the second row
  # na.strings = "" to indicate real empty values. `NA` can be used to indicate that there is data not available. The only time
  # that this should be allowed is in collection date column for longitudinal studies when there is no date available.
  user_file <- read.csv(file = user_csv, header = FALSE, na.strings = "", blank.lines.skip = TRUE)

  valid_actions = c("upload", "move", "search")
  if (!user_action %in% valid_actions) {
    errmsg <- paste("Action is not valid. Valid actions are:", paste(valid_actions, collapse = ", "))
    stop_usage_error(errmsg)
  }

  ## Depending on the csv tool, there may be empty strings or other special characters
  user_file[user_file == ""] <- NA
  user_file[] <- lapply(user_file, function(x) as.character(gsub("[\n\t,]", "", x)))

  ## remove empty rows and columns
  ## we do select columns below, so removing columns is technically duplicate work

  # note: strange bug with search files, ignore for now
  if (user_action %in% c("move", "upload")) {
    empty_rows <- rowSums(user_file == "" | is.na(user_file) | is.null(user_file)) == ncol(user_file)
    empty_cols <- colSums(user_file == "" | is.na(user_file) | is.null(user_file)) == nrow(user_file)
    user_file <- user_file[!empty_rows, !empty_cols]
  }

  ## Read File Specification File
  file_specs_json <- rjson::fromJSON(file = system.file(
    "extdata", "file_specifications.json", package = .sampleDB$pkgname))

  ## Required Column Names
  required_user_column_names <- conditional_user_column_names <- optional_user_column_names <- NULL
  manifest_name <- NULL

  if (user_action %in% c("move", "upload")) {

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

    if (user_action %in% "upload") {
      required_user_column_names <- c(required_user_column_names, file_specs_json$shared$upload[['required']])
      conditional_user_column_names <- c(conditional_user_column_names, file_specs_json$shared$upload[['conditional']])
      optional_user_column_names <- c(optional_user_column_names, file_specs_json$shared$upload[['optional']])

      if (file_type == "traxcer") {
        manifest_name <- "Rack ID"
      } else {
        manifest_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$name
      }

      manifest_barcode_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$barcode
      location_parameters <- file_specs_json$shared$sample_type[[sample_type_index]]$location
      location_parameters <- unlist(location_parameters[c("name", "level_I", "level_II")])

      required_user_column_names <- c(required_user_column_names, c(manifest_name, unname(location_parameters)))
      optional_user_column_names <- c(optional_user_column_names, c(manifest_barcode_name))
    } else if (user_action %in% "move") {
      if (file_type == "traxcer") {
        manifest_name <- "Rack ID"
      } else {
        manifest_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$name
      }
      required_user_column_names <- c(required_user_column_names, manifest_name)
    }
  } else { ## Search
    if (search_type %in% c("barcode", "study_subject")) {
      required_user_column_names <- file_specs_json$shared[[user_action]][[search_type]]$required
    }
  }

  if (is.null(required_user_column_names)) {
    stop(paste("The expected column names for sample type", sample_storage_type, "and file type", file_type, "are not implemented (yet)."))
  }

  traxcer_position <- NULL
  ## If the file type is traxcer, replace with the custom config value
  if (file_type == "traxcer") {

    ## Read Configuration File and replace with user override from user preferences
    config <- yaml::read_yaml(config_yml)

    traxcer_position <- ifelse(
      !is.na(config$traxcer_position$override),
      config$traxcer_position$override,
      config$traxcer_position$default
    )

    if (!is.na(config$traxcer_position$override)) {
      required_user_column_names <- stringr::str_replace(required_user_column_names, config$traxcer_position$default, config$traxcer_position$override)
    }
  }

  ## second row is valid because traxcer will have "plate_label:" in the first row
  valid_header_rows <- 1:2

  header_row <- .FindHeader(user_file = user_file, required_user_column_names = required_user_column_names, valid_header_rows = valid_header_rows)

  if (is.null(header_row)) {
    df.error.formatting <- data.frame(
      column = required_user_column_names,
      reason = "Always Required",
      trigger = "Not detected in file"
    )

    stop_formatting_error(df.error.formatting)
  }

  colnames(user_file) = user_file[header_row,]
  user_file = user_file %>%  slice(-c(1:header_row))

  # Check the parameters of the function and see if some of the data points are there (in case called from R package)
  # then, filter out the columns that could not be resolved, and add to the data frame. This will be a formatting error.
  missing_columns <- required_user_column_names[!required_user_column_names %in% colnames(user_file)]

  if (user_action %in% "move") {
    if (!is.null(container_name) && manifest_name %in% missing_columns) {
      missing_columns <- missing_columns[missing_columns != manifest_name]
      user_file[manifest_name] <- container_name
    }
  }
  else if (user_action %in% "upload") {

    ## these are the special case columns that can be added by users
    if (!is.null(container_name) && manifest_name %in% missing_columns) {
      missing_columns <- missing_columns[missing_columns != manifest_name]
      user_file[manifest_name] <- container_name
    }
    if (!is.null(freezer_address) && all(location_parameters %in% missing_columns)) {
      missing_columns <- missing_columns[!location_parameters %in% missing_columns]
      user_file[location_parameters] <- freezer_address
    }

    if ("StudyCode" %in% colnames(user_file) && "upload" %in% user_action) {
      tmp <- CheckTable("study") %>%
        filter(short_code %in% user_file$StudyCode) %>%
        inner_join(user_file, by = c("short_code" = "StudyCode"))

      # collection date column must exist for all samples that are part of a longitudinal study
      if (!"CollectionDate" %in% colnames(user_file) && nrow(filter(tmp, is_longitudinal == 1)) > 0) {
        df.error.formatting <- rbind(
          df.error.formatting,
          data.frame(
            column = "CollectionDate",
            reason = "Collection date is required for samples of longitudinal studies.",
            trigger = filter(tmp, is_longitudinal == 1) %>% pull(short_code)
          )
        )
      }
    }
  }

  if (length(missing_columns) > 0) {
    df.error.formatting <- rbind(
      df.error.formatting,
      data.frame(
            column = missing_columns,
            reason = "Always Required",
            trigger = "Not detected in file"
          )
      )
  }

  ## Throw if any of the required columns are missing
  # since the application is retrofitting the already released shiny application, there is only a subset of fields checked. This
  # should be expanded upon.
  if (nrow(df.error.formatting)) {
    stop_formatting_error(df = df.error.formatting)
  }

  # include a row number column to let users know where problems are occuring. this should be done here,
  # after the correct columns are found and chosen
  user_file <- dplyr::mutate(user_file)
  if (user_action %in% "upload") {
    user_file <- select(user_file, all_of(required_user_column_names), any_of(conditional_user_column_names), any_of(optional_user_column_names))
  } else if (user_action %in% c("move", "search")) {
    user_file <- select(user_file, all_of(required_user_column_names))
  } # no need to have an else because action input validation is done above!

  message("Required columns detected.")

  if (validate) {

    ### Quality check the data now
    tryCatch({

      # this will map the processed file back to the user file in case there are validation errors
      dbmap <- NULL
      dbmap$row_number <- "RowNumber"

      ## todo - across the app, 1, 2, 3 should be changed to their character counterparts (below)
      ## until then do this for readability
      sample_storage_type = switch(
        sample_storage_type,
        "1" = "micronix",
        "2" = "cryovial",
        "3" = "dbs"
      )

      ## pass the row id to link back to the actual user file, so that
      # we can inform the user if there is an issue with one of their rows

      if (user_action %in% c("upload", "move")) {

        ## Micronix
        if (sample_storage_type == "micronix" && file_type == "na") {
          dbmap$barcode <- "Barcode"
          dbmap$position <- c("Row", "Column")
        } else if (sample_storage_type == "micronix" && file_type == "traxcer") {
          dbmap$barcode <- "Tube ID"
          dbmap$position <- traxcer_position
        } else if (sample_storage_type == "micronix" && file_type == "visionmate") {
          dbmap$barcode <- "Tube Row"
          dbmap$position <- c("LocationRow", "LocationColumn")
        }

        ## Cryovial
        else if (sample_storage_type == "cryovial") {
          dbmap$barcode <- "Barcode"
          dbmap$position <- c("BoxRow", "BoxColumn")

        ## DBS
        } else if (sample_storage_type == "dbs") {
          dbmap$position <- c("Row", "Column")
        } else {
          stop("Unimplemented position formatting code for this sample type.")
        }
      }

      # conditional and optional columns only apply for uploads.
      # search files only contain optional columns.
      # metadata only applies for uploads.

      if ("upload" %in% c(user_action)) {

        dbmap$study_short_code <- "StudyCode"
        dbmap$study_subject <- "StudySubject"
        dbmap$specimen_type <- "SpecimenType"
        dbmap$collection_date <- "CollectionDate"
        dbmap$comment <- "Comment"
        dbmap['name'] <- unname(location_parameters['name'])
        dbmap['level_I'] <- unname(location_parameters['level_I'])
        dbmap['level_II'] <- unname(location_parameters['level_II'])

        dbmap$manifest_barcode <- manifest_barcode_name
      }

      if (user_action %in% c("upload", "move")) {
        dbmap$manifest_name <- manifest_name
      }

      for (nm in names(dbmap)) {
        if (!all(dbmap[[nm]] %in% colnames(user_file))) {
          if (nm == "manifest_name" && !is.null(container_name)) {
            user_file[[dbmap[[nm]]]]  = c(container_name)
          }
        }
      }

      ## In cases where the CSV file was generated by certain software platforms (ie. traxcer),
      ## empty barcodes show be allowed and simply filtered out
      if (sample_storage_type == "micronix" && file_type=="traxcer") {
        message(sprintf("Removed %d rows with no barcode entries.", sum(is.na(user_file$`Tube ID`))))
        user_file <- user_file[!is.na(user_file$`Tube ID`),]
      }

      user_file <- CheckFormattedFileData(
        database = database,
        formatted_csv = user_file,
        file_type = file_type,
        sample_storage_type = sample_storage_type,
        user_action = user_action,
        dbmap = dbmap
      )

      user_file$row_number <- NULL
    },
    validation_error = function(e) {
      user_file = dplyr::mutate(user_file, RowNumber = row_number())

      # TODO: Make this return the users file with error annotation by row number (allow for multiple errors in cell)
      data1 <- lapply(1:length(e$data), function(x) {
        result <- list(Columns = e$data[[x]], CSV = inner_join(e$data[[x]], user_file, by = colnames(e$data[[x]])))

        tmp.2 = colnames(result$CSV)[colnames(result$CSV) != "RowNumber"]
        colnames(result$CSV) = c("RowNumber", tmp.2)

        tmp.2 = colnames(result$Columns)[colnames(result$Columns) != "RowNumber"]
        colnames(result$Columns) = c("RowNumber", tmp.2)

        return(result)
      })

      names(data1) <- names(e$data)
      stop_validation_error(e$message, data1)
    })

    message("Validation complete.")
  }

  return(user_file)
}


CheckFormattedFileData <- function(database, formatted_csv, file_type, sample_storage_type, user_action, dbmap) {

  #NOTE: dbmap is unused right now and should probably be removed from the function

  # this is an internal mapping to the database that should not be exposed to the user
  required_names <- requires_data <- container_metadata <- NULL
  if (user_action %in% c("upload", "move")) {
    required_names <- switch(sample_storage_type,
        "micronix" = c(
          "position",
          "barcode",
          "manifest_name"
        ),
        "cryovial" = c(
          "position",
          "manifest_name"
        ),
        "dbs" = c(
          "position",
          "manifest_name"
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

  # create a dbversion to use for validation. store a backup to replace the
  # when creating the error table

  for (nm in names(dbmap)) {
    if (all(dbmap[[nm]] %in% colnames(formatted_csv))) {
      formatted_csv = dplyr::rename(formatted_csv, unlist(dbmap[nm]))
    } else {
      formatted_csv[[nm]] = c(NA)
    }
  }

  # add rownumbers now
  formatted_csv = formatted_csv %>%
    dplyr::mutate(row_number = row_number())

  # these break down the position from A01 to `A` `01` and are used to map back to the original
  # user file that has the position listed in two columns
  if (!is.null(formatted_csv[["position1"]]) && !is.null(formatted_csv[["position2"]])) {
    formatted_csv$position <- sprintf(
      "%s%02d",
      formatted_csv[["position1"]],
      as.integer(formatted_csv[["position2"]])
    )

  # If position1 and position2 columns don't exist, check the format of position column
} else {
  incorrect_rows <- which(!grepl("^[A-Z][0-9]{2}$", formatted_csv$position))
  if (length(incorrect_rows) > 0) {
    incorrect_data <- formatted_csv[incorrect_rows, c("row_number", "position")]
    err <- .maybe_add_err(err, incorrect_data, "Incorrectly formatted positions", dbmap)
  }
}


  bError <- FALSE
  err <- errmsg <- NULL
  if (user_action %in% c("upload", "move")) {

    tryCatch({

      stopifnot("Required database column names not implemented" = !is.null(required_names))

      matched_columns <- match(required_names, names(formatted_csv))
      if (any(is.na(matched_columns))) {
        errmsg <- paste("Required database column is missing:", paste(required_names[is.na(matched_columns)], collapse = ", "))
        stop(errmsg)
      }

      ## broad sweep check if missing any data in required fields
      rs <- rowSums(is.na(formatted_csv[, requires_data])) > 0
      rn <- formatted_csv[rs, ] %>% pull(row_number)

      cs <- colSums(is.na(formatted_csv[, requires_data])) > 0
      cols <- colnames(formatted_csv[, requires_data])[cs]

      if (length(cols) > 0) {
        df <- formatted_csv[formatted_csv$row_number %in% rn, c("row_number", cols)]
        err <- .maybe_add_err(err, df, "Rows found with missing data", dbmap)
      }

      ## check to make sure there are no duplicated values
      # 1. Make sure that two samples aren't being uploaded to the same place
      if (!is.null(formatted_csv[["position"]])) {
        df = formatted_csv %>%
          group_by(manifest_name, position) %>%
          count() %>%
          collect()

        if (any(df$n > 1)) {

          if (file_type %in% c("traxcer", "visionmate")) {
            df <- formatted_csv[df$n > 1, ] %>% select(row_number, position, manifest_name)
          } else {
            df <- formatted_csv[df$n > 1, ] %>% select(row_number, position1, position2, manifest_name)
          }
          .maybe_add_err(err, df, "Uploading at least two samples to the same position", dbmap)
        }
      }

      ###################################################################
      ### Micronix And Cryovial Barcode Checks (In Memory validation) ###
      ###################################################################

      df = CheckDuplicatedBarcodes(formatted_csv, sample_storage_type)
      err <- .maybe_add_err(err, df, "Uploading at least two samples with identical barcodes", dbmap)

      if (sample_storage_type %in% c("micronix")) {
        df.1 = formatted_csv %>%
          group_by(row_number) %>%
          dplyr::mutate(
            letter_check = substr(position, 1, 1) %in% LETTERS,
            barcode_check = nchar(barcode) == 10
          )

        rn = df.1 %>% select(row_number, letter_check) %>% filter(letter_check == FALSE) %>% pull(row_number)

        if (file_type %in% c("traxcer", "visionmate")) {
          df <- formatted_csv[formatted_csv$row_number %in% rn, c("row_number", "position")]
        } else {
          df <- formatted_csv[formatted_csv$row_number %in% rn, c("row_number", "position1", "position2")]
        }
        err <- .maybe_add_err(err, df, "Rows must use letters", dbmap)

        rn = df.1 %>% select(row_number, barcode_check) %>% filter(barcode_check == FALSE) %>% pull(row_number)
        df <- formatted_csv[formatted_csv$row_number %in% rn, c("row_number", "barcode")]
        err <- .maybe_add_err(err, df, "Micronix Barcodes must be 10 digits long", dbmap)

      } else if (sample_storage_type %in% c("cryovial")) {
        df.1 = formatted_csv %>%
          group_by(row_number) %>%
          dplyr::mutate(
            letter_check = substr(position, 1, 1) %in% LETTERS
          )

        rn = df.1 %>% select(row_number, letter_check) %>% filter(letter_check == FALSE) %>% pull(row_number)
        if (file_type %in% c("traxcer", "visionmate")) {
          df <- formatted_csv[formatted_csv$row_number %in% rn, c("row_number", "position")]
        } else {
          df <- formatted_csv[formatted_csv$row_number %in% rn, c("row_number", "position1", "position2")]
        }
        err <- .maybe_add_err(err, df, "Rows must use letters", dbmap)
      }

      ## Deeper validation using database

      container_tables <- list(
        "manifest" = switch(sample_storage_type,
          "micronix" = "micronix_plate",
          "cryovial" = "cryovial_box",
          "dbs" = "dbs_paper"
        ),
        "container_class" = switch(sample_storage_type,
          "micronix" = "micronix_tube",
          "cryovial" = "cryovial_tube",
          "dbs" = "dbs_spot"
        )
      )

      ## check the formats of dates - right now this is hardcoded
      parsed_dates <- NULL
      token_mask <- NULL

      if (user_action %in% c("upload")) {

        allowed_date_formats = c("%Y-%m-%d")
        tokens = c("unk", "UNK", "unknown", "UNKNOWN")

        ## Start by parsing the string - NAs will appear if the allowed formats could not be detected
        ## this is a fairly minimal check so we need to confirm in other ways that the user is uploading
        ## dates in the correct format (ie. MM/DD/YYYY vs DD/MM/YYYY), particulary when there can be ambiguity
        parsed_dates <- lubridate::parse_date_time(formatted_csv$collection_date, allowed_date_formats, quiet = TRUE, exact = TRUE)

        ## Validate that only dates or NA mask values ("unk", "UNK", "unknown", "UNKNOWN") exist in the column
        token_mask <- !formatted_csv$collection_date %in% tokens

        ## Invalid formats will appear as NA in "parsed_dates". If they are also unrecognized tokens,
        ## report back to the user
        rn <- formatted_csv[!is.na(formatted_csv$collection_date) & is.na(parsed_dates) & token_mask,]$row_number  # Was not left out AND not a recognized date format AND not a recognized token
        df <- formatted_csv[formatted_csv$row_number %in% rn, c("row_number", "collection_date")]
        colnames(df) <- c("row_number", "collection_date")
        string <- paste("Unrecognized strings found in collection date column. Add any of the following if the collection date is unknown:", paste(tokens, collapse=", "))
        err <- .maybe_add_err(err, df, string, dbmap)

        if (length(rn) == 0) {
          formatted_csv$collection_date <- parsed_dates
          formatted_csv$collection_date[!token_mask] <- rep(lubridate::origin, sum(!token_mask))
          formatted_csv$collection_date = as.character(lubridate::as_date(formatted_csv$collection_date))
        }
      }

      con <- DBI::dbConnect(RSQLite::SQLite(), database)
      copy_to(con, formatted_csv)

      #########################
      ## Database validation ##
      #########################

      ## This is where various constraints are tested that involve querying the database.
      ## The formatted csv is queried to  build the error table when invalid rows
      ## are detected (failed to meet criteria)

      ## This is for all uploads regardless of type
      if (user_action %in% c("upload")) {

        ## check that dates exist for longitudinal studies
        df <- tbl(con, "formatted_csv") %>%
          dplyr::inner_join(
            tbl(con, "study") %>%
              select(short_code, is_longitudinal)
            , by = c("study_short_code" = "short_code")
          ) %>%
          filter(is_longitudinal == 1 & is.na(collection_date)) %>%
          select(row_number, study_short_code, collection_date) %>%
          collect()

        df$collection_date = as.character(lubridate::as_date(df$collection_date))

        err <- .maybe_add_err(err, df, "Missing collection date found for sample in longitudinal study", dbmap)
      }

      ## This is for all moves regardless of type
      if (user_action == "move") {

        ## check that the barcodes in the move file exist in the database
        df <- tbl(con, "formatted_csv") %>%
          filter(!is.na(barcode)) %>% # cryovials sometimes don't have barcodes
          left_join(tbl(con, container_tables[["container_class"]]), by = c("barcode")) %>%
          filter(is.na(id)) %>%
          select(row_number, barcode) %>%
          collect()

        err <- .maybe_add_err(err, df, "Barcode not found in database", dbmap)

        ## check if the container exists in the database
        df <- tbl(con, "formatted_csv") %>%
          left_join(tbl(con, container_tables[["manifest"]]) %>% dplyr::rename(manifest_barcode = barcode), by = c("manifest_name" = "name")) %>%
          filter(is.na(id)) %>%
          select(row_number, manifest_name) %>%
          collect()

        err <- .maybe_add_err(err, df, "Container not found", dbmap)

        ## only micronix and cryovial have barcodes (right now)
        if (sample_storage_type %in% c("micronix", "cryovial")) {
          df <- tbl(con, "formatted_csv") %>%
            inner_join(tbl(con, container_tables[["container_class"]]) %>%
              dplyr::rename(container_position = position), by = c("barcode")) %>%
            filter(is.na(id)) %>%
            select(row_number, barcode) %>%
            collect()

          err <- .maybe_add_err(err, df, "Barcodes not found in the database", dbmap)
        }
      }

      ##################################################################
      ### Micronix And Cryovial Barcode Checks (Database validation) ###
      ##################################################################

      ## Barcode checks only occur when uploading a file
      if (user_action == "upload") {

        ## Check for duplicated barcodes in-file (could be done in db..)
        df = CheckDuplicatedBarcodes(formatted_csv, sample_storage_type)
        err <- .maybe_add_err(err, df, "Uploading at least two samples with identical barcodes", dbmap)

        ## check if the barcodes already exist
        ## only micronix and cryovial have barcodes (right now)
        if (sample_storage_type %in% c("micronix", "cryovial")) {

          if (sample_storage_type %in% c("micronix")) {

            ## Check that micronix barcodes are 10 digits long and the
            ## first position character starts with a letter

            df = tbl(con, "formatted_csv") %>%
              group_by(row_number) %>%
              dplyr::mutate(
                letter_check = substr(position, 1, 1) %in% LETTERS,
                barcode_check = nchar(barcode) == 10
              )

            if (file_type %in% c("traxcer", "visionmate")) {
              df.1 <- df %>%
                filter(letter_check == FALSE) %>%
                select(row_number, position) %>%
                collect()
            } else {
              df.1 <- df %>%
                filter(letter_check == FALSE) %>%
                select(row_number, position1, position2) %>%
                collect()
            }

            err <- .maybe_add_err(err, df.1, "Rows must use letters", dbmap)

            df.1 = df %>%
              filter(barcode_check == FALSE) %>%
              select(row_number, barcode_check) %>%
              collect()

            err <- .maybe_add_err(err, df.1, "Micronix Barcodes must be 10 digits long", dbmap)

          } else if (sample_storage_type %in% c("cryovial")) {
            df = formatted_csv %>%
              group_by(row_number) %>%
              dplyr::mutate(
                letter_check = substr(position, 1, 1) %in% LETTERS
              )

            if (file_type %in% c("traxcer", "visionmate")) {
              df = df %>%
                filter(letter_check == FALSE) %>%
                select(row_number, position) %>%
                collect()

            } else {
              df = df %>%
                filter(letter_check == FALSE) %>%
                select(row_number, position1, position2) %>%
                collect()
            }
            err <- .maybe_add_err(err, df, "Rows must use letters", dbmap)
          }

          # Micronix barcodes are univesally unique
          if (sample_storage_type == "micronix") {
            df <- tbl(con, "formatted_csv") %>%
              inner_join(tbl(con, container_tables[["container_class"]]) %>%
                dplyr::rename(container_position = position), by = c("barcode")) %>%
              filter(!is.na(id)) %>%
              select(row_number, barcode) %>%
              collect()

            err <- .maybe_add_err(err, df, "Barcodes already exist in the database", dbmap)

          } else {

            # Cryovial barcodes are unique by study - they are also allowed to be left out
            df <- tbl(con, "formatted_csv") %>%
              filter(!is.na(barcode)) %>%  # Only check barcodes that exist in the upload
              inner_join(tbl(con, container_tables[["container_class"]]) %>%
              dplyr::rename(container_position = position, storage_container_id = id), by = c("barcode")) %>%
              inner_join(tbl(con, "storage_container") %>% dplyr::rename(storage_container_id = id), by = c("storage_container_id")) %>%
              inner_join(tbl(con, "specimen") %>% dplyr::rename("specimen_id" = "id"), by = c("specimen_id")) %>%
              inner_join(tbl(con, "study_subject") %>% dplyr::rename("study_subject_id" = "id"), by = c("study_subject_id")) %>%
              inner_join(tbl(con, "study") %>% dplyr::rename("study_id" = "id"), by = c("study_id", "study_short_code" = "short_code")) %>%
              select(row_number, barcode, study_short_code) %>%
              collect()

            err <- .maybe_add_err(err, df, "Barcodes found that already exist with current study", dbmap)
          }
        }

        ############################################################
        ### Addtl. Cryovial Upload Constraints (annotated below) ###
        ############################################################

        if (user_action == "upload" && sample_storage_type == "cryovial") {

          ## if the study is not longitudinal, StudySubject must be unique within the study
          df <- tbl(con, "formatted_csv") %>%
            inner_join(tbl(con, "study") %>% dplyr::rename("study_id" = "id"), by = c("study_short_code" = "short_code")) %>%
            inner_join(tbl(con, "study_subject") %>% dplyr::rename(study_subject_id=id, study_subject=name), by = c("study_subject", "study_id")) %>%
            filter(is_longitudinal == 0) %>%
            select(row_number, study_short_code, study_subject) %>%
            collect()

          err <- .maybe_add_err(err, df, "Study subjects must be unique in studies that are not longitudinal", dbmap)

          ## If the study is longitudinal, the study subject and collection date must be unique within the study
          df <- tbl(con, "formatted_csv") %>%
            inner_join(tbl(con, "study") %>% dplyr::rename("study_id" = "id"), by = c("study_short_code" = "short_code")) %>%
            inner_join(tbl(con, "study_subject") %>% dplyr::rename(study_subject_id=id, study_subject=name), by = c("study_subject", "study_id")) %>%
            inner_join(tbl(con, "specimen") %>% dplyr::rename(specimen_id=id), by = c("study_subject_id", "collection_date")) %>%
            inner_join(tbl(con, "specimen_type") %>% dplyr::rename(specimen_type_id=id, specimen_type=name), by = c("specimen_type_id")) %>%
            filter(is_longitudinal == 1 & !is.na(collection_date)) %>%
            select(row_number, study_subject, collection_date, study_short_code) %>%
            collect()

          err <- .maybe_add_err(err, df, "Study subject and collection date must be unique within a longitudinal study", dbmap)

          ## Cryovials are required to have collection dates if they have no barcode and there is already a sample from the study subject in the study
          df = tbl(con, "formatted_csv") %>%
            inner_join(tbl(con,"study") %>% dplyr::rename(study_id=id), by = c("study_short_code"="short_code")) %>%
            inner_join(tbl(con, "study_subject") %>% dplyr::rename(study_subject_id=id, study_subject=name), by = c("study_subject", "study_id")) %>%
            filter(is.na(barcode) & is.na(collection_date)) %>%
            select(row_number, study_subject, study_short_code, collection_date) %>%
            collect()

          err <- .maybe_add_err(err, df, "Sample must have a collection date if there is no barcode provided and there is already a sample from this study subject.", dbmap)
        }

        ###########################################################
        ### Check Study, Specimen Type, and Location References ###
        ###########################################################

        df <- tbl(con, "formatted_csv") %>%
          left_join(tbl(con, "study"), by = c("study_short_code" = "short_code")) %>%
          filter(is.na(id)) %>%
          select(row_number, study_short_code) %>%
          collect()

        err <- .maybe_add_err(err, df, "Study not found", dbmap)

        df <- tbl(con, "formatted_csv") %>%
          left_join(tbl(con, "specimen_type"), by = c("specimen_type" = "name")) %>%
          filter(is.na(id)) %>%
          select(row_number, specimen_type) %>%
          collect()

        err <- .maybe_add_err(err, df, "Specimen type not found", dbmap)

        df <- tbl(con, "formatted_csv") %>%
          left_join(tbl(con, "location") %>%
            dplyr::rename(location_id = id), by = c('name', 'level_I', 'level_II')) %>%
          filter(is.na(location_id)) %>%
          select(row_number, name, level_I, level_II) %>%
          collect()

        errstring = sprintf("The following %s, %s and / or %s are not found in the database", dbmap["name"], dbmap["level_I"], dbmap["level_II"])
        err <- .maybe_add_err(err, df, errstring, dbmap)
      }

      ########################################################
      ### Micronix And Cryovial Upload to Empty Well Check ###
      ########################################################

      if (user_action %in% c("upload")) {

        if (sample_storage_type %in% c("micronix", "cryovial")) {
          df <- tbl(con, "storage_container") %>%
            select(status_id, id) %>%
            filter(status_id == 1) %>%
            inner_join(tbl(con, container_tables[["container_class"]]) %>% dplyr::rename(container_class_barcode = barcode), by = c("id" = "id")) %>%
            inner_join(tbl(con, container_tables[["manifest"]]) %>%
                           dplyr::rename(
                               manifest_name = name,
                               manifest_barcode = barcode
                           ), by = c("manifest_id" = "id")) %>%
            inner_join(tbl(con, "formatted_csv"), by = c("manifest_name", "position"))

          if (file_type %in% c("traxcer", "visionmate")) {
            df = df %>%
              select(row_number, position, manifest_name) %>%
              collect()
          } else {
            df = df %>%
              select(row_number, position1, position2, manifest_name) %>%
              collect()
          }

          err <- .maybe_add_err(err, df, "Uploading sample to well location that already has an active sample", dbmap)
        }
      }
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
        DBI::dbDisconnect(con)
      }

      if (bError) {
        stop(errmsg)
      }

      ## throw if bad data found
      if (!is.null(err) && length(err) > 0) {
        stop_validation_error("There was a problem with the content of your file", err)
      }
    }) # end tryCatch

    return(formatted_csv)
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

.maybe_add_err <- function(err, df, msg, dbmap) {
  if (!is.null(df) && nrow(df) > 0) {
    for (nm in colnames(df)) {
      if (grepl("[0-9]$", nm)) {
        idx = as.integer(str_extract(nm, "[0-9]$"))
        nm.1 = sapply(strsplit(nm, "[0-9]$"), "[[", 1)
        sub.name = dbmap[[nm.1]][idx]
        colnames(df)[colnames(df) %in% nm] = sub.name
      } else {
        colnames(df)[colnames(df) %in% nm] = dbmap[nm]
      }
    }

    idx <- ifelse(is.null(err), 1, length(err) + 1)
    err[[idx]] <- df
    names(err)[idx] <- msg
  }

  return(err)
}

stop_usage_error <- function(message) {
  rlang::abort("usage_error", message = message)
}

stop_formatting_error <- function(df) {
  message <- "There was an issue with the format of the file."
  rlang::abort("formatting_error", message = message, df = df)
}

stop_validation_error <- function(message, data) {
  rlang::abort("validation_error", message = message, data = data)
}


#' Checks for duplicated barcodes in users formatted csv file
#'
#' This function is internally used to check a formatted csv file for duplicated barcodes. Note that this should accept the formatted file produced by ProcessCSV().
#'
#' @export CheckDuplicatedBarcodes
#'
#' @param formatted_csv The formatted file produced by ProcessCSV()
#' @param sample_storage_type The type of storage the samples are in. This can be '1', '2' or '3', which identify 'Micronix', 'Cryovial' or 'DBS', respectively.
#'
#' @noRd
CheckDuplicatedBarcodes <- function(formatted_csv, sample_storage_type) {

  # right now only accept Micronix and Cryovial storage types
  if (sample_storage_type %in% c("micronix")) {
    df <- formatted_csv %>%
      filter(!is.na(barcode)) %>%
      group_by(barcode) %>%
      count()

    if (any(df$n > 1)) {
      df <- df %>%
        filter(n > 1 & !is.na(barcode)) %>%
        inner_join(formatted_csv, by = c("barcode")) %>%
        select(row_number, barcode)

      colnames(df) <- c("RowNumber", "Barcode")

      return(df)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}


#' Checks for sample barcodes in users formatted csv file
#'
#' This function is internally used to check a formatted csv file for duplicated positions. Note that this should accept the formatted file produced by ProcessCSV().
#'
#' @export CheckSampleLocationUnique
#'
#' @param formatted_csv The formatted file produced by ProcessCSV()
#'
#' @noRd
CheckSampleLocationUnique <- function(formatted_csv, dbmap) {
  df <- formatted_csv %>%
    group_by(manifest_name, position) %>%
    count()

  if (any(df$n > 1)) {
    df <- formatted_csv.1[df$n > 1, ] %>% select(row_number, position, manifest_name)
    colnames(df) <- c(dbmap["row_number"], dbmap["position"], dbmap["manifest_name"])
    return(df)
  } else {
    return(NULL)
  }
}

