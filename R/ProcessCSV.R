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
#'
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
#' @export


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

      manifest_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$name
      manifest_barcode_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$barcode
      location_parameters <- file_specs_json$shared$sample_type[[sample_type_index]]$location
      location_parameters <- unlist(location_parameters[c("name", "level_I", "level_II")])

      required_user_column_names <- c(required_user_column_names, c(manifest_name, unname(location_parameters)))
      optional_user_column_names <- c(optional_user_column_names, c(manifest_barcode_name))
    } else if (user_action %in% "move") {
      manifest_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$name
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

  if (user_action %in% c("upload", "move")) {
    user_file <- user_file %>% setNames(.[header_row, ]) %>% .[-c(1, header_row), ]
  } else {
    ## TODO: it turns into a list because there's one row, this needs to be cleaned up
    x <- user_file %>% setNames(.[header_row, ]) %>% .[-c(1, header_row), ]
    user_file <- NULL

    if (search_type == "barcode") {
      user_file <- data.frame(
        Barcodes = x %>% select(Barcodes)
      )
    } else {
      user_file <- data.frame(
        StudySubjects = x %>% select(StudySubjects)
      )
    }
  }

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
  user_file <- dplyr::mutate(user_file, RowNumber = row_number())
  if (user_action %in% "upload") {
    user_file <- select(user_file, RowNumber, all_of(required_user_column_names), contains(conditional_user_column_names), contains(optional_user_column_names))
  } else if (user_action %in% "move") {
    user_file <- select(user_file, RowNumber, all_of(required_user_column_names))
  }


  ## Now convert to sampleDB formatting

  # this will contain the processed, validated file that can be used for the associated action
  processed_file <- NULL

  # this will map the processed file back to the user file in case there are validation errors
  dbmap <- NULL

  ## pass the row id to link back to the actual user file, so that
  # we can inform the user if there is an issue with one of their rows
  processed_file$RowNumber <- user_file$RowNumber

  if (user_action %in% c("upload", "move")) {

    ## Micronix
    if (sample_storage_type == 1 && file_type == "na") {
      dbmap$barcode <- "Barcode"
      processed_file$barcode <- user_file[[dbmap$barcode]]

      dbmap$position <- c("Row", "Column")
      processed_file$position <- sprintf("%s%02d", user_file[[dbmap$position[1]]], as.integer(user_file[[dbmap$position[2]]]))

    } else if (sample_storage_type == 1 && file_type == "traxcer") {
      dbmap$barcode <- "Tube ID"
      processed_file$barcode <- user_file[[dbmap$barcode]]

      dbmap$position <- traxcer_position
      processed_file$position <- user_file %>% pull(all_of(dbmap$position))

    } else if (sample_storage_type == 1 && file_type == "visionmate") {
      dbmap$barcode <- "Tube Row"
      processed_file$barcode <- user_file[[dbmap$barcode]]

      dbmap$position <- c("LocationRow", "LocationColumn")
      processed_file$position <- sprintf("%s%02d", user_file[[dbmap$position[1]]], as.integer(user_file[[dbmap$position[2]]]))
    }

    ## Cryovial
    else if (sample_storage_type == 2) {
      dbmap$barcode <- "Barcode"
      processed_file$barcode <- user_file[[dbmap$barcode]]

      dbmap$position <- c("BoxRow", "BoxColumn")
      processed_file$position <- sprintf("%s%02d", user_file[[dbmap$position[1]]], as.integer(user_file[[dbmap$position[2]]]))

    ## DBS
    } else if (sample_storage_type == 3) {
      dbmap$position <- c("Row", "Column")
      processed_file$position <-  sprintf("%s%02d", user_file[[dbmap$position[1]]], as.integer(user_file[[dbmap$position[2]]]))
    } else {
      stop("Unimplemented position formatting code for this sample type.")
    }
  } else if (user_action == "search") {
    processed_file$barcode <- user_file %>% pull(all_of(required_user_column_names))
  }

  # conditional and optional columns only apply for uploads.
  # search files only contain optional columns.
  # metadata only applies for uploads.

  if ("upload" %in% c(user_action)) {

    dbmap$study_short_code <- "StudyCode"
    dbmap$study_subject <- "StudySubject"
    dbmap$specimen_type <- "SpecimenType"

    processed_file$study_short_code <- user_file[[dbmap$study_short_code]]
    processed_file$study_subject <- user_file[[dbmap$study_subject]]
    processed_file$specimen_type <- user_file[[dbmap$specimen_type]]

    dbmap$collection_date <- "CollectionDate"
    if ("CollectionDate" %in% colnames(user_file)) {
      processed_file$collection_date <- user_file[[dbmap$collection_date]]
    } else {
      processed_file$collection_date <- rep(NA, nrow(user_file))
    }

    dbmap$comment <- "Comment"
    if ("Comment" %in% colnames(user_file)) {
      processed_file$comment <- user_file[[dbmap$comment]]
    } else {
      processed_file$comment <- rep(NA, nrow(user_file))
    }

    dbmap['name'] <- unname(location_parameters['name'])
    dbmap['level_I'] <- unname(location_parameters['level_I'])
    dbmap['level_II'] <- unname(location_parameters['level_II'])

    if (all(location_parameters %in% colnames(user_file)) && is.null(freezer_address)) {
      processed_file$name <- user_file %>% pull(all_of(unlist(location_parameters['name'])))
      processed_file$level_I <- user_file %>% pull(all_of(unlist(location_parameters['level_I'])))
      processed_file$level_II <- user_file %>% pull(all_of(unlist(location_parameters['level_II'])))
    } else {
      processed_file$name <- rep(freezer_address[['name']], nrow(user_file))
      processed_file$level_I <- rep(freezer_address[['level_I']], nrow(user_file))
      processed_file$level_II <- rep(freezer_address[['level_II']], nrow(user_file))
    }

    dbmap$manifest_barcode <- manifest_barcode_name
    if (manifest_barcode_name %in% colnames(user_file)) {
      processed_file$manifest_barcode <- user_file %>% pull(all_of(manifest_barcode_name))
    } else {
      processed_file$manifest_barcode <- rep(NA, nrow(user_file))
    }
  }

  if (user_action %in% c("upload", "move")) {
    dbmap$manifest_name <- manifest_name
    if (manifest_name %in% colnames(user_file) && is.null(container_name)) {
      processed_file$manifest_name <- user_file %>% pull(all_of(manifest_name))
    } else {
      processed_file$manifest_name <- rep(container_name, nrow(user_file))
    }
  }

  # note: right now don't worry about this for dbmap...
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
    tryCatch({
      processed_file <- .CheckFormattedFileData(
        database = database,
        formatted_csv = processed_file,
        sample_storage_type = sample_storage_type,
        user_action = user_action,
        dbmap = dbmap
      )

      processed_file$RowNumber <- NULL
    },
    validation_error = function(e) {
      # TODO: Make this return the users file with error annotation by row number (allow for multiple errors in cell)
      data1 <- lapply(1:length(e$data), function(x) {
        result <- list(Columns = e$data[[x]], CSV = inner_join(e$data[[x]], processed_file, by = colnames(e$data[[x]])))
        
        tmp.2 = dbmap[colnames(result$CSV)[colnames(result$CSV) != "RowNumber"]]
        colnames(result$CSV) = c("RowNumber", tmp.2)

        tmp.2 = dbmap[colnames(result$Columns)[colnames(result$Columns) != "RowNumber"]]
        colnames(result$Columns) = c("RowNumber", tmp.2)

        return(result)
      })

      names(data1) <- names(e$data)
      stop_validation_error(e$message, data1)
    })

    message("Validation complete.")
  }

  return(processed_file)
}

.CheckFormattedFileData <- function(database, formatted_csv, sample_storage_type, user_action, dbmap) {

  #NOTE: dbmap is unused right now and should probably be removed from the function

  # this is an internal mapping to the database that should not be exposed to the user
  required_names <- requires_data <- container_metadata <- NULL
  if (user_action %in% c("upload", "move")) {
    required_names <- switch(sample_storage_type,
        "1" = c(
          "position",
          "barcode",
          "manifest_name"
        ),
        "2" = c(
          "position",
          "manifest_name"
        ),
        "3" = c(
          "position",
          "manifest_name"
        )
      )

    # todo
    # switch(sample_storage_type,
    #   "1" = c(
    #     "The location of the Micronix tube needs to be documented",
    #     "Micronix tubes are always required to have barcodes. ",
    #     "The PlateName is always required."
    #   ),
    #   "2" = c(
    #     "",
    #     "manifest_name"
    #   ),
    #   "3" = c(
    #     "position",
    #     "manifest_name"
    #   )
    # )


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


  bError <- FALSE
  err <- errmsg <- NULL

  if (user_action %in% c("upload", "move")) {

    con <- NULL
    tryCatch({

      stopifnot("Required database column names not implemented" = !is.null(required_names))

      matched_columns <- match(required_names, names(formatted_csv))
      if (any(is.na(matched_columns))) {
        errmsg <- paste("Required database column is missing:", paste(required_names[is.na(matched_columns)], collapse = ", "))
        stop(errmsg)
      }

      ## broad sweep check if missing any data in required fields
      rs <- rowSums(is.na(formatted_csv[, requires_data])) > 0
      rn <- formatted_csv[rs, ] %>% pull(RowNumber)

      cs <- colSums(is.na(formatted_csv[, requires_data])) > 0
      cols <- colnames(formatted_csv[, requires_data])[cs]

      if (length(cols) > 0) {
        df <- formatted_csv[rn, c("RowNumber", cols)]
        err <- .maybe_add_err(err, df, "Rows found with missing data")
      }

      ## check to make sure there are no duplicated values
      # 1. Make sure that two samples aren't being uploaded to the same place

      df <- formatted_csv %>%
        group_by(manifest_name, position) %>%
        count()

      if (any(df$n > 1)) {
        df <- formatted_csv[df$n > 1, ] %>% select(RowNumber, position, manifest_name)
        colnames(df) <- c("RowNumber", "position", "manifest_name")
        err <- .maybe_add_err(err, df, "Uploading at least two samples to the same position in a manifest")
      }

      if (sample_storage_type %in% c(1,2)) {
        ## only micronix and cryovial have barcodes (right now)
        # 2. Check for duplicated barcodes
        df <- formatted_csv %>%
          group_by(barcode) %>%
          count()

        if (any(df$n > 1)) {
          df <- df %>%
            filter(n > 1 & !is.na(barcode)) %>%
            inner_join(formatted_csv, by = c("barcode")) %>%
            select(RowNumber, barcode)

          colnames(df) <- c("RowNumber", "barcode")
          err <- .maybe_add_err(err, df, "Uploading at least two samples with identical barcodes")
        }
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

      con <- DBI::dbConnect(RSQLite::SQLite(), database)
      copy_to(con, formatted_csv)

      ## check the formats of dates - right now this is hardcoded
      if (user_action %in% c("upload")) {


        ## Start by parsing the string - NAs will appear if the allowed formats could not be detected
        ## this is a fairly minimal check so we need to confirm in other ways that the user is uploading
        ## dates in the correct format (ie. MM/DD/YYYY vs DD/MM/YYYY), particulary when there can be ambiguity
        parsed_dates <- lubridate::parse_date_time(formatted_csv$collection_date, c("%Y-%m-%d", "%m/%d/%Y"), quiet = TRUE, exact = TRUE)

        ## Validate that only dates or NA mask values ("unk", "UNK", "unknown", "UNKNOWN") exist in the column
        tokens <- c("unk", "UNK", "unknown", "UNKNOWN")
        token_mask <- !formatted_csv$collection_date %in% tokens

        ## Invalid formats will appear as NA in "parsed_dates". If they are also unrecognized tokens,
        ## report back to the user
        rn <- formatted_csv[!is.na(formatted_csv$collection_date) & is.na(parsed_dates) & token_mask,]$RowNumber  # Was not left out AND not a recognized date format AND not a recognized token
        df <- formatted_csv[rn, c("RowNumber", "collection_date")]
        colnames(df) <- c("RowNumber", "collection_date")
        string <- paste("Unrecognized strings found in collection date column. Add any of the following if the collection date is unknown:", paste(tokens, collapse=", "))
        err <- .maybe_add_err(err, df, string)

        rn <- formatted_csv[xor(is.na(parsed_dates[token_mask]), is.na(formatted_csv$collection_date[token_mask])),] %>% pull(RowNumber)

        df <- formatted_csv[rn, c("RowNumber", "collection_date")]

        err <- .maybe_add_err(err, df, "Rows found with improperly formatted dates")
        if (length(rn) == 0) {
          formatted_csv$collection_date <- parsed_dates
          formatted_csv$collection_date[!token_mask] <- rep(lubridate::origin, sum(!token_mask))
        }


        ## check that dates exist for longitudinal studies
        df <- tbl(con, "formatted_csv") %>%
          dplyr::inner_join(
            tbl(con, "study") %>%
              select(short_code, is_longitudinal)
            , by = c("study_short_code" = "short_code")
          ) %>%
          filter(is_longitudinal == 1 & is.na(collection_date)) %>%
          select(RowNumber, study_short_code, collection_date) %>%
          distinct() %>%
          collect()

        colnames(df) <- c("RowNumber", "study_short_code", "collection_date")

        err <- .maybe_add_err(err, df, "Missing collection date found for sample in longitudinal study")
      }

      if (user_action == "move") {

        ## check if the container exists in the database
        rn <- tbl(con, "formatted_csv") %>%
          left_join(tbl(con, container_tables[["manifest"]]) %>% dplyr::rename(manifest_barcode = barcode), by = c("manifest_name" = "name")) %>%
          filter(is.na(id)) %>%
          pull(RowNumber)

        df <- formatted_csv[rn, c("RowNumber", "manifest_name")]

        err <- .maybe_add_err(err, df, "Container not found")

        ## only micronix and cryovial have barcodes (right now)
        if (sample_storage_type %in% c(1,2)) {
          rn <- tbl(con, "formatted_csv") %>%
            inner_join(tbl(con, container_tables[["container_class"]]) %>%
              dplyr::rename(container_position = position), by = c("barcode")) %>%
            filter(is.na(id)) %>%
            pull(RowNumber)

          df <- formatted_csv[rn, c("RowNumber", "barcode")]

          err <- .maybe_add_err(err, df, "Barcodes not found in the database")
        }
      }

      if (user_action == "upload") {

        ## check if the barcodes already exist
        ## only micronix and cryovial have barcodes (right now)
        if (sample_storage_type %in% c(1,2)) {

          # Micronix barcodes are univesally unique
          if (sample_storage_type == 1) {
            rn <- tbl(con, "formatted_csv") %>%
              inner_join(tbl(con, container_tables[["container_class"]]) %>%
                dplyr::rename(container_position = position), by = c("barcode")) %>%
              filter(!is.na(id)) %>%
              pull(RowNumber)

            df <- formatted_csv[rn, c("RowNumber", "barcode")]

            err <- .maybe_add_err(err, df, "Barcodes already exist in the database")

          } else {

            # Cryovials are unique by study - they are also allowed to be left out
            rn <- tbl(con, "formatted_csv") %>%
              filter(!is.na(barcode)) %>%  # Only check barcodes that exist in the upload
              inner_join(tbl(con, container_tables[["container_class"]]) %>%
              dplyr::rename(container_position = position, storage_container_id = id), by = c("barcode")) %>%
              inner_join(tbl(con, "storage_container") %>% dplyr::rename(storage_container_id = id), by = c("storage_container_id")) %>%
              inner_join(tbl(con, "specimen") %>% dplyr::rename("specimen_id" = "id"), by = c("specimen_id")) %>%
              inner_join(tbl(con, "study_subject") %>% dplyr::rename("study_subject_id" = "id"), by = c("study_subject_id")) %>%
              inner_join(tbl(con, "study") %>% dplyr::rename("study_id" = "id"), by = c("study_id", "study_short_code" = "short_code")) %>%
              pull(RowNumber)

            df <- formatted_csv[rn, c("RowNumber", "barcode", "study_short_code")]

            err <- .maybe_add_err(err, df, "Barcodes found that already exist with current study")
          }
        }

        ## Check the references

        rn <- tbl(con, "formatted_csv") %>%
          left_join(tbl(con, "study"), by = c("study_short_code" = "short_code")) %>%
          filter(is.na(id)) %>%
          pull(RowNumber)

        df <- formatted_csv[rn, c("RowNumber", "study_short_code")]

        err <- .maybe_add_err(err, df, "Study not found")

        rn <- tbl(con, "formatted_csv") %>%
          left_join(tbl(con, "specimen_type"), by = c("specimen_type" = "name")) %>%
          filter(is.na(id)) %>%
          pull(RowNumber)

        df <- formatted_csv[rn, c("RowNumber", "specimen_type")]

        err <- .maybe_add_err(err, df, "Specimen type not found")

        rn <- tbl(con, "formatted_csv") %>%
          left_join(tbl(con, "location") %>%
            dplyr::rename(location_id = id), by = c('name', 'level_I', 'level_II')) %>%
          filter(is.na(location_id)) %>%
          pull(RowNumber)

        df <- formatted_csv[rn, c("RowNumber", "name", "level_I", "level_II")]

        errstring = sprintf("The following %s, %s and / or %s are not found in the database", dbmap["name"], dbmap["level_I"], dbmap["level_II"])
        err <- .maybe_add_err(err, df, errstring)
      }

      ## Validation shared by more than one action
      if (user_action %in% c("upload")) {

        action <- switch(
          user_action,
          "move" = "Moving",
          "upload" = "Uploading"
        )

        if (sample_storage_type %in% c(1,2)) {
          rn <- tbl(con, "storage_container") %>%
            select(status_id, id) %>%
            filter(status_id == 1) %>%
            inner_join(tbl(con, container_tables[["container_class"]]) %>% dplyr::rename(container_class_barcode = barcode), by = c("id" = "id")) %>%
            inner_join(tbl(con, container_tables[["manifest"]]) %>%
                           dplyr::rename(
                               manifest_name = name,
                               manifest_barcode = barcode
                           ), by = c("manifest_id" = "id")) %>%
            inner_join(tbl(con, "formatted_csv"), by = c("manifest_name", "position")) %>%
            pull(RowNumber)

          df <- formatted_csv[rn, c("RowNumber", "position", "manifest_name")]

          err <- .maybe_add_err(err, df, paste0(action, " sample to well location that already has an active sample"))

        } else if (sample_storage_type %in% c(3)) {

          # The only difference between this sql and the lines for micronix and cryovial is the barcode rename - dbs does not have a barcode.
          rn <- tbl(con, "storage_container") %>%
            select(status_id, id) %>%
            filter(status_id == 1) %>%
            inner_join(tbl(con, container_tables[["container_class"]]), by = c("id" = "id")) %>%
            inner_join(tbl(con, container_tables[["manifest"]]) %>%
                           dplyr::rename(
                               manifest_name = name,
                               manifest_barcode = barcode
                           ), by = c("manifest_id" = "id")) %>%
            inner_join(tbl(con, "formatted_csv"), by = c("manifest_name", "position")) %>%
            pull(RowNumber)

          df <- formatted_csv[rn, c("RowNumber", "position")]

          err <- .maybe_add_err(err, df, paste0(action, " sample to well location that already has an active sample"))
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

.maybe_add_err <- function(err, df, msg) {
  if (nrow(df) > 0) {
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
