#' Check for Missing Required Columns in a CSV File
#'
#' @description This function checks whether all required columns, as specified in the
#' FileColumnAttributes object, are present in the provided CSV file.
#'
#' @param csv_data A data frame or tibble representing the CSV file.
#' @param FileColumnAttributes An object of the `FileColumnAttributes` class, which provides
#' the list of required columns for the CSV file.
#'
#' @return An `ErrorData` object that provides details about any missing required columns.
#' If no required columns are missing, NULL is returned.
#'
#' @examples
#' \dontrun{
#' csv_sample <- data.frame(A = 1:5, C = 6:10)
#' attributes <- FileColumnAttributes$new(required = c("A", "B"),
#'                                        conditional = NULL,
#'                                        optional = NULL,
#'                                        location = NULL,
#'                                        container = NULL)
#' error <- check_required_columns(csv_sample, attributes)
#' }
#'
#' @export

check_required_columns <- function(formatted_csv, file_column_attrs) {
  # Extracting the database column names from FileColumnAttributes' required fields
  db_column_names <- file_column_attrs$required
  
  # Find missing columns
  matched_columns <- match(db_column_names, names(formatted_csv))
  missing_columns <- db_column_names[is.na(matched_columns)]
  
  if (length(missing_columns) > 0) {
    description <- paste("Required database column is missing:", paste(missing_columns, collapse = ", "))
    return(ErrorData$new(description = description, columns = missing_columns, rows = integer(0)))
  }
  return(NULL)
}

#' Check Invalid Column Positions in Formatted CSV
#'
#' @description This function checks for columns in a formatted CSV file that do not match
#' the expected positions provided by FileColumnAttributes.
#'
#' @param formatted_csv A data frame or tibble representing the formatted CSV file.
#' @param file_column_attrs An object of the `FileColumnAttributes` class, which provides
#' the expected positions of columns from the formatted CSV file.
#'
#' @return A list of `ErrorData` objects, each representing a detected error regarding column positions.
#' If no errors are found, the list will be empty.
#'
#' @examples
#' \dontrun{
#' formatted_csv <- data.frame(A = 1:5, B = 6:10)
#' attributes <- FileColumnAttributes$new(required = c("A", "B"),
#'                                        conditional = NULL,
#'                                        optional = NULL,
#'                                        location = list(A = "B01", B = "A01"),
#'                                        container = NULL)
#' errors <- check_invalid_positions(formatted_csv, attributes)
#' }
#'
#' @export
check_invalid_positions <- function(formatted_csv, file_column_attrs) {
  error_list <- list()
  
  for (file_column_name in names(file_column_attrs$location)) {
    required_position <- file_column_attrs$location[[file_column_name]]
    
    # Handling the "Row", "Column" format and converting to "A01" format
    if (length(required_position) == 2) {
      required_position <- paste0(required_position[1], sprintf("%02d", as.numeric(required_position[2])))
    }
    
    current_position <- which(names(formatted_csv) == file_column_name)
    
    if (required_position != current_position) {
      description <- sprintf("Column '%s' has an incorrect position. Expected '%s' but found '%s'.", 
                             file_column_name, required_position, current_position)
      error_list[[length(error_list) + 1]] <- ErrorData$new(description = description, 
                                                            columns = file_column_name, 
                                                            rows = integer(0))
    }
  }
  
  return(error_list)
}


ValidateSpecimens <- function(database, formatted_csv, file_type, sample_storage_type, user_action, dbmap) {

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