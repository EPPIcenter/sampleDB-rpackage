#' Search for Wetlab Samples in the EPPIcenter SampleDB database
#'
#' @description Search for wetlab samples.
#'
#' @param sample_type A string specifying the type of EPPIcenter sample. (`micronix`, `cryovile`, `rdt` and/or `paper`)
#' @param sample_label A list of vectors specifying the vector micronix, cryovile, rdt, and paper label names (`micronix.label_name`, `cryovile.label_name`, `rdt.label_name` and/or `paper.label_name`)
#' @param container_name A list of vectors specifying the vector micronix, cryovile, rdt, and paper container names (`micronix.container_name`, `cryovile.container_name`, `rdt.container_name` and/or `paper.container_name`)
#' @param study_subject A study subjects string or a vector of study name strings. If `study_subject.file` is TRUE the path to a .csv file containing one column named study_subject can be uploaded and used to search the database for study subjects.
#' @param specimen_type A specimen type string or a vector of specimen type strings.
#' @param study A study short code string or a vector of study short code strings.
#' @param collection_dates A list of date values strings (`date.to` and `date.from`) that can be used to filter EPPIcenter samples
#' @param archived A logical value. `TRUE` filters for archived samples and `FALSE` filters for active samples
#' @param freezer A list specifying the vector `name`, `level_I`, and/or`level_II`
#' @param return_sample_ids A logical value. Setting `return_sample_ids` to `TRUE` means `SearchSamples` returns sample ids as well as search results. Setting `return_sample_ids` to `FALSE` means `SearchSamples` returns only search results. Default value is `FALSE`.
#' @examples
#' \dontrun{
#' SearchSamples(study = "KAM06", study_subject = "subject_1")
#' }
#' @import dplyr
#' @import RSQLite
#' @import purrr
#' @export SearchByType SearchSamples SearchControls


SearchByType <- function(sample_storage_type=NULL, control_type=NULL, filters = NULL, format = NULL, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG"), include_internal_sample_id = FALSE) {

  if (sum(!is.null(sample_storage_type, control_type)) != 1) {
    stop("Only one of sample_storage_type and control_type may be set at a time.")
  }

  search.results <- NULL
  if (!is.null(sample_storage_type)) {
    search.results = SearchSamples(
      sample_storage_type = sample_storage_type,
      filters = filters,
      format = format,
      database = database,
      config_yml = config_yml,
      include_internal_sample_id = include_internal_sample_id
    )
  } else {
    search.results = SearchControls(
      control_type = control_type,
      filters = filters,
      format = format,
      database = database,
      config_yml = config_yml,
      include_internal_sample_id = include_internal_sample_id
    )
  }

  return(search.results)
}

#' Get extractions
#' 
#' @param database A database path
#' @return A tibble with extractions
#' @export
#' @examples
#' database <- Sys.getenv("SDB_PATH")
#' get_extractions(database)
get_extractions <- function(database = Sys.getenv("SDB_PATH")) {
  
  con <- sampleDB::init_db_conn(database)
  on.exit(dbDisconnect(con), add = TRUE)
  tbl_storage_container <- tbl(con, "storage_container")
  tbl_specimen <- tbl(con, "specimen") %>% dplyr::rename(specimen_id = id)
  tbl_study_subject <- tbl(con, "study_subject") %>% dplyr::rename(study_subject_id = id)
  tbl_malaria_blood_control <- tbl(con, "malaria_blood_control") %>% dplyr::rename(malaria_blood_control_id = id)
  tbl_micronix_tube <- tbl(con, "micronix_tube") %>% dplyr::rename(tube_barcode = barcode)
  tbl_micronix_plate <- tbl(con, "micronix_plate") %>%
    dplyr::rename(
      manifest_id = id,
      plate_barcode = barcode,
      plate_name = name
    )
  
  tbl_specimen_type <- tbl(con, "specimen_type") %>%
    dplyr::rename(
      specimen_type_id = id,
      specimen = name
    )
  
  tbl_storage_container %>% 
    inner_join(tbl_specimen, by = c("specimen_id")) %>% 
    inner_join(tbl_specimen_type, by = c("specimen_type_id")) %>%
    inner_join(tbl_study_subject, by = c("study_subject_id")) %>% 
    inner_join(tbl_malaria_blood_control, by = c("study_subject_id")) %>%
    select(-specimen_id, -study_subject_id, -malaria_blood_control_id) %>%
    inner_join(tbl_micronix_tube, by = c("id")) %>%
    inner_join(tbl_micronix_plate, by = c("manifest_id")) %>%
    select(
      Barcode = tube_barcode,
      Position = position,
      Plate = plate_name,
      Specimen = specimen,
    ) %>%
    collect()
}

#' Get all malaria blood controls
#'
#' `SearchControls()` can be used to upload controls to the sampleDB database. This function returns lazy-sql.
#'
#' @param con A dplyr dbConnect() connection object
#'
#' @import dplyr
#' @import RSQLite
#' @import lubridate
#'
SearchControls <- function(filters, control_type = NULL, database = Sys.getenv("SDB_PATH"), include_internal_control_id = FALSE) {
  results = NULL
  con <- dbConnect(RSQLite::SQLite(), database)  # Updated to use the `database` parameter
  tryCatch({

    sql = tbl(con, "composition_strain") %>%
      dplyr::rename(composition_strain_id = id) %>%
      group_by(composition_id) %>%
      dplyr::mutate(n_strain = n()) %>%
      ungroup() %>%
      left_join(tbl(con, "strain") %>% dplyr::rename(strain_id = id, strain = name), by = c("strain_id")) %>%
      left_join(tbl(con, "composition") %>% dplyr::rename(composition_id = id), by = c("composition_id")) %>%
      left_join(tbl(con, "malaria_blood_control") %>% dplyr::rename(malaria_blood_control_id = id), by = c("composition_id")) %>%
      left_join(tbl(con, "study_subject") %>% dplyr::rename(study_subject_id = id, control_uid = name), by = c("study_subject_id")) %>%
      left_join(tbl(con, "study") %>% dplyr::rename(study_id = id, batch_creation_date = created, batch = short_code), by = c("study_id")) %>%
      distinct()

    # Filter conditions updated for consistency
    if (!is.null(filters$strain) && filters$strain != "") {
      sql <- sql %>% filter(strain %in% local(filters$strain))
    }

    if (!is.null(filters$density) && filters$density != "") {
      sql <- sql %>% filter(density %in% local(filters$density))
    }

    if (!is.null(filters$short_code) && filters$short_code != "") {
      sql <- sql %>% filter(batch %in% local(filters$short_code))
    }

    if (!is.null(filters$percentage) && filters$percentage != "") {
      sql <- sql %>% filter(percentage %in% local(filters$percentage))
    }

    if (!is.null(filters$composition_type) && filters$composition_type != "") {
      sql <- sql %>% filter(n_strain %in% local(filters$composition_type))
    }

    if (!is.null(filters$study_subject) && filters$study_subject != "") {
      sql <- sql %>% filter(control_uid %in% local(filters$study_subject))
    }

    if (!is.null(control_type) && control_type == "dbs_sheet") {

      sql <- sql %>%
        inner_join(tbl(con, "blood_spot_collection") %>% dplyr::rename(blood_spot_collection_id = id), by = c("malaria_blood_control_id")) %>%
        inner_join(tbl(con, "dbs_control_sheet") %>% dplyr::rename(dbs_control_sheet_id = id, sheet_label=label), by = c("dbs_control_sheet_id")) %>%
        inner_join(tbl(con, "dbs_bag") %>% dplyr::rename(dbs_bag_id = id, dbs_bag_label = name), by = c("dbs_bag_id"))


      sql <- sql %>%
        left_join(tbl(con, "archived_dbs_blood_spots") %>% dplyr::rename(archived_dbs_blood_spot_id = id), by = c("blood_spot_collection_id")) %>%
        left_join(tbl(con, "status") %>% dplyr::rename(status_id = id, status = name), by = c("status_id"))

      # Filter so that only collections that have active spots are returned
      if (!is.null(filters$state) && !is.null(filters$status)) {
        if (filters$state == "Active" && filters$status == "In Use") {
          sql <- sql %>% filter(is.na(status_id) | status != "Exhausted")
        } else if (filters$state == "Archived") {
          sql <- sql %>% filter(status %in% local(filters$status))
        }
      }

    } else if (!is.null(control_type) && control_type == "whole_blood") {
      sql <- sql %>%
        inner_join(tbl(con, "whole_blood_tube") %>% dplyr::rename(whole_blood_tube_id = id), by = c("malaria_blood_control_id")) %>%
        inner_join(tbl(con, "cryovial_box") %>% dplyr::rename(cryovial_box_id = id, cryovial_box_name = name), by = c("cryovial_box_id"))

      sql <- sql %>%
        inner_join(tbl(con, "state") %>% dplyr::rename(state_id = id, state = name), by = c("state_id")) %>%
        inner_join(tbl(con, "status") %>% dplyr::rename(status_id = id, status = name), by = c("status_id")) %>%
        filter(state %in% local(filters$state) & status %in% local(filters$status))

    } else {
      stop("No search implementation for this control type!")
    }

    sql = FilterByLocation(con, sql, filters$location)

    results = collect(sql)

    if (control_type == "dbs_sheet") {

      if (filters$state == "Archived") {

        results <- results %>%
          dplyr::group_by(malaria_blood_control_id, blood_spot_collection_id, study_subject_id, archived_dbs_blood_spot_id) %>%
          dplyr::mutate(percentage=list(percentage)) %>%
          dplyr::mutate(strain=list(strain)) %>%
          dplyr::ungroup() %>%
          dplyr::distinct() %>%
          dplyr::mutate(n_strain = format_composition_types(n_strain))

        results = results %>%
          dplyr::select(malaria_blood_control_id, archived_dbs_blood_spot_id, archived_spots_count, reason, archived_date, batch,control_uid,n_strain,density,percentage,strain,sheet_label,dbs_bag_label,total,exhausted,location_root,level_I,level_II) %>%
          dplyr::mutate(reason = ifelse(reason == "", NA, reason)) %>%  # NA will be translated to '-' in the UI)
          dplyr::rename(
            ControlID = malaria_blood_control_id,
            ArchivedSpotsID = archived_dbs_blood_spot_id,  # Archived Spots
            ArchivedSpots = archived_spots_count,
            Reason = reason,
            ArchivedDate = archived_date,
            Batch = batch,
            ControlUID = control_uid,
            Composition = n_strain,
            Density = density,
            Percentage = percentage,
            Strain = strain,
            SheetName = sheet_label,
            BagName = dbs_bag_label,
            Total = total,
            Exhausted = exhausted,
            FreezerName = location_root,
            ShelfName = level_I,
            BasketName = level_II
          )

      } else {

        results <- results %>%
          dplyr::group_by(malaria_blood_control_id, blood_spot_collection_id, study_subject_id) %>%
          dplyr::mutate(percentage=list(percentage)) %>%
          dplyr::mutate(strain=list(strain)) %>%
          dplyr::ungroup() %>%
          dplyr::distinct() %>%
          dplyr::mutate(n_strain = format_composition_types(n_strain))

        results = results %>%
          select(malaria_blood_control_id, blood_spot_collection_id, batch,control_uid,n_strain,density,percentage,strain,sheet_label,dbs_bag_label,total,exhausted,location_root,level_I,level_II) %>%
          dplyr::rename(
            ControlID = malaria_blood_control_id,
            CollectionID = blood_spot_collection_id,  # Active spots
            Batch = batch,
            ControlUID = control_uid,
            Composition = n_strain,
            Density = density,
            Percentage = percentage,
            Strain = strain,
            SheetName = sheet_label,
            BagName = dbs_bag_label,
            Total = total,
            Exhausted = exhausted,
            FreezerName = location_root,
            ShelfName = level_I,
            BasketName = level_II
          )
        }

      } else {  # Whole Blood

        results <- results %>%
          dplyr::group_by(malaria_blood_control_id, study_subject_id, whole_blood_tube_id) %>%
          dplyr::mutate(percentage=list(percentage)) %>%
          dplyr::mutate(strain=list(strain)) %>%
          dplyr::ungroup() %>%
          dplyr::distinct() %>%
          dplyr::mutate(n_strain = format_composition_types(n_strain))

        results = results %>%
          select(malaria_blood_control_id, whole_blood_tube_id, batch,control_uid,n_strain,density,percentage,strain,position,cryovial_box_name,location_root,level_I,level_II,state,status) %>%
          dplyr::rename(
            ControlID = malaria_blood_control_id,
            TubeID = whole_blood_tube_id,
            Batch = batch,
            ControlUID = control_uid,
            `Composition Type` = n_strain,
            Density = density,
            Percentage = percentage,
            Strain = strain,
            Position = position,
            BoxName = cryovial_box_name,
            FreezerName = location_root,
            ShelfName = level_I,
            BasketName = level_II,
            State = state,
            Status = status
          )
      }
  },
  error = function(e) {
    message(sprintf("Error in GetControls(): %s", e$message))
    stop(e$message)
  }, finally = {
    dbDisconnect(con)
  })

  return(distinct(results))
}

#' Search for Compositions in Database
#'
#' This function searches for compositions in a SQLite database using specified filters.
#' The filters include strain, percentage, and composition types.
#'
#' @param filters A named list containing filter criteria.
#'   - `strain`: a vector of strains to filter by
#'   - `percentage`: a vector of percentages to filter by
#'   - `composition_types`: a vector of composition types to filter by
#' @param database The path to the SQLite database. Defaults to the value from the SDB_PATH environment variable.
#'
#' @return A data frame containing the filtered compositions.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- search_compositions(filters = list(strain = c("3D7", "D6"), percentage = c(0.32, 0.33)))
#' }
search_compositions <- function(filters, database = Sys.getenv("SDB_PATH")) {

  # Initialize database connection
  con <- init_db_conn(database)
  # Ensure the connection is closed when exiting
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Create the basic query
  query <- tbl(con, "composition_strain") %>%
    dplyr::inner_join(tbl(con, "strain") %>% dplyr::rename(strain_id = id, strain = name), by = c("strain_id")) %>%
    dplyr::inner_join(tbl(con, "composition") %>% dplyr::rename(composition_id = id), by = c("composition_id"))

  # Count the number of strains in each composition
  # This is needed to filter by composition types
  query <- query %>%
    dplyr::group_by(composition_id) %>%
    dplyr::add_count(composition_id, name = "strain_count") %>%
    dplyr::ungroup() %>%
    select(composition_id, strain, percentage, strain_count, legacy, index, label)

  # Applying filters
  if (!is.null(filters$strain)) {
    query <- query %>% dplyr::filter(strain %in% !!filters$strain)
  }

  if (!is.null(filters$percentage)) {
    query <- query %>% dplyr::filter(percentage %in% !!filters$percentage)
  }

  if (!is.null(filters$composition_types)) {
    query <- query %>% dplyr::filter(strain_count %in% !!filters$composition_types)
  }

  # Collect the result and apply further transformations
  result <- collect(query) %>%
    format_labels() %>%
    dplyr::mutate(
      strain_count = format_composition_types(strain_count),
      legacy = ifelse(legacy == 1, "True", "False")
    ) %>%
    select(-c(index))

  return(result)
}

FilterByLocation <- function(con, sql, location) {

  sql <- sql %>%
    inner_join(tbl(con, "location") %>%
      dplyr::rename(location_id = id) %>%
      select(location_id, location_root, level_I, level_II)
    , by = c("location_id"))

  if (!is.null(location)) {
    if (!is.null(location[['location_root']]) & !is.null(location[['level_I']]) & !is.null(location[['level_II']])) {
      sql <- filter(sql, location_root == local(location[['location_root']]) & level_I == local(location[['level_I']]) & level_II == local(location[['level_II']]))
    } else if (!is.null(location[['location_root']]) & !is.null(location[['level_I']])) {
      sql <- filter(sql, location_root == local(location[['location_root']]) & level_I == local(location[['level_I']]))
    } else if (!is.null(location[['location_root']])) {
      sql <- filter(sql, location_root == local(location[['location_root']]))
    }
  }

  return (sql)
}


SearchSamples <- function(sample_storage_type, filters = NULL, format = "na", database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG"), include_internal_sample_id = FALSE) {
  db.results <- NULL

  if (is.null(sample_storage_type) || !sample_storage_type %in% c("micronix", "cryovial", "dbs_sample", "static_plate")) {
    stop("No search implemenation available for this sample_storage_type")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  dbBegin(con)

  tryCatch({
    container_tables <- list(
      "manifest" = switch(sample_storage_type,
        "micronix" = "micronix_plate",
        "cryovial" = "cryovial_box",
        "dbs_sample" = switch(
          filters$container_type,
          "bag" = "bag",
          "box" = "box",
          "all" = "all"
        ),
        "static_plate" = "micronix_plate"
      ),
      "container_class" = switch(sample_storage_type,
        "micronix" = "micronix_tube",
        "cryovial" = "cryovial_tube",
        "dbs_sample" = "paper",
        "static_plate" = "static_well"
      )
    )

    sql <- tbl(con, "study") %>% dplyr::rename(study_id = id) %>% select(study_id, short_code)
    if (!is.null(filters$short_code)) {
      sql <- filter(sql, short_code %in% local(filters$short_code))
    }

    sql <- inner_join(sql, tbl(con, "study_subject") %>%
      dplyr::rename(study_subject_id = id, study_subject = name) %>%
      select(study_subject_id, study_subject, study_id), by = c("study_id"))

    if (!is.null(filters$study_subject)) {
      sql <- filter(sql, study_subject %in% local(filters$study_subject))
    }

    sql <- sql %>%
      inner_join(
        tbl(con, "specimen") %>%
          dplyr::rename(specimen_id = id) %>%
          select(specimen_id, study_subject_id, specimen_type_id, collection_date)
        , by = c("study_subject_id"))

    sql <- sql %>% inner_join(
      tbl(con, "specimen_type") %>%
        dplyr::rename(specimen_type_id = id, specimen_type = name) %>%
        select(specimen_type_id, specimen_type)
      , by = c("specimen_type_id"))

    if (!is.null(filters$specimen_type)) {
      sql <- filter(sql, specimen_type == local(filters$specimen_type))
    }

    sql <- inner_join(sql, tbl(con, "storage_container") %>% dplyr::rename(storage_container_id = id) %>% select(-c(created, last_updated)), by = c("specimen_id"))
    sql <- inner_join(sql, tbl(con, "status") %>% dplyr::rename(status_id = id, status = name), by = c("status_id"))
    sql <- inner_join(sql, tbl(con, "state") %>% dplyr::rename(state_id = id, state = name), by = c("state_id"))

    if (!is.null(filters$state)) {
      sql <- filter(sql, state == local(filters$state))
    }

    if (!is.null(filters$status)) {
      sql <- filter(sql, status == local(filters$status))
    }

    sql <- inner_join(sql, tbl(con, container_tables[["container_class"]]) %>% dplyr::rename(storage_container_id = id), by = c("storage_container_id")) %>% collapse()

    if (!is.null(filters$barcode)) {
      sql <- filter(sql, barcode %in% local(filters$barcode))
    }

    # Join manifest table
    if (sample_storage_type %in% c("cryovial", "micronix", "static_plate")) {
      sql <- inner_join(sql, tbl(con, container_tables[["manifest"]]) %>% dplyr::rename(manifest_id = id, manifest = name, manifest_barcode = barcode), by = c("manifest_id")) %>% collapse()
    } else if (sample_storage_type == "dbs_sample"){
      if (filters$container_type == "all") {
        box_tbl <- tbl(con, "box") %>%
          dplyr::mutate(manifest_type = "box") %>%
          dplyr::rename(manifest_id = id, manifest = name)

        bag_tbl <- tbl(con, "bag") %>%
          dplyr::mutate(manifest_type = "bag") %>%
          dplyr::rename(manifest_id = id, manifest = name)

        box_bag_union_tbl <- union_all(box_tbl, bag_tbl)

        sql <- left_join(sql, box_bag_union_tbl, by = join_by(manifest_id, manifest_type)) %>%
          collapse()

      } else {
        sql <- inner_join(sql, tbl(con, container_tables[["manifest"]]) %>%
          dplyr::rename(manifest_id = id, manifest = name), by = c("manifest_id")) %>%
        filter(manifest_type == !!container_tables[["manifest"]]) %>%
        collapse()
      }

      sql <- sql %>%
        mutate(manifest_type = ifelse(manifest_type == "bag", "Bag", "Box"))

    } else {
      stop("Invalid sample storage type!!!")
    }

    if (!is.null(filters$manifest)) {
      sql <- filter(sql, manifest == local(filters$manifest))
    }

    sql = FilterByLocation(con, sql, filters$location)

    ## map results to the final columns
    ## note: order matters here
    dbmap <- get_db_map(sample_storage_type, format = format, config = config_yml, filters = filters)

    if (include_internal_sample_id) {

      ## Do date collection here because lubridate and purrr::map (used by dplyr sql backend) is not cooperating
      db.results <- sql %>% select("storage_container_id", names(dbmap)) %>% collect() %>% dplyr::mutate(collection_date = as_date(collection_date))

      if (!is.null(filters$collection_date) && sum(is.na(filters$collection_date)) == 0) {
        if (!is.null(filters$collection_date$date.from) && !is.null(filters$collection_date$date.to)) {
          intervals <- list()
          for (i in 1:length(filters$collection_date$date.from)) {
            intervals <- append(
              intervals,
              list(
                interval(
                  lubridate::as_date(local(filters$collection_date$date.from[i])),
                  lubridate::as_date(local(filters$collection_date$date.to[i]))
                )
              )
            )
          }
          db.results <- filter(db.results, collection_date %within% intervals)
        }

      }

      if (!is.null(format)) {
        colnames(db.results) <- c("Sample ID", unname(dbmap))
      }

    } else {
      db.results <- sql %>% select(names(dbmap)) %>% collect() %>% dplyr::mutate(collection_date = as_date(collection_date))

      if (!is.null(filters$collection_date) && sum(is.na(filters$collection_date)) == 0) {
        if (!is.null(filters$collection_date$date.from) && !is.null(filters$collection_date$date.to)) {
          intervals <- list()
          for (i in 1:length(filters$collection_date$date.from)) {
            intervals <- append(
              intervals,
              list(
                interval(
                  lubridate::as_date(local(filters$collection_date$date.from[i])),
                  lubridate::as_date(local(filters$collection_date$date.to[i]))
                )
              )
            )
          }
          db.results <- filter(db.results, collection_date %within% intervals)
        }
      }

      if (!is.null(format)) {
        colnames(db.results) <- unname(dbmap)
      }
    }
  },
  error = function(e) {
    message(e$message)

  }, finally = {
    dbDisconnect(con)
  })

  return (db.results)
}



#' Extract Search Criteria from User CSV File
#'
#' This function reads a CSV file provided by the user and extracts
#' search criteria based on a specified search type (either "barcode" or "study_subject").
#'
#' @param user_csv A path to the user-provided CSV file.
#' @param search_type A string indicating the type of search ("barcode" or "study_subject").
#'
#' @return A message indicating whether the required columns were detected.
#' @export
extract_search_criteria <- function(user_csv, search_type) {
  if (!require(dplyr)) {
    stop("Function requires dplyr for database access!")
  }

  # Read the user CSV
  user_file <- read_and_preprocess_csv(user_csv)

  # Set possible names for columns
  possible_barcode_names <- c("Barcode", "Barcodes", "Tube Code", "Tube Codes", "TubeCode", "TubeCodes", "TubeID", "Tube ID")
  possible_study_subject_names <- c("Study Subject", "Study Subjects", "StudySubjects", "StudySubject")
  possible_collection_date_names <- c("Collection Date", "CollectionDate", "Collection Dates", "CollectionDates")
  possible_study_code_names <- c("Study Codes", "StudyCodes", "Studies", "Study Code", "StudyCode")
  possible_specimen_type_names <- c("Specimen Type", "SpecimenType", "Specimen Types", "SpecimenTypes")

  # Set required and optional column names based on search type
  if (search_type == "barcode") {
    required_user_column_names <- possible_barcode_names
    optional_user_column_names <- NULL
  } else if (search_type == "study_subject") {
    required_user_column_names <- possible_study_subject_names
    optional_user_column_names <- list(CollectionDate = possible_collection_date_names, StudyCode = possible_study_code_names, SpecimenType = possible_specimen_type_names)
  } else {
    stop(paste("Unknown search_type:", search_type))
  }

  # Find the header row
  valid_header_rows <- 1:2
  header_row <- find_header(user_file = user_file, required_user_column_names = required_user_column_names, valid_header_rows = valid_header_rows)

  # If no header is found, stop and raise an error
  if (is.null(header_row)) {
    df.error.formatting <- data.frame(
      column = required_user_column_names,
      reason = "Always Required",
      trigger = "Not detected in file"
    )
    stop_formatting_error("Could not find required header row", df.error.formatting)
  }

  # Initialize available_columns
  available_columns <- character(0)

  # Set the actual column names
  user_file <- set_header_row(user_file, header_row)

  # Detect optional columns if available
  if (!is.null(optional_user_column_names)) {
    for (name in names(optional_user_column_names)) {
      optional_column_present <- intersect(optional_user_column_names[[name]], colnames(user_file))
      if (length(optional_column_present) > 0) {
        available_columns <- c(available_columns, optional_column_present[1])
      }
    }
  }

  # Check for found required columns
  found_required_column <- intersect(required_user_column_names, colnames(user_file))

  if (length(found_required_column) != 1) {
    df.error.formatting <- data.frame(
      column = required_user_column_names,
      reason = "At least one required",
      trigger = paste("Available headers: ", paste(colnames(user_file), collapse = ", "))
    )
    stop_formatting_error("None of the required columns were detected. Please ensure at least one of the required columns is present.", df.error.formatting)
  }

  # Add found required column
  available_columns <- c(available_columns, found_required_column)
  user_file <- select(user_file, all_of(available_columns))

  # Utility to find the column and add to user_file
  find_and_add_column <- function(data, possible_names) {
    for (col_name in possible_names) {
      if (col_name %in% colnames(data)) {
        return(data[[col_name]])
      }
    }
    return(NULL) # return NULL if none of the possible names are found
  }

  ## Grab the possible columns if this is a study subject search (possible because they may not all be there, which is okay)
  if (search_type == "study_subject") {
    user_file$StudySubject <- user_file[[found_required_column]]
    user_file$CollectionDate <- find_and_add_column(user_file, possible_collection_date_names)
    user_file$StudyCode <- find_and_add_column(user_file, possible_study_code_names)
    user_file$SpecimenType <- find_and_add_column(user_file, possible_specimen_type_names)
  } else if (search_type == "barcode") {
    user_file$Barcodes <- find_and_add_column(user_file, possible_barcode_names)
  } else {
    stop("Invalid search type!!!")
  }

  message("Required columns detected.")
  return(user_file)
}

#' Search Micronix Tube Samples
#'
#' This function searches through Micronix tube samples in the database to find matches
#' based on a given DataFrame. It identifies discrepancies in positions, missing samples,
#' or samples that are in an archived status.
#'
#' @param database A string specifying the path to the SQLite database.
#' @param micronix_search_df A DataFrame containing the micronix tube samples to be searched.
#'   The DataFrame must contain at least a 'barcode' column.
#' @return A list containing three DataFrames: `missing_from_db`, `additional_in_db`, and
#'   `archived_samples`. Each DataFrame provides details on samples that are missing from the
#'   database, additional in the database but not in the search DataFrame, and samples that are
#'   in an archived status, respectively.
#' @export
search_micronix_tube <- function(database, micronix_search_df) {
  # Establish database connection
  con <- dbConnect(RSQLite::SQLite(), database)
  on.exit(dbDisconnect(con), add = TRUE)

  # Set up lazy loading for database tables
  micronix_tube <- tbl(con, "micronix_tube")
  micronix_plate <- tbl(con, "micronix_plate")
  storage_container <- tbl(con, "storage_container")
  status <- tbl(con, "status")
  archived_statuses <- tbl(con, sql("SELECT id, name FROM view_archive_statuses"))
  locations <- tbl(con, "location")
  study_subject <- tbl(con, "study_subject") %>% dplyr::rename(subject = name)
  specimen <- tbl(con, "specimen")
  study <- tbl(con, "study")

  # Sanity check
  if (nrow(micronix_search_df) == 0) {
    stop("No samples to search for.")
  }

  # Add RowNumber so that we keep track of the original barcodes from the plate
  micronix_search_df <- micronix_search_df %>%
    group_by(PlateName) %>%
    mutate(RowNumber = row_number()) %>%
    ungroup()

  # Convert scanner_input into a lazy table
  dplyr::copy_to(con, micronix_search_df)
  micronix_search_tbl <- tbl(con, "micronix_search_df")

  # Join tables to compare positions and check statuses
  comparison_result <- micronix_tube %>%
    dplyr::rename(db_position = position, micronix_id = id, micronix_barcode = barcode) %>%
    dplyr::inner_join(micronix_plate, by = c("manifest_id" = "id")) %>%
    dplyr::rename(plate_barcode = barcode, plate_name = name) %>%
    dplyr::inner_join(storage_container, by = c("micronix_id" = "id")) %>%
    dplyr::inner_join(status, by = c("status_id" = "id")) %>%
    dplyr::full_join(micronix_search_tbl, by = c("micronix_barcode" = "Barcode")) %>%
    dplyr::filter((is.na(RowNumber) & !is.na(plate_name)) | !is.na(PlateName)) %>%
    dplyr::left_join(locations, by = c("location_id" = "id")) %>%
    dplyr::left_join(specimen, by = c("specimen_id" = "id")) %>%
    dplyr::left_join(study_subject, by = c("study_subject_id" = "id")) %>%
    dplyr::left_join(study, by = c("study_id" = "id")) %>%
    dplyr::select(
      micronix_id,
      RowNumber,
      Barcode = micronix_barcode,
      CurrentWell = Position,
      DBWell = db_position,
      status_name = name,
      DBPlate = plate_name,
      PlateName,
      DBFreezer = level_I,
      DBBasket = level_II,
      Study = short_code,
      ID = subject,
      Date = collection_date
    ) %>%
    collect()

  # Output data frame
  g <- expand.grid(LETTERS[1:8], 1:12)
  g <- data.frame(CurrentWell = sprintf("%s%02d", g$Var1, g$Var2))

  # Create the confusion matrix. `CorrectError` and `EmptyError` represent the correct (TP)
  # and non-existing (TN) samples, respectively. `NotFoundError` and `IncorrectLocationError`
  # represent the missing (FN) and incorrect (FP) samples, respectively. `ArchivedError` indicates
  # if any scanned samples are in archived status.
  g %>%
    dplyr::left_join(comparison_result, by = "CurrentWell") %>%
    mutate(
      NotFoundError = ifelse(is.na(micronix_id) & !is.na(Barcode), "NotFound", NA_character_),
      EmptyError = ifelse(is.na(DBWell) & is.na(Barcode), "Empty", NA_character_),
      IncorrectLocationError = ifelse(DBPlate != PlateName | DBWell != CurrentWell, "IncorrectLocation", NA_character_),
      ArchivedError = ifelse(status_name %in% (archived_statuses %>% dplyr::pull(name)), "Archived", NA_character_),
      CorrectError = ifelse(DBPlate == PlateName & DBWell == CurrentWell, "Correct", NA_character_)
    ) %>%
    rowwise() %>%
    mutate(
      Status = paste(na.omit(c(NotFoundError, EmptyError, IncorrectLocationError, ArchivedError, CorrectError)), collapse = ";")
    ) %>%
    ungroup() %>%
    select(CurrentWell, Barcode, Status, DBWell, DBPlate, DBBasket, DBFreezer, Study, ID, Date) %>%
    arrange(CurrentWell)

}


#' Search by Study Subject (and other longitudinal study criteria)
#'
#' This function takes a sample storage type, filters, and optional database and config parameters
#' to search by study subjects and retrieve relevant data.
#'
#' @param sample_storage_type A character string indicating the type of sample storage
#'        ("micronix", "cryovial").
#' @param filters A list of filters to apply to the query.
#' @param database An optional parameter indicating the database path. Defaults to the
#'        value of the environment variable 'SDB_PATH'.
#' @param config_yml An optional parameter indicating the configuration YAML file. Defaults
#'        to the value of the environment variable 'SDB_CONFIG'.
#'
#' @return A dataframe containing search results based on provided filters and sample storage type.
#' @export
#'
#' @examples
#' \dontrun{
#'   results <- search_by_study_subject_file_upload("micronix", list(short_code = c("SC01"), study_subject = c("SS01")))
#'   print(results)
#' }
#'
search_by_study_subject_file_upload <- function(sample_storage_type, filters, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG")) {
  db.results <- NULL

  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  dbBegin(con)

  dbmap <- get_db_map(sample_storage_type, format = "na", config = config_yml)

  tryCatch({

    container_tables <- list(
      "manifest" = switch(sample_storage_type,
        "micronix" = "micronix_plate",
        "cryovial" = "cryovial_box"
      ),
      "container_class" = switch(sample_storage_type,
        "micronix" = "micronix_tube",
        "cryovial" = "cryovial_tube"
      )
    )

    sql <- tbl(con, "study") %>% dplyr::rename(study_id = id) %>% select(study_id, short_code)

    if (!is.null(filters$short_code)) {
      sql <- filter(sql, short_code %in% local(filters$short_code))
    }

    sql <- inner_join(sql, tbl(con, "study_subject") %>%
      dplyr::rename(study_subject_id = id, study_subject = name) %>%
      select(study_subject_id, study_subject, study_id), by = c("study_id"))


    sql <- sql %>%
      inner_join(
        tbl(con, "specimen") %>%
          dplyr::rename(specimen_id = id) %>%
          select(specimen_id, study_subject_id, specimen_type_id, collection_date)
        , by = c("study_subject_id"))


    # Join with specimen_type
    sql <- sql %>%
      inner_join(tbl(con, "specimen_type") %>%
                   dplyr::rename(specimen_type_id = id, specimen_type = name) %>%
                   select(specimen_type_id, specimen_type),
                 by = c("specimen_type_id"))


    # Make sure to include the sample ID
    sql <- inner_join(sql, tbl(con, "storage_container") %>% dplyr::rename(storage_container_id = id) %>% select(-c(created, last_updated)), by = c("specimen_id"))

    # Filter by study subject
    sql <- filters %>% inner_join(collect(sql)) # columns can change depending on what the user chooses to include in the search

    sql <- inner_join(sql, dbReadTable(con, "status") %>% dplyr::rename(status_id = id, status = name), by = c("status_id"))
    sql <- inner_join(sql, dbReadTable(con, "state") %>% dplyr::rename(state_id = id, state = name), by = c("state_id"))
    sql <- inner_join(sql, dbReadTable(con, container_tables[["container_class"]]) %>% dplyr::rename(storage_container_id = id), by = c("storage_container_id")) %>% collapse()
    sql <- inner_join(sql, dbReadTable(con, container_tables[["manifest"]]) %>% dplyr::rename(manifest_id = id, manifest = name, manifest_barcode = barcode), by = c("manifest_id")) %>% collapse()
    sql <- inner_join(sql, dbReadTable(con, "location") %>% dplyr::rename(location_id = id) %>% select(location_id, location_root, level_I, level_II), by = c("location_id"))

    db.results <- sql %>% select("storage_container_id", names(dbmap)) %>% dplyr::rename(`Sample ID` = storage_container_id)

    # Safe approach to only rename columns that are present in both db.results and dbmap
    possible_identifiers <- c("Sample ID")
    for (col in names(db.results)) {
      if (col %in% possible_identifiers) {
        next
      }
      if (col %in% names(dbmap)) {
        names(db.results)[names(db.results) == col] <- dbmap[[col]]
      }
    }
  }, finally = {
    dbDisconnect(con)
  })

  return(db.results)
}


get_db_map <- function(sample_storage_type, format = "na", config = Sys.getenv("SDB_CONFIG"), filters = NULL) {

    dbmap <- list()
    if (sample_storage_type == "dbs_sample" && (is.null(filters) || is.null(filters$container_type))) {
      stop("Must have container type selected for dbs samples.")
    }

   ## Micronix
    if (!is.null(format) && sample_storage_type == "micronix" && format == "na") {
      dbmap$barcode <- "Barcode"
      dbmap$position <- "Position"
    } else if (!is.null(format) && sample_storage_type == "micronix" && format == "traxcer") {
      dbmap$barcode <- "Tube ID"
      dbmap$position <- ifelse(
        !is.na(config$traxcer_position$override),
        config$traxcer_position$override,
        config$traxcer_position$default
      )
    } else if (!is.null(format) && sample_storage_type == "micronix" && format == "visionmate") {
      dbmap$barcode <- "TubeCode"
      dbmap$position <- "Position"
    }

    ## Cryovial
    else if (sample_storage_type == "cryovial") {
      dbmap$barcode <- "Barcode"
      dbmap$position <-  "Position"
    }
    ## DBS doesn't have a position or barcode but has label
    else if (sample_storage_type == "dbs_sample" && !is.null(filters$container_type)) {
      dbmap$label <- "Label"
      if (filters$container_type == "bag") {
        dbmap$manifest <- "Container Name"
        dbmap$manifest_type <- "Container Type"
      } else if (filters$container_type == "box") {
        dbmap$manifest <- "Container Name"
        dbmap$manifest_type <- "Container Type"
      } else if (filters$container_type == "all") {
        dbmap$manifest <- "Container Name"
        dbmap$manifest_type <- "Container Type"
      }
    } else if (sample_storage_type == "static_plate") {
      dbmap$position <- "Static Position"
    } else {
      stop("Invalid sample type!!!")
    }

    dbmap$short_code <- "Study Code"
    dbmap$study_subject <- "Study Subject"
    dbmap$specimen_type <- "Specimen Type"
    dbmap$collection_date <- "Collection Date"

    dbmap$location_root <- "Location"
    if (sample_storage_type == "micronix") {
      dbmap$location_root <- "Freezer Name"
      dbmap$level_I <- "Shelf Name"
      dbmap$level_II <- "Basket Name"
      dbmap$manifest <- "Plate Name"
      dbmap$manifest_barcode <- "Plate Barcode"
    } else if (sample_storage_type == "cryovial") {
      dbmap$location_root <- "Freezer Name"
      dbmap$level_I <- "Rack Number"
      dbmap$level_II <- "Rack Position"
      dbmap$manifest <- "Box Name"
      dbmap$manifest_barcode <- "Box Barcode"
    } else if (sample_storage_type == "dbs_sample" && !is.null(filters$container_type)) {
      dbmap$location_root <- "Freezer Name"
      dbmap$level_I <- "Shelf Name"
      dbmap$level_II <- "Basket Name"
    } else if (sample_storage_type == "static_plate") {
      dbmap$location_root <- "Freezer Name"
      dbmap$level_I <- "Shelf Name"
      dbmap$level_II <- "Basket Name"
      dbmap$manifest <- "Plate Name"
      dbmap$manifest_barcode <- "Plate Barcode"
    } else {
      stop("Invalid container type!!!")
    }

    dbmap$comment <- "Comment"
    dbmap$state <- "State"
    dbmap$status <- "Status"


    return(dbmap)
}
