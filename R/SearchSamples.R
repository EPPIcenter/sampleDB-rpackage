#' Search for Wetlab Samples in the EPPIcenter SampleDB database
#'
#' @description
#'
#' @param sample_type A string specifying the type of EPPIcenter sample. (`micronix`, `cryovile`, `rdt` and/or `paper`)
#' @param sample_label A list of vectors specifying the vector micronix, cryovile, rdt, and paper label names (`micronix.label_name`, `cryovile.label_name`, `rdt.label_name` and/or `paper.label_name`)
#' @param container_name A list of vectors specifying the vector micronix, cryovile, rdt, and paper container names (`micronix.container_name`, `cryovile.container_name`, `rdt.container_name` and/or `paper.container_name`)
#' @param study_subject A study subjects string or a vector of study subject strings. If `study_subject.file` is TRUE the path to a .csv file containing one column named study_subject can be uploaded and used to search the database for study subjects.
#' @param specimen_type A specimen type string or a vector of specimen type strings.
#' @param study A study short code string or a vector of study short code strings.
#' @param collection_dates A list of date values strings (`date.to` and `date.from`) that can be used to filter EPPIcenter samples
#' @param archived A logical value. `TRUE` filters for archived samples and `FALSE` filters for active samples
#' @param freezer A list specifying the vector `location_name`, `level_I`, and/or`level_II`
#' @param return_sample_ids A logical value. Setting `return_sample_ids` to `TRUE` means `SearchSamples` returns sample ids as well as search results. Setting `return_sample_ids` to `FALSE` means `SearchSamples` returns only search results. Default value is `FALSE`.
#' @examples
#' \dontrun{
#' SearchSamples(study = "KAM06", study_subject = "subject_1")
#' }
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import purrr
#' @import readr
#' @import tidyr
#' @export


#NOTE: Unfortunately the whole database has to be read into memory inorder to search for something
#Im sure there is a faster sql way to do this

SearchSamples <- function(sample_type = NULL, sample_label = NULL, container_name = NULL, study_subject = NULL, specimen_type = NULL,
                          study = NULL, collection_dates = NULL, status = NULL, state = NULL, freezer = NULL, return_sample_ids = FALSE){

  database <- Sys.getenv("SDB_PATH")

  # SET FILTERS
  filters <- list(search.type = sample_type,
                  search.label = sample_label,
                  search.container = container_name,
                  search.study_subject = study_subject,
                  search.specimen_type = specimen_type,
                  search.study = study,
                  search.date = collection_dates,
                  search.status = status,
                  search.state = state,
                  search.location = freezer) %>% discard(., function(x) is.null(x) | "" %in% x | length(x) == 0)

  # GET ALL THE TABLES FROM THE DATABASE
  tables.database <- .GetDatabaseTables(database = database)

  # GET SEARCH TERM AND FILTERING TERM(S)
  clean_filters <- discard(filters, function(x) is.null(x))
  term.search <- clean_filters[1] %>% names()
  terms.filter <- clean_filters[-1] %>% names()

  if (!is.null(filters$search.date)) {
    filters$search.date$date.to <- lubridate::as_date(collection_dates$date.to)
    filters$search.date$date.from <- lubridate::as_date(collection_dates$date.from)
  }

  # ERR IF SAMPLE LABEL AND CONTAINER IS SEARCHED FOR WITH OUT PROVIDING SEARCH TYPE
  stopifnot("SEARCH TYPE MUST BE PROVIDED IN ORDER TO SEARCH BY SAMPLES LABELS OR CONTAINER NAMES" = term.search != "search.label" || term.search != "search.container")

  # RETURN NULL IF THERE ARE NO FILTERS
  stopifnot("THERE ARE NO EPPICENTER WETLAB SAMPLES THAT MATCH THIS SEARCH" = length(clean_filters) != 0)

  # AGGREGATE THE DATASET INTO A TABLE BY USING A "SEARCH TERM" -- OUTPUT IS STORAGE CONTAINER IDS
  aggregated.storage_container_id <- .UseSearchTermToAggregateSamples(filters = filters, term.search = term.search, tables.database = tables.database)

  # RETRIEVE INTERNAL DATA USING STORAGE CONTAINER IDS
  aggregated.internal_data <- .UseStorageContainerIDToGetInternalData(storage_container_id = aggregated.storage_container_id,
                                                                      tables.database = tables.database)

  # RETRIEVE EXTERNAL DATA USING STORAGE CONTAINER IDS
  aggregated.external_data <- .UseStorageContainerIDToGetExternalData(storage_container_id = aggregated.storage_container_id,
                                                                      tables.database = tables.database)

  # COMBINE INTERNAL AND EXTERNAL DATA TO CREATE SEARCH RESULTS
  aggregated.results <- .UseInternalAndExternalDataToGetResults(internal_data = aggregated.internal_data,
                                                                 external_data = aggregated.external_data)

  # TODO: this is temporary until we get a better
  # search system down. The previous search terms were
  # already there, renaming for ease of use and because
  # short on time.
  filters <- list(type = sample_type,
                  label = sample_label,
                  container_name = container_name,
                  subject_uid = study_subject,
                  specimen_type = specimen_type,
                  study = study,
                  collection_date = collection_dates,
                  status = status,
                  state = state,
                  freezer = freezer) %>% discard(., function(x) is.null(x) | "" %in% x | length(x) == 0)


  # FILTER THE AGGREGATED TABLE BY FILTER TERMS
  results.filtered <- .FilterSearchResults(filters = filters,
                                           results.search_term = aggregated.results)

  # BEAUTIFY RESULTS TABLE
  results.cleaned_and_filtered <- .BeautifyResultsTable(results.filter_and_search = results.filtered)


  # OUTPUT MESSAGE IF NO RESULTS ARE FOUND
  stopifnot("THERE ARE NO EPPICENTER WETLAB SAMPLES THAT MATCH THIS SEARCH" = nrow(results.cleaned_and_filtered) != 0)
  id.wetlab_samples <- results.cleaned_and_filtered$"storage_container_id"
  results <- results.cleaned_and_filtered %>% select(-c("storage_container_id"))
  if(return_sample_ids == TRUE){
    results <- list(id.wetlab_samples = id.wetlab_samples,
                    results = results)}
  return(results)
}

.GetDatabaseTables <- function(database){
  database.tables <- list(table.storage_container = sampleDB::CheckTable(database = database, "storage_container"),
                          table.study_subject = sampleDB::CheckTable(database = database, "study_subject"),
                          table.specimen = sampleDB::CheckTable(database = database, "specimen"),
                          table.study = sampleDB::CheckTable(database = database, "study"),
                          table.location = sampleDB::CheckTable(database = database, "location"),
                          table.specimen_type = sampleDB::CheckTable(database = database, "specimen_type"),
                          table.box = sampleDB::CheckTable(database = database, "box"),
                          table.tube = sampleDB::CheckTable(database = database, "tube"),
                          table.bag = sampleDB::CheckTable(database = database, "bag"),
                          table.rdt = sampleDB::CheckTable(database = database, "rdt"),
                          table.paper = sampleDB::CheckTable(database = database, "paper"),
                          table.plate = sampleDB::CheckTable(database = database, "matrix_plate"),
                          table.matrix_tube = sampleDB::CheckTable(database = database, "matrix_tube"),
                          table.state = sampleDB::CheckTable(database = database, "state"),
                          table.status = sampleDB::CheckTable(database = database, "status"))
  return(database.tables)
}

.UseSearchTermToAggregateSamples <- function(filters, term.search, tables.database){
  # USE TYPE TO GET STORAGE CONTAINER ID
  if(term.search == "search.type"){
    if(length(filters$search.type) == 1){
      if(filters$search.type == "all"){
      storage_container_id <- tables.database$table.storage_container$id
      }else{
        storage_container_id <- filter(tables.database$table.storage_container, type %in% filters$search.type)$id
      }
    }else{
      storage_container_id <- filter(tables.database$table.storage_container, type %in% filters$search.type)$id
    }
  }

  # USE ARCHIVAL INFO TO GET STORAGE CONTAINER ID
  if(term.search == "search.status"){
    storage_container_id <- filter(tables.database$table.storage_container, filters$search.status %in% status)$id
  }
  # USE STUDY TO GET STORAGE CONTAINER ID
  if(term.search == "search.study"){
    study_ref_id <- filter(tables.database$table.study, short_code %in% filters$search.study)$id
    study_subject_ref_id <- filter(tables.database$table.study_subject, study_id %in% study_ref_id)$id
    specimen_ref_id <- filter(tables.database$table.specimen, study_subject_id %in% study_subject_ref_id)$id
    storage_container_id <- filter(tables.database$table.storage_container, specimen_id %in% specimen_ref_id)$id
  }
  # USE SPECIMEN TYPE TO GET STORAGE CONTAINER ID
  if(term.search == "search.specimen_type"){
    specimen_ref_id <- filter(tables.database$table.specimen_type, label %in% filters$search.specimen_type)$id
    specimen_ref_id <- filter(tables.database$table.specimen, specimen_type_id %in% specimen_ref_id)$id
    storage_container_id <- filter(tables.database$table.storage_container, specimen_id %in% specimen_ref_id)$id
  }
  # USE SUBJECT TO GET STORAGE CONTAINER ID
  if(term.search == "search.study_subject"){
    study_subject_ref_id <- filter(tables.database$table.study_subject, subject %in% filters$search.study_subject)$id
    specimen_ref_id <- filter(tables.database$table.specimen, study_subject_id %in% study_subject_ref_id)$id
    storage_container_id <- filter(tables.database$table.storage_container, specimen_id %in% specimen_ref_id)$id
  }
  # USE LOCATION LIST TO GET STORAGE CONTAINER ID
  if(term.search == "search.location"){
    location_ref_id <- .GetLocationID(filters = filters, tables.database = tables.database)
    tubes_id <- filter(tables.database$table.tube, box_id %in% filter(tables.database$table.box, location_id %in% location_ref_id)$id)$id
    rdt_id <- filter(tables.database$table.rdt, bag_id %in% filter(tables.database$table.bag, location_id %in% location_ref_id)$id)$id
    paper_id <- filter(tables.database$table.paper, bag_id %in% filter(tables.database$table.bag, location_id %in% location_ref_id)$id)$id
    matrix_tubes_id <- filter(tables.database$table.matrix_tube, plate_id %in% filter(tables.database$table.plate, location_id %in% location_ref_id)$id)$id
    storage_container_id <- filter(tables.database$table.storage_container, id %in% c(tubes_id, rdt_id, paper_id, matrix_tubes_id))$id
  }
  # USE DATE TO GET STORAGE CONTAINER ID
  if(term.search == "search.date"){
    stopifnot("date.from and data.to must be in YMD format." = all(!is.na(parse_date_time(c(filters$search.date$date.from, filters$search.date$date.to), orders = "ymd")) == TRUE))
    date.from <- lubridate::as_date(filters$search.date$date.from)
    date.to <- lubridate::as_date(filters$search.date$date.to)
    specimen_ids <- filter(tables.database$table.specimen, lubridate::as_date(collection_date) %within% interval(date.from,date.to))$id
    storage_container_id <- filter(tables.database$table.storage_container, specimen_id %in% specimen_ids)$id
  }
  # USE TYPE SPECIFIC THINGS TO FILTER...
  return(storage_container_id)
}

.GetLocationID <- function(filters, tables.database){
  # make sure you can search by just one aspect of locations in the filters$search.location(ie freezer name, l1, l2), and that that search can include multiple names (l1 = c("A_rack", "B_rack"))
  tmp.table.location <- tables.database$table.location
  if(!is.null(filters$search.location$location_name)){
    tmp.table.location <- filter(tmp.table.location, location_name %in% filters$search.location$location_name)
  }
  if(!is.null(filters$search.location$level_I)){
    tmp.table.location <- filter(tmp.table.location, level_I %in% filters$search.location$level_I)
  }
  if(!is.null(filters$search.location$level_II)){
    tmp.table.location <- filter(tmp.table.location, level_II %in% filters$search.location$level_II)
  }
  location_ref_id <- tmp.table.location$id

  return(location_ref_id)
}

.UseStorageContainerIDToGetInternalData <- function(storage_container_id, tables.database){

  # comments <- filter(tables.database$table.storage_container, id %in% storage_container_id)$comments
  specimen_ids <- filter(tables.database$table.storage_container, id %in% storage_container_id)
  table.ref1 <- inner_join(specimen_ids, tables.database$table.specimen, by = c("specimen_id" = "id"))
  comment <- table.ref1$comment
  study_subject_id <- table.ref1$study_subject_id
  specimen_type_ids <- table.ref1$specimen_type_id
  collection_date <- table.ref1$collection_date

  table.ref2 <- inner_join(table.ref1, tables.database$table.study_subject, by = c("study_subject_id" = "id"))
  subject_uids <- table.ref2$subject
  study_short_code <- inner_join(table.ref2, tables.database$table.study, by = c("study_id" = "id"))$short_code
  status_info <- inner_join(table.ref2, tables.database$table.status, by = c("status_id" = "id"))$name
  state_info <- inner_join(table.ref2, tables.database$table.state, by = c("state_id" = "id"))$name
  specimen_type_labels <- inner_join(table.ref1, tables.database$table.specimen_type, by = c("specimen_type_id" = "id"))$label

  internal_data <- list(storage_container_id = storage_container_id,
                        status_info = status_info,
                        state_info = state_info,
                        collection_date = collection_date,
                        study_short_code = study_short_code,
                        subject_uids = subject_uids,
                        specimen_type_labels = specimen_type_labels,
                        comment = comment)

  return(internal_data)
}

.UseStorageContainerIDToGetExternalData <- function(storage_container_id, tables.database){
  tube_dat <- inner_join(filter(tables.database$table.tube, id %in% storage_container_id),
                         tables.database$table.box[, c("id","box_name", "location_id")],
                         by = c("box_id" = "id")) %>%
    select(-c(box_id)) %>%
    rename(container_position = box_position,
           container_name = box_name) %>%
    mutate(type = "Cryovial")

  micr_dat <- inner_join(filter(tables.database$table.matrix_tube, id %in% storage_container_id),
                         tables.database$table.plate[, c("id","plate_name", "location_id")],
                         by = c("plate_id" = "id")) %>%
    select(-c(plate_id)) %>%
    rename(container_position = well_position,
           container_name = plate_name,
           label = barcode) %>%
    mutate(type = "Micronix")

  rdt_dat <- inner_join(filter(tables.database$table.rdt, id %in% storage_container_id),
                        tables.database$table.bag[, c("id","bag_name", "location_id")],
                        by = c("bag_id" = "id")) %>%
    select(-c(bag_id)) %>%
    mutate(type = "RDT",
           container_position = NA) %>%
    rename(container_name = bag_name)

  paper_dat <- inner_join(filter(tables.database$table.paper, id %in% storage_container_id),
                          tables.database$table.bag[, c("id","bag_name", "location_id")],
                          by = c("bag_id" = "id")) %>%
    select(-c(bag_id)) %>%
    mutate(type = "Paper",
           container_position = NA) %>%
    rename(container_name = bag_name)

  # add freezer data to external date
  external_data <- rbind(tube_dat, micr_dat, rdt_dat, paper_dat)

  external_data <- inner_join(external_data, tables.database$table.location, by = c("location_id" = "id")) %>%
    select(-c("created", "last_updated"))

  archived_df <-  tibble(id = setdiff(storage_container_id, external_data$id), label = NA,
                         container_position = NA, container_name = NA, location_id = NA, type = NA,
                         location_name = NA, location_type = NA, level_I = NA, level_II = NA, level_III = NA)

  external_data <- rbind(external_data, archived_df) %>% arrange(id)

  return(external_data)
}

.UseInternalAndExternalDataToGetResults <- function(internal_data, external_data){
  search_results <- tibble(storage_container_id = internal_data$storage_container_id,
                           subject_uid = internal_data$subject_uids %>% as.factor(),
                           study = internal_data$study_short_code %>% as.factor(),
                           specimen_type = internal_data$specimen_type_labels %>% as.factor(),
                           collection_date = lubridate::as_date(internal_data$collection_date),
                           container_name = external_data$container_name %>% as.factor(),
                           container_position = external_data$container_position %>% as.factor(),
                           label = external_data$label,
                           type = external_data$type %>% as.factor(),
                           freezer = external_data$location_name %>% as.factor(),
                           freezer_l1 = external_data$level_I %>% as.factor(),
                           freezer_l2 = external_data$level_II %>% as.factor(),
                           status = internal_data$status_info %>% as.factor(),
                           state = internal_data$state_info %>% as.factor(),
                           comment = internal_data$comment %>% as.character())
  return(search_results)
}

.FilterSearchResults <- function(filters, results.search_term) {

  # passed in my reference; don't want to
  # return capitalized values. Capitalizing to normalize.
  tmp.search_term <- results.search_term %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(across(where(is.character), toupper))

  filters <- lapply(filters, function(x) {
    if (is.character(x)) {
      toupper(x)
    } else {
      x
    }
  })

  search_mat <- matrix(data = FALSE, nrow = nrow(tmp.search_term), ncol = length(filters))
  filter_terms <- names(filters)
  for (filter_index in seq_along(filter_terms)) {
    search_term <- filter_terms[filter_index]
    if (!search_term %in% "collection_date") {
      search_mat[, filter_index] <- (tmp.search_term %>% pull(search_term)) %in% filters[[search_term]]
    } else {
      intervals <- list()
      for (i in 1:length(filters$collection_date$date.from)) {
        intervals <- append(
          intervals,
          list(
            interval(
              lubridate::as_date(filters$collection_date$date.from[i]),
              lubridate::as_date(filters$collection_date$date.to[i])
            )
          )
        )
      }

      search_mat[, filter_index] <- results.search_term %>% pull(collection_date) %within% intervals
      search_mat[, filter_index] <- replace(search_mat[, filter_index], is.na(search_mat[, filter_index]), FALSE)
    }
  }

  return(results.search_term[rowSums(search_mat) == ncol(search_mat),])
}

.BeautifyResultsTable <- function(results.filter_and_search){
  usr_results <- results.filter_and_search %>%
    rename(`Sample Type` = type,
           `Container Name` = container_name,
           `Container Position` = container_position,
           `Label` = label,
           `Study Subject` = subject_uid,
           `Study Code` = study,
           `Specimen Type` = specimen_type,
           `Storage Location` = freezer,
           `Storage Location.Level I` = freezer_l1,
           `Storage Location.Level II` = freezer_l2,
           `Collected Date` = collection_date,
           `State` = state,
           `Status` = status,
           `Comment` = comment)

  return(usr_results)
}
