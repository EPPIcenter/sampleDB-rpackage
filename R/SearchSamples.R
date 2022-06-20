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
#' @param study_subject.file A logical value.  Setting `study_subject.file` to `TRUE` allows the user to search for study subjects using a .csv file. Setting `study_subject.file` to `FALSE` specifies that the user searches for study subjects using either a string or vector of strings. Default value is `FALSE`.
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
                          study = NULL, collection_dates = NULL, archived = NULL, freezer = NULL, study_subject.file = FALSE, return_sample_ids = FALSE){
  
  database <- sampleDB:::.GetSampleDBPath()
  
  # SET FILTERS
  filters <- list(search.type = sample_type,
                  search.label = sample_label,
                  search.container = container_name,
                  search.study_subject = study_subject,
                  search.specimen_type = specimen_type,
                  search.study = study,
                  search.date = collection_dates,
                  search.exhausted = archived,
                  search.location = freezer) %>% discard(., function(x) is.null(x) | "" %in% x | length(x) == 0)
  
  # FLEXIBLY USE STUDY SUBJECT ITEM OR FILE 
  if(study_subject.file == TRUE & !is.null(filters$search.study_subject)){
    eval.search.study_subject  <- read.csv(filters$search.study_subject)$subject_uid
    filters$search.study_subject <- eval.search.study_subject[eval.search.study_subject != ""]} # remove any blank entries that may be in vector

  # GET ALL THE TABLES FROM THE DATABASE
  tables.database <- .GetDatabaseTables(database = database)

  # GET SEARCH TERM AND FILTERING TERM(S)
  clean_filters <- discard(filters, function(x) is.null(x))
  term.search <- clean_filters[1] %>% names()
  terms.filter <- clean_filters[-1] %>% names()
  
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
  
  # FILTER THE AGGREGATED TABLE BY FILTER TERMS
  results.filtered <- .FilterSearchResults(filters = filters, 
                                           terms.filter = terms.filter,
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
                          table.matrix_tube = sampleDB::CheckTable(database = database, "matrix_tube"))
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
  if(term.search == "search.exhausted"){
    storage_container_id <- filter(tables.database$table.storage_container, as.logical(exhausted) == filters$search.exhausted)$id
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

  exhausted_info <- filter(tables.database$table.storage_container, id %in% storage_container_id)$exhausted
  # comments <- filter(tables.database$table.storage_container, id %in% storage_container_id)$comments
  specimen_ids <- filter(tables.database$table.storage_container, id %in% storage_container_id)
  table.ref1 <- inner_join(specimen_ids, tables.database$table.specimen, by = c("specimen_id" = "id"))
  study_subject_id <- table.ref1$study_subject_id
  specimen_type_ids <- table.ref1$specimen_type_id
  collection_date <- table.ref1$collection_date
  
  table.ref2 <- inner_join(table.ref1, tables.database$table.study_subject, by = c("study_subject_id" = "id"))
  subject_uids <- table.ref2$subject
  study_short_code <- inner_join(table.ref2, tables.database$table.study, by = c("study_id" = "id"))$short_code
  
  specimen_type_labels <- inner_join(table.ref1, tables.database$table.specimen_type, by = c("specimen_type_id" = "id"))$label
  internal_data <- list(storage_container_id = storage_container_id,
                        exhausted_info = exhausted_info,
                        collection_date = collection_date,
                        study_short_code = study_short_code,
                        subject_uids = subject_uids,
                        specimen_type_labels = specimen_type_labels)
  
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
                           exhausted = as.logical(internal_data$exhausted_info) %>% as.factor())
  return(search_results)
}

.FilterSearchResults <- function(filters, terms.filter, results.search_term){
  if(length(terms.filter) > 0){
    #FILTER BY FILTER TERMS
    for(filter_term in terms.filter){
      if(filter_term == "search.type"){
        results.search_term <- filter(results.search_term, type %in% filters[["search.type"]])
      }
      if(!is.null(filters$search.label) &!is.null(filters$search.type)){
        tmp_results.micronix_label <- filter(results.search_term, type == "Micronix" & label %in% filters$search.label$micronix.labels)
        tmp_results.cryovial_label <- filter(results.search_term, type == "Cryovial" & label %in% filters$search.label$cryovial.labels)
        tmp_results.rdt_label <- filter(results.search_term, type == "RDT" & label %in% filters$search.label$rdt.labels)
        tmp_results.paper_label <- filter(results.search_term, type == "Paper" & label %in% filters$search.label$paper.labels)
        results.search_term <- rbind(tmp_results.micronix_label, tmp_results.cryovial_label, tmp_results.rdt_label, tmp_results.paper_label)
      }
      if(!is.null(filters$search.container) &!is.null(filters$search.type)){
        tmp_results.micronix_container_name <- filter(results.search_term, type == "Micronix" & container_name %in% filters$search.container$micronix.container_name)
        tmp_results.cryovial_container_name <- filter(results.search_term, type == "Cryovial" & container_name %in% filters$search.container$cryovial.container_name)
        tmp_results.rdt_container_name <- filter(results.search_term, type == "RDT" & container_name %in% filters$search.container$rdt.container_name)
        tmp_results.paper_container_name <- filter(results.search_term, type == "Paper" & container_name %in% filters$search.container$paper.container_name)
        results.search_term <- rbind(tmp_results.micronix_container_name, tmp_results.cryovial_container_name, tmp_results.rdt_container_name, tmp_results.paper_container_name)
      }
      if(filter_term == "search.study"){
        results.search_term <- filter(results.search_term, study %in% filters[["search.study"]])
      }
      if(filter_term == "search.specimen_type"){
        results.search_term <- filter(results.search_term, specimen_type %in% filters[["search.specimen_type"]])
      }
      if(filter_term == "search.study_subject"){
        results.search_term <- filter(results.search_term, subject_uid %in% filters[["search.study_subject"]])
      }
      if(filter_term == "search.location"){
        results.search_term <- filter(results.search_term, freezer %in% filters[["search.location"]][["location_name"]])
        if(!is.null(filters[["search.location"]][["level_I"]])){
          results.search_term <- filter(results.search_term, freezer_l1 %in% filters[["search.location"]][["level_I"]])
        }
        if(!is.null(filters[["search.location"]][["level_II"]])){
          results.search_term <- filter(results.search_term, freezer_l2 %in% filters[["search.location"]][["level_II"]])
        }
      }
      if(filter_term == "search.exhausted"){
        results.search_term <- filter(results.search_term, as.logical(exhausted) == filters[["search.exhausted"]])
      }
      if(filter_term == "search.date"){
        stopifnot("date.from and data.to must be in YMD format." = all(!is.na(parse_date_time(c(filters$search.date$date.from, filters$search.date$date.to), orders = "ymd")) == TRUE))
        date.from <- lubridate::as_date(filters[["search.date"]][["date.from"]])
        date.to <- lubridate::as_date(filters[["search.date"]][["date.to"]])
        results.search_term <- filter(results.search_term, lubridate::as_date(collection_date) %within% interval(date.from, date.to))
      }
      results.filter_and_search <- results.search_term
    }
  }else{
    results.filter_and_search <- results.search_term
  }
  return(results.filter_and_search)
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
           `Exhausted` = exhausted)
  
  return(usr_results)
}
