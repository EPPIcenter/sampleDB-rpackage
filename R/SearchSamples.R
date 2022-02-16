#' Search for Wetlab Samples in the EPPIcenter sampleDB database
#' 
#' @param filters A list containing micronix barcodes, plate name, study code, location and/or specimen_type
#' @param study_subject.file TRUE or FALSE
#' @examples
#' x search.container = #tricky bc are container names uniq throughout the whole db or just within a certain sample type?
#' - search.date = list(date.from = "YMD", date.to = "YMD")
#' - search.exhausted = T/F
#' x search.label = #tricky bc are labels uniq throughout the whole db or just within a certain sample type?
#' + search.location = list(location_name = c(), level_I = c(), level_II = c())
#' + search.specimen_type = c()
#' + search.study = c()
#' + search.study_subject = c()
#' + search.type = c("RDT", "Micronix", "Paper", "Cryovile")
#' 
#' NOTE: Ufortunately the whole database has to be read into memory inorder to search for something
#' Im sure there is a faster sql way to do this
#' Examples:
#' SearchSamples(filters = list(name.plate = c("100","101"), search.location = c("Left -20 Freezer")))
#' SearchSamples(filters = list(file.barcodes = "/path/to/barcodes.csv", name.plate = "dummy_name", search.study = "dummy_study", search.location = "fridge", search.specimen_type = "specimen_type1"))
#' SearchSamples(filters = list(search.location = list(location_name = "Freezer A", level_I = "dummy.levelI", level_II = "dummy.levelII"))) # feb 10 2022
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import purrr
#' @import readr
#' @import tidyr
#' @export

SearchSamples <- function(filters, study_subject.file = FALSE){
  
  database <- "/databases/new.sampleDB.db"
  
  # FLEXIBLY USE STUDY SUBJECT ITEM OR FILE 
  if(study_subject.file == TRUE & !is.null(filters$search.study_subject)){
    eval.search.study_subject  <- read.csv(filters$search.study_subject)$subject_uid
    filters$search.study_subject <- eval.search.study_subject[eval.search.study_subject != ""] # remove any blank entries that may be in vector
  }

  # GET ALL THE TABLES FROM THE DATABASE
  tables.database <- .GetDatabaseTables(database = database)

  # GET SEARCH TERM AND FILTERING TERM(S)
  clean_filters <- discard(filters, function(x) is.null(x))
  term.search <- clean_filters[1] %>% names()
  terms.filter <- clean_filters[-1] %>% names()
  
  # print(clean_filters)
  
  if(length(clean_filters) == 0){
    usr_results <- NULL
  }else{
    
    # AGGREGATE THE DATASET INTO A TABLE BY USING A "SEARCH TERM" -- OUTPUT IS STORAGE CONTAINER IDS
    storage_container_id <- .UseSearchTermToGetStorageContainerIDs(filters = filters, term.search = term.search, tables.database = tables.database)
    
    # RETRIEVE INTERNAL DATA USING STORAGE CONTAINER IDS
    internal_data <- .UseStorageContainerIDToGetInternalData(storage_container_id = storage_container_id, 
                                                             tables.database = tables.database)
    
    # RETRIEVE EXTERNAL DATA USING STORAGE CONTAINER IDS
    external_data <- .UseStorageContainerIDToGetExternalData(storage_container_id = storage_container_id,
                                                             tables.database = tables.database)
      
    # COMBINE INTERNAL AND EXTERNAL DATA TO CREATE SEARCH RESULTS
    results.search_term <- .UseInternalAndExternalDataToGetResults(internal_data = internal_data, 
                                                                   external_data = external_data)
    
    # FILTER THE AGGREGATED TABLE BY FILTER TERMS
    results.filter_and_search <- .FilterSearchResults(filters = filters, 
                                                      terms.filter = terms.filter,
                                                      results.search_term = results.search_term)
    
    # BEAUTIFY RESULTS TABLE
    usr_results <- .BeautifyResultsTable(results.filter_and_search = results.filter_and_search)
    
    # OUTPUT MESSAGE IF NO RESULTS ARE FOUND
    if(nrow(usr_results) == 0){
      message("THERE ARE NO EPPICENTER WETLAB SAMPLES THAT MATCH THIS SEARCH")
      usr_results <- NULL
    }
  }
  return(usr_results)
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

.UseSearchTermToGetStorageContainerIDs <- function(filters, term.search, tables.database){
  # USE TYPE TO GET STORAGE CONTAINER ID -- 
  if(term.search == "search.type"){
    storage_container_id <- filter(tables.database$table.storage_container, type %in% filters$search.type)$id
  }
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
    mutate(type = "Cryovile")
  
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
    select(-c("created", "last_updated", "id"))
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
      if(filter_term == "search.type"){
        results.search_term <- filter(results.search_term, type %in% filters[["search.type"]])
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