#' Search for Wetlab Samples in the EPPIcenter sampleDB database
#' 
#' @param filters A list containing micronix barcodes, plate name, study code, location and/or specimen_type
#' @param study_subject.file TRUE or FALSE
#' @examples
#' SearchSamples(filters = list(name.plate = c("100","101"), name.location = c("Left -20 Freezer")))
#' SearchSamples(filters = list(file.barcodes = "/path/to/barcodes.csv", name.plate = "dummy_name", name.study = "dummy_study", name.location = "fridge", name.specimen_type = "specimen_type1"))
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import purrr
#' @import readr
#' @import tidyr
#' @export

SearchSamples <- function(filters, study_subject.file = F){
  
  database <- "/databases/sampledb_database.sqlite"
  
  if(study_subject.file){
    eval.name.study_subject  <- read.csv(filter$name.study_subject)$subject_uid
    filters$name.study_subject <- eval.name.study_subject[eval.name.study_subject != ""] # remove any blank entries that may be in vector
  }

  # GET ALL THE TABLES FROM THE DATABASE
  table.location <- sampleDB::CheckTable(database = database, "location")
  table.study <- sampleDB::CheckTable(database = database, "study")
  table.specimen_type <- sampleDB::CheckTable(database = database, "specimen_type")
  table.matrix_plate <- sampleDB::CheckTable(database = database, "matrix_plate")
  table.matrix_tube <- sampleDB::CheckTable(database = database, "matrix_tube")
  table.study_subject <- sampleDB::CheckTable(database = database, "study_subject")
  table.specimen <- sampleDB::CheckTable(database = database, "specimen")
  table.storage_container <- sampleDB::CheckTable(database = database, "storage_container")

  #COLLECT SEARCH AND FILTERING TERMS - FIRST ITEM NOT "" IS SEARCH TERM THE REST ARE FILTER TERMS
  term.search <- discard(filters, function(x) is.null(x))[1] %>% names()
  terms.filter <- names(discard(filters, function(x) is.null(x)))[-1]
  
  if(is.na(term.search)){
    search_results <- NULL
  }else{

    #USE SEARCH TERM TO GET TO MATRIX_TUBE_IDS
    if(term.search == "file.barcodes"){
      barcodes <- read_csv(filters$file.barcodes)$barcode
      matrix_tube_ids <- filter(table.matrix_tube, barcode %in% barcodes)$id
    }
    if(term.search == "name.plate"){
      plate_ref_id <-  filter(table.matrix_plate, uid %in% filters$name.plate)$id
      matrix_tube_ids <- filter(table.matrix_tube, plate_id %in% plate_ref_id)$id
    }
    if(term.search == "name.location"){
      location_ref_id <- filter(table.location, description %in% filters$name.location)$id
      plate_ref_id <- filter(table.matrix_plate, location_id %in% location_ref_id)$id
      matrix_tube_ids <- filter(table.matrix_tube, plate_id %in% plate_ref_id)$id
    }
    if(term.search == "name.specimen_type"){
      specimen_ref_id <- filter(table.specimen_type, label %in% filters$name.specimen_type)$id
      specimen_ref_id <- filter(table.specimen, specimen_type_id %in% specimen_ref_id)$id
      storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
      matrix_tube_ids <- filter(table.matrix_tube, id %in% storage_container_id)$id
    }
    if(term.search == "name.study_subject"){
      study_subject_ref_id <- filter(table.study_subject, uid %in% filters$name.study_subject)$id
      specimen_ref_id <- filter(table.specimen, study_subject_id %in% study_subject_ref_id)$id
      storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
      matrix_tube_ids <- filter(table.matrix_tube, id %in% storage_container_id)$id
    }
    if(term.search == "name.study"){
      study_ref_id <- filter(table.study, short_code %in% filters$name.study)$id
      study_subject_ref_id <- filter(table.study_subject, study_id %in% study_ref_id)$id
      specimen_ref_id <- filter(table.specimen, study_subject_id %in% study_subject_ref_id)$id
      storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
      matrix_tube_ids <- filter(table.matrix_tube, id %in% storage_container_id)$id
    }
     
    #NOTE:DO WE WANT TO ADD MORE INFO TO SEARCH RESULTS TABLE (E.G. LEAD STUDY PERSON)
    #USE MATRIX_TUBE_IDS TO GET THE ITEMS FOR THE SEARCH RESULT TABLE
    table.ref1 <-  filter(table.matrix_tube, id %in% matrix_tube_ids)
    output.well_positions <- table.ref1$well_position
    output.barcodes <- table.ref1$barcode
  
    table.ref2 <- inner_join(table.ref1, table.matrix_plate, by = c("plate_id" = "id"))
    output.plate_uids <- table.ref2$uid
    output.location_descriptions <- inner_join(table.ref2, table.location, by = c("location_id" = "id"))$description
  
    specimen_ids <- filter(table.storage_container, id %in% matrix_tube_ids)
  
    table.ref3 <- inner_join(specimen_ids, table.specimen, by = c("specimen_id" = "id"))
    study_subject_id <- table.ref3$study_subject_id
    specimen_type_ids <- table.ref3$specimen_type_id
    output.collection_date <- table.ref3$collection_date
  
    table.ref4 <- inner_join(table.ref3, table.study_subject, by = c("study_subject_id" = "id"))
    output.subject_uids <- table.ref4$uid
    output.study_short_code <- inner_join(table.ref4, table.study, by = c("study_id" = "id"))$short_code
  
    output.specimen_type_labels <- inner_join(table.ref3, table.specimen_type, by = c("specimen_type_id" = "id"))$label
  
    #STITCH TOGETHER SEARCH RESULTS
    search_results <- tibble(well_position = output.well_positions,
                             barcode = output.barcodes,
                             subject_uid = output.subject_uids,
                             study = output.study_short_code,
                             specimen_type = output.specimen_type_labels,
                             location = output.location_descriptions,
                             plate_uid = output.plate_uids,
                             collection_date = output.collection_date)
    
    if(length(terms.filter) > 0){
      #FILTER BY FILTER TERMS
      for(filter_term in terms.filter){
        if(filter_term == "name.plate"){
          search_results <- filter(search_results, plate_uid %in% filters[["name.plate"]])
        }
        if(filter_term == "name.study_subject"){
          search_results <- filter(search_results, subject_uid %in% filters[["name.study_subject"]])
        }
        if(filter_term == "name.study"){
          search_results <- filter(search_results, study %in% filters[["name.study"]])
        }
        if(filter_term == "name.location"){
          search_results <- filter(search_results, location %in% filters[["name.location"]])
        }
        if(filter_term == "name.specimen_type"){
          search_results <- filter(search_results, specimen_type %in% filters[["name.specimen_type"]])
        }
      } 
    }
    
    # CLEAN SEARCH TABLE
    search_results <- search_results %>%
      relocate("plate_uid") %>%
      rename(`Plate Name` = plate_uid,
             `Barcode` = barcode,
             `Study Subject` = subject_uid,
             `Study Code` = study,
             `Specimen Type` = specimen_type,
             `Storage Location` = location,
             `Collected Date` = collection_date) %>%
      mutate(letter = substring(well_position, 1, 1), 
             number = as.numeric(substring(well_position, 2))) %>% 
      select(-well_position) %>% 
      group_by(`Plate Name`, `Barcode`, letter, number) %>% 
      arrange(`Plate Name`, letter, number) %>% 
      ungroup() %>% 
      mutate(`Well Position` = paste0(letter, number), .after = `Plate Name`) %>% 
      select(-c(letter, number))
    
    if(nrow(search_results) == 0){
      # message("THERE ARE NO EPPICENTER WETLAB SAMPLES THAT MATCH THIS SEARCH")
      search_results <- NULL
    }
  }
  return(search_results)
}
