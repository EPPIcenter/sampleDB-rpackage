#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import purrr
#' @import readr
#' @import tidyr
#' @export

SearchSamples <- function(database, barcode_search_file, search_plate_uid, search_subject_uid, search_study, search_location, search_specimen_type){

  table.location <- sampleDB::CheckTable(database = database, "location")
  table.study <- sampleDB::CheckTable(database = database, "study")
  table.specimen_type <- sampleDB::CheckTable(database = database, "specimen_type")
  table.matrix_plate <- sampleDB::CheckTable(database = database, "matrix_plate")
  table.matrix_tube <- sampleDB::CheckTable(database = database, "matrix_tube")
  table.study_subject <- sampleDB::CheckTable(database = database, "study_subject")
  table.specimen <- sampleDB::CheckTable(database = database, "specimen")
  table.storage_container <- sampleDB::CheckTable(database = database, "storage_container")

  #CREATE A LIST OUT OF THE FUNCTION ARGS
  SearchFilters <- list(barcode_search_file = barcode_search_file,
                            search_plate_uid =  search_plate_uid,
                            search_subject_uid = search_subject_uid,
                            search_study = search_study,
                            search_location = search_location,
                            search_specimen_type = search_specimen_type)
  # print("HERE84")
  # print(SearchFilters)
  # print(SearchFilters %>% discard(function(x) "" %in% x) %>% length())
  if(length(SearchFilters %>% discard(function(x) "" %in% x)) == 0){
    search_results <- tibble(well_position = NA,
                             barcode = NA,
                             subject_uid = NA,
                             study = NA,
                             specimen_type = NA,
                             location = NA,
                             plate_uid = NA,
                             collection_date = NA) %>% filter(well_position == 0)

  }else{
    # print("HERE50")
    #COLLECT SEARCH AND FILTERING TERMS - FIRST ITEM NOT "" IS SEARCH TERM THE REST ARE FILTER TERMS
    search_term <- discard(SearchFilters, function(x) "" %in% x)[1] %>% names()
    filter_terms <- names(discard(SearchFilters, function(x) "" %in% x))[-1]
    # print(filter_terms)

    #USE SEARCH TERM TO GET TO MATRIX_TUBE_IDS
    if(search_term == "barcode_search_file"){
      barcodes <- read_csv(barcode_search_file)$barcode
      matrix_tube_ids <- filter(table.matrix_tube, barcode %in% barcodes)$id
    }

    if(search_term == "search_plate_uid"){
      # print(search_plate_uid)
      plate_ref_id <-  filter(table.matrix_plate, uid %in% search_plate_uid)$id
      matrix_tube_ids <- filter(table.matrix_tube, plate_id %in% plate_ref_id)$id
      # print(matrix_tube_ids)
    }

    if(search_term == "search_location"){
      location_ref_id <- filter(table.location, description %in% search_location)$id
      plate_ref_id <- filter(table.matrix_plate, location_id %in% location_ref_id)$id
      matrix_tube_ids <- filter(table.matrix_tube, plate_id %in% plate_ref_id)$id
    }

    if(search_term == "search_specimen_type"){
      specimen_ref_id <- filter(table.specimen_type, label %in% search_specimen_type)$id
      specimen_ref_id <- filter(table.specimen, specimen_type_id %in% specimen_ref_id)$id
      storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
      matrix_tube_ids <- filter(table.matrix_tube, id %in% storage_container_id)$id
    }

    if(search_term == "search_subject_uid"){
      study_subject_ref_id <- filter(table.study_subject, uid %in% search_subject_uid)$id
      specimen_ref_id <- filter(table.specimen, study_subject_id %in% study_subject_ref_id)$id
      storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
      matrix_tube_ids <- filter(table.matrix_tube, id %in% storage_container_id)$id
    }

    if(search_term == "search_study"){
      study_ref_id <- filter(table.study, short_code %in% search_study)$id
      study_subject_ref_id <- filter(table.study_subject, study_id %in% study_ref_id)$id
      specimen_ref_id <- filter(table.specimen, study_subject_id %in% study_subject_ref_id)$id
      storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
      matrix_tube_ids <- filter(table.matrix_tube, id %in% storage_container_id)$id
    }

    #NOTE:DO WE WANT TO ADD MORE INFO TO SEARCH RESULTS TABLE (E.G. LEAD STUDY PERSON)
    #USE MATRIX_TUBE_IDS TO GET THE ITEMS FOR THE SEARCH RESULT TABLE

    table.ref1 <-  filter(table.matrix_tube, id %in% matrix_tube_ids)
    well_positions <- table.ref1$well_position
    barcodes <- table.ref1$barcode

    table.ref2 <- inner_join(table.ref1, table.matrix_plate, by = c("plate_id" = "id"))
    plate_uids <- table.ref2$uid
    location_descriptions <- inner_join(table.ref2, table.location, by = c("location_id" = "id"))$description

    specimen_ids <- filter(table.storage_container, id %in% matrix_tube_ids)

    table.ref3 <- inner_join(specimen_ids, table.specimen, by = c("specimen_id" = "id"))
    study_subject_id <- table.ref3$study_subject_id
    specimen_type_ids <- table.ref3$specimen_type_id
    collection_date <- table.ref3$collection_date

    table.ref4 <- inner_join(table.ref3, table.study_subject, by = c("study_subject_id" = "id"))
    subject_uids <- table.ref4$uid
    study_short_code <- inner_join(table.ref4, table.study, by = c("study_id" = "id"))$short_code

    # print(table.ref3 %>% as.data.frame())
    specimen_type_labels <- inner_join(table.ref3, table.specimen_type, by = c("specimen_type_id" = "id"))$label

    # print(specimen_type_labels)
    #STITCH TOGETHER SEARCH RESULTS
    search_results <- tibble(well_position = well_positions,
                             barcode = barcodes,
                             subject_uid = subject_uids,
                             study = study_short_code,
                             specimen_type = specimen_type_labels,
                             location = location_descriptions,
                             plate_uid = plate_uids,
                             collection_date = collection_date)

    #FILTER BY FILTER TERMS
    for(filter_term in filter_terms){
      if(filter_term == "search_plate_uid"){
        # print("HERE2")
        # print(SearchFilters[["search_plate_uid"]])
        search_results <- filter(search_results, plate_uid == SearchFilters[["search_plate_uid"]])
      }
      if(filter_term == "search_subject_uid"){
        search_results <- filter(search_results, subject_uid == SearchFilters[["search_subject_uid"]])
      }
      if(filter_term == "search_study"){
        search_results <- filter(search_results, study == SearchFilters[["search_study"]])
      }
      if(filter_term == "search_location"){
        search_results <- filter(search_results, location == SearchFilters[["search_location"]])
      }
      if(filter_term == "search_specimen_type"){
        search_results <- filter(search_results, specimen_type == SearchFilters[["search_specimen_type"]])
      }
    }

    return(search_results)
  }
}
