#' @import dplyr
#' @export

SearchSamples <- function(barcode_search_file, search_plate_uid, search_subject_uid, search_study, search_location, search_specimen_type){

  table.location <- sampleDB::CheckTable("location")
  table.study <- sampleDB::CheckTable("study")
  table.specimen_type <- sampleDB::CheckTable("specimen_type")
  table.matrix_plate <- sampleDB::CheckTable("matrix_plate")
  table.matrix_tube <- sampleDB::CheckTable("matrix_tube")
  table.study_subject <- sampleDB::CheckTable("study_subject")
  table.specimen <- sampleDB::CheckTable("specimen")
  table.storage_container <- sampleDB::CheckTable("storage_container")

  #SELECT ONE SEARCH FILTER TO USE IF MULTIPLE ARE GIVEN
  SearchFilters <- list(barcode_search_file = barcode_search_file,
                            search_plate_uid =  search_plate_uid,
                            search_subject_uid = search_subject_uid,
                            search_study = search_study,
                            search_location = search_location,
                            search_specimen_type = search_specimen_type)

  search_term <- keep(SearchFilters, function(x) x != "")[1] %>% names()
  filter_terms <- names(keep(SearchFilters, function(x) x != ""))[-1]

  # SearchSamplesArgs2 <- c()
  # for(i in 1:length(names(SearchFilters))){
  #   if(SearchFilters[[i]] != ""){
  #     SearchSamplesArgs2 <- c(SearchSamplesArgs2, names(SearchFilters)[i])
  #   }
  # }
  # print(SearchFilters)
  # print(SearchSamplesArgs2)
  # print("hi")
  # print(keep(SearchFilters, function(x) x != "") %>% names())
  # print(keep(SearchFilters, function(x) x != "")[1])
  # filter_terms <- names(keep(SearchFilters, function(x) x != ""))[-1]
  # filter_terms1 <- SearchSamplesArgs2[-1]

  # print(search_term1)
  # print(search_term)
  # print(filter_terms1)
  # print(filter_terms)

  #step 1 use just one input to get to matrix_tube_ids
  #barcode_search_file to matrix_tube_ids
  if(search_term == "barcode_search_file"){
    barcodes <- read_csv(barcode_search_file)$barcode
    matrix_tube_ids <- filter(table.matrix_tube, barcode %in% barcodes)$id
  }

  #plate_uid to matrix_tube_ids
  if(search_term == "search_plate_uid"){
  plate_ref_id <-  filter(table.matrix_plate, uid == search_plate_uid)$id
  matrix_tube_ids <- filter(table.matrix_tube, plate_id %in% plate_ref_id)$id
  }
  #seach_location to matrix_tube_ids
  if(search_term == "search_location"){
    location_ref_id <- filter(table.location, description %in% search_location)$id
    plate_ref_id <- filter(table.matrix_plate, location_id %in% location_ref_id)$id
    matrix_tube_ids <- filter(table.matrix_tube, plate_id %in% plate_ref_id)$id
  }
  #search_specimen_type to matrix_tube_ids ??
  if(search_term == "search_specimen_type"){
    specimen_ref_id <- filter(table.specimen_type, label %in% search_specimen_type)$id
    specimen_ref_id <- filter(table.specimen, specimen_type_id %in% specimen_ref_id)$id
    storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
    matrix_tube_ids <- filter(table.matrix_tube, id %in% storage_container_id)$id
  }
  #search_subject_uid to matrix_tube_ids ???
  if(search_term == "search_subject_uid"){
    study_subject_ref_id <- filter(table.study_subject, uid %in% search_subject_uid)$id
    specimen_ref_id <- filter(table.specimen, study_subject_id %in% study_subject_ref_id)$id
    storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
    matrix_tube_ids <- filter(table.matrix_tube, id %in% storage_container_id)$id
  }

  #search_study to matrix_tube_ids ???
  if(search_term == "search_study"){
    study_ref_id <- filter(table.study, short_code %in% search_study)$id
    study_subject_ref_id <- filter(table.study_subject, study_id %in% study_ref_id)$id
    specimen_ref_id <- filter(table.specimen, study_subject_id %in% study_subject_ref_id)$id
    storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
    matrix_tube_ids <- filter(table.matrix_tube, id %in% storage_container_id)$id
  }

  #NOTE DO we want to add lead person (or other data) to the search?
  #step 2: got to matrix_tube_ids, now get cols for search result
  #well_positions, barcodes, subject_uids, study_short_code, specimen_type_labels, location_descriptions, plate_uids

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

  table.ref4 <- inner_join(table.ref3, table.study_subject, by = c("study_subject_id" = "id"))
  subject_uids <- table.ref4$uid
  study_short_code <- inner_join(table.ref4, table.study, by = c("study_id" = "id"))$short_code

  specimen_type_labels <- inner_join(table.ref3, table.specimen_type, by = c("specimen_type_id" = "id"))$label

  # create results table
  search_results <- tibble(well_position = well_positions,
                           barcode = barcodes,
                           subject_uid = subject_uids,
                           study = study_short_code,
                           specimen_type = specimen_type_labels,
                           location = location_descriptions,
                           plate_uid = plate_uids)

  #step 3 filter by search item(s) not used to get to matrix_tube_ids
  for(filter_term in filter_terms){
    if(filter_term == "search_plate_uid"){
      search_results <- filter(search_results, plate_uid == SearchFilters[["search_plate_uid"]])
    }
    if(filter_term == "search_subject_uid"){
      search_results <- filter(search_results, subject_uid == SearchFilters[["search_subject_uid"]])
    }
    if(filter_term == "search_study"){
      search_results <- filter(search_results, study == SearchFilters[["search_subject_uid"]])
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
