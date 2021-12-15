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

  #step 0 select just one input if multiple are given

  # barcode_search_file <- "aaa"
  # search_plate_uid <- "bbb"
  # search_subject_uid <- "ccc"
  # search_study <- ""
  # search_location <- "eee"
  # search_specimen_type <- "fff"

  SearchSamplesArgs <- list(barcode_search_file = barcode_search_file,
                            search_plate_uid =  search_plate_uid,
                            search_subject_uid = search_subject_uid,
                            search_study = search_study,
                            search_location = search_location,
                            search_specimen_type = search_specimen_type)

  SearchSamplesArgs2 <- c()
  for(i in 1:length(names(SearchSamplesArgs))){
    if(SearchSamplesArgs[[i]] != ""){
      SearchSamplesArgs2 <- c(SearchSamplesArgs2, names(SearchSamplesArgs)[i])
    }
  }
  step_one_search_term <- SearchSamplesArgs2[1]
  filter_search_s <- SearchSamplesArgs2[-1]

  print(step_one_search_term)
  print(filter_search_s)

  #step 1 use just one input to get to matrix_tube_ids
  #barcode_search_file to matrix_tube_ids
  if(step_one_search_term == "barcode_search_file"){
    barcodes <- read_csv(barcode_search_file) %>% pull(barcode)
    matrix_tube_ids <- table.matrix_tube %>% filter(barcode %in% barcodes) %>% pull(id)
  }

  #plate_uid to matrix_tube_ids
  if(step_one_search_term == "search_plate_uid"){
  plate_ref_id <- table.matrix_plate %>% filter(uid == search_plate_uid) %>% pull(id)
  matrix_tube_ids <- table.matrix_tube %>% filter(plate_id %in% plate_ref_id) %>% pull(id)
  }
  #seach_location to matrix_tube_ids
  if(step_one_search_term == "search_location"){
    location_ref_id <- table.location %>% filter(description %in% search_location) %>% pull(id)
    plate_ref_id <- table.matrix_plate %>% filter(location_id %in% location_ref_id) %>% pull(id)
    matrix_tube_ids <- table.matrix_tube %>% filter(plate_id %in% plate_ref_id) %>% pull(id)
  }
  #search_specimen_type to matrix_tube_ids ??
  if(step_one_search_term == "search_specimen_type"){
    specimen_ref_id <- table.specimen_type %>% filter(label %in% search_specimen_type) %>% pull(id)
    specimen_ref_id <- table.specimen %>% filter(specimen_type_id %in% specimen_ref_id) %>% pull(id)
    storage_container_id <- table.storage_container %>% filter(specimen_id %in% specimen_ref_id) %>% pull(id)
    matrix_tube_ids <- table.matrix_tube %>% filter(id %in% storage_container_id) %>% pull(id)
  }
  #search_subject_uid to matrix_tube_ids ???
  if(step_one_search_term == "search_subject_uid"){
    study_subject_ref_id <- table.study_subject %>% filter(uid %in% search_subject_uid) %>% pull(id)
    specimen_ref_id <- table.specimen %>% filter(study_subject_id %in% study_subject_ref_id) %>% pull(id)
    storage_container_id <- table.storage_container %>% filter(specimen_id %in% specimen_ref_id) %>% pull(id)
    matrix_tube_ids <- table.matrix_tube %>% filter(id %in% storage_container_id) %>% pull(id)
  }

  #search_study to matrix_tube_ids ???
  if(step_one_search_term == "search_study"){
    study_ref_id <- table.study %>% filter(short_code %in% search_study) %>% pull(id)
    study_subject_ref_id <- table.study_subject %>% filter(study_id %in% study_ref_id) %>% pull(id)
    specimen_ref_id <- table.specimen %>% filter(study_subject_id %in% study_subject_ref_id) %>% pull(id)
    storage_container_id <- table.storage_container %>% filter(specimen_id %in% specimen_ref_id) %>% pull(id)
    matrix_tube_ids <- table.matrix_tube %>% filter(id %in% storage_container_id) %>% pull(id)
  }

  #NOTE DO we want to add lead person (or other data) to the search?
  #step 2: got to matrix_tube_ids, now get cols for search result
  #well_positions, barcodes, subject_uids, study_short_code, specimen_type_labels, location_descriptions, plate_uids

  table.ref1 <- table.matrix_tube %>% filter(id %in% matrix_tube_ids)
  well_positions <- table.ref1 %>% pull(well_position)
  barcodes <- table.ref1 %>% pull(barcode)

  table.ref2 <- inner_join(table.ref1, table.matrix_plate, by = c("plate_id" = "id"))
  plate_uids <- table.ref2 %>% pull(uid)
  location_descriptions <- inner_join(table.ref2, table.location, by = c("location_id" = "id")) %>% pull(description)

  specimen_ids <- table.storage_container %>% filter(id %in% matrix_tube_ids)

  table.ref3 <- inner_join(specimen_ids, table.specimen, by = c("specimen_id" = "id"))
  study_subject_id <- table.ref3 %>% pull(study_subject_id)
  specimen_type_ids <- table.ref3 %>% pull(specimen_type_id)

  table.ref4 <- inner_join(table.ref3, table.study_subject, by = c("study_subject_id" = "id"))
  subject_uids <- table.ref4 %>% pull(uid)
  study_short_code <- inner_join(table.ref4, table.study, by = c("study_id" = "id")) %>% pull(short_code)

  specimen_type_labels <- inner_join(table.ref3, table.specimen_type, by = c("specimen_type_id" = "id")) %>% pull(label)

  # create results table
  search_results <- tibble(well_position = well_positions,
                           barcode = barcodes,
                           subject_uid = subject_uids,
                           study = study_short_code,
                           specimen_type = specimen_type_labels,
                           location = location_descriptions,
                           plate_uid = plate_uids)

  #step 3 filter by search item(s) not used to get to matrix_tube_ids
  for(filter_term in filter_search_s){
    if(filter_term == "search_plate_uid"){
      search_results <- search_results %>% filter(plate_uid == SearchSamplesArgs[["search_plate_uid"]])
    }
    if(filter_term == "search_subject_uid"){
      search_results <- search_results %>% filter(subject_uid == SearchSamplesArgs[["search_subject_uid"]])
    }
    if(filter_term == "search_study"){
      search_results <- search_results %>% filter(study == SearchSamplesArgs[["search_subject_uid"]])
    }
    if(filter_term == "search_location"){
      search_results <- search_results %>% filter(location == SearchSamplesArgs[["search_location"]])
    }
    if(filter_term == "search_specimen_type"){
      search_results <- search_results %>% filter(specimen_type == SearchSamplesArgs[["search_specimen_type"]])
    }
  }

  return(search_results)
}
