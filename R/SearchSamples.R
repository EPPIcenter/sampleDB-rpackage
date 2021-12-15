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

  #matrix_tube_ids for testing
  write_csv(tibble(matrix_tube_ids = matrix_tube_ids), "~/Desktop/test_matrix_tube_ids.csv")

  # matrix_tube_ids <- read_csv("~/Desktop/test_matrix_tube_ids.csv") %>% pull(matrix_tube_ids)

  print("HERE1")
  #step 2: got to matrix_tube_ids, now get cols for search result
  #specimen_ids
  specimen_ids <- table.storage_container %>% filter(id %in% matrix_tube_ids) %>% pull(specimen_id)

  #study_subject_ids
  #specimen_type_ids
  tmp.table.specimen_table <- matrix(nrow = length(specimen_ids), ncol = ncol(table.specimen)) %>% as.data.frame()
  for(i in 1:length(specimen_ids)){
    specimen_id <- specimen_ids[i]
    tmp.table.specimen_table[i,] <- table.specimen %>% filter(id %in% specimen_id)
  }
  names(tmp.table.specimen_table) <- names(table.specimen)

  study_subject_ids <- tmp.table.specimen_table %>% pull(study_subject_id)
  specimen_type_ids <- tmp.table.specimen_table %>% pull(specimen_type_id)
  print("HERE2")
  #specimen type info
  tmp.table.specimen_type <- matrix(nrow = length(specimen_type_ids), ncol = ncol(table.specimen_type)) %>% as.data.frame()
  for(i in 1:length(specimen_type_ids)){
    specimen_type_id <- specimen_type_ids[i]
    tmp.table.specimen_type[i,] <- table.specimen_type %>% filter(id %in% specimen_type_id)
  }
  names(tmp.table.specimen_type) <- names(table.specimen_type)
  specimen_type_labels <- tmp.table.specimen_type %>% pull(label)
  print("HERE3")
  #subject info
  tmp.table.study_subject <- matrix(nrow = length(study_subject_ids), ncol = ncol(table.study_subject)) %>% as.data.frame()
  for(i in 1:length(study_subject_ids)){
    study_subject_id <- study_subject_ids[i]
    tmp.table.study_subject[i,] <- table.study_subject %>% filter(id %in% study_subject_id)
  }
  names(tmp.table.study_subject) <- names(table.study_subject)
  subject_uids <- tmp.table.study_subject %>% pull(uid)
  print("HERE4")
  #study info
  study_ids <- tmp.table.study_subject %>% pull(study_id)
  tmp.table.study <- matrix(nrow = length(study_ids), ncol = ncol(table.study)) %>% as.data.frame()
  for(i in 1:length(study_ids)){
    study_id <- study_ids[i]
    tmp.table.study[i,] <- table.study %>% filter(id %in% study_id)
  }
  names(tmp.table.study) <- names(table.study)
  study_short_code <- tmp.table.study %>% pull(short_code)

  #plate info
  plate_ids <- table.matrix_tube %>% filter(id %in% matrix_tube_ids) %>% pull(plate_id)

  tmp.table.matrix_plate <- matrix(nrow = length(plate_ids), ncol = ncol(table.matrix_plate)) %>% as.data.frame()
  for(i in 1:length(plate_ids)){
    plate_id <- plate_ids[i]
    tmp.table.matrix_plate[i,] <- table.matrix_plate %>% filter(id %in% plate_id)
  }
  names(tmp.table.matrix_plate) <- names(table.matrix_plate)
  plate_uids <- tmp.table.matrix_plate %>% pull(uid)

  #location info
  location_ids <- tmp.table.matrix_plate %>% pull(location_id)
  tmp.table.location <- matrix(nrow = length(location_ids), ncol = ncol(table.location)) %>% as.data.frame()
  for(i in 1:length(location_ids)){
    location_id <- location_ids[i]
    tmp.table.location[i,] <- table.location %>% filter(id %in% location_id)
  }
  names(tmp.table.location) <- names(table.location)
  location_descriptions <- tmp.table.location %>% pull(description)

  print("HERE5")
  well_positions <- table.matrix_tube %>% filter(id %in% matrix_tube_ids) %>% pull(well_position)
  barcodes <- table.matrix_tube %>% filter(id %in% matrix_tube_ids) %>% pull(barcode)

  # subject_uids <- table.study_subject %>% filter(id %in% study_subject_ids) %>% pull(uid)
  # studies <- table.study %>% filter(id %in% study_ids) %>% pull(short_code)
  # specimen_types <- table.specimen_type %>% filter(id %in% specimen_type_ids) %>% pull(label)
  # plate_uid <- table.plate %>% filter
  # location <- table.location %>% filter(id %in% location_id) %>% pull(description)

  # print("length(well_positions)")
  # print(length(well_positions))
  # print("length(barcodes)")
  # print(length(barcodes))
  # print("length(subject_uids)")
  # print(length(subject_uids))
  # print("length(studies)")
  # print(length(studies))
  # print("length(specimen_types)")
  # print(length(specimen_types))
  # print("length(location)")
  # print(length(location))
  # print("length(search_plate_uid)")
  # print(length(search_plate_uid))

  search_results <- tibble(well_position = well_positions,
                             barcode = barcodes,
                             subject_uid = subject_uids,
                             study = study_short_code,
                             specimen_type = specimen_type_labels,
                             location = location_descriptions,
                             plate_uid = plate_uids)

  # print(search_results)
  #step 3 filter by search item(s) not used to get to matrix_tube_ids
  for(filter_term in filter_search_s){
    print(filter_term)
    #filtering should be easy, just use the user input strings to filter the table in a loop
  }

  return(search_results)
}
