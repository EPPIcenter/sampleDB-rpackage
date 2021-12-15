#' @export

SearchSamples <- function(barcode_search_file, search_plate_uid, search_subject_uid, search_study, seach_location, search_specimen_type){

  table.location <- sampleDB::CheckTable("location")
  table.study <- sampleDB::CheckTable("study")
  table.specimen_type <- sampleDB::CheckTable("specimen_type")
  table.matrix_plate <- sampleDB::CheckTable("matrix_plate")
  table.matrix_tube <- sampleDB::CheckTable("matrix_tube")
  table.study_subject <- sampleDB::CheckTable("study_subject")
  table.specimen <- sampleDB::CheckTable("specimen")
  table.storage_container <- sampleDB::CheckTable("storage_container")

  #option 1 search
  #only search_plate_uid is input
  #get plate ref id for user selected plate
  plate_ref_id <- table.matrix_plate %>% filter(uid == search_plate_uid) %>% pull(id)
  print("plate_ref_id")
  print(plate_ref_id)

  #find all tubes asso w the plate ref id
  matrix_tube_ids <- table.matrix_tube %>% filter(plate_id == plate_ref_id) %>% pull(id) #uses these to ref storage_container to get specimen_id
  print("matrix_tube_ids")
  print(matrix_tube_ids)

  #use the matrix tube ids to find the barcodes and well positions asso w the tubes
  well_positions <- table.matrix_tube %>% filter(plate_id == plate_ref_id) %>% pull(well_position)
  print("well_positions")
  print(well_positions)

  barcodes <- table.matrix_tube %>% filter(plate_id == plate_ref_id) %>% pull(barcode)
  print(barcodes)
  print("barcodes")

  #find all the specimen_ids asso w tubes using the storage container table
  specimen_ids <- table.storage_container %>% filter(id %in% matrix_tube_ids) %>% pull(specimen_id)
  print("specimen_ids")
  print(specimen_ids)

  #use the specimen ids to find all the study subject ids asso w the tubes
  study_subject_ids <- table.specimen %>% filter(id %in% specimen_ids) %>% pull(study_subject_id)
  print("study_subject_ids")
  print(study_subject_ids)

  #use the specimen ids to find all the specimen_ids asso w the tubes
  specimen_type_ids <- table.specimen %>% filter(id %in% specimen_ids) %>% pull(specimen_type_id)
  print("specimen_type_ids")
  print(specimen_type_ids)

  #option 2 search
  #use all the study subject ids to find all the subject_uids asso w the tubes
  subject_uids <- table.study_subject %>% filter(id %in% study_subject_ids) %>% pull(uid)
  print("subject_uids")
  print(subject_uids)

  #use all the study subject ids to find all the study ids asso w the tubes
  study_ids <- table.study_subject %>% filter(id %in% study_subject_ids) %>% pull(study_id)
  print("study_ids")
  print(study_ids)

  #option 3 search
  #use all the study ids to find the studies asso w the tubes
  studies <- table.study %>% filter(id %in% study_ids) %>% pull(short_code)
  print("studies")
  print(studies)

  #option 5 search
  #use all the specimen type ids to find the specimen types asso w the tubes
  specimen_types <- table.specimen_type %>% filter(id %in% specimen_type_ids) %>% pull(label)
  print("specimen_types")
  print(specimen_types)

  #find the location_ids asso w the plate
  location_id <- table.matrix_plate %>% filter(uid %in% search_plate_uid) %>% pull(location_id)
  print("location_id")
  print(location_id)

  #option 4 search
  #find the location description asso w the plate
  location <- table.location %>% filter(id %in% location_id) %>% pull(description)
  print("location")
  print(location)

  search_results <- tibble(well_position = well_positions,
                           barcode = barcodes,
                           subject_uid = subject_uids,
                           study = studies,
                           specimen_type = specimen_types,
                           location = location,
                           plate_uid = search_plate_uid)

  return(search_results)
}
