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

  if(is.null(barcode_search_file) & search_plate_uid != "" & search_subject_uid == "" & search_study == "" & seach_location == "" & search_specimen_type == ""){

    #well positions and barcodes
    plate_ref_id <- table.matrix_plate %>% filter(uid == search_plate_uid) %>% pull(id)

    matrix_tube_ids <- table.matrix_tube %>% filter(plate_id == plate_ref_id) %>% pull(id)
    specimen_ids <- table.storage_container %>% filter(id %in% matrix_tube_ids) %>% pull(specimen_id)
    study_subject_ids <- table.specimen %>% filter(id %in% specimen_ids) %>% pull(study_subject_id)
    specimen_type_ids <- table.specimen %>% filter(id %in% specimen_ids) %>% pull(specimen_type_id)
    study_ids <- table.study_subject %>% filter(id %in% study_subject_ids) %>% pull(study_id)
    location_id <- table.matrix_plate %>% filter(uid %in% search_plate_uid) %>% pull(location_id)

    subject_uids <- table.study_subject %>% filter(id %in% study_subject_ids) %>% pull(uid)
    studies <- table.study %>% filter(id %in% study_ids) %>% pull(short_code)
    specimen_types <- table.specimen_type %>% filter(id %in% specimen_type_ids) %>% pull(label)
    location_id <- table.matrix_plate %>% filter(uid %in% search_plate_uid) %>% pull(location_id)
    well_positions <- table.matrix_tube %>% filter(plate_id == plate_ref_id) %>% pull(well_position)
    barcodes <- table.matrix_tube %>% filter(plate_id == plate_ref_id) %>% pull(barcode)

    search_results <- tibble(well_position = well_positions,
                             barcode = barcodes,
                             subject_uid = subject_uids,
                             study = studies,
                             specimen_type = specimen_types,
                             location = location,
                             plate_uid = search_plate_uid)

  }

  return(search_results)
}
