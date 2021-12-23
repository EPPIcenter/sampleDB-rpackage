#' @import dplyr
#' @export

# UploadSamples <- function(barcode_file, barcode_type, longitudinal, plate_id, location){
UploadSamples <- function(barcode_file, barcode_type, longitudinal, plate_id, location, study_short_code){

  #OBTAIN TABLES AS THEY ARE IN THE DATABASE RIGHT NOW (SNAPSHOT)
  table.location <- sampleDB::CheckTable("location")
  table.study <- sampleDB::CheckTable("study")
  table.specimen_type <- sampleDB::CheckTable("specimen_type")
  # table.matrix_plate <- sampleDB::CheckTable("matrix_plate")
  # table.study_subject <- sampleDB::CheckTable("study_subject")
  # table.specimen <- sampleDB::CheckTable("specimen")

  #READIN CSV FROM USER WITH VISIONMATE/TRAXER BARCODES,
  if(barcode_type == "traxer"){

    csv <- read_csv(barcode_file) %>%
      drop_na() %>%
      mutate(barcode = `Tube ID`,
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      select(-c(Position:Date))

  }else{

    csv <- read_csv(barcode_file) %>%
      drop_na() %>%
      mutate(barcode = TubeCode,
             well_position = paste0(LocationRow, LocationColumn)) %>%
      select(-c(LocationRow, LocationColumn, TubeCode))
  }

  #ADD PLATE_ID AND FREEZER LOCATION TO _*_MATRIX PLATE_*_ TABLE
  sampleDB::AddToTable("matrix_plate",
                       list(created = "dummy",
                            last_updated = "dummy",
                            uid = plate_id,
                            hidden = 0,
                            location_id = table.location %>% filter(description == location) %>% dplyr::pull(id)))

  #ADD PLATE_ID, BARCODE AND WELL POSITION TO _*_MATRIX TUBE_*_ TABLE
  for(i in 1:nrow(csv)){
    sampleDB::AddToTable("matrix_tube",
                         list(plate_id = sampleDB::CheckTable("matrix_plate") %>% tail(1) %>% dplyr::pull(id),
                              barcode = csv %>% slice(i) %>% dplyr::pull(barcode) %>% as.character(),
                              well_position = csv %>% slice(i) %>% dplyr::pull(well_position)))
  }

  #IF THE INDIVIDUAL_ID DOES NOT EXIST ADD IT TO THE _*_STUDY_SUBJECT_*_ TABLE
  for(i in 1:nrow(csv)){

    #CHECKING TO SEE IF INDIE ID EXISTS
    if(csv[i, "individual_id"] %in% CheckTable("study_subject")$uid){

      #IF INDIE ID EXISTS FETCH STUDY_SUBJECT_TABLE_ID ASSO W IT
      study_subject_table_id <- CheckTable("study_subject") %>% filter(uid == csv[i, "individual_id"]) %>% dplyr::pull(id)

    #INDIE ID IS NEW
    }else{

      #CREATE A NEW STUDY_STUBJECT_TABLE ENTRY
      sampleDB::AddToTable("study_subject",
                           list(created = "dummy",
                                last_updated = "dummy",
                                uid = csv %>% slice(i) %>% dplyr::pull(individual_id),
                                study_id = CheckTable("study") %>% filter(short_code == study_short_code) %>% dplyr::pull(id)))

      #FETCH THE NEW STUDY_SUBJECT_TABLE_ID
      study_subject_table_id <- CheckTable("study_subject") %>% tail(1) %>% dplyr::pull(id)
    }

    #ADD TO  _*_SPECIMEN_*_ TABLE AND _*_STORAGE_CONTAINER_*_ TABLE

    #CHECKING TO SEE IF INDIE EXISTS; IF INDIE EXISTS SPECIMEN_ID EXISTS
    if(csv[i, "individual_id"] %in% CheckTable("study_subject")$uid){

      #ADD SPECIMEN_ID TO  _*_STORAGE_CONTAINER_*_ TABLE IF STUDY_SUBJECT ENTRY EXISTS
      sampleDB::AddToTable("storage_container",
                           list(created = "dummy",
                                last_updated = "dummy",
                                type = NA,
                                specimen_id = CheckTable("specimen") %>% filter(study_subject_id == study_subject_table_id, specimen_type_id == filter(table.specimen_type, label == csv[i, "specimen_type"])$id) %>% dplyr::pull(id),
                                comments = NA,
                                exhausted = 0))
    }else{

      #ADD STUDY_SUBJECT_ID AND SPECIMEN_TYPE_ID TO _*_SPECIMEN_TABLE_*_ AND ADD THE NEW SPECIMEN_ID TO _*_STORAGE_CONTAINER_*_ TABLE IF SUBJECT_STUDY ENTRY DOES NOT EXIST
      sampleDB::AddToTable("specimen",
                           list(created = "dummy",
                                last_updated = "dummy",
                                study_subject_id = study_subject_table_id,
                                specimen_type_id = filter(table.specimen_type, label == csv[i, "specimen_type"])$id,
                                collection_date = NA))
    }

    #STORAGE CONTAINER TABLE
    sampleDB::AddToTable("storage_container",
                         list(created = "dummy",
                              last_updated = "dummy",
                              type = NA,
                              specimen_id = CheckTable("specimen") %>% tail(1) %>% dplyr::pull(id),
                              comments = NA,
                              exhausted = 0))
  }

}
