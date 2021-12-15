#' @export

# UploadSamples <- function(barcode_file, barcode_type, longitudinal, plate_id, location){
UploadSamples <- function(barcode_file, barcode_type, longitudinal, plate_id, location, study_short_code){

  table.location <- sampleDB::CheckTable("location")
  table.study <- sampleDB::CheckTable("study")
  table.specimen_type <- sampleDB::CheckTable("specimen_type")
  table.matrix_plate <- sampleDB::CheckTable("matrix_plate")
  table.study_subject <- sampleDB::CheckTable("study_subject")
  table.specimen <- sampleDB::CheckTable("specimen")

  # read in user supplied csv
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

  # adding data to the csv
  csv <- csv %>% mutate(location = location,
                        plate_id = plate_id,
                        study_short_code = study_short_code)

  # MATRIX_PLATE: add plate_id & location
  location_id <- table.location %>% filter(description == location) %>% pull(id)
  sampleDB::AddToTable("matrix_plate",
                       list(created = "dummy",
                            last_updated = "dummy",
                            uid = plate_id,
                            hidden = 0,
                            location_id = location_id))

  # MATRIX_TUBE: add matrix_plate_id & barcode & well position
  plate_id <- sampleDB::CheckTable("matrix_plate") %>% tail(1) %>% pull(id)
  for(i in 1:nrow(csv)){
    barcode <- csv %>% slice(i) %>% pull(barcode) %>% as.character
    print(barcode)
    well_position <- csv %>% slice(i) %>% pull(well_position)
    print(well_position)

    sampleDB::AddToTable("matrix_tube",
                         list(plate_id = plate_id,
                              barcode = barcode,
                              well_position = well_position))
  }

  #STUDY_SUBJECT: add uid & study_id (first check if uid is already in table)
  study_id <- table.study %>% filter(short_code == study_short_code) %>% pull(id)

  for(i in 1:nrow(csv)){

    uid <- csv %>% slice(i) %>% pull(uid)

    switch.subject_study_id <- FALSE
    if(uid %in% (table.study_subject %>% pull(uid))){
      study_subject_id <- table.study_subject %>% filter(uid == uid) %>% pull(id)
      switch.subject_study_id <- TRUE
    }else{
      sampleDB::AddToTable("study_subject",
                           list(created = "dummy",
                                last_updated = "dummy",
                                uid = uid,
                                study_id = study_id))
      study_subject_id <- CheckTable("study_subject") %>% tail(1) %>% pull(id)
    }

    specimen_type_id <- table.specimen_type %>% filter(label == csv %>% slice(i) %>% pull(specimen_type)) %>% pull(id)

    #if the study_subject entry already exists;then use the specimen id
    if(switch.subject_study_id){

      print("subject_study_id already exists")
      specimen_id <- table.specimen %>% filter(study_subject_id == study_subject_id, specimen_type_id == specimen_type_id) %>% pull(id)

      sampleDB::AddToTable("storage_container",
                           list(created = "dummy",
                                last_updated = "dummy",
                                type = NA,
                                specimen_id = specimen_id,
                                comments = NA,
                                exhausted = 0))
    }else{

      print("subject_study_id DOES NOT exists")
      #SPECIMEN TABLE
      sampleDB::AddToTable("specimen",
                           list(created = "dummy",
                                last_updated = "dummy",
                                study_subject_id = study_subject_id,
                                specimen_type_id = specimen_type_id,
                                collection_date = NA))
    }

    specimen_id <- CheckTable("specimen") %>% tail(1) %>% pull(id)

    #STORAGE CONTAINER TABLE
    sampleDB::AddToTable("storage_container",
                         list(created = "dummy",
                              last_updated = "dummy",
                              type = NA,
                              specimen_id = specimen_id,
                              comments = NA,
                              exhausted = 0))
  }

}
