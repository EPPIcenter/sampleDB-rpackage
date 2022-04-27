
# Storage Type Check
.CheckSampleStorageType <- function(sample_type){
  out <- sample_type %in% c("micronix")
  return(out)
}

# Upload File Colnames Check
.CheckFormattedColnames <- function(formatted_upload_file){
  valid_colnames <- c("well_position", "label", "study_subject_id", "specimen_type", "study_short_code", "collection_date")
  out <- all(valid_colnames %in% names(formatted_upload_file))
  return(out)
}

# Freezer Address Check
.CheckFreezerAddress <- function(freezer_address, database){
  # check freezer address exists
  tmp.location.tbl <- inner_join(tibble(location_name = freezer_address$location_name, level_I = freezer_address$level_I, level_II = freezer_address$level_II), 
                                 sampleDB::CheckTable(database = database, "location"), 
                                 by = c("location_name", "level_I", "level_II"))
  out <- nrow(tmp.location.tbl) != 0
  return(out)
}

# App Upload File Colname Check
.CheckColnamesOfUserProvidedMicronixFileFormat <- function(upload_file_type, users_upload_file){
  
  #establish required metadata column names
  names.base <- c("Participant", "SpecimenType", "StudyCode")
  
  if(upload_file_type == "visionmate"){
    required_visionmate_colnames <- c(names.base, "LocationRow", "LocationColumn", "TubeCode")
    visionmate_colnames_withdate <- c(required_visionmate_colnames, "CollectionDate")
    out <- all(required_visionmate_colnames %in% names(users_upload_file)) || all(visionmate_colnames_withdate %in% names(users_upload_file))
  }
  else if(upload_file_type == "traxcer"){
    users_upload_file <- users_upload_file %>% setNames(.[1,]) %>% .[-1,]
    required_traxcer_colnames <- c(names.base, "Position", "Tube ID")
    traxcer_colnames_withdate <- c(required_traxcer_colnames, "CollectionDate")
    out <- all(required_traxcer_colnames %in% names(users_upload_file)) || all(traxcer_colnames_withdate %in% names(users_upload_file))
  }
  else{
    general_colnames <- c(names.base, "MicronixBarcode", "Row", "Column")
    out <- all(general_colnames %in% names(users_upload_file))
  }
  return(out)
}

# General Upload File Checks
.CheckUploadSpecimenTypes <- function(database, formatted_upload_file){

  specimen_types <- formatted_upload_file %>% pull(specimen_type)
  out <- all(specimen_types %in% sampleDB::CheckTable(database = database, table = "specimen_type")$label)
  return(out)
}

.CheckUploadStudyShortCodes <- function( database, formatted_upload_file){
  
  study_short_codes <- formatted_upload_file %>% pull(study_short_code)
  out <- all(study_short_codes %in% sampleDB::CheckTable(database = database, table = "study")$short_code)
  return(out)
}

.CheckUploadDateFormat <- function(database, formatted_upload_file){
  
  if("collection_date" %in% names(formatted_upload_file)){
    collection_dates <- formatted_upload_file %>% pull(collection_date)
    out <- all(!is.na(parse_date_time(collection_dates, orders = "ymd")) == TRUE)
  }else{
    out <- TRUE
  }
  return(out)
}

.CheckBarcodeIsntInDB <- function(database, formatted_upload_file){
  
  upload_barcodes <- formatted_upload_file %>% pull(label)
  barcodes.existing <- upload_barcodes[which(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))]
  out1 <- all(!(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)))
  out <- list(out1 = out1, out2 = barcodes.existing)
  return(out)
}

.CheckBarcodeArentRepeated <- function(database, formatted_upload_file){
  
  upload_barcodes <- formatted_upload_file %>% pull(label)
  dups <- upload_barcodes[duplicated(upload_barcodes)]
  out1 <- length(dups) == 0
  out <- list(out1 = out1, out2 = dups)
  return(out)
}

# Plate Checks
.CheckUploadContainerNameDuplication <- function(plate_name, database){
  
  out <- all(!(plate_name %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_name)))
  return(out)
}

.CheckUploadContainerBarcodeDuplication <- function(plate_barcode, database){
  
  if(plate_barcode != "" && !is.null(plate_barcode)){
    out <- all(!(plate_barcode %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_barcode)))
  }else{
    out <- TRUE
  }
  return(out)
}

# File Formatting
.FormatMicronixUploadData <- function(upload_file_type, users_upload_file){
  
  if(upload_file_type == "traxcer"){
    formatted_upload_file <- users_upload_file %>% 
      setNames(.[1,]) %>% .[-1,] %>%
      rename(specimen_type = SpecimenType,
             study_short_code = StudyCode,
             study_subject_id = Participant) %>%
      mutate(label = replace(`Tube ID`, nchar(`Tube ID`) != 10, NA),
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      tidyr::drop_na() %>%
      select(-c("Position","Tube ID"))
  }
  else if(upload_file_type == "visionmate"){
    formatted_upload_file <- users_upload_file %>%
      rename(specimen_type = SpecimenType,
             study_short_code = StudyCode,
             study_subject_id = Participant) %>% 
      mutate(label = replace(TubeCode, nchar(TubeCode) != 10, NA),
             well_position = paste0(LocationRow, LocationColumn)) %>%
      tidyr::drop_na() %>%
      select(-c("TubeCode","LocationRow", "LocationColumn"))
  }
  else{
    formatted_upload_file <- users_upload_file %>%
      rename(specimen_type = SpecimenType,
             study_short_code = StudyCode,
             study_subject_id = Participant) %>% 
      mutate(label = replace(MicronixBarcode, nchar(MicronixBarcode) != 10, NA),
             well_position = paste0(Row, Column)) %>%
      tidyr::drop_na() 
  }
  #removing row if micronix barcode is not string len 10
  
  if("CollectionDate" %in% names(formatted_upload_file)){
    formatted_upload_file <- formatted_upload_file %>% rename(collection_date = CollectionDate)
  }
  
  return(formatted_upload_file)
}