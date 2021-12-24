#' @import dplyr
#' @import emojifont
#' @export

# UploadSamples <- function(barcode_file, barcode_type, longitudinal, plate_id, location){
UploadSamples <- function(barcode_file, barcode_type, longitudinal, plate_id, location, study_short_code, session){

  #OBTAIN TABLES AS THEY ARE IN THE DATABASE RIGHT NOW (SNAPSHOT)
  table.location <- sampleDB::CheckTable("location")
  table.study <- sampleDB::CheckTable("study")
  table.specimen_type <- sampleDB::CheckTable("specimen_type")

  #READIN CSV FROM USER WITH VISIONMATE/TRAXER BARCODES,
  if(barcode_type == "traxer"){

    csv <- read_csv(barcode_file) %>%
      drop_na() %>%
      mutate(barcode = `Tube ID`,
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      select(-c(Position:Date))
    if(longitudinal == "true_longitudinal"){
      dates <- drop_na(read_csv(barcode_file))$dates
    }
  }else{

    csv <- read_csv(barcode_file) %>%
      drop_na() %>%
      mutate(barcode = TubeCode,
             well_position = paste0(LocationRow, LocationColumn)) %>%
      select(-c(LocationRow, LocationColumn, TubeCode))

    if(longitudinal == "true_longitudinal"){
      dates <- drop_na(read_csv(barcode_file))$dates
    }
  }

  #ADD PLATE_ID AND FREEZER LOCATION TO _*_MATRIX PLATE_*_ TABLE
  sampleDB::AddToTable("matrix_plate",
                       list(created = lubridate::now("UTC") %>% as.character(),
                            last_updated = lubridate::now("UTC") %>% as.character(),
                            uid = plate_id,
                            hidden = 0,
                            location_id = filter(table.location, description == location)$id))

  #ADD PLATE_ID, BARCODE AND WELL POSITION TO _*_MATRIX TUBE_*_ TABLE
  for(i in 1:nrow(csv)){
    sampleDB::AddToTable("matrix_tube",
                         list(plate_id = tail(sampleDB::CheckTable("matrix_plate"), 1)$id,
                              barcode = csv[i,]$"barcode" %>% as.character(),
                              well_position = csv[i,]$"well_position"))
  }

  #IF THE INDIVIDUAL_ID DOES NOT EXIST ADD IT TO THE _*_STUDY_SUBJECT_*_ TABLE
  for(i in 1:nrow(csv)){

    #CHECKING TO SEE IF INDIE ID EXISTS
    if(csv[i, "individual_id"] %in% CheckTable("study_subject")$uid){

      #IF INDIE ID EXISTS FETCH STUDY_SUBJECT_TABLE_ID ASSO W IT
      study_subject_table_id <- filter(CheckTable("study_subject"), uid == csv[i, ]$"individual_id")$id


      #ADD TO NEW SPECIMEN _*_SPECIMEN_*_ TABLE -- IF THE INDIE ID ALREADY EXISTS THEN IT MUST BE PART OF A LONGITUDINAL STUDY
      sampleDB::AddToTable("specimen",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                study_subject_id = study_subject_table_id,
                                specimen_type_id = filter(table.specimen_type, label == csv[i, ]$"specimen_type")$id,
                                collection_date = as.character(dates[i]))) #dates need to be in y:m:d format

      #ADD SPECIMEN_ID TO  _*_STORAGE_CONTAINER_*_ TABLE IF STUDY_SUBJECT ENTRY EXISTS
      sampleDB::AddToTable("storage_container",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                type = "NA",
                                specimen_id = filter(CheckTable("specimen"), study_subject_id == study_subject_table_id, specimen_type_id == filter(table.specimen_type, label == csv[i, ]$"specimen_type")$id)$id,
                                comments = "NA",
                                exhausted = 0))

    #INDIE ID IS NEW
    }else{

      #CREATE A NEW STUDY_STUBJECT_TABLE ENTRY
      sampleDB::AddToTable("study_subject",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                uid = csv[i,]$individual_id,
                                study_id = filter(CheckTable("study"), short_code == study_short_code)$id))

      #FETCH THE NEW STUDY_SUBJECT_TABLE_ID
      study_subject_table_id <- tail(CheckTable("study_subject"), 1)$id

      #ADD STUDY_SUBJECT_ID AND SPECIMEN_TYPE_ID TO _*_SPECIMEN_TABLE_*_ AND ADD THE NEW SPECIMEN_ID TO _*_STORAGE_CONTAINER_*_ TABLE IF SUBJECT_STUDY ENTRY DOES NOT EXIST
      print("HERE1")
      if(longitudinal == "true_longitudinal"){

        sampleDB::AddToTable("specimen",
                             list(created = lubridate::now("UTC") %>% as.character(),
                                  last_updated = lubridate::now("UTC") %>% as.character(),
                                  study_subject_id = study_subject_table_id,
                                  specimen_type_id = filter(table.specimen_type, label == csv[i,]$"specimen_type")$id,
                                  collection_date = as.character(dates[i]))) %>% print() #date is in y:m:d format

      }else{
        sampleDB::AddToTable("specimen",
                             list(created = lubridate::now("UTC") %>% as.character(),
                                  last_updated = lubridate::now("UTC") %>% as.character(),
                                  study_subject_id = study_subject_table_id,
                                  specimen_type_id = filter(table.specimen_type, label == csv[i,]$"specimen_type")$id,
                                  collection_date = "NA"))
      }

    #STORAGE CONTAINER TABLE
    sampleDB::AddToTable("storage_container",
                         list(created = lubridate::now("UTC") %>% as.character(),
                              last_updated = lubridate::now("UTC") %>% as.character(),
                              type = "NA",
                              specimen_id = tail(sampleDB::CheckTable("specimen"), 1)$id,
                              comments = "NA",
                              exhausted = 0))
    }
  }

  updateSelectizeInput(session = session,
                       "SearchByPlateID",
                       choices = sampleDB::CheckTable("matrix_plate")$uid,
                       label = NULL)

  return(paste("Upload Complete", emoji('tada')))

}
