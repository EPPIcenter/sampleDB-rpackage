#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @export

# UploadSamples <- function(barcode_file, barcode_type, longitudinal, plate_id, location){
UploadSamples <- function(database, barcode_file, plate_id, location, study_short_code, session){

  #UNTIL READING THE COLNAMES ASSUME DATE IS NOT LONGITUDINAL
  toggle.is_longitudinal <- FALSE

  #READIN CSV FROM USER WITH VISIONMATE/TRAXER BARCODES,
  csv <- read_csv(barcode_file)

  #REFORMAT CSV -- IF LOCATIONROW IS A COLUMN THEN THE DATA CAME OFF VISIONMATE
  if(!("LocationRow" %in% names(csv))){
    csv <- drop_na(csv) %>%
      mutate(barcode = `Tube ID`,
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      select(-c(Position:Date))
    if("dates" %in% names(csv)){
      dates <- drop_na(read_csv(barcode_file))$dates
      toggle.is_longitudinal <- TRUE
    }
  }else{

    csv <- drop_na(csv) %>%
      mutate(barcode = TubeCode,
             well_position = paste0(LocationRow, LocationColumn)) %>%
      select(-c(LocationRow, LocationColumn, TubeCode))

    if("dates" %in% names(csv)){
      dates <- drop_na(read_csv(barcode_file))$dates
      toggle.is_longitudinal <- TRUE
    }
  }

  #ADD PLATE_ID AND FREEZER LOCATION TO _*_MATRIX PLATE_*_ TABLE
  sampleDB::AddToTable(database = database, "matrix_plate",
                       list(created = lubridate::now("UTC") %>% as.character(),
                            last_updated = lubridate::now("UTC") %>% as.character(),
                            uid = plate_id,
                            hidden = 0,
                            location_id = filter(CheckTable(database = database, "location"), description == location)$id))

  #ADD PLATE_ID, BARCODE AND WELL POSITION TO _*_MATRIX TUBE_*_ TABLE
  for(i in 1:nrow(csv)){
    sampleDB::AddToTable(database = database, "matrix_tube",
                         list(plate_id = tail(sampleDB::CheckTable(database = database, "matrix_plate"), 1)$id,
                              barcode = csv[i,]$"barcode" %>% as.character(),
                              well_position = csv[i,]$"well_position"))
  }

  message <- NULL
  #IF THE INDIVIDUAL_ID DOES NOT EXIST ADD IT TO THE _*_STUDY_SUBJECT_*_ TABLE
  for(i in 1:nrow(csv)){

    #GET SPECIMEN TYPE ID, STUDY_ID, STUDY_SUBJ_ID AND COLLECTION_DATE ASSO W THE TUBE
    specimen_type_id <- csv[i, ]$"specimen_type"
    study_id <- filter(CheckTable(database = database, "study"), short_code == study_short_code)$id
    uid <- csv[i, ]$"individual_id"
    if(toggle.is_longitudinal){
      collection_date <- dates[i]
    }else{
      collection_date <- NA
    }

    #CHECK TO SEE IF STUDY_SUBJECT_ID/STUDY_ID EXISTS
    if(nrow(inner_join(CheckTable(database = database, "study_subject")[, c("uid", "study_id")], tibble(uid = uid, study_id = study_id))) > 0){

      #IF STUDY_SUBJECT_ID/STUDY_ID EXISTS FETCH STUDY_SUBJECT_TABLE ENTRY'S ID
      study_subject_id <- filter(CheckTable(database = database, "study_subject"), uid == uid & study_id == study_id)$id

      #CHECK TO SEE THERE IS A SPECIMEN ENTRY WITH THE SAME STUDY_SUBJECT_ID/STUDY_ID, SPECIMEN_TYPE AND COLLECTION DATE INFO
      if(nrow(inner_join(CheckTable(database = database, "specimen")[,c("study_subject_id", "specimen_type_id", "collection_date")], tibble(study_subject_id = study_subject_id, specimen_type_id = specimen_type_id, collection_date = collection_date ))) > 0){

        #IF THERE IS AN EXISTING ENTRY GET THE SPECIMEN ID
        if(is.na(collection_id)){
          specimen_id <- filter(CheckTable(databae = database, "specimen"),
                                study_subject_id == study_subject_id,
                                specimen_type_id == specimen_type_id,
                                is.na(collection_date))$id
        }else{
          specimen_id <- filter(CheckTable(databae = database, "specimen"),
                                study_subject_id == study_subject_id,
                                specimen_type_id == specimen_type_id,
                                collection_date == collection_date)$id
        }

      }else{

        #IF AN ENTRY DOES NOT EXIST CREATE A NEW SPECIMEN_ID ENTRY WITH STUDY_SUBJECT_ID/STUDY_ID, SPECIMEN_TYPE AND COLLECTION DATE INFO
        sampleDB::AddToTable(database = database, "specimen",
                             list(created = lubridate::now("UTC") %>% as.character(),
                                  last_updated = lubridate::now("UTC") %>% as.character(),
                                  study_subject_id = study_subject_id,
                                  specimen_type_id = specimen_type_id,
                                  collection_date = collection_date)) #dates need to be in y:m:d format

        #GET THE SPECIMEN ID
        specimen_id <- tail(CheckTable(database = database, "specimen"), 1)

      }

      #ADD SPECIMEN INFORMATION TO STORAGE CONTAINER TABLE
      sampleDB::AddToTable(database = database, "storage_container",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                type = "matrix_tube",
                                specimen_id = specimen_id,
                                comments = "NA",
                                exhausted = 0))

    #IF THE STUDY_SUBJ_ID/STUDY_ID DOES NOT EXIST
    }else{

      #CREATE A NEW STUDY_STUBJECT_TABLE ENTRY
      sampleDB::AddToTable(database = database, "study_subject",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                uid = uid,
                                study_id = study_id))

      #FETCH THE NEW STUDY_SUBJECT_TABLE_ID
      study_subject_id <- tail(CheckTable(database = database, "study_subject"), 1)$id

      #ADD STUDY_SUBJECT_ID AND SPECIMEN_TYPE_ID TO _*_SPECIMEN_TABLE_*_
      sampleDB::AddToTable(database = database, "specimen",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                study_subject_id = study_subject_id,
                                specimen_type_id = specimen_type_id,
                                collection_date = collection_date)) #date is in y:m:d format
      specimen_id <- tail(CheckTable(database = database, "specimen"), 1)

    #STORAGE CONTAINER TABLE
    sampleDB::AddToTable(database = database, "storage_container",
                         list(created = lubridate::now("UTC") %>% as.character(),
                              last_updated = lubridate::now("UTC") %>% as.character(),
                              type = "matrix_tube",
                              specimen_id = specimen_id,
                              comments = NA,
                              exhausted = 0))
    }
  }

  #UPDATE THE SEARCH DROPDOWNS
  updateSelectizeInput(session = session,
                       "SearchByPlateID",
                       choices = sampleDB::CheckTable(database = database, "matrix_plate")$uid,
                       label = NULL)

  message <- paste("Upload Complete", emoji('tada'))
  return(message)

}
