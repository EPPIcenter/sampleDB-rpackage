#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

UploadSamples <- function(database, barcode_file, plate_id, location, study_short_code, session, output){

  #UNTIL READING THE COLNAMES ASSUME DATE IS NOT LONGITUDINAL
  toggle.is_longitudinal <- FALSE

  #READIN CSV FROM USER WITH VISIONMATE/TRAXER BARCODES,
  csv <- read.csv(barcode_file, check.names = F)
  if("collection_date" %in% names(csv)){
    toggle.is_longitudinal <- TRUE
  }

  #REFORMAT CSV -- IF LOCATIONROW IS A COLUMN THEN THE DATA CAME OFF VISIONMATE
  if(!("LocationRow" %in% names(csv))){
    csv <- drop_na(csv) %>%
      mutate(barcode = `Tube ID`,
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      select(-c(Position:Date))
  }else{
    csv <- drop_na(csv) %>%
      mutate(barcode = TubeCode,
             well_position = paste0(LocationRow, LocationColumn)) %>%
      select(-c(LocationRow, LocationColumn, TubeCode))
  }

  #ADD TO MATRIX_PLATE TABLE
  sampleDB::AddToTable(database = database, "matrix_plate",
                       list(created = lubridate::now("UTC") %>% as.character(),
                            last_updated = lubridate::now("UTC") %>% as.character(),
                            uid = plate_id,
                            hidden = 0,
                            location_id = filter(CheckTable(database = database, "location"), description == location)$id))
  
  #ADD TO MATRIX_TUBE TABLE
  for(i in 1:nrow(csv)){
    sampleDB::AddToTable(database = database, "matrix_tube",
                         list(plate_id = tail(sampleDB::CheckTable(database = database, "matrix_plate"), 1)$id,
                              barcode = csv[i,]$"barcode" %>% as.character(),
                              well_position = csv[i,]$"well_position"))
  }
  message(paste("UPLOADING PLATE", plate_id, "CONTAINING", i, "SAMPLES"))

  ###########################################
  # PARSE THROUGH EACH ROW IN THE UPLOADCSV #
  ###########################################

  message <- NULL
  for(i in 1:nrow(csv)){
    
    #GET SPECIMEN TYPE ID
    eval.specimen_type_id <- filter(CheckTable(database = database, "specimen_type"), label == csv[i, ]$"specimen_type")$id
    #GET STUDY_ID
    eval.study_id <- filter(CheckTable(database = database, "study"), short_code == csv[i, ]$"study_short_code")$id
    #GET STUDY_SUBJ_ID
    eval.uid <- csv[i, ]$"study_subject_id"
    #GET COLLECTION_DATE (IF LONGITUINAL)
    if(toggle.is_longitudinal){
      eval.collection_date <- ymd(csv[i, ]$"collection_date")
      eval.collection_date <- paste(year(eval.collection_date), month(eval.collection_date), day(eval.collection_date), sep = "-")
    }else{
      eval.collection_date <- NA
    }

    #CHECK IF STUDY_SUBJECT_ID/STUDY_ID EXISTS
    tmp_table.study_subject <- inner_join(CheckTable(database = database, "study_subject")[, c("uid", "study_id")],
                                          tibble(uid = eval.uid, study_id = eval.study_id),
                                          by = c("uid", "study_id"))
    
    if(nrow(tmp_table.study_subject) > 0){

      #IF STUDY_SUBJECT_ID/STUDY_ID EXISTS: GET STUDY_SUBJECT_TABLE ID
      eval.study_subject_id <- filter(CheckTable(database = database, "study_subject"), uid == eval.uid & study_id == eval.study_id)$id

      #CHECK IF SPECIMEN ENTRY EXISTS
      tmp_table.specimen <- inner_join(CheckTable(database = database, "specimen")[,c("study_subject_id", "specimen_type_id", "collection_date")],
                                       tibble(study_subject_id = eval.study_subject_id, specimen_type_id = eval.specimen_type_id, collection_date = eval.collection_date),
                                       by = c("study_subject_id", "specimen_type_id", "collection_date"))

      if(nrow(tmp_table.specimen) > 0){

        #IF SPECIMEN ENTRY EXISTS: GET SPECIMEN ID
        if(is.na(eval.collection_date)){
          eval.specimen_id <- filter(CheckTable(database = database, "specimen"),
                                     study_subject_id == eval.study_subject_id,
                                     specimen_type_id == eval.specimen_type_id,
                                     is.na(eval.collection_date))$id
        }else{
          eval.specimen_id <- filter(CheckTable(database = database, "specimen"),
                                     study_subject_id == eval.study_subject_id,
                                     specimen_type_id == eval.specimen_type_id,
                                     collection_date == eval.collection_date)$id
        }

      }else{

        #IF SPECIMEN ENTRY DOES NOT EXIST: CREATE SPECIMEN ENTRY
        sampleDB::AddToTable(database = database, "specimen",
                             list(created = lubridate::now("UTC") %>% as.character(),
                                  last_updated = lubridate::now("UTC") %>% as.character(),
                                  study_subject_id = eval.study_subject_id,
                                  specimen_type_id = eval.specimen_type_id,
                                  collection_date = eval.collection_date)) #dates need to be in y:m:d format...needs to be one date at a time

        #GET SPECIMEN ID
        eval.specimen_id <- tail(CheckTable(database = database, "specimen"), 1)$id
      }

      #ADD SPECIMEN ID TO STORAGE CONTAINER
      sampleDB::AddToTable(database = database, "storage_container",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                type = "matrix_tube",
                                specimen_id = eval.specimen_id,
                                comments = NA,
                                exhausted = 0))
      
    }else{
      
      #################################################
      # IF THE STUDY_SUBJ_ID/STUDY_ID DOES NOT EXIST #
      #################################################

      #CREATE STUDY_STUBJECT
      sampleDB::AddToTable(database = database, "study_subject",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                uid = eval.uid,
                                study_id = eval.study_id))

      #GET STUDY_SUBJECT ID
      eval.study_subject_id <- tail(CheckTable(database = database, "study_subject"), 1)$id

      #ADD STUDY_SUBJECT & SPECIMEN_TYPE TO SPECIMEN_TABLE
      if(toggle.is_longitudinal){
        sampleDB::AddToTable(database = database, "specimen",
                             list(created = lubridate::now("UTC") %>% as.character(),
                                  last_updated = lubridate::now("UTC") %>% as.character(),
                                  study_subject_id = eval.study_subject_id,
                                  specimen_type_id = eval.specimen_type_id,
                                  collection_date = eval.collection_date))        
      }else{
        sampleDB::AddToTable(database = database, "specimen",
                             list(created = lubridate::now("UTC") %>% as.character(),
                                  last_updated = lubridate::now("UTC") %>% as.character(),
                                  study_subject_id = eval.study_subject_id,
                                  specimen_type_id = eval.specimen_type_id,
                                  collection_date = NA))
      }

      #GET SPECIMEN ID
      eval.specimen_id <- tail(CheckTable(database = database, "specimen"), 1)$id

    #STORAGE CONTAINER TABLE
    sampleDB::AddToTable(database = database, "storage_container",
                         list(created = lubridate::now("UTC") %>% as.character(),
                              last_updated = lubridate::now("UTC") %>% as.character(),
                              type = "matrix_tube",
                              specimen_id = eval.specimen_id,
                              comments = NA,
                              exhausted = 0))
    }
  }
  
  message("UPLOAD COMPLETE")
  
  #UPDATE THE SEARCH DROPDOWNS
  updateSelectizeInput(session = session,
                       "SearchByPlateID",
                       choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$uid),
                       label = NULL)
}
