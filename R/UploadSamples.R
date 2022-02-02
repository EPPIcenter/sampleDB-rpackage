#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

UploadSamples <- function(csv.upload, name.plate, location){
  
  database <- "/databases/sampledb_database.sqlite"
  message(paste0("Connecting to database at", database))

  #READ IN USR SUPPLIED UPLOADCSV
  csv.upload <- read.csv(csv.upload, check.names = F)
  
  #UNITL A CHECK FOR A DATE COL IS PERFORMED ASSUME UPLOAD IS NOT LONGITUDINAL
  toggle.is_longitudinal <- FALSE
  if("collection_date" %in% names(csv.upload)){
    toggle.is_longitudinal <- TRUE
  }
  
  #CREATE A CSV WITH ALL THE ESSENTIAL INFO FROM EITHER TRAXER OR VISIONMATE CSV
  csv <- sampleDB::ReformatUploadCSV(csv.upload)
  
  ##############################################################################################
  # PERFORM CHECKS
  
  # CHECK PLATE NAME IS UNIQUE
  if(!location %in% c(sampleDB::CheckTable(database = database, "location")$description)){
    stop("FREEZER NAME DOES NOT EXITS")
  }
  
  # CHECK PLATE NAME IS UNIQUE
  if(name.plate %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$uid)){
    stop("PLATE NAME IS NOT UNIQUE")
  }
  
  #CHECK COLNAMES ARE NOT MALFORMED
  names.traxer.nodate <- c("Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date", "study_subject_id", "specimen_type", "study_short_code")
  names.traxer.date <- c("Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date", "study_subject_id", "specimen_type", "study_short_code", "collection_date")
  names.visionmate.nodate <- c("LocationRow", "LocationColumn", "TubeCode", "study_subject_id", "specimen_type", "study_short_code")
  names.visionmate.date <- c("LocationRow", "LocationColumn", "TubeCode", "study_subject_id", "specimen_type", "study_short_code", "collection_date")
  if(all(names.traxer.nodate %in% names(csv.upload)) || all(names.traxer.date %in% names(csv.upload)) || all(names.visionmate.nodate %in% names(csv.upload)) || all(names.visionmate.date %in% names(csv.upload))){
  }else{
    stop("UPLOADCSV COLNAMES ARE MALFORMED")
  }
  
  # CHECK DATE (IF PRESENT) IS IN CORRECT FORMAT
  if("collection_date" %in% names(csv)){
    if(is.na(parse_date_time(csv$"collection_date", orders = "ymd")) == TRUE){
      stop("COLLECTION DATE MUSE BE YMD")
    }
  }
  
  # CHECK THAT SPECIMEN TYPE(S) EXISTS
  if(!all(csv$"specimen_type" %in% CheckTable(database = database, table = "specimen_type")$label)){
    stop("ERROR: SPECIMEN TYPE(S) NOT FOUND")
  }
  
  # CHECK THAT NO BARCODE ALREADY EXISTS -- NEEDS TO BE REFLECTED IN UPLOADCHECKS.HELPER.R
  if(all(!csv$barcode %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))){
  }else{
    barcodes.existing <- csv$barcode[which(csv$barcode %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))]
    stop(paste("Error: Barcode Unique Constraint", barcodes.existing))
  }
  
  # CHECK THAT STUDY SUBJECT UID IS PRESENT IF IT IS REQUIRED
  tmp_table.specimen <- tibble(uid = csv$"study_subject_id", 
                                     study_id = filter(CheckTable(database = database, "study"), short_code %in% csv$study_short_code)$id, 
                                     specimen_type_id = filter(CheckTable(database = database, "specimen_type"), label %in% csv$specimen_type)$id)
  
  tmp_table.specimen <- inner_join(CheckTable(database = database, "study_subject"),
                                 tmp_table.specimen,
                                 by = c("uid", "study_id"))

  if(nrow(tmp_table.specimen) != 0){
    
    if("collection_date" %in% names(csv)){
      tmp_table.specimen$collection_date <- csv$collection_date
    }else{
      tmp_table.specimen$collection_date <- NA
    } 
 
    #CHECK IF SPECIMEN ALREADY EXISTS
    tmp_table.specimen <- inner_join(CheckTable(database = database, "specimen"), 
                                     tmp_table.specimen %>% rename("study_subject_id" = "id"), 
                                     by = c("study_subject_id", "specimen_type_id", "collection_date"))
    
    if(nrow(tmp_table.specimen) > 0){
      stop("SPECIMEN ALREADY EXISTS IN THE DATABASE. SPECIMENS UNIQUE CONSTRAINT: SUBJECT + STUDY + SPECIMEN TYPE + COLLECTION DATE")
    }
  }
  

  ###############################################################################################
  
  #ADD TO MATRIX_PLATE TABLE
  eval.location_id <- filter(CheckTable(database = database, "location"), description == location)$id
  sampleDB::AddToTable(database = database, 
                       "matrix_plate",
                       list(created = lubridate::now("UTC") %>% as.character(),
                            last_updated = lubridate::now("UTC") %>% as.character(),
                            uid = name.plate,
                            hidden = 0,
                            location_id = eval.location_id))
  
  #ADD TO MATRIX_TUBE TABLE
  for(i in 1:nrow(csv)){
    eval.plate_id <- tail(sampleDB::CheckTable(database = database, "matrix_plate"), 1)$id
    eval.barcode <- csv[i,]$"barcode" %>% as.character()
    eval.well_position <- csv[i,]$"well_position"
    
    sampleDB::AddToTable(database = database,
                         "matrix_tube",
                         list(plate_id = eval.plate_id,
                              barcode = eval.barcode,
                              well_position = eval.well_position))
  }
  
  message(paste("UPLOADING PLATE", name.plate, "CONTAINING", i, "SAMPLES"))

  ###########################################
  # PARSE THROUGH EACH ROW IN THE UPLOADCSV #
  ###########################################
  
  message <- NULL
  for(i in 1:nrow(csv)){
    
    #GET VARIABLES IN UPLOAD
    eval.specimen_type_id <- filter(CheckTable(database = database, "specimen_type"), label == csv[i, ]$"specimen_type")$id
    eval.study_id <- filter(CheckTable(database = database, "study"), short_code == csv[i, ]$"study_short_code")$id
    eval.uid <- csv[i, ]$"study_subject_id"
    if(toggle.is_longitudinal){
      eval.collection_date <- ymd(csv[i, ]$"collection_date")
      eval.collection_date <- paste(year(eval.collection_date), month(eval.collection_date), day(eval.collection_date), sep = "-")
    }else{
      eval.collection_date <- NA
    }

    #CHECK IF THIS SUBJECT + STUDY COMBO EXISTS
    tmp_table.study_subject <- inner_join(CheckTable(database = database, "study_subject")[, c("uid", "study_id")],
                                          tibble(uid = eval.uid, study_id = eval.study_id),
                                          by = c("uid", "study_id"))
    
    if(nrow(tmp_table.study_subject) > 0){
      #IF THIS SUBJECT + STUDY COMBINATION EXISTS GET ITS STUDY_SUBJECT_TABLE ID
      eval.study_subject_id <- filter(CheckTable(database = database, "study_subject"), uid == eval.uid & study_id == eval.study_id)$id

      #CHECK IF THIS SPECIMEN EXISTS (SUBJECT + STUDY + SPECIMEN_TYPE)
      tmp_table.specimen <- inner_join(CheckTable(database = database, "specimen")[,c("study_subject_id", "specimen_type_id", "collection_date")],
                                       tibble(study_subject_id = eval.study_subject_id, specimen_type_id = eval.specimen_type_id, collection_date = eval.collection_date),
                                       by = c("study_subject_id", "specimen_type_id", "collection_date"))

      if(nrow(tmp_table.specimen) > 0){

        #IF SPECIMEN EXISTS: GET SPECIMEN ID
        
        #IF THE SPECIMEN THAT EXISTS HAS NA FOR THE COLLECTION DATE
        if(is.na(eval.collection_date)){
          eval.specimen_id <- filter(CheckTable(database = database, "specimen"),
                                     study_subject_id == eval.study_subject_id,
                                     specimen_type_id == eval.specimen_type_id,
                                     is.na(eval.collection_date))$id
        
        #IF THE SPECIMEN THAT EXISTS HAS A COLLECTION DATE
        }else{
          eval.specimen_id <- filter(CheckTable(database = database, "specimen"),
                                     study_subject_id == eval.study_subject_id,
                                     specimen_type_id == eval.specimen_type_id,
                                     collection_date == eval.collection_date)$id
        }

      }else{

        #IF THIS SPECIMEN DOES NOT EXIST CREATE IT
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
      
      # IF THIS COMBINATION OF  SUBJECT AND STUDY DOES NOT EXIST 

      #CREATE STUDY_STUBJECT
      sampleDB::AddToTable(database = database, "study_subject",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                uid = eval.uid,
                                study_id = eval.study_id))

      #GET STUDY_SUBJECT ID
      eval.study_subject_id <- tail(CheckTable(database = database, "study_subject"), 1)$id

      #ADD STUDY + SUBJECT + SPECIMEN_TYPE COMBINATION TO SPECIMEN_TABLE
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
}
