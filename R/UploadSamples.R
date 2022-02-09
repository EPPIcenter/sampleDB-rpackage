#' Upload EPPIcenter Wetlab Samples to the sampleDB database
#' 
#' @param csv.upload An UploadCSV
#' @param container A plate name
#' @param list.location A freezer in the lab + a level I in that freezer + a level II in that freezer
#' @examples
#' UploadSamples(csv.upload = "/path/to/UploadCSV.csv", container = "dummy_name", location = list.location = list(location_name = "Freezer A", level_I = "dummy.levelI", level_II = "dummy.levelII"))
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

UploadSamples <- function(type, csv.upload, container, list.location){
  
  database <- "/databases/new.sampleDB.db"
  message(paste0("Connecting to database at", database))

  # READ IN USR SUPPLIED UPLOADCSV
  csv.upload <- read.csv(csv.upload, check.names = F)
  
  # UNITL A CHECK FOR A DATE COL IS PERFORMED ASSUME UPLOAD IS NOT LONGITUDINAL
  toggle.is_longitudinal <- FALSE
  if("collection_date" %in% names(csv.upload)){
    toggle.is_longitudinal <- TRUE
  }
  
  if(type == "micronix"){
    # CREATE A CSV WITH ALL THE ESSENTIAL INFO FROM EITHER TRAXER OR VISIONMATE CSV
    csv <- sampleDB::ReformatUploadMicronixCSV(csv.upload)
    
    # PERFORM CHECKS
    UploadMicronixChecks(input, database, list.location, container, csv.upload = csv.upload, csv.reformatted = csv)

    # ADD TO MATRIX_PLATE TABLE
    UploadMicronixPlate(database, container, list.location)    
    
    # ADD TO MATRIX_TUBE TABLE
    num_samples <- UploadMicronixTubes(database, csv)
    
    # UPLOAD MICRONIX MESSAGE
    message(paste("UPLOADING PLATE", container, "CONTAINING", num_samples, "MICRONIX SAMPLES"))
  }
  else if(type == "cryo"){
    #CREATE A CSV WITH ALL THE ESSENTIAL INFO FROM EITHER TRAXER OR VISIONMATE CSV
    # csv <- sampleDB::ReformatUploadMicronixCSV(csv.upload)
    csv <- csv.upload
    
    # PERFORM CHECKS
    # UploadMicronixChecks(input, database, list.location, container, csv.upload = csv.upload, csv.reformatted = csv)
    
    #ADD TO CRYO BOX TO TABLE
    UploadCryoBox(database, container, list.location)
    
    #ADD TO CRYO TUBE TABLE
    num_samples <- UploadCryoTubes(database, csv)
    
    message(paste("UPLOADING BOX", container, "CONTAINING", num_samples, "TUBES"))
  }
  else if(type == "rdt"){    #CREATE A CSV WITH ALL THE ESSENTIAL INFO FROM EITHER TRAXER OR VISIONMATE CSV
    # csv <- sampleDB::ReformatUploadMicronixCSV(csv.upload)
    csv <- csv.upload
    
    # PERFORM CHECKS
    # UploadMicronixChecks(input, database, list.location, container, csv.upload = csv.upload, csv.reformatted = csv)
    
    #ADD TO BAG TABLE
    UploadBag(database, container, list.location) 
    
    #ADD TO RDT TABLE
    num_samples <- UploadRDT(database, csv)
    
    message(paste("UPLOADING BAG", container, "CONTAINING", num_samples, "RDT SAMPLES"))
  }
  else if(type == "paper"){
    # csv <- sampleDB::ReformatUploadMicronixCSV(csv.upload)
    
    # PERFORM CHECKS
    # UploadMicronixChecks(input, database, list.location, container, csv.upload = csv.upload, csv.reformatted = csv)
    
    #ADD TO BAG TABLE
    UploadBag(database, container, list.location) 
    
    #ADD TO PAPER TABLE
    num_samples <- UploadPaper(database, csv)
    
    message(paste("UPLOADING BAG", container, "CONTAINING", num_samples, "PAPER SAMPLES"))
  }
  else{message("User Upload Type was not a valid option. Valid options are: micronix, cryo, rdt and paper")}


  ###########################################
  # PARSE THROUGH EACH ROW IN THE UPLOADCSV #
  ###########################################
  
  message <- NULL
  for(i in 1:nrow(csv)){
    
    #GET VARIABLES IN UPLOAD
    eval.specimen_type_id <- filter(CheckTable(database = database, "specimen_type"), label == csv[i, ]$"specimen_type")$id
    eval.study_id <- filter(CheckTable(database = database, "study"), short_code == csv[i, ]$"study_short_code")$id
    eval.subject <- csv[i, ]$"study_subject_id"
    if(toggle.is_longitudinal){
      eval.collection_date <- ymd(csv[i, ]$"collection_date")
      eval.collection_date <- paste(year(eval.collection_date), month(eval.collection_date), day(eval.collection_date), sep = "-")
    }else{
      eval.collection_date <- as.character(NA)
    }

    #CHECK IF THIS SUBJECT + STUDY COMBO EXISTS
    tmp_table.study_subject <- inner_join(CheckTable(database = database, "study_subject")[, c("subject", "study_id")],
                                          tibble(subject = eval.subject, study_id = eval.study_id),
                                          by = c("subject", "study_id"))
    
    if(nrow(tmp_table.study_subject) > 0){
      #IF THIS SUBJECT + STUDY COMBINATION EXISTS GET ITS STUDY_SUBJECT_TABLE ID
      eval.study_subject_id <- filter(CheckTable(database = database, "study_subject"), subject == eval.subject & study_id == eval.study_id)$id

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
                                type = type,
                                specimen_id = eval.specimen_id,
                                exhausted = 0))
      
    }else{
      
      # IF THIS COMBINATION OF  SUBJECT AND STUDY DOES NOT EXIST 

      #CREATE STUDY_STUBJECT
      sampleDB::AddToTable(database = database, "study_subject",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                subject = eval.subject,
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
                              type = type,
                              specimen_id = eval.specimen_id,
                              exhausted = 0))
    }
  }
  
  message("UPLOAD COMPLETE")
}
