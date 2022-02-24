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

# am now uploadin internal data before containers and samples
# so i dont think upload container and samples (eg matrix_tubes, rdt) are uploaded regardless of whether internal upload is successful
# there are two problems
# 1. is that internal upload (specimen, storage_container, study_subject) occurs before container (plates, boxes, bags) and samples (cryo tubes, rdt, micronix tubes)
# this means that in order for the ids of samples to match the upload ids no errors can take place during either of these uploads. they work in tandem so must fail together or be successful together
# this leads to issue 2
# 2. connections to the database are (to the best of my knowledge) opening and closing all throughout the upload. This is bad, it means that if two usrs try to upload something at the same time
# the can get their connections to the db (via an upload) can get crossed and ids storage container ids could be useless
# There are three ways to overcome these problems:
# A. upload internal data and container/sample data at the same time (is this possible? idk. it is how thing *should* be)
# B. open a connection to the database only at the beginning of the upload and close the connection only at the end on the upload. many sampleDB funs open and close the db so the use of those funs here is
# very problematic (it is how things i think *need* to be in order for 2+ users to work with the db at the same time)
# C. Adds to the db tables occur not in a loop but all at once. (this is how things *should* be)
UploadSamples <- function(type, csv.upload, container, list.location){
  
  database <- "/databases/sampledb/v0.0.2/sampledb_database.sqlite"
  message(paste("Connecting to database at", database))

  # Read in usr supplied upload csv
  csv.upload <- read.csv(csv.upload, check.names = F) %>% tidyr::drop_na()
  
  # If type equals micronix reformat the csv.upload
  if(type == "micronix"){
    csv.upload <- .ReformatUploadMicronixCSV(csv.upload)
    }
  
  # Check for a date col
  toggle.is_longitudinal <- FALSE
  if("collection_date" %in% names(csv.upload)){
    toggle.is_longitudinal <- TRUE
  }
  
  # Perform sample upload checks
  .UploadChecks(type, input, database, list.location, container, csv.upload = csv.upload)
  
  # Parse through each row in the upload csv
  sc_ids <- .InternalUpload(csv.upload = csv.upload, database = database, toggle.is_longitudinal = toggle.is_longitudinal, type = type)
  
  # Upload samples
  if(type == "micronix"){
    .UploadMicronixPlate(database, container, list.location)    
    .UploadMicronixTubes(database, csv.upload, sc_ids)
    message(paste("UPLOADING PLATE", container, "CONTAINING", nrow(csv.upload), "MICRONIX SAMPLES"))
  }
  else if(type == "cryo"){
    stopifnot("Malformed csv.upload column names" = setequal(names(csv.upload), c("label", "row","column","study_short_code", "study_subject_id", "specimen_type")) || setequal(names(csv.upload), c("label", "row","column", "study_short_code", "study_subject_id", "specimen_type", "collection_date")))
    .UploadCryoBox(database, container, list.location)
    .UploadCryoTubes(database, csv.upload, sc_ids)
    message(paste("UPLOADING BOX", container, "CONTAINING", nrow(csv.upload), "TUBES"))
  }
  else if(type == "rdt"){
    stopifnot("Malformed csv.upload column names" = setequal(names(csv.upload), c("label", "study_short_code", "study_subject_id", "specimen_type")) || setequal(names(csv.upload), c("label", "study_short_code", "study_subject_id", "specimen_type", "collection_date")))
    .UploadBag(database, container, list.location) 
    .UploadRDT(database, csv.upload, sc_ids)
    message(paste("UPLOADING BAG", container, "CONTAINING", nrow(csv.upload), "RDT SAMPLES"))
  }
  else if(type == "paper"){
    stopifnot("Malformed csv.upload column names" = setequal(names(csv.upload), c("label", "study_short_code", "study_subject_id", "specimen_type")) || setequal(names(csv.upload), c("label", "study_short_code", "study_subject_id", "specimen_type", "collection_date")))
    .UploadBag(database, container, list.location) 
    .UploadPaper(database, csv.upload, sc_ids)
    message(paste("UPLOADING BAG", container, "CONTAINING", nrow(csv.upload), "PAPER SAMPLES"))
  }
  else{
    message("User Upload Type was not a valid option. Valid options are: micronix, cryo, rdt and paper")
  }
  
  # message("UPLOAD COMPLETE")
}

.ReformatUploadMicronixCSV <- function(csv.upload){
  names.base <- c("study_subject_id", "specimen_type", "study_short_code")
  names.traxer.nodate <- c(names.base, "Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date")
  names.traxer.date <- c(names.traxer.nodate, "collection_date")
  names.visionmate.nodate <- c(names.base, "LocationRow", "LocationColumn", "TubeCode")
  names.visionmate.date <- c(names.visionmate.nodate, "collection_date")
  
  stopifnot("UPLOADCSV COLNAMES ARE MALFORMED" = (setequal(names.traxer.nodate, names(csv.upload)) || setequal(names.traxer.date, names(csv.upload)) || setequal(names.visionmate.nodate, names(csv.upload)) || setequal(names.visionmate.date, names(csv.upload))))
  
  #REFORMAT CSV -- IF LOCATIONROW IS A COLUMN THEN THE DATA CAME OFF VISIONMATE
  if(!("LocationRow" %in% names(csv.upload))){
    csv.reformatted <- csv.upload %>%
      mutate(label = `Tube ID`,
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      select(-c(Position:Date))
    message("UploadCSV from Traxer detected...")
  }else{
    csv.reformatted <- csv.upload %>%
      mutate(label = TubeCode,
             well_position = paste0(LocationRow, LocationColumn)) %>%
      select(-c(LocationRow, LocationColumn, TubeCode))
    message("UploadCSV from VisionMate detected...")
  }
  
  return(csv.reformatted)
}

.UploadChecks <- function(type, input, database, list.location, container, csv.upload){
  message("PERFORMING CHECKS")
  
  stopifnot("SAMPLE TYPE IS NOT VALID" = type %in% c("cryo", "micronix", "paper", "rdt"))
  
  # CHECK FREEZER EXISTS
  tmp.location.tbl <- inner_join(tibble(location_name = list.location$location_name, level_I = list.location$level_I, level_II = list.location$level_II), 
                                 sampleDB::CheckTable(database = database, "location"), 
                                 by = c("location_name", "level_I", "level_II"))
  stopifnot("FREEZER NAME + LEVEL I + LEVEL II DOES NOT EXITS" = nrow(tmp.location.tbl) != 0)
  
  # CHECK PLATE NAME IS UNIQUE
  stopifnot("CONTAINER NAME IS NOT UNIQUE" = !(container %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_name,
                                                            sampleDB::CheckTable(database = database, "box")$box_name,
                                                            sampleDB::CheckTable(database = database, "bag")$bag_name)))
  
  # CHECK THAT SPECIMEN TYPE(S) EXISTS
  stopifnot("ERROR: SPECIMEN TYPE(S) NOT FOUND" = all(csv.upload$"specimen_type" %in% CheckTable(database = database, table = "specimen_type")$label))
  
  # CHECK DATE (IF PRESENT) IS IN CORRECT FORMAT
  if("collection_date" %in% names(csv.upload)){
    stopifnot("COLLECTION DATE MUST BE YMD" = all(!is.na(parse_date_time(csv.upload$"collection_date", orders = "ymd")) == TRUE))
  }
  
  # CHECK THAT NO BARCODE ALREADY EXISTS
  stopifnot("Error: Barcode Unique Constraint" = all(!csv.upload$label %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode,
                                                                              sampleDB::CheckTable(database = database, "tube")$label,
                                                                              sampleDB::CheckTable(database = database, "rdt")$label,
                                                                              sampleDB::CheckTable(database = database, "paper")$label)))
  
  NonUniqueLabels <- csv.upload$label[which(csv.upload$label %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode,
                                                                    sampleDB::CheckTable(database = database, "tube")$label,
                                                                    sampleDB::CheckTable(database = database, "rdt")$label,
                                                                    sampleDB::CheckTable(database = database, "paper")$label))]
  
  if(length(NonUniqueLabels) != 0){print(NonUniqueLabels)}

  #CHECK IF SPECIMEN ALREADY EXISTS
  # check.study_subject <- inner_join(sampleDB::CheckTable(database = database, "study_subject"),
  #                                   tibble(subject = csv.upload$"study_subject_id",
  #                                          study_id = filter(sampleDB::CheckTable(database = database, "study"), short_code %in% csv.upload$study_short_code)$id,
  #                                          specimen_type_id = filter(sampleDB::CheckTable(database = database, "specimen_type"), label %in% csv.upload$specimen_type)$id),
  #                                   by = c("subject", "study_id"))
  # 
  # if(nrow(check.study_subject) != 0){
  #   
  #   if("collection_date" %in% names(csv.upload)){
  #     test_table.specimen <- check.study_subject %>% rename("study_subject_id" = "id")
  #     test_table.specimen$collection_date <- as.double(as.Date(csv.upload$collection_date))
  #   }else{
  #     test_table.specimen <- check.study_subject %>% rename("study_subject_id" = "id")
  #     test_table.specimen$collection_date <- as.double(as.Date(NA))
  #   } 
  #   
  #   check.specimen <- inner_join(sampleDB::CheckTable(database = database, "specimen"), 
  #                                    test_table.specimen, 
  #                                    by = c("study_subject_id", "specimen_type_id", "collection_date"))
  #   
  #   stopifnot("SPECIMEN ALREADY EXISTS IN THE DATABASE. SPECIMENS UNIQUE CONSTRAINT: SUBJECT + STUDY + SPECIMEN TYPE + COLLECTION DATE" = nrow(check.specimen) == 0)
  # }
}

.InternalUpload <- function(csv.upload, database, toggle.is_longitudinal, type){
  #GET THE CURRENT NEWEST STORAGE CONTAINER ID
  if(nrow(sampleDB::CheckTable(database = database, "storage_container")) == 0){
    before_upload.newest_sc_id <- 0
  }else{
    before_upload.newest_sc_id <- tail(sampleDB::CheckTable(database = database, "storage_container"), 1)$id 
  }
  
  for(i in 1:nrow(csv.upload)){
    
    #GET VARIABLES IN UPLOAD
    eval.specimen_type_id <- filter(sampleDB::CheckTable(database = database, "specimen_type"), label == csv.upload[i, ]$"specimen_type")$id
    eval.study_id <- filter(sampleDB::CheckTable(database = database, "study"), short_code == csv.upload[i, ]$"study_short_code")$id
    eval.subject <- csv.upload[i, ]$"study_subject_id"
    if(toggle.is_longitudinal){
      eval.collection_date <- ymd(csv.upload[i, ]$"collection_date")
      eval.collection_date <- as.double(as.Date(paste(year(eval.collection_date), month(eval.collection_date), day(eval.collection_date), sep = "-")))
    }else{
      eval.collection_date <- as.double(as.Date(NA))
    }
    
    #CHECK IF THIS SUBJECT + STUDY COMBO EXISTS
    tmp_table.study_subject <- inner_join(sampleDB::CheckTable(database = database, "study_subject")[, c("subject", "study_id")],
                                          tibble(subject = eval.subject, study_id = eval.study_id),
                                          by = c("subject", "study_id"))
    
    if(nrow(tmp_table.study_subject) > 0){
      #IF THIS SUBJECT + STUDY COMBINATION EXISTS GET ITS STUDY_SUBJECT_TABLE ID
      eval.study_subject_id <- filter(sampleDB::CheckTable(database = database, "study_subject"), subject == eval.subject & study_id == eval.study_id)$id
      
      #CHECK IF THIS SPECIMEN EXISTS (SUBJECT + STUDY + SPECIMEN_TYPE)
      tmp_table.specimen <- inner_join(sampleDB::CheckTable(database = database, "specimen")[,c("study_subject_id", "specimen_type_id", "collection_date")],
                                       tibble(study_subject_id = eval.study_subject_id, specimen_type_id = eval.specimen_type_id, collection_date = eval.collection_date),
                                       by = c("study_subject_id", "specimen_type_id", "collection_date"))
      
      if(nrow(tmp_table.specimen) > 0){
        
        #IF SPECIMEN EXISTS: GET SPECIMEN ID
        
        #IF THE SPECIMEN THAT EXISTS HAS NA FOR THE COLLECTION DATE
        if(is.na(eval.collection_date)){
          eval.specimen_id <- filter(sampleDB::CheckTable(database = database, "specimen"),
                                     study_subject_id == eval.study_subject_id,
                                     specimen_type_id == eval.specimen_type_id,
                                     is.na(eval.collection_date))$id
          
          #IF THE SPECIMEN THAT EXISTS HAS A COLLECTION DATE
        }else{
          eval.specimen_id <- filter(sampleDB::CheckTable(database = database, "specimen"),
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
                                  collection_date = eval.collection_date))
        
        #GET SPECIMEN ID
        eval.specimen_id <- tail(sampleDB::CheckTable(database = database, "specimen"), 1)$id
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
      eval.study_subject_id <- tail(sampleDB::CheckTable(database = database, "study_subject"), 1)$id
      
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
      eval.specimen_id <- tail(sampleDB::CheckTable(database = database, "specimen"), 1)$id
      
      #STORAGE CONTAINER TABLE
      sampleDB::AddToTable(database = database, "storage_container",
                           list(created = lubridate::now("UTC") %>% as.character(),
                                last_updated = lubridate::now("UTC") %>% as.character(),
                                type = type,
                                specimen_id = eval.specimen_id,
                                exhausted = 0))
    }
    after_upload.newest_sc_id <- tail(sampleDB::CheckTable(database = database, "storage_container"), 1)$id
    # message("UPLOAD COMPLETE")
  }
  sc_ids <- list(before_upload.newest_sc_id = before_upload.newest_sc_id, 
                 after_upload.newest_sc_id = after_upload.newest_sc_id)
  return(sc_ids)
}

.UploadMicronixTubes <- function(database, csv, sc_ids){
  for(i in 1:nrow(csv)){
    eval.plate_id <- tail(sampleDB::CheckTable(database = database, "matrix_plate"), 1)$id
    eval.barcode <- csv[i,]$"label" %>% as.character()
    eval.well_position <- csv[i,]$"well_position"
    
    storage_container_ids <- tail(sampleDB::CheckTable(database = database, "storage_container"), 1)$id
    sampleDB::AddToTable(database = database,
                         "matrix_tube",
                         list(id = sc_ids$before_upload.newest_sc_id + i,
                              plate_id = eval.plate_id,
                              barcode = eval.barcode,
                              well_position = eval.well_position))
  }
}

.UploadMicronixPlate <- function(database, container, list.location){
  eval.location_id <- filter(CheckTable(database = database, "location"), location_name == list.location$location, level_I == list.location$level_I, level_II == list.location$level_II)$id
  sampleDB::AddToTable(database = database, 
                       "matrix_plate",
                       list(created = lubridate::now("UTC") %>% as.character(),
                            last_updated = lubridate::now("UTC") %>% as.character(),
                            location_id = eval.location_id,
                            plate_name = container)) 
}

.UploadCryoTubes <- function(database, csv, sc_ids){
  for(i in 1:nrow(csv)){
    eval.box_id <- tail(sampleDB::CheckTable(database = database, "box"), 1)$id
    eval.label <- csv[i,]$"label" %>% as.character()
    eval.box_position <- paste(csv[i,]$"row", csv[i,]$"column", sep = "")
    
    sampleDB::AddToTable(database = database,
                         "tube",
                         list(id = sc_ids$before_upload.newest_sc_id + i,
                              box_id = eval.box_id,
                              label = eval.label,
                              box_position = eval.box_position))
  }
}

.UploadCryoBox <- function(database, container, list.location){
  eval.location_id <- filter(CheckTable(database = database, "location"), location_name == list.location$location, level_I == list.location$level_I, level_II == list.location$level_II)$id
  sampleDB::AddToTable(database = database,
                       "box",
                       list(created = lubridate::now("UTC") %>% as.character(),
                            last_updated = lubridate::now("UTC") %>% as.character(),
                            location_id = eval.location_id,
                            box_name = container)) 
}

.UploadRDT <- function(database, csv, sc_ids){
  for(i in 1:nrow(csv)){
    eval.bag_id <- tail(sampleDB::CheckTable(database = database, "bag"), 1)$id
    eval.label <- csv[i,]$"label" %>% as.character()
    
    sampleDB::AddToTable(database = database,
                         "rdt",
                         list(id = sc_ids$before_upload.newest_sc_id + i,
                              bag_id = eval.bag_id,
                              label = eval.label))
  }
}

.UploadPaper <- function(database, csv, sc_ids){
  for(i in 1:nrow(csv)){
    eval.bag_id <- tail(sampleDB::CheckTable(database = database, "bag"), 1)$id
    eval.label <- csv[i,]$"label" %>% as.character()
    
    sampleDB::AddToTable(database = database,
                         "paper",
                         list(id = sc_ids$before_upload.newest_sc_id + i,
                              bag_id = eval.bag_id,
                              label = eval.label))
  }
}

.UploadBag <- function(database, container, list.location){
  eval.location_id <- filter(CheckTable(database = database, "location"), location_name == list.location$location, level_I == list.location$level_I, level_II == list.location$level_II)$id
  sampleDB::AddToTable(database = database, 
                       "bag",
                       list(created = lubridate::now("UTC") %>% as.character(),
                            last_updated = lubridate::now("UTC") %>% as.character(),
                            location_id = eval.location_id,
                            bag_name = container)) 
}
