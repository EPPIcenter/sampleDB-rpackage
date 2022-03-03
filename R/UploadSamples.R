#' Upload EPPIcenter Wetlab Samples to the SampleDB database
#' 
#' @param sample_type A string specifying the type of samples that are being moved. Options include: `micronix`, `cryovial`, `rdt` and `paper`
#' @param upload_file A path string for the SampleDB Upload CSV file.
#' 
#' **Upload Micronix CSV structure**
#' 
#' | row | column | label | study_subject_id | specimen_type | study_short_code |
#' | --- |------- | ----- | ---------------- | ------------- | ---------------- |
#' | A   | 0      | xxx1  | subject_1        | PLASMA        | KAM06            |
#' | A   | 1      | xxx2  | subject_2        | PLASMA        | KAM06            |
#' 
#' **Upload Cryovial CSV structure**
#' 
#' | row | column | label | study_subject_id | specimen_type | study_short_code |
#' | --- |------- | ----- | ---------------- | ------------- | ---------------- |
#' | A   | 0      | xxx1  | subject_1        | PLASMA        | KAM06            |
#' | A   | 1      | xxx2  | subject_2        | PLASMA        | KAM06            |
#' 
#' @param container_name A string specifying the name of the container the samples are in. Names must be unique within each sample type.
#' @param freezer A list specifying the freezer used to store samples. \cr 
#' Required items in the freezer list are `location_name`, `level_I` and `level_II`.
#' If the freezer type is `minus eighty` then `level_I` and `level_II` items specify the rack and position, respecively. 
#' If the freezer type is `minus twenty` then `level_I` and `level_II` items specify the shelf and basket, respecively. 
#' @examples
#' \dontrun{
#' UploadSamples(upload_file = "/path/to/UploadCSV.csv", container_name = "dummy_name", location = freezer = list(location_name = "Freezer A", level_I = "dummy.levelI", level_II = "dummy.levelII"))
#' }
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

UploadSamples <- function(sample_type, upload_file, container_name, freezer){
  
  database <- "/databases/sampledb/v0.0.2/sampledb_database.sqlite"
  message(paste("Connecting to database at", database))

  # Read in usr supplied upload csv
  upload_file <- read.csv(upload_file, check.names = F) %>% tidyr::drop_na()
  
  # If type equals micronix reformat the upload_file
  if(sample_type == "micronix"){
    upload_file <- .ReformatUploadMicronixCSV(upload_file)
    }
  
  # Check for a date col
  toggle.is_longitudinal <- FALSE
  if("collection_date" %in% names(upload_file)){
    toggle.is_longitudinal <- TRUE
  }
  
  # Perform sample upload checks
  .UploadChecks(sample_type, input, database, freezer, container_name, upload_file = upload_file)
  
  # Parse through each row in the upload csv
  sc_ids <- .InternalUpload(upload_file = upload_file, database = database, toggle.is_longitudinal = toggle.is_longitudinal, sample_type = sample_type)
  
  # Upload samples
  if(sample_type == "micronix"){
    .UploadMicronixPlate(database, container_name, freezer)    
    .UploadMicronixTubes(database, upload_file, sc_ids)
    message(paste("UPLOADING PLATE", container_name, "CONTAINING", nrow(upload_file), "MICRONIX SAMPLES"))
  }
  else if(sample_type == "cryo"){
    stopifnot("Malformed upload_file column names" = setequal(names(upload_file), c("label", "row","column","study_short_code", "study_subject_id", "specimen_type")) || setequal(names(upload_file), c("label", "row","column", "study_short_code", "study_subject_id", "specimen_type", "collection_date")))
    .UploadCryoBox(database, container_name, freezer)
    .UploadCryoTubes(database, upload_file, sc_ids)
    message(paste("UPLOADING BOX", container_name, "CONTAINING", nrow(upload_file), "TUBES"))
  }
  else if(sample_type == "rdt"){
    stopifnot("Malformed upload_file column names" = setequal(names(upload_file), c("label", "study_short_code", "study_subject_id", "specimen_type")) || setequal(names(upload_file), c("label", "study_short_code", "study_subject_id", "specimen_type", "collection_date")))
    .UploadBag(database, container_name, freezer) 
    .UploadRDT(database, upload_file, sc_ids)
    message(paste("UPLOADING BAG", container_name, "CONTAINING", nrow(upload_file), "RDT SAMPLES"))
  }
  else if(sample_type == "paper"){
    stopifnot("Malformed upload_file column names" = setequal(names(upload_file), c("label", "study_short_code", "study_subject_id", "specimen_type")) || setequal(names(upload_file), c("label", "study_short_code", "study_subject_id", "specimen_type", "collection_date")))
    .UploadBag(database, container_name, freezer) 
    .UploadPaper(database, upload_file, sc_ids)
    message(paste("UPLOADING BAG", container_name, "CONTAINING", nrow(upload_file), "PAPER SAMPLES"))
  }
  else{
    message("User Upload Type was not a valid option. Valid options are: micronix, cryo, rdt and paper")
  }
  
  # message("UPLOAD COMPLETE")
}

.ReformatUploadMicronixCSV <- function(upload_file){
  names.base <- c("study_subject_id", "specimen_type", "study_short_code")
  names.traxer.nodate <- c(names.base, "Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date")
  names.traxer.date <- c(names.traxer.nodate, "collection_date")
  names.visionmate.nodate <- c(names.base, "LocationRow", "LocationColumn", "TubeCode")
  names.visionmate.date <- c(names.visionmate.nodate, "collection_date")
  
  stopifnot("UPLOADCSV COLNAMES ARE MALFORMED" = (setequal(names.traxer.nodate, names(upload_file)) || setequal(names.traxer.date, names(upload_file)) || setequal(names.visionmate.nodate, names(upload_file)) || setequal(names.visionmate.date, names(upload_file))))
  
  #REFORMAT CSV -- IF LOCATIONROW IS A COLUMN THEN THE DATA CAME OFF VISIONMATE
  if(!("LocationRow" %in% names(upload_file))){
    csv.reformatted <- upload_file %>%
      mutate(label = `Tube ID`,
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      select(-c(Position:Date))
    message("UploadCSV from Traxer detected...")
  }else{
    csv.reformatted <- upload_file %>%
      mutate(label = TubeCode,
             well_position = paste0(LocationRow, LocationColumn)) %>%
      select(-c(LocationRow, LocationColumn, TubeCode))
    message("UploadCSV from VisionMate detected...")
  }
  
  return(csv.reformatted)
}

.UploadChecks <- function(sample_type, input, database, freezer, container_name, upload_file){
  message("PERFORMING CHECKS")
  
  stopifnot("SAMPLE TYPE IS NOT VALID" = sample_type %in% c("cryo", "micronix", "paper", "rdt"))
  
  # CHECK FREEZER EXISTS
  tmp.location.tbl <- inner_join(tibble(location_name = freezer$location_name, level_I = freezer$level_I, level_II = freezer$level_II), 
                                 sampleDB::CheckTable(database = database, "location"), 
                                 by = c("location_name", "level_I", "level_II"))
  stopifnot("FREEZER NAME + LEVEL I + LEVEL II DOES NOT EXITS" = nrow(tmp.location.tbl) != 0)
  
  # CHECK PLATE NAME IS UNIQUE
  stopifnot("CONTAINER NAME IS NOT UNIQUE" = !(container_name %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_name,
                                                            sampleDB::CheckTable(database = database, "box")$box_name,
                                                            sampleDB::CheckTable(database = database, "bag")$bag_name)))
  
  # CHECK THAT SPECIMEN TYPE(S) EXISTS
  stopifnot("ERROR: SPECIMEN TYPE(S) NOT FOUND" = all(upload_file$"specimen_type" %in% CheckTable(database = database, table = "specimen_type")$label))
  
  # CHECK DATE (IF PRESENT) IS IN CORRECT FORMAT
  if("collection_date" %in% names(upload_file)){
    stopifnot("COLLECTION DATE MUST BE YMD" = all(!is.na(parse_date_time(upload_file$"collection_date", orders = "ymd")) == TRUE))
  }
  
  # CHECK THAT NO BARCODE ALREADY EXISTS
  stopifnot("Error: Barcode Unique Constraint" = all(!upload_file$label %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode,
                                                                              sampleDB::CheckTable(database = database, "tube")$label,
                                                                              sampleDB::CheckTable(database = database, "rdt")$label,
                                                                              sampleDB::CheckTable(database = database, "paper")$label)))
  
  NonUniqueLabels <- upload_file$label[which(upload_file$label %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode,
                                                                    sampleDB::CheckTable(database = database, "tube")$label,
                                                                    sampleDB::CheckTable(database = database, "rdt")$label,
                                                                    sampleDB::CheckTable(database = database, "paper")$label))]
  
  if(length(NonUniqueLabels) != 0){print(NonUniqueLabels)}

  #CHECK IF SPECIMEN ALREADY EXISTS
  # check.study_subject <- inner_join(sampleDB::CheckTable(database = database, "study_subject"),
  #                                   tibble(subject = upload_file$"study_subject_id",
  #                                          study_id = filter(sampleDB::CheckTable(database = database, "study"), short_code %in% upload_file$study_short_code)$id,
  #                                          specimen_type_id = filter(sampleDB::CheckTable(database = database, "specimen_type"), label %in% upload_file$specimen_type)$id),
  #                                   by = c("subject", "study_id"))
  # 
  # if(nrow(check.study_subject) != 0){
  #   
  #   if("collection_date" %in% names(upload_file)){
  #     test_table.specimen <- check.study_subject %>% rename("study_subject_id" = "id")
  #     test_table.specimen$collection_date <- as.double(as.Date(upload_file$collection_date))
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

.InternalUpload <- function(upload_file, database, toggle.is_longitudinal, sample_type){
  #GET THE CURRENT NEWEST STORAGE CONTAINER ID
  if(nrow(sampleDB::CheckTable(database = database, "storage_container")) == 0){
    before_upload.newest_sc_id <- 0
  }else{
    before_upload.newest_sc_id <- tail(sampleDB::CheckTable(database = database, "storage_container"), 1)$id 
  }
  
  for(i in 1:nrow(upload_file)){
    
    #GET VARIABLES IN UPLOAD
    eval.specimen_type_id <- filter(sampleDB::CheckTable(database = database, "specimen_type"), label == upload_file[i, ]$"specimen_type")$id
    eval.study_id <- filter(sampleDB::CheckTable(database = database, "study"), short_code == upload_file[i, ]$"study_short_code")$id
    eval.subject <- upload_file[i, ]$"study_subject_id"
    if(toggle.is_longitudinal){
      eval.collection_date <- ymd(upload_file[i, ]$"collection_date")
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
                                type = sample_type,
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
                                type = sample_type,
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

.UploadMicronixPlate <- function(database, container_name, freezer){
  eval.location_id <- filter(CheckTable(database = database, "location"), location_name == freezer$location, level_I == freezer$level_I, level_II == freezer$level_II)$id
  sampleDB::AddToTable(database = database, 
                       "matrix_plate",
                       list(created = lubridate::now("UTC") %>% as.character(),
                            last_updated = lubridate::now("UTC") %>% as.character(),
                            location_id = eval.location_id,
                            plate_name = container_name)) 
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

.UploadCryoBox <- function(database, container_name, freezer){
  eval.location_id <- filter(CheckTable(database = database, "location"), location_name == freezer$location, level_I == freezer$level_I, level_II == freezer$level_II)$id
  sampleDB::AddToTable(database = database,
                       "box",
                       list(created = lubridate::now("UTC") %>% as.character(),
                            last_updated = lubridate::now("UTC") %>% as.character(),
                            location_id = eval.location_id,
                            box_name = container_name)) 
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

.UploadBag <- function(database, container_name, freezer){
  eval.location_id <- filter(CheckTable(database = database, "location"), location_name == freezer$location, level_I == freezer$level_I, level_II == freezer$level_II)$id
  sampleDB::AddToTable(database = database, 
                       "bag",
                       list(created = lubridate::now("UTC") %>% as.character(),
                            last_updated = lubridate::now("UTC") %>% as.character(),
                            location_id = eval.location_id,
                            bag_name = container_name)) 
}
