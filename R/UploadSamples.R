#' Upload EPPIcenter Wetlab Samples to the SampleDB database
#' 
#' `UploadSamples()` can be used to upload wetlab samples to the sampleDB database.
#' Currently the type of wetlab samples supported is only `micronix`. <!---cryovial`, `rdt` and `paper` sample uploads will appear in the next version.-->
#' 
#' @param sample_type A string specifying the type of samples that are being uploaded Options include: `micronix`, `cryovial`, `rdt` and `paper`
#' @param upload_data A dataframe of SampleDB Upload data.\cr
#' Required `upload_data` columns are:\cr
#' `well_position`: the row and column of the sample in the storage housing
#' `label`: the sample's label or barcode
#' `study_subject_id`: the participant id of for the subject in the cohort (ie study)
#' `study_short_code`: the code of the study
#' `specimen_type`: the sample type
#' `collection_date`: (optional) the date the sample was first collected from the cohort participant
#' 
#' #' **upload data example without collection_date**
#' 
#' | well_position | label | study_subject_id | specimen_type | study_short_code |
#' | ------------- | ----- | ---------------- | ------------- | ---------------- |
#' | A0            | xxx1  | subject_1        | PLASMA        | KAM06            |
#' | A1            | xxx2  | subject_2        | PLASMA        | KAM06            |
#' 
#' **upload data example with collection_date**
#' 
#' | well_position | label | study_subject_id | specimen_type | study_short_code | collection_data |
#' | ------------- | ----- | ---------------- | ------------- | ---------------- | --------------- |
#' | A0            | xxx1  | subject_1        | PLASMA        | KAM06            | 2022-04-11      |
#' | A1            | xxx2  | subject_2        | PLASMA        | KAM06            | 2022-04-11      |
#' 
#' @param container_name A string specifying the name of the container the samples are in. Names must be unique within each sample type.
#' @param container_barcode A string specifying the barcode for the container the samples are in. Container barcodes are optional. Barcodes must be unique within each sample type.
#' @param freezer_address A list specifying the freezer address used to store samples. \cr 
#' Required items in the freezer_address list are `location_name`, `level_I` and `level_II`.
#' If the freezer_address type is `minus eighty` then `level_I` and `level_II` items specify the rack and position, respecively. 
#' If the freezer_address type is `minus twenty` then `level_I` and `level_II` items specify the shelf and basket, respecively. 
#' @examples
#' \dontrun{
#' UploadSamples(upload_data = dataframe(), container_name = "dummy_name", location = freezer_address = list(location_name = "Freezer A", level_I = "dummy.levelI", level_II = "dummy.levelII"))
#' }
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

# there are two problems
# 1. is that internal upload (specimen, storage_container, study_subject) occurs before container (plates, boxes, bags) and samples (cryo tubes, rdt, micronix tubes)
# this means that in order for the ids of samples to match the upload ids no errors can take place during either of these uploads. they work in tandem so must fail together or be successful together
# this leads to issue 2
# 2. connections to the database are (to the best of my knowledge) opening and closing all throughout the upload. This is bad, it means that if two usrs try to upload something at the same time
# the can get their connections to the db (via an upload) can get crossed and ids storage container ids could be useless
#
# There are three ways to overcome these problems:
# A. upload internal data and external data at the same time (is this possible? idk. it is how thing *should* be)
# B. open a connection to the database only at the beginning of the upload and close the connection only at the end on the upload. many sampleDB funs open and close the db so the use of those funs here is
# very problematic (it is how things i think *need* to be in order for 2+ users to work with the db at the same time)
# C. Adds to the db tables occur not in a loop but all at once. (this is how things *should* be)

UploadSamples <- function(sample_type, upload_data, container_name, container_barcode = NULL, freezer_address){
  
  # locate the database and connect to it
  database <- Sys.getenv("SDB_PATH")
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
  
  # save a copy of the upload data as a csv
  .SaveUploadCSV(upload_data, container_name)
  
  # if there is no collection date, for standardization create a collection date column of NAs
  if(!"collection_date" %in% names(upload_data)){
    upload_data$collection_date <- NA
  }
  
  # perform upload checks
  .UploadChecks(sample_type = sample_type, input = input, database = database, 
                freezer_address = freezer_address, container_name = container_name, 
                container_barcode = container_barcode, upload_data = upload_data)
  
  # upload data
  .UploadSamples(upload_data = upload_data, database = database, sample_type = sample_type, 
                 conn = conn, container_name = container_name, freezer_address = freezer_address,
                 container_barcode = container_barcode)

  message(paste("UPLOADING CONTAINER", container_name, "CONTAINING", nrow(upload_data), "SAMPLES"))
  
  #close connection
  tryCatch(
    RSQLite::dbDisconnect(conn),
    warning=function(w){})

}

.UploadChecks <- function(sample_type, input, database, freezer_address, container_name, container_barcode, upload_data){
  
  message("Performing upload checks...")
  
  # check storage type
  # stopifnot("Sample type is not valid." = sample_type %in% c("cryo", "micronix", "paper", "rdt"))
  stopifnot("Error: Storage type is not valid." = sample_type %in% c("micronix")) #for now, only micronix samples are permitted
  
  # check upload data colnames
  valid_colnames <- c("well_position", "label", "study_subject_id", "specimen_type", "study_short_code", "collection_date")
  stopifnot("Error: Malformed colnames. Valid colnames are:" = all(valid_colnames %in% names(upload_data)))

  # check freezer address exists
  tmp.location.tbl <- inner_join(tibble(location_name = freezer_address$location_name, level_I = freezer_address$level_I, level_II = freezer_address$level_II), 
                                 sampleDB::CheckTable(database = database, "location"), 
                                 by = c("location_name", "level_I", "level_II"))
  stopifnot("Error: Freezer address does not exist" = nrow(tmp.location.tbl) != 0)
  
  # check plate name
  stopifnot("Error: Container name is not unique" = !(container_name %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_name,
                                                            sampleDB::CheckTable(database = database, "box")$box_name,
                                                            sampleDB::CheckTable(database = database, "bag")$bag_name)))
  
  # check plate barcode
  if(!is.null(container_barcode)){
    if(container_barcode != ""){
      stopifnot("Error: Container barcode is not unique" = !(container_barcode %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_barcode)))
      # stopifnot("CONTAINER BARCODE IS NOT UNIQUE" = !(container_barcode %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_barcode,
      #                                                                    sampleDB::CheckTable(database = database, "box")$box_barcode,
      #                                                                    sampleDB::CheckTable(database = database, "bag")$bag_barcode)))  
    }
  }
  
  # check that specimen type exists
  stopifnot("Error: Specimen type(s) does not exist" = all(upload_data$"specimen_type" %in% CheckTable(database = database, table = "specimen_type")$label))
  
  # check collection date format is correct
  #create a vector from the collection date column and remove all NAs in the vector the perform this check
  collection_dates <- upload_data$"collection_date"[!is.na(upload_data$"collection_date")]
  if(length(collection_dates) != 0){
    stopifnot("Error: Collection date is not in Year, Month, Day format" = all(!is.na(parse_date_time(collection_dates, orders = "ymd")) == TRUE))
  }
  
  # check that micronix containerbarcodes are unique
  stopifnot("Error: Barcode Unique Constraint" = all(!upload_data$label %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode,
                                                                               sampleDB::CheckTable(database = database, "tube")$label,
                                                                               sampleDB::CheckTable(database = database, "rdt")$label,
                                                                               sampleDB::CheckTable(database = database, "paper")$label)))
  
  NonUniqueLabels <- upload_data$label[which(upload_data$label %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode,
                                                                    sampleDB::CheckTable(database = database, "tube")$label,
                                                                    sampleDB::CheckTable(database = database, "rdt")$label,
                                                                    sampleDB::CheckTable(database = database, "paper")$label))]
  
  if(length(NonUniqueLabels) != 0){print(NonUniqueLabels)}
  
  # check that micronix container barcodes are not duplicated
  dups <- upload_data$label[duplicated(upload_data$label)]
  stopifnot("Barcodes in upload data are duplicated" = length(dups) == 0)
  if(length(dups) != 0){print(dups)}
  
  #CHECK IF SPECIMEN ALREADY EXISTS
  # check.study_subject <- inner_join(sampleDB::CheckTable(database = database, "study_subject"),
  #                                   tibble(subject = upload_data$"study_subject_id",
  #                                          study_id = filter(sampleDB::CheckTable(database = database, "study"), short_code %in% upload_data$study_short_code)$id,
  #                                          specimen_type_id = filter(sampleDB::CheckTable(database = database, "specimen_type"), label %in% upload_data$specimen_type)$id),
  #                                   by = c("subject", "study_id"))
  # 
  # if(nrow(check.study_subject) != 0){
  #   
  #   if("collection_date" %in% names(upload_data)){
  #     test_table.specimen <- check.study_subject %>% rename("study_subject_id" = "id")
  #     test_table.specimen$collection_date <- as.double(as.Date(upload_data$collection_date))
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

.UploadSamples <- function(upload_data, database, sample_type, conn, container_name, container_barcode, freezer_address){
  
  for(i in 1:nrow(upload_data)){
    
    #1. get upload item's metadata
    eval.specimen_type <- upload_data[i, ]$"specimen_type" %>% as.character()
    eval.study_code <- upload_data[i, ]$"study_short_code" %>% as.character()
    eval.subject <- upload_data[i, ]$"study_subject_id" %>% as.character()
    eval.barcode <- upload_data[i,]$"label" %>% as.character()
    eval.well_position <- upload_data[i,]$"well_position"
    if(is.na(upload_data[i, ]$"collection_date")){
      eval.collection_date <- as.double(as.Date(NA))
    }else{
      eval.collection_date <- ymd(upload_data[i, ]$"collection_date")
      eval.collection_date <- as.double(as.Date(paste(year(eval.collection_date), month(eval.collection_date), day(eval.collection_date), sep = "-")))
    }
    
    #get a database id for a upload item's specimen_type and study
    eval.specimen_type_id <- filter(sampleDB::CheckTable(database = database, "specimen_type"), label == eval.specimen_type)$id
    eval.study_id <- filter(sampleDB::CheckTable(database = database, "study"), short_code == eval.study_code)$id
    
    #2a. check if this upload item's participant (subject+study combination) exists in the database
    tmp_table.study_subject <- inner_join(sampleDB::CheckTable(database = database, "study_subject")[, c("subject", "study_id")],
                                          tibble(subject = eval.subject, study_id = eval.study_id),
                                          by = c("subject", "study_id"))
    
    #if this upload item's participant exists in the database, then get the necessary "study_subject" id
    if(nrow(tmp_table.study_subject) > 0){
      eval.study_subject_id <- filter(sampleDB::CheckTable(database = database, "study_subject"), subject == eval.subject, study_id == eval.study_id)$id
      
      #3a. check if this sample exists (subject+study+specimen_type) in the database
      tmp_table.specimen <- inner_join(sampleDB::CheckTable(database = database, "specimen")[,c("study_subject_id", "specimen_type_id", "collection_date")],
                                       tibble(study_subject_id = eval.study_subject_id, specimen_type_id = eval.specimen_type_id, collection_date = eval.collection_date),
                                       by = c("study_subject_id", "specimen_type_id", "collection_date"))
      
      #if this upload item's sample exists in the database, then get the necessary "specimen" id
      if(nrow(tmp_table.specimen) > 0){
        
        #how specimen id is retrieved depends on whether the collection date is NA or !is NA
        if(is.na(eval.collection_date)){
          eval.specimen_id <- filter(sampleDB::CheckTable(database = database, "specimen"),
                                     study_subject_id == eval.study_subject_id,
                                     specimen_type_id == eval.specimen_type_id,
                                     is.na(eval.collection_date))$id
          
        }else{
          eval.specimen_id <- filter(sampleDB::CheckTable(database = database, "specimen"),
                                     study_subject_id == eval.study_subject_id,
                                     specimen_type_id == eval.specimen_type_id,
                                     collection_date == eval.collection_date)$id
        }
      }else{
        
        #3b. if this participant (study_subject) exists in the database but sample (specimen) does not, then create a new "specimen"
        sampleDB::AddToTable(database = database, "specimen",
                             list(created = lubridate::now() %>% as.character(),
                                  last_updated = lubridate::now() %>% as.character(),
                                  study_subject_id = eval.study_subject_id,
                                  specimen_type_id = eval.specimen_type_id,
                                  collection_date = eval.collection_date),
                             conn = conn) %>% suppressWarnings()
        
        #retrieve newly made specimen id
        eval.specimen_id <- tail(sampleDB::CheckTable(database = database, "specimen"), 1)$id
      }
    }else{
      #2b. if this upload item's participant (combination of subject+study) does not exist in the database then create a new study_subject entry in the database
      sampleDB::AddToTable(database = database, "study_subject",
                           list(created = lubridate::now() %>% as.character(),
                                last_updated = lubridate::now() %>% as.character(),
                                subject = eval.subject,
                                study_id = eval.study_id),
                           conn = conn) %>% suppressWarnings()
      
      # get the newly created study_subject id
      eval.study_subject_id <- tail(sampleDB::CheckTable(database = database, "study_subject"), 1)$id
      
      #3b. create a new sample (study+subject+specimen_type) (ie specimen) in the database
      sampleDB::AddToTable(database = database, "specimen",
                           list(created = lubridate::now() %>% as.character(),
                                last_updated = lubridate::now() %>% as.character(),
                                study_subject_id = eval.study_subject_id,
                                specimen_type_id = eval.specimen_type_id,
                                collection_date = eval.collection_date),
                           conn = conn) %>% suppressWarnings()
      
      # get the newly created specimen id
      eval.specimen_id <- tail(sampleDB::CheckTable(database = database, "specimen"), 1)$id
    }
      
    #4. create the new item's storage container to the database using the item's sample id
    sampleDB::AddToTable(database = database, "storage_container",
                         list(created = lubridate::now() %>% as.character(),
                              last_updated = lubridate::now() %>% as.character(),
                              type = sample_type,
                              specimen_id = eval.specimen_id,
                              exhausted = 0),
                         conn = conn) %>% suppressWarnings()
    
    #5. get just item's storage container id and use it as the primary key in the matrix tube table
    eval.id <- tail(sampleDB::CheckTable(database = database, "storage_container"), 1)$id
    
    # 6. Create new sample housing (if it does not alread exist) and upload samples into housing
    if(sample_type == "micronix"){
      # create a new housing (if it does not already exist)
      if(!container_name %in% CheckTable(database = database, "matrix_plate")$plate_name){
        eval.plate_id <- .UploadMicronixPlate(database = database, conn = conn, container_name = container_name, container_barcode = container_barcode, freezer_address = freezer_address) 
      }else{
        eval.plate_id <- filter(sampleDB::CheckTable("matrix_plate"), plate_name == container_name)$id
      }
      
      # 7. upload micronix sample
      sampleDB::AddToTable(database = database,
                           table_name = "matrix_tube",
                           info_list = list(id = eval.id,
                                            plate_id = eval.plate_id, 
                                            well_position = eval.well_position, 
                                            barcode = eval.barcode),
                           conn = conn) %>% suppressWarnings()
    }
    # else if(sample_type == "cryo"){
    #   # create a new container (if it does not already exist)
    #   if(!container_name %in% CheckTable(database = database, "box")$box_name){
    #     eval.box_id <- .UploadCryoBox(database = database, container_name = container_name, freezer_address = freezer_address, conn = conn)
    #   }
    #   
    #   # upload cryovial sample
    #   .UploadCryoTubes(database = database, upload_data = upload_data, i = i, conn = conn, eval.box_id = eval.box_id)
    # }
    # else if(sample_type == "rdt"){
    #   # create a new container (if it does not already exist)
    #   if(!container_name %in% CheckTable(database = database, "bag")$bag_name){
    #     eval.bag_id <- .UploadBag(database = database, container_name = container_name, freezer_address = freezer_address, conn = conn)
    #   }
    #   
    #   # upload rdt sample
    #   .UploadRDT(database = database, upload_data = upload_data, i = i, conn = conn, eval.bag_id = eval.bag_id)
    # }
    # else{
    #   # create a new container (if it does not already exist)
    #   if(!container_name %in% CheckTable(database = database, "bag")$bag_name){
    #     eval.bag_id <- .UploadBag(database = database, container_name = container_name, freezer_address = freezer_address, conn = conn)
    #   }
    #   
    #   # upload paper sample
    #   .UploadPaper(database = database, upload_data = upload_data, i = i, conn = conn, eval.bag_id = eval.bag_id)
    # }
  }
}

.UploadMicronixPlate <- function(database, container_name, container_barcode, freezer_address, conn){
  eval.location_id <- filter(CheckTable(database = database, "location"), location_name == freezer_address$location, level_I == freezer_address$level_I, level_II == freezer_address$level_II)$id
  if(is.null(container_barcode)){
    container_barcode <- NA
  }
  else if(container_barcode == ""){
    container_barcode <- NA
  }
  else{
    container_barcode <- container_barcode
  }
  # print(container_barcode)
  sampleDB::AddToTable(database = database,
                       "matrix_plate",
                       list(created = lubridate::now() %>% as.character(),
                            last_updated = lubridate::now() %>% as.character(),
                            location_id = eval.location_id,
                            plate_name = container_name,
                            plate_barcode = container_barcode),
                       conn = conn) %>% suppressWarnings()
  eval.plate_id <- tail(sampleDB::CheckTable(database = database, "matrix_plate"), 1)$id
  
  return(eval.plate_id)
  
}

# .UploadMicronixTubes <- function(database, conn, micronix_tube_info){
# 
#   sampleDB::AddToTable(database = database,
#                        table_name = "matrix_tube",
#                        info_list = micronix_tube_info,
#                        conn = conn) %>% suppressWarnings()
#   # sampleDB::AddToTable(database = database,
#   #                      "matrix_tube",
#   #                      list(plate_id = eval.plate_id,
#   #                           barcode = eval.barcode,
#   #                           well_position = eval.well_position),
#   #                      conn = conn) %>% suppressWarnings()
# }

.UploadCryoBox <- function(database, container_name, freezer_address, conn){
  eval.location_id <- filter(CheckTable(database = database, "location"), location_name == freezer_address$location, level_I == freezer_address$level_I, level_II == freezer_address$level_II)$id
  sampleDB::AddToTable(database = database,
                       "box",
                       list(created = lubridate::now() %>% as.character(),
                            last_updated = lubridate::now() %>% as.character(),
                            location_id = eval.location_id,
                            box_name = container_name),
                       conn = conn) %>% suppressWarnings()
  eval.box_id <- tail(sampleDB::CheckTable(database = database, "box"), 1)$id
  
  return(eval.box_id)
}

.UploadCryoTubes <- function(database, upload_data, i, conn, eval.box_id){

  eval.label <- csv[i,]$"label" %>% as.character()
  eval.box_position <- paste(csv[i,]$"row", csv[i,]$"column", sep = "")
  
  sampleDB::AddToTable(database = database,
                       "tube",
                       list(box_id = eval.box_id,
                            label = eval.label,
                            box_position = eval.box_position),
                            conn = conn) %>% suppressWarnings()
}

.UploadBag <- function(database, container_name, freezer_address, conn){
  eval.location_id <- filter(CheckTable(database = database, "location"), location_name == freezer_address$location, level_I == freezer_address$level_I, level_II == freezer_address$level_II)$id
  sampleDB::AddToTable(database = database, 
                       "bag",
                       list(created = lubridate::now() %>% as.character(),
                            last_updated = lubridate::now() %>% as.character(),
                            location_id = eval.location_id,
                            bag_name = container_name),
                       conn = conn) %>% suppressWarnings() 
  
  eval.bag_id <- tail(sampleDB::CheckTable(database = database, "bag"), 1)$id
  
  return(eval.bag_id)
}

.UploadRDT <- function(database, upload_data, i, conn, eval.bag_id){
  
  eval.label <- csv[i,]$"label" %>% as.character()
  sampleDB::AddToTable(database = database,
                       "rdt",
                       list(bag_id = eval.bag_id,
                            label = eval.label),
                       conn = conn) %>% suppressWarnings()
}

.UploadPaper <- function(database, upload_data, i, conn, eval.bag_id){

  eval.label <- csv[i,]$"label" %>% as.character()
  sampleDB::AddToTable(database = database,
                       "paper",
                       list(bag_id = eval.bag_id,
                            label = eval.label),
                       conn = conn) %>% suppressWarnings()
}

.SaveUploadCSV <- function(upload_data, container_name){
  path <- "/var/lib/sampleDB/upload_files/"
  if(dir.exists(path)){
    write.csv(upload_data, 
              paste0(path, 
                     gsub(" ", "-", date()),
                     "_", container_name, "_", 
                     "UPLOAD.csv"),
              row.names = FALSE)
  }
}

# .ReformatUploadMicronixCSV <- function(upload_data){
#   names.base <- c("study_subject_id", "specimen_type", "study_short_code")
#   names.traxer.nodate <- c(names.base, "Position", "Tube ID")
#   names.traxer.date <- c(names.traxer.nodate, "collection_date")
#   names.visionmate.nodate <- c(names.base, "LocationRow", "LocationColumn", "TubeCode")
#   names.visionmate.date <- c(names.visionmate.nodate, "collection_date")
#   
#   #REFORMAT CSV -- IF LOCATIONROW IS A COLUMN THEN THE DATA CAME OFF VISIONMATE
#   if("LocationRow" %in% names(upload_data)){
#     stopifnot("UPLOADCSV COLNAMES ARE MALFORMED" = (all(names.visionmate.nodate %in% names(upload_data)) || all(names.visionmate.date %in% names(upload_data))))    
#     
#     csv.reformatted <- upload_data %>%
#       mutate(label = na_if(TubeCode, ""),
#              well_position = paste0(LocationRow, LocationColumn)) %>%
#       tidyr::drop_na()
#     
#   }else{
#   
#     stopifnot("UPLOADCSV COLNAMES ARE MALFORMED" = (all(names.traxer.nodate %in% names(upload_data)) || all(names.traxer.date %in% names(upload_data))))
#     
#     csv.reformatted <- upload_data %>%
#       mutate(label = na_if(`Tube ID`, ""),
#              well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
#       tidyr::drop_na()
# 
#   }
#   
#   return(csv.reformatted)
# }