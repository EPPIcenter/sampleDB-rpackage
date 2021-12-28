#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @export

#DO WE WANT USERS TO BE ABLE TO CHANGE THE STUDY CODE ASSO W TUBES?
#DO WE WANT USERS TO BE ABLE TO DELETE TUBES FROM THE DATABASE?

MoveTubes <- function(barcode_file, new_plate_id, location){


  #OBTAIN TABLES AS THEY ARE IN THE DATABASE RIGHT NOW (SNAPSHOT)
  table.location <- sampleDB::CheckTable("location")
  table.study <- sampleDB::CheckTable("study")
  table.specimen_type <- sampleDB::CheckTable("specimen_type")

  #UNTIL READING THE COLNAMES ASSUME DATE IS NOT LONGITUDINAL
  toggle.is_longitudinal <- FALSE

  #READIN CSV FROM USER WITH VISIONMATE/TRAXER BARCODES,
  csv <- read_csv(barcode_file)

  if(!("LocationRow" %in% names(csv))){
    csv <- csv %>%
      drop_na() %>%
      mutate(barcode = `Tube ID`,
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      select(-c(Position:Date))
    if("dates" %in% names(csv)){
      dates <- drop_na(read_csv(barcode_file))$dates
      toggle.is_longitudinal <- TRUE
    }
  }else{

    csv <- csv %>%
      drop_na() %>%
      mutate(barcode = TubeCode,
             well_position = paste0(LocationRow, LocationColumn)) %>%
      select(-c(LocationRow, LocationColumn, TubeCode))

    if("dates" %in% names(csv)){
      dates <- drop_na(read_csv(barcode_file))$dates
      toggle.is_longitudinal <- TRUE
    }
  }

  #FIND PLATE_ID ASSO W EXISTING BARCODES
  for (i in  1:nrow(csv)){
    filter(CheckTable("matrix_tube"), barcode == csv[i,]$"barcode")$id %>% print()

    #modify matrix_tube table to swap out new_plate_id and well position with usr input new_plate and well poisiton
    sampleDB::ModifyTable("matrix_tube",
                          info_list = list(plate_id = new_plate_id,
                                           barcode = csv[i,]$"barcode",
                                           well_position = csv[i,]$"well_position"),
                          id = filter(CheckTable("matrix_tube"), barcode == csv[i,]$"barcode")$id)
  }


  # #ADD PLATE_ID AND FREEZER LOCATION TO _*_MATRIX PLATE_*_ TABLE
  # sampleDB::AddToTable("matrix_plate",
  #                      list(created = lubridate::now("UTC") %>% as.character(),
  #                           last_updated = lubridate::now("UTC") %>% as.character(),
  #                           uid = plate_id,
  #                           hidden = 0,
  #                           location_id = filter(table.location, description == location)$id))
  #
  # #ADD PLATE_ID, BARCODE AND WELL POSITION TO _*_MATRIX TUBE_*_ TABLE
  # for(i in 1:nrow(csv)){
  #   sampleDB::AddToTable("matrix_tube",
  #                        list(plate_id = tail(sampleDB::CheckTable("matrix_plate"), 1)$id,
  #                             barcode = csv[i,]$"barcode" %>% as.character(),
  #                             well_position = csv[i,]$"well_position"))
  # }
  #
  # message <- NULL
  # #IF THE INDIVIDUAL_ID DOES NOT EXIST ADD IT TO THE _*_STUDY_SUBJECT_*_ TABLE
  # for(i in 1:nrow(csv)){
  #
  #   #CHECKING TO SEE IF INDIE ID EXISTS
  #   if(csv[i, "individual_id"] %in% CheckTable("study_subject")$uid){
  #
  #     #IF INDIE ID EXISTS FETCH STUDY_SUBJECT_TABLE_ID ASSO W IT
  #     study_subject_table_id <- filter(CheckTable("study_subject"), uid == csv[i, ]$"individual_id")$id
  #
  #
  #     #ADD TO NEW SPECIMEN _*_SPECIMEN_*_ TABLE -- IF THE INDIE ID ALREADY EXISTS THEN IT MUST BE PART OF A LONGITUDINAL STUDY
  #     if(toggle.is_longitudinal){
  #       sampleDB::AddToTable("specimen",
  #                            list(created = lubridate::now("UTC") %>% as.character(),
  #                                 last_updated = lubridate::now("UTC") %>% as.character(),
  #                                 study_subject_id = study_subject_table_id,
  #                                 specimen_type_id = filter(table.specimen_type, label == csv[i, ]$"specimen_type")$id,
  #                                 collection_date = as.character(dates[i]))) #dates need to be in y:m:d format
  #
  #       #ADD SPECIMEN_ID TO  _*_STORAGE_CONTAINER_*_ TABLE IF STUDY_SUBJECT ENTRY EXISTS
  #       sampleDB::AddToTable("storage_container",
  #                            list(created = lubridate::now("UTC") %>% as.character(),
  #                                 last_updated = lubridate::now("UTC") %>% as.character(),
  #                                 type = "NA",
  #                                 specimen_id = filter(CheckTable("specimen"), study_subject_id == study_subject_table_id, specimen_type_id == filter(table.specimen_type, label == csv[i, ]$"specimen_type")$id)$id,
  #                                 comments = "NA",
  #                                 exhausted = 0))
  #     }else{
  #       message <- "DATA IS NOT LONGITUDINAL AND IT SHOULD BE"
  #     }
  #
  #     #INDIE ID IS NEW
  #   }else{
  #
  #     #CREATE A NEW STUDY_STUBJECT_TABLE ENTRY
  #     sampleDB::AddToTable("study_subject",
  #                          list(created = lubridate::now("UTC") %>% as.character(),
  #                               last_updated = lubridate::now("UTC") %>% as.character(),
  #                               uid = csv[i,]$individual_id,
  #                               study_id = filter(CheckTable("study"), short_code == study_short_code)$id))
  #
  #     #FETCH THE NEW STUDY_SUBJECT_TABLE_ID
  #     study_subject_table_id <- tail(CheckTable("study_subject"), 1)$id
  #
  #     #ADD STUDY_SUBJECT_ID AND SPECIMEN_TYPE_ID TO _*_SPECIMEN_TABLE_*_ AND ADD THE NEW SPECIMEN_ID TO _*_STORAGE_CONTAINER_*_ TABLE IF SUBJECT_STUDY ENTRY DOES NOT EXIST
  #     if(toggle.is_longitudinal){
  #
  #       sampleDB::AddToTable("specimen",
  #                            list(created = lubridate::now("UTC") %>% as.character(),
  #                                 last_updated = lubridate::now("UTC") %>% as.character(),
  #                                 study_subject_id = study_subject_table_id,
  #                                 specimen_type_id = filter(table.specimen_type, label == csv[i,]$"specimen_type")$id,
  #                                 collection_date = as.character(dates[i]))) %>% print() #date is in y:m:d format
  #
  #     }else{
  #       sampleDB::AddToTable("specimen",
  #                            list(created = lubridate::now("UTC") %>% as.character(),
  #                                 last_updated = lubridate::now("UTC") %>% as.character(),
  #                                 study_subject_id = study_subject_table_id,
  #                                 specimen_type_id = filter(table.specimen_type, label == csv[i,]$"specimen_type")$id,
  #                                 collection_date = "NA"))
  #     }
  #
  #     #STORAGE CONTAINER TABLE
  #     sampleDB::AddToTable("storage_container",
  #                          list(created = lubridate::now("UTC") %>% as.character(),
  #                               last_updated = lubridate::now("UTC") %>% as.character(),
  #                               type = "NA",
  #                               specimen_id = tail(sampleDB::CheckTable("specimen"), 1)$id,
  #                               comments = "NA",
  #                               exhausted = 0))
  #   }
  # }
  #
  # updateSelectizeInput(session = session,
  #                      "SearchByPlateID",
  #                      choices = sampleDB::CheckTable("matrix_plate")$uid,
  #                      label = NULL)
  #
  # message <- paste("Upload Complete", emoji('tada'))
  # return(message)

}
