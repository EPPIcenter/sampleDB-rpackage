#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @export

#DO WE WANT USERS TO BE ABLE TO CHANGE THE STUDY CODE ASSO W TUBES?
#DO WE WANT USERS TO BE ABLE TO DELETE TUBES FROM THE DATABASE?

MoveTubes <- function(database, barcode_file, new_plate_id, location){


  #OBTAIN TABLES AS THEY ARE IN THE DATABASE RIGHT NOW (SNAPSHOT)
  table.location <- sampleDB::CheckTable(database = database, "location")
  table.study <- sampleDB::CheckTable(database = database, "study")
  table.specimen_type <- sampleDB::CheckTable(database = database, "specimen_type")

  #UNTIL READING THE COLNAMES ASSUME DATE IS NOT LONGITUDINAL
  toggle.is_longitudinal <- FALSE

  #READIN CSV FROM USER WITH VISIONMATE/TRAXER BARCODES,
  # csv <- read_csv(barcode_file)

  #TESTING VARS
  csv <- read_csv("~/Desktop/move_samples_example/visionmate_no_date-newuid.csv")
  database <- "example_19-Oct-21.sample_db.sqlite"

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

    id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
    print(id)

  }

  # {
  #   filter(CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id %>% print()
  #
  #   #modify matrix_tube table to swap out new_plate_id and well position with usr input new_plate and well poisiton
  #   sampleDB::ModifyTable(database = database, "matrix_tube",
  #                         info_list = list(plate_id = new_plate_id,
  #                                          barcode = csv[i,]$"barcode",
  #                                          well_position = csv[i,]$"well_position"),
  #                         id = filter(CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id)
  # }
}
