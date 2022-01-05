#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

#NOTE: THIS FUNCTIONS SHOULD BE USED TO RENAME THE PLATE A TUBE IS LINKED TO WHEN TUBE(S) ARE MOVED TO A NEW BLANK PLATE

MoveTubes <- function(database, barcode_file, new_plate_uid, location, session){


  #UNTIL READING THE COLNAMES ASSUME DATE IS NOT LONGITUDINAL
  toggle.is_longitudinal <- FALSE

  #READIN CSV FROM USER WITH VISIONMATE/TRAXER BARCODES,
  csv <- read_csv(barcode_file)

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

  #CREATE A PLATE ID FOR THE NEW BLANK PLATE
  AddToTable(database = database, "matrix_plate", list(created = lubridate::now("UTC") %>% as.character(),
                                                       last_updated = lubridate::now("UTC") %>% as.character(),
                                                       uid = new_plate_uid,
                                                       hidden = 0,
                                                       location_id = location))
  #GET MATRIX PLATE ID
  plate_id <- tail(CheckTable(database = database, "matrix_plate"), 1)$id

  #MODIFY ALL OF THE TUBES IN THE CSV TO REFLECT THE NEW PLATE
  for (i in  1:nrow(csv)){

    id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
    ModifyTable(database, "matrix_tube", list(plate_id = plate_id), id)

  }

  #UPDATE THE SEARCH DROPDOWNS
  updateSelectizeInput(session = session,
                       "SearchByPlateID",
                       choices = sampleDB::CheckTable(database = database, "matrix_plate")$uid,
                       label = NULL)

  message <- paste("Upload Complete", emoji('tada'))
  return(message)

}
