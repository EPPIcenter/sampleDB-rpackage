#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

#NOTE: THIS FUNCTIONS SHOULD BE USED TO RENAME THE PLATE A TUBE IS LINKED TO WHEN TUBE(S) ARE MOVED TO A NEW BLANK PLATE

MoveTubes <- function(database, barcode_file, plate_type, new_plate_uid, existing_plate_uid, location, session){

  #READIN CSV FROM USER WITH VISIONMATE/TRAXER BARCODES,
  csv <- read_csv(barcode_file)

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

  if(plate_type == "new_plate"){
    #CREATE A PLATE ID FOR THE NEW BLANK PLATE
    AddToTable(database = database, "matrix_plate", list(created = lubridate::now("UTC") %>% as.character(),
                                                         last_updated = lubridate::now("UTC") %>% as.character(),
                                                         uid = new_plate_uid,
                                                         hidden = 0,
                                                         location_id = location))
    #GET MATRIX PLATE ID
    plate_id <- tail(CheckTable(database = database, "matrix_plate"), 1)$id

    #MODIFY ALL OF THE TUBES IN THE CSV TO REFLECT THE NEW PLATE
    for (i in 1:nrow(csv)){

      id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
      ModifyTable(database, "matrix_tube", list(plate_id = plate_id), id)

    }
  }else{

    #CREATE A DUMMY (-100) PLATE_ID FOR ALL THE TUBES TO GO INTO
    for (i in  1:nrow(csv)){
      id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
      ModifyTable(database = database,
                  "matrix_tube",
                  info_list = list(plate_id = -100),
                  id = id)

    }

    #GET THE PLATE_ID ASSO W THE EXISTING PLATE UID
    plate_id <- filter(CheckTable(database = database, "matrix_plate"), plate_uid == existing_plate_uid)$id

    #MODIFY THE TUBES TO BE ASSOCIATED WITH THE EXISTING PLATE ID
    for (i in  1:nrow(csv)){
      id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
      ModifyTable(database = database,
                  "matrix_tube",
                  info_list = list(plate_id = plate_id),
                  id = id)

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
