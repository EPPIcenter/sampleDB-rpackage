#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

#NOTE: THIS FUNCTIONS SHOULD BE USED TO RENAME THE PLATE A TUBE IS LINKED TO WHEN TUBE(S) ARE MOVED TO A NEW BLANK PLATE

MoveTubes <- function(database, barcode_file, plate_type, new_plate_uid, existing_plate_uid, location, session){

  #READIN CSV FROM USER WITH VISIONMATE/TRAXER BARCODES
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

  location_id <- filter(CheckTable(database = database, "location"), description == location)$id

  if(plate_type == "new_plate"){
    #CREATE A PLATE ID FOR THE NEW BLANK PLATE
    AddToTable(database = database, "matrix_plate", list(created = lubridate::now("UTC") %>% as.character(),
                                                         last_updated = lubridate::now("UTC") %>% as.character(),
                                                         uid = new_plate_uid,
                                                         hidden = 0,
                                                         location_id = location_id))
    #GET MATRIX PLATE ID
    plate_id <- tail(CheckTable(database = database, "matrix_plate"), 1)$id

    #MODIFY ALL OF THE TUBES IN THE CSV TO REFLECT THE NEW PLATE
    for (i in 1:nrow(csv)){

      id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
      ModifyTable(database, "matrix_tube", list(plate_id = plate_id), id)

    }
  }else{

    #CLEAR SPACE IN THE EXISTING PLATE BY PUTTING ALL TUBES IN THE EXISTING PLATE INTO DUMMY PLATE -100
    plate_id_existing <- filter(CheckTable(database = database, "matrix_plate"), uid == existing_plate_uid)$id
    print(plate_id_existing)
    #CREATE MATRIX WITH ALL THE TUBES ASSO W THE EXISTING PLATE
    tube_data.existing_plate <- filter(CheckTable(database = database, "matrix_tube"), plate_id == plate_id_existing)
    print("HERE1")
    print(tube_data.existing_plate)

    print("HERE2")
    for(id in tube_data.existing_plate$id){
      print(id)
      #PUT THEM IN A DUMMY PLATE
      ModifyTable(database = database,
                  "matrix_tube",
                  info_list = list(plate_id = -100),
                  id = id)
    }
    print(filter(CheckTable(database = database, "matrix_tube"), plate_id == -100))

    #MODIFY THE TUBES TO BE ASSOCIATED WITH THE EXISTING PLATE ID
    for (i in  1:nrow(csv)){
      id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
      ModifyTable(database = database,
                  "matrix_tube",
                  info_list = list(plate_id = plate_id_existing),
                  id = id)

    }

    #PRINT OUT THE TUBES THAT WERE ORPHANED
    print("HERE3")
    print(filter(CheckTable(database = database, "matrix_tube"), plate_id == plate_id_existing))
  }

  #UPDATE THE SEARCH DROPDOWNS
  updateSelectizeInput(session = session,
                       "SearchByPlateID",
                       choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$uid),
                       label = NULL)

  message <- paste("Successfully Moved Samples", emoji('tada'))
  return(message)

}
