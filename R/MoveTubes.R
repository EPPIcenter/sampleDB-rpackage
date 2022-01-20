#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

MoveTubes <- function(database, barcode_file, plate_type, new_plate_uid, existing_plate_uid, location, session){

  list.move <- list()
  for(i in 1:length(barcode_file[,1])){
    list.move[[barcode_file[[i, 'name']]]] <- read_csv(barcode_file[[i, 'datapath']], col_types = cols())
  }
  
  print(list.move)
  
  # #READIN CSV FROM USER WITH VISIONMATE/TRAXER BARCODES
  # csv <- read_csv(barcode_file)
  # 
  # if(!("LocationRow" %in% names(csv))){
  #   message("Traxer MoveCSV")
  #   csv <- drop_na(csv) %>%
  #     mutate(barcode = `Tube ID`,
  #            well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
  #     select(-c(Position:Date))
  # }else{
  # 
  #   message("VisionMate MoveCSV")
  #   csv <- drop_na(csv) %>%
  #     mutate(barcode = TubeCode,
  #            well_position = paste0(LocationRow, LocationColumn)) %>%
  #     select(-c(LocationRow, LocationColumn, TubeCode))
  # }
  # 
  # location_id <- filter(CheckTable(database = database, "location"), description == location)$id

  # # MOVING SAMPLES TO A NEW (AKA BLANK) PLATE
  # if(plate_type == "new_plate"){
  #   #CREATE A PLATE ID FOR THE NEW BLANK PLATE
  #   AddToTable(database = database, "matrix_plate", list(created = lubridate::now("UTC") %>% as.character(),
  #                                                        last_updated = lubridate::now("UTC") %>% as.character(),
  #                                                        uid = new_plate_uid,
  #                                                        hidden = 0,
  #                                                        location_id = location_id))
  #   #GET MATRIX PLATE ID
  #   plate_id <- tail(CheckTable(database = database, "matrix_plate"), 1)$id
  # 
  #   #MODIFY ALL OF THE TUBES IN THE CSV TO REFLECT THE NEW PLATE
  #   for (i in 1:nrow(csv)){
  # 
  #     id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
  #     ModifyTable(database, "matrix_tube", list(plate_id = plate_id), id)
  # 
  #   }
  #   
  #   # MOVING SAMPLES TO A PLATE THAT HAS SAMPLES
  # }else{
  #   
  #   # MAKE A COPY OF THE DATABASE TO TEST THAT MOVE DOES NOT PRODUCE ORPHANS
  #   system(paste0("cp ", "/databases/example_19-Oct-21.sample_db.sqlite ", "/databases/modifytest"))
  #   
  #   # CLEAR SPACE IN THE EXISTING PLATE BY PUTTING ALL TUBES IN THE EXISTING PLATE INTO DUMMY PLATE -100
  #   #clear space in each existing plate...existing plate is the name of the file w/o.csv
  #   #it is possible that the existing plate is incorrectly written by the user
  #   # can put the rest of this section into a for loop
  #   
  #   # - GET THE PLATE ID FOR EXISTING PLATE
  #   existing.plate <- filter(CheckTable(database = database, "matrix_plate"), uid == existing_plate_uid)$id
  #   
  #   # - CREATE REF DF FOR ALL TUBES IN THE EXISTING PLATE
  #   tube_data.existing_plate <- filter(CheckTable(database = database, "matrix_tube"), plate_id == existing.plate)
  # 
  #   # - PUT SAMPLES INTO A DUMMY PLATE
  #   for(id in tube_data.existing_plate$id){
  #     ModifyTable(database = "/databases/modifytest",
  #                 "matrix_tube",
  #                 info_list = list(plate_id = -100),
  #                 id = id)
  #   }
  # 
  #   # MODIFY THE TUBES SO THEY ARE IN THE EXISTING PLATE
  #   #for csv in list of csvs
  #   for (i in  1:nrow(csv)){
  #     id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
  #     ModifyTable(database = "/databases/modifytest",
  #                 "matrix_tube",
  #                 info_list = list(plate_id = existing.plate),
  #                 id = id)
  # 
  #   }
  #   
  #   #check that there are no orphaned tubes (ie no tubes in dummy plate)
  # }
  # 
  # #UPDATE THE SEARCH DROPDOWNS
  # updateSelectizeInput(session = session,
  #                      "SearchByPlateID",
  #                      choices = c("", sampleDB::CheckTable(database = database, "matrix_plate")$uid),
  #                      label = NULL)
  # 
  # message <- paste("Successfully Moved Samples", emoji('tada'))
  # return(message)

}
