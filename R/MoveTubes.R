#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

MoveTubes <- function(database, barcode_file, plate_type, new_plate_uid, existing_plate_uid, location, session){

  # CREATE A LIST WHERE THE KEY IS THE FILENAME AND THE VALUE IS THE CSV
  list.move <- list()
  for(i in 1:length(barcode_file[,1])){
    plate.name <- barcode_file[[i, 'name']] %>% gsub("\\.csv","",.)
    list.move[[plate.name]] <- read_csv(barcode_file[[i, 'datapath']], col_types = cols()) %>% drop_na()
  }
  
  # print(names(list.move))
  # print(list.move)
  # print("HERE62")
  
  # TEST WHETHER MOVE WILL PRODUCE ORPHANS
  # GET A COPY OF THE PLATES INVOLVED IN THE MOVE - SAVE AS A LIST WITH KEYS BEING FILENAMES
  test.list <- list()
  for(plate.name in names(list.move)){
    # print(plate.name)
    eval.plate_id <- filter(CheckTable(database = database, "matrix_plate"), uid == plate.name)$id
    # print(eval.plate_id)
    test.list[[as.character(eval.plate_id)]] <- filter(CheckTable(database = database, "matrix_tube"), plate_id == eval.plate_id)
    # print(test.list[[as.character(eval.plate_id)]])
    # print(test.list)
  }
  
  # print("HERE52")
  # print(test.list)
  
  # MAKE A LIST WHERE EACH PLATE IS CONVERTED TO A DUMMY PLATE
  # - MAKE DUMMY PLATE LIST
  dummy.list <- test.list
  
  # - CHANGE DUMMY LIST PLATES
  for(i in 1:length(names(dummy.list))){
    eval.plate_id <- names(dummy.list)[i]
    # print(eval.plate_id)
    dummy.list[[eval.plate_id]]$"plate_id" <- -(i)
  }
  
  # print("HERE48")
  # print(dummy.list)
  
  # - ROW BIND DUMMY PLATES
  dummy.tbl <- bind_rows(dummy.list)
  
  # print("HERE41")
  # print(dummy.tbl %>% as.data.frame())
  
  # USE CSVS TO ASSIGN PLATES TO TUBES
  for(csv.name in names(list.move)){
    
    # - GET MOVECSV
    csv <- list.move[[csv.name]]
    
    # - GET PLATE_ID 
    eval.plate_id <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), uid == csv.name)$id
    
    # print("HERE30")
    # print(eval.plate_id)
    
    # - GET DATA FOR MOVE (BARCODE, WELL, POS)
    for (i in 1:nrow(csv)){
      if("TubeCode" %in% names(csv)){
        eval.barcode <- csv[i,]$"TubeCode"
        eval.well <- csv[i,]$"LocationRow"
        eval.pos <- csv[i,]$"LocationColumn"
      }else{
        eval.barcode <- csv[i,]$"Tube ID"
        eval.well <- csv[i,]$"Position" %>% substring(., 1, 1)
        eval.pos <- csv[i,]$"Position" %>% substring(., 2)
      }
      
      # print("HERE77")
      # print(eval.barcode)
      # print(paste0(eval.well, eval.pos))
      
      #GET ROW BARCODE IS IN
      m <- which(dummy.tbl$barcode == eval.barcode)
      
      # PUT THAT BARCODE/TUBE IN A NEW PLATE
      dummy.tbl[m, "plate_id"] <- eval.plate_id
      
      # PUT THAT BARCODE/TUBE IN A WELL
      dummy.tbl[m, "well_position"] <- paste0(eval.well, eval.pos)
      # print(dummy.tbl)
    }
  }
  # print("HERE80")
  # print(dummy.tbl %>% as.data.frame())
  
  # TEST IF NEG NUM IS IN PLATE_ID, CAN TEST IF LENGTH(UNIQ(PASTE(EVAL.WELL,PLATE.ID))) == NUMBER OF ROWS...MAY WANT TO DROP ALL NA'S FROM READIN CSV
  if(!all(dummy.tbl$plate_id > 0)){
    #find what the negative number is and look back at the plate name asso w that negative number
    #whatever is asso w that neg number links to the missing file
    # return("somthing is missing...id like to be more specific")
    
    # return("printout names of all plates tubes were associated with so usr can search")
    return("Move Failed")
  }else{

    # CLEAR SPACE IN THE EXISTING PLATE BY PUTTING ALL TUBES IN THE EXISTING PLATE INTO DUMMY PLATE -100
    for(i in 1:length(names(list.move))){
      plate.name <- names(list.move)[i]
  
      # - GET THE PLATE ID FOR EXISTING PLATE
      existing.plate <- filter(CheckTable(database = database, "matrix_plate"), uid == plate.name)$id
  
      # - CREATE REF DF FOR ALL TUBES IN THE EXISTING PLATE
      tube_data.existing_plate <- filter(CheckTable(database = database, "matrix_tube"), plate_id == existing.plate)
  
      # - PUT SAMPLES INTO A DUMMY PLATE (USING NEW DATABASE)
      for(id in tube_data.existing_plate$id){
  
        ModifyTable(database = database,
                    "matrix_tube",
                    info_list = list(plate_id = -(i)),
                    id = id)
      }
    }
  
    # MODIFY THE TUBES SO THEY ARE IN THE EXISTING PLATE
    for(csv.name in names(list.move)){
  
      # - GET MOVECSV
      csv <- list.move[[csv.name]]
  
      # - GET PLATE_ID
      eval.plate_id <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), uid == csv.name)$id
  
      # - GET DATA FOR MOVE (BARCODE, WELL, POS)
      for (i in 1:nrow(csv)){
        if("TubeCode" %in% names(csv)){
          eval.barcode <- csv[i,]$"TubeCode"
          eval.well <- csv[i,]$"LocationRow"
          eval.pos <- csv[i,]$"LocationColumn"
        }else{
          eval.barcode <- csv[i,]$"Tube ID"
          eval.well <- csv[i,]$"Position" %>% substring(., 1, 1)
          eval.pos <- csv[i,]$"Position" %>% substring(., 2)
        }
  
        # - GET BARCODE_ID
        id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == eval.barcode)$id
  
        #MOVE SAMPLES INTO THE NEW PLATE
        ModifyTable(database = database,
                    "matrix_tube",
                    info_list = list(plate_id = eval.plate_id,
                                     well_position = paste0(eval.well, eval.pos)),
                    id = id)
  
      }
    }
    return("Moves Compete")
  }
  #######################################################################
  
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
    # # MAKE A COPY OF THE DATABASE TO TEST THAT MOVE DOES NOT PRODUCE ORPHANS
    # system(paste0("cp ", "/databases/example_19-Oct-21.sample_db.sqlite ", "/databases/modifytest"))

    # # CLEAR SPACE IN THE EXISTING PLATE BY PUTTING ALL TUBES IN THE EXISTING PLATE INTO DUMMY PLATE -100
    # #clear space in each existing plate...existing plate is the name of the file w/o.csv
    # #it is possible that the existing plate is incorrectly written by the user
    # # can put the rest of this section into a for loop
    # 
    # for(plate.name in names(list.move)){ #instead of getting the plate names this way, i could just go through and use the barcodes to find all of the plate names associated with the tubes...then use those to zero things out
    #   # - GET THE PLATE ID FOR EXISTING PLATE
    #   existing.plate <- filter(CheckTable(database = database, "matrix_plate"), uid == plate.name)$id
    #   
    #   # - CREATE REF DF FOR ALL TUBES IN THE EXISTING PLATE
    #   tube_data.existing_plate <- filter(CheckTable(database = database, "matrix_tube"), plate_id == existing.plate)
    #   
    #   # - PUT SAMPLES INTO A DUMMY PLATE (USING NEW DATABASE)
    #   for(id in tube_data.existing_plate$id){
    #     ModifyTable(database = "/databases/modifytest",
    #                 "matrix_tube",
    #                 info_list = list(plate_id = -100),
    #                 id = id)
    #   }
    #   
    #   #CHECK THAT ALL SAMPLES IN THE EXISTING PLATES ARE NOW IN DUMMY PLATES
    # }
    
    # # - GET THE PLATE ID FOR EXISTING PLATE
    # existing.plate <- filter(CheckTable(database = database, "matrix_plate"), uid == existing_plate_uid)$id
    # 
    # # - CREATE REF DF FOR ALL TUBES IN THE EXISTING PLATE
    # tube_data.existing_plate <- filter(CheckTable(database = database, "matrix_tube"), plate_id == existing.plate)
    # 
    # # - PUT SAMPLES INTO A DUMMY PLATE
    # for(id in tube_data.existing_plate$id){
    #   ModifyTable(database = "/databases/modifytest",
    #               "matrix_tube",
    #               info_list = list(plate_id = -100),
    #               id = id)
    # }

    # # MODIFY THE TUBES SO THEY ARE IN THE EXISTING PLATE
    # #for csv in list of csvs
    # 
    # for(csv.name in names(list.move)){
    #   csv <- list.move[csv.name]
    #   
    #   for (i in  1:nrow(csv)){
    #     id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
    #     ModifyTable(database = "/databases/modifytest",
    #                 "matrix_tube",
    #                 info_list = list(plate_id = existing.plate),
    #                 id = id)
    #     
    #   }
    # }
    # for (i in  1:nrow(csv)){
    #   id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == csv[i,]$"barcode")$id
    #   ModifyTable(database = "/databases/modifytest",
    #               "matrix_tube",
    #               info_list = list(plate_id = existing.plate),
    #               id = id)
    # 
    # }

    #check that there are no orphaned tubes (ie no tubes in dummy plate)
    
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
