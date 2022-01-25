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

  # TEST WHETHER MOVE WILL PRODUCE ORPHANS
  # GET A COPY OF THE PLATES INVOLVED IN THE MOVE - SAVE AS A LIST WITH KEYS BEING FILENAMES
  test.list <- list()
  for(plate.name in names(list.move)){
    eval.plate_id <- filter(CheckTable(database = database, "matrix_plate"), uid == plate.name)$id
    test.list[[as.character(eval.plate_id)]] <- filter(CheckTable(database = database, "matrix_tube"), plate_id == eval.plate_id)
  }
  
  # MAKE A LIST WHERE EACH PLATE IS CONVERTED TO A DUMMY PLATE
  # - MAKE DUMMY PLATE LIST
  dummy.list <- test.list
  
  # - CHANGE DUMMY LIST PLATES
  for(i in 1:length(names(dummy.list))){
    eval.plate_id <- names(dummy.list)[i]
    dummy.list[[eval.plate_id]]$"plate_id" <- -(i)
  }
  
  # - ROW BIND DUMMY PLATES
  dummy.tbl <- bind_rows(dummy.list)
  
  # USE CSVS TO ASSIGN PLATES TO TUBES
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
      
      #GET ROW BARCODE IS IN
      m <- which(dummy.tbl$barcode == eval.barcode)
      
      # PUT THAT BARCODE/TUBE IN A NEW PLATE
      dummy.tbl[m, "plate_id"] <- eval.plate_id
      
      # PUT THAT BARCODE/TUBE IN A WELL
      dummy.tbl[m, "well_position"] <- paste0(eval.well, eval.pos)
    }
  }
  
  # TEST IF NEG NUM IS IN PLATE_ID, CAN TEST IF LENGTH(UNIQ(PASTE(EVAL.WELL,PLATE.ID))) == NUMBER OF ROWS...MAY WANT TO DROP ALL NA'S FROM READIN CSV
  if(!all(dummy.tbl$plate_id > 0)){
    
    # GET BARCODE STILL IN DUMMY PLATE
    barcode.missing <- filter(dummy.tbl, plate_id < 0)$barcode
    
    # GET PLATE ID/PLATE NAME WHICH CONTAINED BARCODE STILL IN DUMMY
    plate_id_with_missing_barcode <- filter(CheckTable(database = database, "matrix_tube"), barcode %in% barcode.missing)$plate_id
    plate_name_with_missing_barcode <- filter(CheckTable(database = database, "matrix_plate"), id %in% plate_id_with_missing_barcode)$uid
    
    return(paste0("Move Failed: Orphaned tube with barcode ", barcode.missing, " from plate ", plate_name_with_missing_barcode, "\n"))
    
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
}
