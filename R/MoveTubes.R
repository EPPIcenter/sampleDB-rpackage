#' Move Wetlab Samples in the EPPIcenter sampleDB database
#' 
#' @param file.barcode A list containing an item for each plate involved in the move. Key is plate name, value is path to MoveCSV
#' @examples
#' sampleDB::MoveTubes(file.barcode = list("name1" = "~/path/name1.csv", "name2" = "~/path/name2.csv"))
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

MoveTubes <- function(file.barcode){
  
  database <- "/databases/new.sampleDB.db"
  
  # READ IN FILES
  list.move <- modify(file.barcode, function(x){x <- read_csv(x, col_types = cols()) %>% drop_na()})
  
  # RUN CHECKS
  toggle <- MovePlatesOrphanCheck(list.move, database)[[1]]
  dummy.tbl <- MovePlatesOrphanCheck(list.move, database)[[2]]
  
  # MOVE SAMPLES IN DATABASE
  if(toggle){
    
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
