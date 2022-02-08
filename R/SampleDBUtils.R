#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export

#REFORMAT CSV -- IF LOCATIONROW IS A COLUMN THEN THE DATA CAME OFF VISIONMATE
ReformatUploadCSV <- function(csv.upload){

  if(!("LocationRow" %in% names(csv.upload))){
    csv.reformatted <- drop_na(csv.upload) %>%
      mutate(barcode = `Tube ID`,
             well_position = paste0(substring(Position, 1, 1), substring(Position, 2))) %>%
      select(-c(Position:Date))
    message("UploadCSV from Traxer detected...")
  }else{
    csv.reformatted <- drop_na(csv.upload) %>%
      mutate(barcode = TubeCode,
             well_position = paste0(LocationRow, LocationColumn)) %>%
      select(-c(LocationRow, LocationColumn, TubeCode))
    message("UploadCSV from VisionMate detected...")
  }
  
  return(csv.reformatted)
}

UploadMicronixChecks <- function(input, database, list.location, name.plate, csv.upload, csv.reformatted){
  # PERFORM CHECKS
  
  # CHECK FREEZER EXISTS
  tmp.location.tbl <- inner_join(tibble(location_name = list.location$location_name, level_I = list.location$level_I, level_II = list.location$level_II), 
                                 sampleDB::CheckTable(database = database, "location"), 
                                 by = c("location_name", "level_I", "level_II"))
  if(nrow(tmp.location.tbl) == 0){
    stop("FREEZER NAME + LEVEL I + LEVEL II DOES NOT EXITS")
  }
  
  # CHECK PLATE NAME IS UNIQUE
  if(name.plate %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_name)){
    stop("PLATE NAME IS NOT UNIQUE")
  }
  
  #CHECK COLNAMES ARE NOT MALFORMED
  names.traxer.nodate <- c("Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date", "study_subject_id", "specimen_type", "study_short_code")
  names.traxer.date <- c("Position", "Tube ID",	"Status",	"Error Count",	"Rack ID",	"Date", "study_subject_id", "specimen_type", "study_short_code", "collection_date")
  names.visionmate.nodate <- c("LocationRow", "LocationColumn", "TubeCode", "study_subject_id", "specimen_type", "study_short_code")
  names.visionmate.date <- c("LocationRow", "LocationColumn", "TubeCode", "study_subject_id", "specimen_type", "study_short_code", "collection_date")
  if(setequal(names.traxer.nodate, names(csv.upload)) || setequal(names.traxer.date, names(csv.upload)) || setequal(names.visionmate.nodate, names(csv.upload)) || setequal(names.visionmate.date, names(csv.upload))){
  }else{
    stop("UPLOADCSV COLNAMES ARE MALFORMED")
  }
  
  # CHECK DATE (IF PRESENT) IS IN CORRECT FORMAT
  if("collection_date" %in% names(csv.reformatted)){
    if(is.na(parse_date_time(csv.reformatted$"collection_date", orders = "ymd")) == TRUE){
      stop("COLLECTION DATE MUSE BE YMD")
    }
  }
  
  # CHECK THAT SPECIMEN TYPE(S) EXISTS
  if(!all(csv.reformatted$"specimen_type" %in% CheckTable(database = database, table = "specimen_type")$label)){
    stop("ERROR: SPECIMEN TYPE(S) NOT FOUND")
  }
  
  # CHECK THAT NO BARCODE ALREADY EXISTS -- NEEDS TO BE REFLECTED IN UPLOADCHECKS.HELPER.R
  if(all(!csv.reformatted$barcode %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))){
  }else{
    barcodes.existing <- csv.reformatted$barcode[which(csv.reformatted$barcode %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))]
    stop(paste("Error: Barcode Unique Constraint", barcodes.existing))
  }
  
  # CHECK THAT STUDY SUBJECT subject IS PRESENT IF IT IS REQUIRED
  tmp_table.specimen <- tibble(subject = csv.reformatted$"study_subject_id", 
                               study_id = filter(CheckTable(database = database, "study"), short_code %in% csv.reformatted$study_short_code)$id, 
                               specimen_type_id = filter(CheckTable(database = database, "specimen_type"), label %in% csv.reformatted$specimen_type)$id)
  
  tmp_table.specimen <- inner_join(CheckTable(database = database, "study_subject"),
                                   tmp_table.specimen,
                                   by = c("subject", "study_id"))
  
  if(nrow(tmp_table.specimen) != 0){
    
    if("collection_date" %in% names(csv.reformatted)){
      tmp_table.specimen$collection_date <- csv.reformatted$collection_date
    }else{
      tmp_table.specimen$collection_date <- NA
    } 
    
    #CHECK IF SPECIMEN ALREADY EXISTS
    tmp_table.specimen <- inner_join(CheckTable(database = database, "specimen"), 
                                     tmp_table.specimen %>% rename("study_subject_id" = "id"), 
                                     by = c("study_subject_id", "specimen_type_id", "collection_date"))
    
    if(nrow(tmp_table.specimen) > 0){
      stop("SPECIMEN ALREADY EXISTS IN THE DATABASE. SPECIMENS UNIQUE CONSTRAINT: SUBJECT + STUDY + SPECIMEN TYPE + COLLECTION DATE")
    }
  }
}

MovePlatesOrphanCheck <- function(list.move, database){
  
  # TEST WHETHER MOVE WILL PRODUCE ORPHANS
  # GET A COPY OF THE PLATES INVOLVED IN THE MOVE - SAVE AS A LIST WITH KEYS BEING FILENAMES
  test.list <- list()
  for(plate.name in names(list.move)){
    tmp.matrix_plate <- filter(CheckTable(database = database, "matrix_plate"), plate_name == plate.name)
    stopifnot("PLATE IS NOT FOUND IN THE DATABASE" = nrow(tmp.matrix_plate) != 0)
    eval.plate_id <- tmp.matrix_plate$id
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
    eval.plate_id <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), plate_name == csv.name)$id
    
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
  if(!all(dummy.tbl$plate_id > 0)){
    out <- list(TRUE, dummy.tbl)
  }else{
    out <- list(FALSE, dummy.tbl)
  }
  
  return(out)
}