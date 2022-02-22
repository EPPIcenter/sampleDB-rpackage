#' Move Wetlab Samples in the EPPIcenter sampleDB database
#' 
#' @param file.container_samples A list containing an item for each plate involved in the move. Key is plate name, value is path to MoveCSV
#' @examples
#' sampleDB::MoveTubes(file.container_samples = list("container_name_1" = "~/path/container_1_samples.csv", "container_name_2" = "~/path/container_2_samples.csv"))
#' write.csv(tibble(LocationRow = rep("A", 10), LocationColumn = c(1:10), TubeCode = c(paste("XXXXX", 0:9))), "~/path/container_1_samples.csv")
#' write.csv(tibble(Position = rep("A", 10), "Tube ID" = c(paste("XXXXX", 0:9)), "Status" = NA,	"Error Count" = NA,	"Rack ID" = NA,	"Date" = NA), "~/path/container_1_samples.csv")
#' write.csv(tibble(row = rep("A", 10), column = c(1:10), "label" = c(paste("XXXXX", 0:9)), "~/path/container_1_samples.csv")
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

#want to be able to move samples and to move containers
MoveSamples <- function(type, file.container_samples){
  
  database <- "/databases/sampledb/v0.0.2/sampledb_database.sqlite"
  
  # READ IN FILES
  list.move <- modify(file.container_samples, function(x){x <- read_csv(x, col_types = cols()) %>% drop_na()})
  
  # RUN CHECKS
  outs.oprhan_check <- .MoveSamplesOrphanCheck(list.move, database, type = type)
  
  # GET CHECK DATA
  toggle <- outs.oprhan_check[[1]]
  dummy.tbl <- outs.oprhan_check[[2]]

  # MOVE SAMPLES IN DATABASE
  if(toggle){

    message.fail <- .GetFailedOrphanCheckSamples(type = type, dummy.tbl = dummy.tbl)
    
    return(message(message.fail))
    
  }else{

    # CLEAR SPACE IN THE EXISTING PLATE BY PUTTING ALL TUBES IN THE EXISTING PLATE INTO DUMMY PLATE -100
    .ClearSpaceInContainers(type = type, list.move = list.move, database = database)
    
    # MODIFY THE SAMPLES SO THEY ARE IN NEW CONTAINERS
    message.successful <- .CarryOutMoves(type = type, list.move = list.move, database = database)
    
    return(message(message.successful))
  }
}

.GetFailedOrphanCheckSamples <- function(type, dummy.tbl){
  # GET LABEL STILL IN DUMMY PLATE
  if(type == "matrix"){
    label.missing <- filter(dummy.tbl, plate_id < 0)$barcode
    
    # GET PLATE ID/PLATE NAME WHICH CONTAINED BARCODE STILL IN DUMMY
    container_id_with_missing_label <- filter(CheckTable(database = database, "matrix_tube"), barcode %in% label.missing)$plate_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "matrix_plate"), id %in% container_id_with_missing_label)$plate_name
  }
  else if(type == "cryo"){
    label.missing <- filter(dummy.tbl, box_id < 0)$label
    container_id_with_missing_label <- filter(CheckTable(database = database, "tube"), label %in% label.missing)$box_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "box"), id %in% container_id_with_missing_label)$box_name
  }
  else if(type == "rdt"){
    label.missing <- filter(dummy.tbl, bag_id < 0)$label
    container_id_with_missing_label <- filter(CheckTable(database = database, "rdt"), label %in% label.missing)$bag_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "bag"), id %in% container_id_with_missing_label)$bag_name
  }
  else{
    label.missing <- filter(dummy.tbl, bag_id < 0)$label
    container_id_with_missing_label <- filter(CheckTable(database = database, "paper"), label %in% label.missing)$bag_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "bag"), id %in% container_id_with_missing_label)$bag_name
  }
  
  message.fail <- paste0("Move Failed:\n",
                    "\tOrphaned Samples Detected:", label.missing ,"\n",
                    "\tOf Type:", type, "\n",
                    "\tFrom Container:", container_name_with_missing_label)
  return(message.fail)
}

.MoveSamplesOrphanCheck <- function(list.move, database, type){
  
  message("Checking for oprhaned samples...")
  
  # GET COPY OF THE CONTAINERS INVOLVED IN THE MOVE - SAVE AS A LIST WITH KEYS BEING FILENAMES
  test.list <- .CopyContainers(list.move = list.move, type = type, database = database)
  
  # - CHANGE DUMMY LIST PLATES
  dummy.tbl <- .ClearSpaceInTestContainers(test.list = test.list, type = type)
  
  # USE CSVS TO ASSIGN PLATES TO TUBES
  for(csv.name in names(list.move)){
    
    # - GET MOVECSV
    csv <- list.move[[csv.name]]
    
    # - GET DATA FOR MOVE (BARCODE, WELL, POS)
    for (i in 1:nrow(csv)){
      if(type == "matrix"){
        
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
        dummy.tbl[m, "plate_id"] <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), plate_name == csv.name)$id 
        
        # PUT THAT BARCODE/TUBE IN A WELL
        dummy.tbl[m, "well_position"] <- paste0(eval.well, eval.pos)
      }
      else if(type == "cryo"){
        
        #GET ROW BARCODE IS IN
        m <- which(dummy.tbl$label == csv[i,]$"label")
        
        # PUT THAT BARCODE/TUBE IN A NEW PLATE
        dummy.tbl[m, "box_id"] <- filter(sampleDB::CheckTable(database = database, "box"), box_name == csv.name)$id 
        
        # PUT THAT BARCODE/TUBE IN A WELL
        dummy.tbl[m, "box_position"] <- paste0(csv[i,]$"row", csv[i,]$"col")
      }
      else if(type == "rdt"){
        
        #GET ROW LABEL IS IN
        m <- which(dummy.tbl$label == csv[i,]$"label")
        
        # PUT THAT LABEL IN A NEW PLATE
        dummy.tbl[m, "bag_id"] <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == csv.name)$id 
      }
      else{
        
        #GET ROW LABEL IS IN
        m <- which(dummy.tbl$label == csv[i,]$"label")
        
        # PUT THAT LABEL IN A NEW PLATE
        dummy.tbl[m, "bag_id"] <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == csv.name)$id 
      }
    }
  }
  
  if(!all(dummy.tbl$plate_id > 0)){
    out <- list(TRUE, dummy.tbl)
  }else{
    out <- list(FALSE, dummy.tbl)
  }
  
  return(out)
}

.CopyContainers <- function(list.move, type, database){
  
  stopifnot("Type is not a valid option" = type %in% c("matrix", "cryo", "rdt", "paper"))
  
  if(type == "matrix"){
    container_type <- "matrix_plate"
    sample_type <- "matrix_tube"
    colname.container_name <- "plate_name"
    colname.container_id <- "plate_id"
  }
  else if(type == "cryo"){
    container_type <- "box"
    sample_type <- "tube"
    colname.container_name <- "box_name"
    colname.container_id <- "box_id"
  }
  else if(type == "rdt"){
    container_type <- "bag"
    sample_type <- "rdt"
    colname.container_name <- "bag_name"
    colname.container_id <- "bag_id"
  }
  else{
    container_type <- "bag"
    sample_type <- "paper"
    colname.container_name <- "bag_name"
    colname.container_id <- "bag_id"
  }
  
  test.list <- list()
  for(container.name in names(list.move)){
    tmp.container <- filter(sampleDB::CheckTable(database = database, container_type), !!as.name(colname.container_name) == container.name) 
    stopifnot("CONTAINER IS NOT FOUND IN THE DATABASE" = nrow(tmp.container) != 0)
    eval.container_id <- tmp.container$id
    test.list[[as.character(eval.container_id)]] <- filter(sampleDB::CheckTable(database = database, sample_type), !!as.name(colname.container_id) == eval.container_id)
  }
  return(test.list)
}

.ClearSpaceInContainers <- function(type, list.move, database){
  stopifnot(type %in% c("matrix", "cryo", "rdt", "paper"))
  
  for(i in 1:length(names(list.move))){
    container.name <- names(list.move)[i]
    
    if(type == "matrix"){
      container_type <- "matrix_plate"
      sample_type <- "matrix_tube"
    }
    else if(type == "cryo"){
      container_type <- "box"
      sample_type <- "tube"
    }
    else if(type == "rdt"){
      container_type <- "bag"
      sample_type <- "rdt"
    }
    else{
      container_type <- "bag"
      sample_type <- "paper"
    }
    
    # - GET THE PLATE ID FOR EXISTING PLATE
    existing.container <- filter(CheckTable(database = database, container_type), plate_name == container.name)$id
    
    # - CREATE REF DF FOR ALL TUBES IN THE EXISTING PLATE
    sample_data.existing_container <- filter(CheckTable(database = database, sample_type), plate_id == existing.container)
    
    # - PUT SAMPLES INTO A DUMMY PLATE (USING NEW DATABASE)
    for(id in sample_data.existing_container$id){
      
      ModifyTable(database = database,
                  sample_type,
                  info_list = list(plate_id = -(i)),
                  id = id)
    }
  }
}

.CarryOutMoves <- function(type, list.move, database){
  
  container_names <- c()
  number_samples <- c()

  for(csv.name in names(list.move)){
    container_names <- c(container_names, csv.name)
    number_samples <- c(number_samples, nrow(csv.name))
    
    # - GET MOVECSV
    csv <- list.move[[csv.name]]
    
    # - GET DATA FOR MOVE (BARCODE, WELL, POS)
    for (i in 1:nrow(csv)){
      if(type == "matrix"){
        
        if("TubeCode" %in% names(csv)){
          eval.barcode <- csv[i,]$"TubeCode"
          eval.well <- csv[i,]$"LocationRow"
          eval.pos <- csv[i,]$"LocationColumn"
        }else{
          eval.barcode <- csv[i,]$"Tube ID"
          eval.well <- csv[i,]$"Position" %>% substring(., 1, 1)
          eval.pos <- csv[i,]$"Position" %>% substring(., 2)
        }
        
        # - GET LABEL_ID
        id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == eval.barcode)$id
        # - GET CONTAINER_ID
        eval.container_id <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), plate_name == csv.name)$id 
        
        #MOVE SAMPLES INTO THE NEW PLATE
        ModifyTable(database = database,
                    "matrix_tube",
                    info_list = list(plate_id = eval.container_id,
                                     well_position = paste0(eval.well, eval.pos)),
                    id = id)
      }
      else if(type == "cryo"){
        # - GET LABEL_ID
        id <- filter(sampleDB::CheckTable(database = database, "tube"), label == csv[i,]$"label")$id
        # - GET CONTAINER_ID
        eval.container_id <- filter(sampleDB::CheckTable(database = database, "box"), box_name == csv.name)$id
        
        #MOVE SAMPLES INTO THE NEW PLATE
        ModifyTable(database = database,
                    "tube",
                    info_list = list(box_id = eval.container_id,
                                     box_position = paste0(csv[i,]$"row", csv[i,]$"column")),
                    id = id)
      }
      else if(type == "rdt"){
        # - GET LABEL_ID
        id <- filter(sampleDB::CheckTable(database = database, "rdt"), label == csv[i,]$"label")$id
        
        # - GET CONTAINER_ID
        eval.container_id <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == csv.name)$id 
        
        #MOVE SAMPLES INTO THE NEW PLATE
        ModifyTable(database = database,
                    "rdt",
                    info_list = list(bag_id = eval.container_id),
                    id = id)
      }
      else{
        # - GET LABEL_ID
        id <- filter(sampleDB::CheckTable(database = database, "paper"), label == csv[i,]$"label")$id
        
        # - GET CONTAINER_ID
        eval.container_id <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == csv.name)$id 
        
        #MOVE SAMPLES INTO THE NEW PLATE
        ModifyTable(database = database,
                    "paper",
                    info_list = list(bag_id = eval.container_id),
                    id = id)
      }
    }
  }
  #NOTE: number of samples from each container
  message <- paste0("Sucessfully Moved Samples:\n",
                    "\tType:", type, "\n",
                    "\tContainer Name: Number of Samples\n",
                    "\t", container_name, ":", number_samples, "\n")
  return(message)
}

.ClearSpaceInTestContainers <- function(test.list, type){
  
  dummy.list <- test.list

  for(i in 1:length(names(dummy.list))){
    eval.container_id <- names(dummy.list)[i]
    
    if(type == "matrix"){
      dummy.list[[eval.container_id]]$"plate_id" <- -(i) 
    }
    if(type == "cryo"){
      dummy.list[[eval.container_id]]$"box_id" <- -(i) 
    }
    else{
      dummy.list[[eval.container_id]]$"bag_id" <- -(i) 
    }
  }
  
  # - ROW BIND DUMMY PLATES
  dummy.tbl <- bind_rows(dummy.list)
  
  return(dummy.tbl)
}
