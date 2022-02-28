#' Move Wetlab Samples in the EPPIcenter sampleDB database
#' 
#' @param sample_type A string specifying the type of samples that are being moved. Options include: `micronix`, `cryovial`, `rdt` and `paper`
#' @param move_files A list of paths to SampleDB Move CSV files. The items in the `move_files` list must be named after the container associate with each Move CSV file.
#' 
#' The basic structure of a Move CSV file is shown below.
#' 
#' | row | column | label |
#' | --- |------- | ----- |
#' | A   | 0      | xxx1  |
#' | A   | 1      | xxx2  |
#' 
#' Accepted names for the `row` column include: `row`, `Row`, and `LocationRow`. \cr
#' Accepted names for the `column` column include: `column`, `Column`, and `LocationColumn`. \cr
#' Also accepted is a single column named `Position` that represents the both the `row` column and the `column` column. 
#' If the `Position` column is used then the `row` and `column` columns can be ommited.
#' @examples
#' \dontrun{
#' move_files <- list("move_csv1_container_name" = "~/path/to/move_csv1.csv", "move_csv2_container_name" = "~/path/to/move_csv2.csv")
#' MoveSamples(sample_type = "micronix", move_files = move_files)
#' }
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import readr
#' @import tidyr
#' @import lubridate
#' @export

#want to be able to move samples and to move containers
MoveSamples <- function(sample_type, move_files){
  
  stopifnot("Sample type is not valid" = operation %in% c("micronix", "cryovial", "rdt", "paper"))
  
  database <- "/databases/sampledb/v0.0.2/sampledb_database.sqlite"
  
  # READ IN FILES
  list.move <- modify(move_files, function(x){x <- read_csv(x, col_types = cols()) %>% drop_na()})
  
  # RUN CHECKS
  outs.oprhan_check <- .CheckForOrphans(list.move, database, sample_type = sample_type)
  
  # GET CHECK DATA
  toggle <- outs.oprhan_check[[1]]
  dummy.tbl <- outs.oprhan_check[[2]]

  # MOVE SAMPLES IN DATABASE
  if(toggle){

    message.fail <- .GetOrphanedSamples(sample_type = sample_type, dummy.tbl = dummy.tbl)
    
    return(message(message.fail))
    
  }else{

    # CLEAR SPACE IN THE EXISTING PLATE BY PUTTING ALL TUBES IN THE EXISTING PLATE INTO DUMMY PLATE -100
    .ClearSpaceInContainers(sample_type = sample_type, list.move = list.move, database = database)
    
    # MODIFY THE SAMPLES SO THEY ARE IN NEW CONTAINERS
    message.successful <- .MoveSamples(sample_type = sample_type, list.move = list.move, database = database)
    
    return(message(message.successful))
  }
}

.CheckForOrphans <- function(list.move, database, sample_type){
  
  message("Checking for oprhaned samples...")
  
  # GET COPY OF THE CONTAINERS INVOLVED IN THE MOVE - SAVE AS A LIST WITH KEYS BEING FILENAMES
  test.list <- .CopyContainersForTests(list.move = list.move, sample_type = sample_type, database = database)
  
  # - CHANGE DUMMY LIST PLATES
  dummy.tbl <- .ClearSpaceForTests(test.list = test.list, sample_type = sample_type)
  
  # USE CSVS TO ASSIGN PLATES TO TUBES
  for(csv.name in names(list.move)){
    
    # - GET MOVECSV
    csv <- list.move[[csv.name]]
    
    # - GET DATA FOR MOVE (BARCODE, WELL, POS)
    for (i in 1:nrow(csv)){
      if(sample_type == "matrix"){
        
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
      else if(sample_type == "cryo"){
        
        #GET ROW BARCODE IS IN
        m <- which(dummy.tbl$label == csv[i,]$"label")
        
        # PUT THAT BARCODE/TUBE IN A NEW PLATE
        dummy.tbl[m, "box_id"] <- filter(sampleDB::CheckTable(database = database, "box"), box_name == csv.name)$id 
        
        # PUT THAT BARCODE/TUBE IN A WELL
        dummy.tbl[m, "box_position"] <- paste0(csv[i,]$"row", csv[i,]$"col")
      }
      else if(sample_type == "rdt"){
        
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

.CopyContainersForTests <- function(list.move, sample_type, database){
  
  stopifnot("Type is not a valid option" = sample_type %in% c("matrix", "cryo", "rdt", "paper"))
  
  if(sample_type == "matrix"){
    container_type <- "matrix_plate"
    sample_type <- "matrix_tube"
    colname.container_name <- "plate_name"
    colname.container_id <- "plate_id"
  }
  else if(sample_type == "cryo"){
    container_type <- "box"
    sample_type <- "tube"
    colname.container_name <- "box_name"
    colname.container_id <- "box_id"
  }
  else if(sample_type == "rdt"){
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

.ClearSpaceForTests <- function(test.list, sample_type){
  
  dummy.list <- test.list
  
  for(i in 1:length(names(dummy.list))){
    eval.container_id <- names(dummy.list)[i]
    
    if(sample_type == "matrix"){
      dummy.list[[eval.container_id]]$"plate_id" <- -(i) 
    }
    if(sample_type == "cryo"){
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

.GetOrphanedSamples <- function(sample_type, dummy.tbl){
  # GET LABEL STILL IN DUMMY PLATE
  if(sample_type == "matrix"){
    label.missing <- filter(dummy.tbl, plate_id < 0)$barcode
    
    # GET PLATE ID/PLATE NAME WHICH CONTAINED BARCODE STILL IN DUMMY
    container_id_with_missing_label <- filter(CheckTable(database = database, "matrix_tube"), barcode %in% label.missing)$plate_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "matrix_plate"), id %in% container_id_with_missing_label)$plate_name
  }
  else if(sample_type == "cryo"){
    label.missing <- filter(dummy.tbl, box_id < 0)$label
    container_id_with_missing_label <- filter(CheckTable(database = database, "tube"), label %in% label.missing)$box_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "box"), id %in% container_id_with_missing_label)$box_name
  }
  else if(sample_type == "rdt"){
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
                    "\tOf Type:", sample_type, "\n",
                    "\tFrom Container:", container_name_with_missing_label)
  return(message.fail)
}

.ClearSpaceInContainers <- function(sample_type, list.move, database){
  stopifnot(sample_type %in% c("matrix", "cryo", "rdt", "paper"))
  
  for(i in 1:length(names(list.move))){
    container.name <- names(list.move)[i]
    
    if(sample_type == "matrix"){
      container_type <- "matrix_plate"
      sample_type <- "matrix_tube"
    }
    else if(sample_type == "cryo"){
      container_type <- "box"
      sample_type <- "tube"
    }
    else if(sample_type == "rdt"){
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

.MoveSamples <- function(sample_type, list.move, database){
  
  container_names <- c()
  number_samples <- c()

  for(csv.name in names(list.move)){
    container_names <- c(container_names, csv.name)
    number_samples <- c(number_samples, nrow(csv.name))
    
    # - GET MOVECSV
    csv <- list.move[[csv.name]]
    
    # - GET DATA FOR MOVE (BARCODE, WELL, POS)
    for (i in 1:nrow(csv)){
      if(sample_type == "matrix"){
        
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
      else if(sample_type == "cryo"){
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
      else if(sample_type == "rdt"){
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
                    "\tType:", sample_type, "\n",
                    "\tContainer Name: Number of Samples\n",
                    "\t", container_name, ":", number_samples, "\n")
  return(message)
}
