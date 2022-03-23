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
  
  stopifnot("Sample type is not valid" = sample_type %in% c("micronix", "cryovial", "rdt", "paper"))
  database <- Sys.getenv("SDB_PATH")
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
  
  # Read in move files
  # Create list -- keys: container name; values: container's samples
  move_data_list <- modify(move_files, function(x){x <- read_csv(x, col_types = cols()) %>% drop_na()})
  
  # Save MoveCSVs
  .SaveMoveCSVs(move_data_list)
  
  # Check if move creates orphans - returns TRUE if pass, FALSE if fail
  orphan_check_return <- .CheckForOrphans(move_data_list = move_data_list, database, sample_type = sample_type)
  
  # Link samples to containers in move
  if(orphan_check_return$orphan_check_toggle){

    # Move samples out of containers - place in container id with negative number
    .ClearSpaceInContainers(sample_type = sample_type, move_data_list = move_data_list, database = database, conn = conn)
    
    # Link samples to containers in move
    message.successful <- .MoveSamples(sample_type = sample_type, move_data_list = move_data_list, database = database, conn = conn)
    
    #close connection
    tryCatch(
      RSQLite::dbDisconnect(conn),
      warning=function(w){})
    
    return(message(message.successful))
    
  }else{
  
    # print samples that would be orphaned by move
    message.fail <- .GetOrphanedSamples(sample_type = sample_type, stacked_orphaned_sample_data = orphan_check_return$stacked_orphaned_sample_data, database = database)
    
    #close connection
    tryCatch(
      RSQLite::dbDisconnect(conn),
      warning=function(w){})
    
    return(message(message.fail))
      
  }
}

.CheckForOrphans <- function(move_data_list, database, sample_type){
  
  message("Checking for oprhaned samples...")
  
  # For containers involved in the move, extract sample level data from sampleDB (barcode, container position, container id)
  sample_data <- .CopyContainersForTests(move_data_list = move_data_list, sample_type = sample_type, database = database)
  
  # Change sample's container ids to negative numbers (i.e. remove samples from containers)
  stacked_orphaned_sample_data <- .ClearSpaceForTests(sample_data = sample_data, sample_type = sample_type)
  
  # Use move data to assign containers to samples
  for(container_name in names(move_data_list)){
    
    # Get samples in container
    samples <- move_data_list[[container_name]]

    # Get sample data
    for (i in 1:nrow(samples)){
      if(sample_type == "micronix"){
        
        if("TubeCode" %in% names(samples)){
          eval.barcode <- samples[i,]$"TubeCode"
          eval.well <- samples[i,]$"LocationRow"
          eval.pos <- samples[i,]$"LocationColumn"
        }else{
          eval.barcode <- samples[i,]$"Tube ID"
          eval.well <- samples[i,]$"Position" %>% substring(., 1, 1)
          eval.pos <- samples[i,]$"Position" %>% substring(., 2)
        }
        
        # Find move data that matches sample data
        m <- which(stacked_orphaned_sample_data$barcode == eval.barcode)
        
        # Use move data to place sample into proper container
        stacked_orphaned_sample_data[m, "plate_id"] <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), plate_name == container_name)$id 
        
        # Use move data to place sample into proper container position
        stacked_orphaned_sample_data[m, "well_position"] <- paste0(eval.well, eval.pos)
      }
      else if(sample_type == "cryovial"){
        
        # Find move data that matches sample data
        m <- which(stacked_orphaned_sample_data$label == samples[i,]$"label")
        
        # Use move data to place sample into proper container
        stacked_orphaned_sample_data[m, "box_id"] <- filter(sampleDB::CheckTable(database = database, "box"), box_name == container_name)$id 
        
        # Use move data to place sample into proper container position
        stacked_orphaned_sample_data[m, "box_position"] <- paste0(samples[i,]$"row", samples[i,]$"col")
      }
      else{
        
        # Find move data that matches sample data
        m <- which(stacked_orphaned_sample_data$label == samples[i,]$"label")
        
        # Use move data to place sample into proper container
        stacked_orphaned_sample_data[m, "bag_id"] <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == container_name)$id 
      }
    }
  }
  
  if(sample_type == "micronix"){
    # check if there are any samples with the same barcode
    stopifnot("AT LEAST TWO SAMPLES HAVE THE SAME BARCODE" = sum(duplicated(stacked_orphaned_sample_data$barcode)) == 0)
    
    if(!all(stacked_orphaned_sample_data$plate_id > 0)){
      # there are orphans left - move would produce orphans
      out <- list(orphan_check_toggle = FALSE, stacked_orphaned_sample_data = stacked_orphaned_sample_data)
      
    }else{
      # there are no orphans left - move would not produce orphans
      out <- list(orphan_check_toggle = TRUE, stacked_orphaned_sample_data = stacked_orphaned_sample_data)
    }
  }
  else if(sample_type == "cryovial"){
    # check if there are any samples with the same label
    stopifnot("AT LEAST TWO SAMPLES HAVE THE SAME LABEL" = sum(duplicated(stacked_orphaned_sample_data$label)) == 0)
    
    if(!all(stacked_orphaned_sample_data$box_id > 0)){
      # there are orphans left - move would produce orphans
      out <- list(orphan_check_toggle = FALSE, stacked_orphaned_sample_data = stacked_orphaned_sample_data)
      
    }else{
      # there are no orphans left - move would not produce orphans
      out <- list(orphan_check_toggle = TRUE, stacked_orphaned_sample_data = stacked_orphaned_sample_data)
    }
  }
  else{
    # check if there are any samples with the same barcode
    stopifnot("AT LEAST TWO SAMPLES HAVE THE SAME LABEL" = sum(duplicated(stacked_orphaned_sample_data$label)) == 0)
    
    if(!all(stacked_orphaned_sample_data$bag_id > 0)){
      # there are orphans left - move would produce orphans
      out <- list(orphan_check_toggle = FALSE, stacked_orphaned_sample_data = stacked_orphaned_sample_data)
      
    }else{
      # there are no orphans left - move would not produce orphans
      out <- list(orphan_check_toggle = TRUE, stacked_orphaned_sample_data = stacked_orphaned_sample_data)
    }
  }
  
  return(out)
}

.ClearSpaceInContainers <- function(sample_type, move_data_list, database, conn){
  
  # for each sample, change container id to be a negative number (negative numbers are temporary containers)
  for(i in 1:length(names(move_data_list))){
    container.name <- names(move_data_list)[i]
    
    if(sample_type == "micronix"){
      
      # Get sample's container id
      existing.container <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), plate_name == container.name)$id
      
      # Make a reference df with all samples in container
      sample_data.existing_container <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), plate_id == existing.container)
      
      # Put samples into container with negative id number
      for(id in sample_data.existing_container$id){
        ModifyTable(database = database,
                    "matrix_tube",
                    info_list = list(plate_id = -(i)),
                    id = id,
                    conn = conn) %>% suppressWarnings()
      }
    }
    else if(sample_type == "cryovial"){
      # Get container id
      existing.container <- filter(sampleDB::CheckTable(database = database, "box"), box_name == container.name)$id
      
      # Make a reference df for all samples in container
      sample_data.existing_container <- filter(sampleDB::CheckTable(database = database, "tube"), box_id == existing.container)
      
      # Put samples into container with negative id number
      for(id in sample_data.existing_container$id){
        ModifyTable(database = database,
                    "tube",
                    info_list = list(box_id = -(i)),
                    id = id,
                    conn = conn) %>% suppressWarnings()
      }
    }
    else if(sample_type == "rdt"){
      # Get container id
      existing.container <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == container.name)$id
      
      # Make a reference df for all samples in container
      sample_data.existing_container <- filter(sampleDB::CheckTable(database = database, "rdt"), bag_id == existing.container)
      
      # Put samples into container with negative id number
      for(id in sample_data.existing_container$id){
        ModifyTable(database = database,
                    "rdt",
                    info_list = list(bag_id = -(i)),
                    id = id,
                    conn = conn) %>% suppressWarnings()
      }
    }
    else{
      # Get container id
      existing.container <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == container.name)$id
      
      # Make a reference df for all samples in container
      sample_data.existing_container <- filter(sampleDB::CheckTable(database = database, "paper"), bag_id == existing.container)
      
      # Put samples into container with negative id number
      for(id in sample_data.existing_container$id){
        ModifyTable(database = database,
                    "paper",
                    info_list = list(bag_id = -(i)),
                    id = id,
                    conn = conn) %>% suppressWarnings()
      }
    }
  }
}

.MoveSamples <- function(sample_type, move_data_list, database, conn){
  
  container_names <- c()
  number_samples <- c()

  # use move data to link samples with the proper container
  for(container_name in names(move_data_list)){
    container_names <- c(container_names, container_name)
    number_samples <- c(number_samples, nrow(container_name))
    
    # Get samples in container
    move_data <- move_data_list[[container_name]]
    
    # Get data for each sample
    for (i in 1:nrow(move_data)){
      if(sample_type == "micronix"){
        
        if("TubeCode" %in% names(move_data)){
          eval.barcode <- move_data[i,]$"TubeCode"
          eval.well <- move_data[i,]$"LocationRow"
          eval.pos <- move_data[i,]$"LocationColumn"
        }else{
          eval.barcode <- move_data[i,]$"Tube ID"
          eval.well <- move_data[i,]$"Position" %>% substring(., 1, 1)
          eval.pos <- move_data[i,]$"Position" %>% substring(., 2)
        }
        
        # get sample id
        id <- filter(sampleDB::CheckTable(database = database, "matrix_tube"), barcode == eval.barcode)$id
        
        # get container id
        eval.container_id <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), plate_name == container_name)$id 
        
        # link sample with container id
        ModifyTable(database = database,
                    "matrix_tube",
                    info_list = list(plate_id = eval.container_id,
                                     well_position = paste0(eval.well, eval.pos)),
                    id = id,
                    conn = conn) %>% suppressWarnings()
      }
      else if(sample_type == "cryovial"){
        # get sample id
        id <- filter(sampleDB::CheckTable(database = database, "tube"), label == move_data[i,]$"label")$id
        
        # get container id
        eval.container_id <- filter(sampleDB::CheckTable(database = database, "box"), box_name == container_name)$id
        
        # link sample with container id
        ModifyTable(database = database,
                    "tube",
                    info_list = list(box_id = eval.container_id,
                                     box_position = paste0(move_data[i,]$"row", move_data[i,]$"column")),
                    id = id,
                    conn = conn) %>% suppressWarnings()
      }
      else if(sample_type == "rdt"){
        # get sample id
        id <- filter(sampleDB::CheckTable(database = database, "rdt"), label == move_data[i,]$"label")$id
        
        # get container id
        eval.container_id <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == container_name)$id 
        
        # link sample with container id
        ModifyTable(database = database,
                    "rdt",
                    info_list = list(bag_id = eval.container_id),
                    id = id,
                    conn = conn) %>% suppressWarnings()
      }
      else{
        # get sample id
        id <- filter(sampleDB::CheckTable(database = database, "paper"), label == move_data[i,]$"label")$id
        
        # get container id
        eval.container_id <- filter(sampleDB::CheckTable(database = database, "bag"), bag_name == container_name)$id 
        
        # link sample with container id
        ModifyTable(database = database,
                    "paper",
                    info_list = list(bag_id = eval.container_id),
                    id = id,
                    conn = conn) %>% suppressWarnings()
      }
    }
  }

  message <- paste0("Sucessfully Moved Samples\n",
                    "\tType: ", sample_type, "\n",
                    "\tContainer Name: ", container_names, "\n",
                    "\tNumber of Samples: ", number_samples, "\n")
  return(message)
}

.GetOrphanedSamples <- function(sample_type, stacked_orphaned_sample_data, database){
  # GET LABEL STILL IN DUMMY PLATE
  if(sample_type == "micronix"){
    label.missing <- filter(stacked_orphaned_sample_data, plate_id < 0)$barcode
    
    # GET PLATE ID/PLATE NAME WHICH CONTAINED BARCODE STILL IN DUMMY
    container_id_with_missing_label <- filter(CheckTable(database = database, "matrix_tube"), barcode %in% label.missing)$plate_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "matrix_plate"), id %in% container_id_with_missing_label)$plate_name
  }
  else if(sample_type == "cryovial"){
    label.missing <- filter(stacked_orphaned_sample_data, box_id < 0)$label
    container_id_with_missing_label <- filter(CheckTable(database = database, "tube"), label %in% label.missing)$box_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "box"), id %in% container_id_with_missing_label)$box_name
  }
  else if(sample_type == "rdt"){
    label.missing <- filter(stacked_orphaned_sample_data, bag_id < 0)$label
    container_id_with_missing_label <- filter(CheckTable(database = database, "rdt"), label %in% label.missing)$bag_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "bag"), id %in% container_id_with_missing_label)$bag_name
  }
  else{
    label.missing <- filter(stacked_orphaned_sample_data, bag_id < 0)$label
    container_id_with_missing_label <- filter(CheckTable(database = database, "paper"), label %in% label.missing)$bag_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "bag"), id %in% container_id_with_missing_label)$bag_name
  }
  
  message.fail <- paste0("Move Failed:\n",
                         "\tOrphaned Samples Detected:", label.missing ,"\n",
                         "\tOf Type:", sample_type, "\n",
                         "\tFrom Container:", container_name_with_missing_label, "\n")
  return(message.fail)
}

.CopyContainersForTests <- function(move_data_list, sample_type, database){
  
  # set sample type variables
  if(sample_type == "micronix"){
    container_type <- "matrix_plate"
    sample_type <- "matrix_tube"
    colname.container_name <- "plate_name"
    colname.container_id <- "plate_id"
  }
  else if(sample_type == "cryovial"){
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
  
  # Extract sample level data from sampleDB (barcode, container position, container id) for containers involved in the move
  sample_data <- list()
  for(container.name in names(move_data_list)){
    tmp.container <- filter(sampleDB::CheckTable(database = database, container_type), !!as.name(colname.container_name) == container.name)
    stopifnot("CONTAINER IS NOT FOUND IN THE DATABASE" = nrow(tmp.container) != 0)
    eval.container_id <- tmp.container$id
    sample_data[[as.character(eval.container_id)]] <- filter(sampleDB::CheckTable(database = database, sample_type), !!as.name(colname.container_id) == eval.container_id)
  }
  return(sample_data)
}

.ClearSpaceForTests <- function(sample_data, sample_type){
  
  # change the container id for each sample
  for(i in 1:length(names(sample_data))){
    eval.container_id <- names(sample_data)[i]
    
    if(sample_type == "micronix"){
      sample_data[[eval.container_id]]$"plate_id" <- -(i)
    }
    else if(sample_type == "cryovial"){
      sample_data[[eval.container_id]]$"box_id" <- -(i)
    }
    else{
      sample_data[[eval.container_id]]$"bag_id" <- -(i)
    }
  }
  
  #stack sample test data
  stacked_orphaned_sample_data <- bind_rows(sample_data)
  
  return(stacked_orphaned_sample_data)
}

.SaveMoveCSVs <- function(move_data_list){
  path <- "/databases/sampledb/backups/sampleDB_user_uploaded_files/"
  for(container_name in names(move_data_list)){
    move_file <- move_data_list[[container_name]]
    if(dir.exists(path)){
      write.csv(move_file,
                paste0(path,
                       gsub(" ", "-", date()),
                       "_", container_name, "_",
                       "MOVE.csv"),
                row.names = FALSE)
    }
  } 
}