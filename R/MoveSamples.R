#' Move Wetlab Samples
#'
#' `MoveSamples()` can be used to move existing wetlab samples from:
#' 1. Between two (or more) existing containers
#' 2. From one (or more) existing container into one (or more) existing container
#'
#' @param sample_type A string specifying the type of samples that are being moved. Options include: `micronix`, `cryovial`, `whole_blood`, `rdt`, and `paper`.
#' @param move_data A list of SampleDB move dataframes, where the name of each dataframe item is the container that the samples are in after the move.
#'
#' @examples
#' \dontrun{
#' move_data <- list("move_csv1_container_name" = dataframe(), "move_csv2_container_name" = dataframe())
#' MoveSamples(sample_type = "micronix", move_data = move_data)
#' }
#' @import dplyr
#' @import RSQLite
#' @import lubridate
#' @export
MoveSamples <- function(sample_type, move_data){

  database <- Sys.getenv("SDB_PATH")
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
  RSQLite::dbBegin(conn)

  # Save MoveCSVs
  .SaveMoveCSVs(move_data)

  if (sample_type == "dbs_sheet") {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    on.exit(dbDisconnect(con))

    # Iterate over each tube and update its location in the database
    for (i in 1:nrow(move_data)) {
      tube_id <- move_data$id[i]

      # SQL query to update the cryovial_box_id field for the specific tube
      query <- paste0("UPDATE whole_blood_tube SET cryovial_box_id = ", target_cryovial_box_id, " WHERE id = ", tube_id)
      dbExecute(con, query)
    }
  } else { # The sample types below need to be checked for orphans

    # Check if move creates orphans - returns TRUE if pass, FALSE if fail
    orphan_check_return <- .CheckForOrphans(move_data_list = move_data, database, sample_type = sample_type)

    if (orphan_check_return$error == TRUE) {
      return(paste("Move failed due to error."))
    }

    # Link samples to containers in move
    if(orphan_check_return$orphan_check_toggle){

      # Move samples out of containers - place in container id with negative number
      .ClearSpaceInContainers(sample_type = sample_type, move_data_list = move_data, conn = conn)

      # Link samples to containers in move
      message.successful <- .MoveSamples(sample_type = sample_type, move_data_list = move_data, database = database, conn = conn)

      message(message.successful)

      tryCatch({
        RSQLite::dbCommit(conn)
        RSQLite::dbDisconnect(conn)
        },
        warning=function(w){})

      return(message.successful)

    } else {

      # print samples that would be orphaned by move
      message.fail <- .GetOrphanedSamples(sample_type = sample_type, stacked_orphaned_sample_data = orphan_check_return$stacked_orphaned_sample_data, database = database)

      # Close connection
      tryCatch(
        RSQLite::dbDisconnect(conn),
        warning=function(w){})

      message(message.fail)

      return(message.fail)
    }
  } # End orphan centric move functionality
}

.CheckForOrphans <- function(move_data_list, database, sample_type) {

  message("Checking for orphaned samples...")

  # For containers involved in the move, extract sample-level data from sampleDB (barcode, container position, container id)
  sample_data <- .CopyContainersForTests(move_data_list = move_data_list, sample_type = sample_type, database = database)

  # Change sample's container ids to negative numbers (i.e. remove samples from containers)
  stacked_orphaned_sample_data <- .ClearSpaceForTests(sample_data = sample_data, sample_type = sample_type)

  # Use move data to assign containers to samples
  for(container_name in names(move_data_list)) {

    # Get samples in container
    samples <- move_data_list[[container_name]]

    # Get sample data
    for (i in 1:nrow(samples)){
      if(sample_type == "micronix"){
        eval.barcode <- safe_extract(samples[i,], "Barcode", "Tube ID", "TubeCode")
        eval.well_pos <- safe_extract(samples[i,], "Position", "ExtractedDNAPosition")

        # Find move data that matches sample data
        m <- which(stacked_orphaned_sample_data$barcode == eval.barcode)

        # Use move data to place sample into proper container
        stacked_orphaned_sample_data[m, "manifest_id"] <- filter(CheckTable(database = database, "micronix_plate"), name == container_name)$id

        # Use move data to place sample into proper container position
        stacked_orphaned_sample_data[m, "position"] <- eval.well_pos

      } else if (sample_type == "cryovial" || sample_type == "whole_blood") {

        eval.barcode <- safe_extract(samples[i,], "Barcode", "Tube ID", "TubeCode")
        eval.well_pos <- safe_extract(samples[i,], "Position", "ExtractedDNAPosition")

        # Find move data that matches sample data
        m <- which(stacked_orphaned_sample_data$barcode == eval.barcode)

        # Use move data to place sample into proper container
        column_name <- if (sample_type == "whole_blood") "cryovial_box_id" else "manifest_id"
        stacked_orphaned_sample_data[m, column_name] <- filter(CheckTable(database = database, "cryovial_box"), name == container_name)$id

        # Use move data to place sample into proper container position
        stacked_orphaned_sample_data[m, "position"] <- eval.well_pos
      } else {
        stop("Invalid Sample Type!!!")
      }
    }
  }

  # Check if there are any samples with the same barcode
  stopifnot("AT LEAST TWO SAMPLES HAVE THE SAME BARCODE" = sum(duplicated(stacked_orphaned_sample_data$barcode)) == 0)

  if(any(startsWith(stacked_orphaned_sample_data$position, '-'))) {
    # there are orphans left - move would produce orphans
    out <- list(error = FALSE, orphan_check_toggle = FALSE, stacked_orphaned_sample_data = stacked_orphaned_sample_data)
  } else {
    # there are no orphans left - move would not produce orphans
    out <- list(error = FALSE, orphan_check_toggle = TRUE, stacked_orphaned_sample_data = stacked_orphaned_sample_data)
  }

  return(out)
}

.ClearSpaceInContainers <- function(sample_type, move_data_list, conn) {

  # For each sample, change container id to be a negative number (negative numbers are temporary containers)
  for(i in 1:length(names(move_data_list))) {
    container.name <- names(move_data_list)[i]

    if(sample_type == "micronix"){

      # Get sample's container id
      existing.container <- filter(CheckTableTx(conn = conn, "micronix_plate"), name == container.name)$id

      # Make a reference df with all samples in container
      sample_data.existing_container <- filter(CheckTableTx(conn = conn, "micronix_tube"), manifest_id == existing.container) %>%
        inner_join(CheckTableTx(conn = conn, "storage_container"), by = c("id" = "id"))

      # Put samples into container with negative id number
      for(i in 1:nrow(sample_data.existing_container)) {
        state <- sample_data.existing_container[i,]$state_id
        if (!is.na(state) && !is_empty(state) && state == 1) {
          id <- sample_data.existing_container[i,]$id
          ModifyTable(conn = conn,
                      table_name = "micronix_tube",
                      info_list = list(position = paste0('-', (sample_data.existing_container[i,]$position))),
                      id = id) %>% suppressWarnings()
        }
      }

    } else if (sample_type == "cryovial" || sample_type == "whole_blood") {

      # Get sample's container id
      existing.container <- filter(CheckTableTx(conn = conn, "cryovial_box"), name == container.name)$id

      # Make a reference df with all samples in container
      sample_data.existing_container <- filter(CheckTableTx(conn = conn, paste0(sample_type, "_tube")), manifest_id == existing.container) %>%
        inner_join(CheckTableTx(conn = conn, "storage_container"), by = c("id" = "id"))

      # Put samples into container with negative id number
      for(i in 1:nrow(sample_data.existing_container)) {
        state <- sample_data.existing_container[i,]$state_id
        if (!is.na(state) && !is_empty(state) && state == 1) {
          id <- sample_data.existing_container[i,]$id
          ModifyTable(conn = conn,
                      table_name = paste0(sample_type, "_tube"),
                      info_list = list(position = paste0('-', (sample_data.existing_container[i,]$position))),
                      id = id) %>% suppressWarnings()
        }
      }
    } else {
      stop("Invalid Sample Type!!!")
    }
  }
}

.MoveSamples <- function(sample_type, move_data_list, database, conn){

  container_names <- c()
  number_samples <- c()

  # Use move data to link samples with the proper container
  for(container_name in names(move_data_list)){
    container_names <- c(container_names, container_name)

    # Get samples in container
    move_data <- move_data_list[[container_name]]
    number_samples <- c(number_samples, nrow(move_data))

    if(nrow(move_data) != 0){

      # Get data for each sample
      for (i in 1:nrow(move_data)){
        if(sample_type == "micronix"){
          eval.barcode <- safe_extract(move_data[i,], "Barcode", "Tube ID")
          eval.well_pos <- safe_extract(move_data[i,], "Position")

          # Get sample id
          id <- filter(CheckTable(database = database, "micronix_tube"), barcode == eval.barcode)$id

          # Get container id
          eval.container_id <- filter(CheckTable(database = database, "micronix_plate"), name == container_name)$id

          # Link sample with container id
          ModifyTable(conn = conn,
                      "micronix_tube",
                      info_list = list(manifest_id = eval.container_id,
                                       position = eval.well_pos),
                      id = id) %>% suppressWarnings()

        } else if (sample_type == "cryovial" || sample_type == "whole_blood") {
          eval.barcode <- safe_extract(move_data[i,], "Barcode")
          eval.well_pos <- safe_extract(move_data[i,], "Position")

          # Get sample id
          id <- filter(CheckTable(database = database, paste0(sample_type, "_tube")), barcode == eval.barcode)$id

          # Get container id
          column_name <- if (sample_type == "whole_blood") "cryovial_box_id" else "manifest_id"
          eval.container_id <- filter(CheckTable(database = database, "cryovial_box"), name == container_name)$id

          # Link sample with container id
          ModifyTable(conn = conn,
                      paste0(sample_type, "_tube"),
                      info_list = list(!!sym(column_name) := eval.container_id,
                                       position = eval.well_pos),
                      id = id) %>% suppressWarnings()
        } else {
          stop("Invalid Sample Type!!!")
        }
      }
    }
  }

  message <- paste0("Successfully Moved Samples\n",
                    "\tType: ", sample_type, "\n",
                    "\tContainer Name: ", container_names, "\n",
                    "\tNumber of Samples: ", number_samples, "\n")
  return(message)
}

.GetOrphanedSamples <- function(sample_type, stacked_orphaned_sample_data, database){
  # GET LABEL STILL IN DUMMY PLATE
  if(sample_type == "micronix") {
    remaining_well_positions <- grepl("-", stacked_orphaned_sample_data %>% pull(position), fixed = TRUE)
    label.missing <- stacked_orphaned_sample_data[remaining_well_positions, ] %>% pull(barcode)

    # GET PLATE ID/PLATE NAME WHICH CONTAINED BARCODE STILL IN DUMMY
    container_id_with_missing_label <- filter(CheckTable(database = database, "micronix_tube"), barcode %in% label.missing)$manifest_id
    container_name_with_missing_label <- filter(CheckTable(database = database, "micronix_plate"), id %in% container_id_with_missing_label)$name

  } else if (sample_type == "cryovial" || sample_type == "whole_blood") {
    remaining_well_positions <- grepl("-", stacked_orphaned_sample_data %>% pull(position), fixed = TRUE)
    label.missing <- stacked_orphaned_sample_data[remaining_well_positions, ] %>% pull(barcode)

    # GET PLATE ID/PLATE NAME WHICH CONTAINED BARCODE STILL IN DUMMY
    container_identifier <- if (sample_type == "whole_blood") "cryovial_box_id" else "manifest_id" 
    container_id_with_missing_label <- if (sample_type == "whole_blood") {
      filter(CheckTable(database = database, "whole_blood_tube"), barcode %in% label.missing) %>% pull(cryovial_tube_id)
    } else {
      filter(CheckTable(database = database, "cryovial_tube"), barcode %in% label.missing) %>% pull(manifest_id)      
    }
    container_name_with_missing_label <- filter(CheckTable(database = database, "cryovial_box"), id %in% container_id_with_missing_label)$name

  } else {
    stop("Invalid Sample Type!!!")
  }

  message.fail <- paste0("Move Failed:\n",
                         "\tOrphaned Samples Detected: ", label.missing ,"\n",
                         "\tType: ", sample_type, "\n",
                         "\tFrom Container: ", container_name_with_missing_label, "\n")
  return(message.fail)
}

.CopyContainersForTests <- function(move_data_list, sample_type, database){

  # Extract sample level data from sampleDB (barcode, container position, container id) for containers involved in the move
  # set sample type variables

  stopifnot("*** ERROR: Sample type move not implemented" = sample_type %in% c("micronix", "cryovial"))
  if(sample_type == "micronix"){
    container_type <- "micronix_plate"
    sample_type <- "micronix_tube"
    colname.container_name <- "name"
    colname.container_id <- "manifest_id"
  } else if (sample_type == "cryovial") {
    container_type <- "cryovial_box"
    sample_type <- "cryovial_tube"
    colname.container_name <- "name"
    colname.container_id <- "manifest_id" 
  } else {
    stop("Invalid Sample Type!!!")
  }

  tbl.plate_names <- CheckTable(database = database, container_type) %>%
    reframe(
      plate_name_matches := names(move_data_list) %in% get(colname.container_name)
    )

  if (!all(tbl.plate_names$plate_name_matches)) {
    stop("Container name not found in database") 
  }

  sample_data <- list()
  for(container.name in names(move_data_list)) {
    tmp.container <- filter(CheckTable(database = database, container_type), !!as.name(colname.container_name) == container.name)
    stopifnot("CONTAINER IS NOT FOUND IN THE DATABASE" = nrow(tmp.container) != 0)
    eval.container_id <- tmp.container$id
    # sample_data[[as.character(eval.container_id)]] <- filter(CheckTable(database = database, sample_type), !!as.name(colname.container_id) == eval.container_id)
    sample_data[[as.character(eval.container_id)]] <- CheckTable(database = database, sample_type) %>%
      inner_join(CheckTable(database = database, "storage_container"), by = c("id" = "id")) %>%
      filter(!!as.name(colname.container_id) == eval.container_id) # For moves, all containers retured (even inactives)
  }
  return(sample_data)
}

.ClearSpaceForTests <- function(sample_data, sample_type) {
  # change the container id for each sample
  for(i in 1:length(names(sample_data))) {
    eval.container_id <- names(sample_data)[i]

    #if the well position is NA, then the sample has been archived.
    #keeping plate information for the time being.
    if(0 < nrow(sample_data[[eval.container_id]])) {
      sample_data[[eval.container_id]] <- sample_data[[eval.container_id]] %>%
        filter(sample_type == "micronix" & !is.na(sample_data[[eval.container_id]]$position))

      if (nrow(sample_data[[eval.container_id]]) > 0)
        sample_data[[eval.container_id]] <- sample_data[[eval.container_id]] %>%
                 mutate(position = paste0('-', (sample_data[[eval.container_id]]$position)))
    }
  }

  #stack sample test data
  stacked_orphaned_sample_data <- bind_rows(sample_data)

  return(stacked_orphaned_sample_data)
}


.SaveMoveCSVs <- function(move_data_list){
  path <- normalizePath(
      file.path(dirname(Sys.getenv("SDB_PATH")), "move_files"))

  for(container_name in names(move_data_list)){
    move_file <- move_data_list[[container_name]]
    if(dir.exists(path)){
      write.csv(move_file,
        suppressWarnings(
          normalizePath(
            file.path(path,
                   paste0(gsub("[T:]", "_",
                      lubridate::format_ISO8601(lubridate::now())),
                   "_", container_name, "_",
                   "MOVE.csv")))),
            row.names = FALSE)
    }
  }
}
