library(yaml)
# Utility Functions that are shared between Pkg Funs and Shiny App Funs
# Use ::: to access, these functions are not package exports

.Backup <- function(database, backup_dest) {

  args <- paste(paste0("\"", database, "\""), paste0("\".backup ", paste0("\'", backup_dest, "\'"), "\""))
  system2("sqlite3", args)

  return(file.exists(backup_dest))
}

.Uncompress <- function(source, destination, buffer_size = 1e+07) {
  compressed_file <- paste0(source, ".gz")

  # From R.UTILS

  inn <- gzfile(source, open="rb")
  on.exit(if (!is.null(inn)) close(inn))

  outComplete <- FALSE
  out <- file(destination, open="wb")
  on.exit({
    if (!is.null(out)) close(out)
    # Remove incomplete file?
    if (!outComplete) file.remove(destination)
  }, add=TRUE)

  # Process
  nbytes <- 0
  repeat {
    bfr <- readBin(inn, what=raw(0L), size=1L, n=buffer_size)
    n <- length(bfr)
    if (n == 0L) break
    nbytes <- nbytes + n
    writeBin(bfr, con=out, size=1L)
    bfr <- NULL  # Not needed anymore
  }
  outComplete <- TRUE
  close(out)
  out <- NULL

  return(file.exists(destination))
}


.Compress <- function(source, destination, buffer_size = 1e+07) {
  is_windows <- (Sys.info()[['sysname']] == "Windows")
  compressed_file <- paste0(destination, ".gz")

  # From R.UTILS

  # Setup input and output connections

  inn <- file(source, open="rb")
  on.exit(if (!is.null(inn)) close(inn))

  outComplete <- FALSE
  out <- gzfile(compressed_file, open="wb")
  on.exit({
    if (!is.null(out)) close(out)
    # Remove incomplete file?
    if (!outComplete) {
      file.remove(compressed_file)
    }
  }, add=TRUE)

  # Process
  nbytes <- 0
  repeat {
    bfr <- readBin(inn, what=raw(0L), size=1L, n=buffer_size)
    n <- length(bfr)
    if (n == 0L) break
    nbytes <- nbytes + n
    writeBin(bfr, con=out, size=1L)
    bfr <- NULL  # Not needed anymore
  }
  outComplete <- TRUE
  close(out)
  out <- NULL

  return(file.exists(compressed_file))
}


.CheckUploadContainerBarcodeDuplication <- function(plate_barcode, database){

  if(plate_barcode != "" && !is.null(plate_barcode)){
    out <- all(!(plate_barcode %in% c(CheckTable(database = database, "micronix_plate")$plate_barcode)))
  }else{
    out <- TRUE
  }
  return(out)
}
#Freezer Checks
.CheckFreezerNameIsUnique <- function(input, database, freezer_address){

  freezer_address_dup_test <- filter(CheckTable("location"),
                                     name == freezer_address$freezer_name,
                                     level_I == freezer_address$freezer_levelI,
                                     level_II == freezer_address$freezer_levelII) %>% nrow()

  if(freezer_address_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

.CheckFreezerDeletion <- function(input, database, freezer_address){
  num_items_at_address <- 0

  freezer_address <- filter(CheckTable(database = database, "location"),
                            name == freezer_address$freezer_name,
                            level_I == freezer_address$freezer_levelI,
                            level_II == freezer_address$freezer_levelII)
  if(length(freezer_address$id) > 0){
    items_at_address <- filter(CheckTable(database = database, "micronix_plate"), location_id == freezer_address$id)
    num_items_at_address <- items_at_address %>% nrow()
  }
  if(num_items_at_address > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

#Specimen Type Check
.CheckSpecimenTypeUnique <- function(input, database, specimen_type){
  specimen_type_dup_test <- filter(CheckTable(database = database, "specimen_type"), name == specimen_type) %>% nrow()
  if(specimen_type_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}


.CheckSpecimenTypeDeletion <- function(input, database, specimen_type){

  num_items_of_specimen_type <- 0
  specimen_type <- filter(CheckTable(database = database, "specimen_type"), name == specimen_type)
  if(length(specimen_type$id) > 0){
    num_items_of_specimen_type <- filter(CheckTable(database = database, "specimen"), specimen_type_id == specimen_type$id) %>% nrow()
  }
  if(num_items_of_specimen_type > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

#Study Check
.CheckStudyTitleIsUnique <- function(study_title, test, input, database){
  study_title_dup_test <- filter(CheckTable(database = database, "study"), title == study_title) %>% nrow()
  if(study_title_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

.CheckStudyShortCodeIsUnique <- function(study_short_code, test, input, database){
  study_short_code_dup_test <- filter(CheckTable(database = database, "study"), short_code == study_short_code) %>% nrow()
  if(study_short_code_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

.CheckStudyDeletion <- function(study_ui, input, database){
  num_items_of_studies <- 0
  studies <- filter(CheckTable(database = database, "study"), short_code == study_ui)
  if(length(studies$id) > 0){
    num_items_of_studies <- filter(CheckTable(database = database, "study_subject"), study_id == studies$id) %>% nrow()
  }
  if(num_items_of_studies > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

#upload a new micronix plate
.UploadPlate <- function(conn, container_name, container_barcode, freezer_address, table){
  eval.location_id <- filter(CheckTableTx(conn = conn, "location"), name == freezer_address$location, level_I == freezer_address$level_I, level_II == freezer_address$level_II)$id
  if(is.null(container_barcode) | is.na(container_barcode)) {
    container_barcode <- NA
  }
  else if(container_barcode == "" | container_barcode == "NA") {
    container_barcode <- NA
  }
  else{
    container_barcode <- container_barcode
  }

  AddToTable(table,
                       list(created = lubridate::now() %>% as.character(),
                            last_updated = lubridate::now() %>% as.character(),
                            location_id = eval.location_id,
                            name = container_name,
                            barcode = container_barcode),
                       conn = conn) %>% suppressWarnings()

  eval.plate_id <- tail(CheckTableTx(conn = conn, table), 1)$id

  return(eval.plate_id)

}


.ViewArchiveStatuses <- function(database) {
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
  out_table <- RSQLite::dbGetQuery(conn, "SELECT * FROM view_archive_statuses") %>% tibble()
  RSQLite::dbDisconnect(conn)
  return(out_table)
}
