
library(yaml)
# Utility Functions that are shared between Pkg Funs and Shiny App Funs
# Use ::: to access, these functions are not package exports

.Backup <- function(database, backup_dest) {

  args <- paste(database, paste0("\".backup ", paste0("\'", backup_dest, "\'"), "\""))
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


# Storage Type Check
.CheckSampleStorageType <- function(sample_type){
  out <- sample_type %in% c("micronix")
  return(out)
}

# Logistical Checks
.CheckLogisticalColnamesOfUserProvidedMicronixFile <- function(upload_file_type, users_upload_file){

  if(upload_file_type == "visionmate"){
    users_upload_file <- users_upload_file %>% setNames(.[1,]) %>% .[-c(1),]
    required_visionmate_colnames <- c("LocationRow", "LocationColumn", "TubeCode")
    visionmate_colnames_withdate <- c(required_visionmate_colnames, "CollectionDate")
    out <- all(required_visionmate_colnames %in% names(users_upload_file)) || all(visionmate_colnames_withdate %in% names(users_upload_file))
  }
  else if(upload_file_type == "traxcer"){
    config <- yaml::read_yaml(Sys.getenv("SDB_CONFIG"))
    traxcer_position <- ifelse(
      is.na(config$traxcer_position$override),
      config$traxcer_position$default,
      config$traxcer_position$override
    )

    users_upload_file <- users_upload_file %>% setNames(.[2,]) %>% .[-c(1, 2),]
    required_traxcer_colnames <- c(traxcer_position, "Tube ID")
    traxcer_colnames_withdate <- c(required_traxcer_colnames, "CollectionDate")
    out <- all(required_traxcer_colnames %in% names(users_upload_file)) || all(traxcer_colnames_withdate %in% names(users_upload_file))
  }
  else{
    users_upload_file <- users_upload_file %>% setNames(.[1,]) %>% .[-c(1),]
    general_colnames <- c("Barcode", "Row", "Column")
    out <- all(general_colnames %in% names(users_upload_file))
  }
  return(out)
}

.CheckFormattedLogisticalColnames <- function(formatted_upload_file){
  valid_logistical_colnames <- c("well_position", "label")
  out <- all(valid_logistical_colnames %in% names(formatted_upload_file))

  return(out)
}

.CheckBarcodeIsntInDB <- function(database, formatted_upload_file){

  upload_barcodes <- formatted_upload_file %>% pull(label)
  barcodes.existing <- upload_barcodes[which(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))]
  out1 <- all(!(upload_barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)))
  out <- list(out1 = out1, out2 = barcodes.existing)
  return(out)
}

.CheckWellPositionIsValid <- function(database, formatted_upload_file) {

  # just accept empty file as okay
  if (nrow(formatted_upload_file) == 0) {
    out1 <- TRUE
    out2 <- list(
      invalid_row = character(),
      invalid_col = character(),
      duplicated = character()
    )
    out <- list(out1 = out1, out2 = out2)
    return(out)
  }

  well_positions <- formatted_upload_file %>% pull(well_position)

  # check row letters
  row_letter_check <- substr(well_positions, 1, 1) %in% LETTERS

  # make sure the columns are numbers and above zero
  col_numbers <- substr(well_positions, 2, nchar(well_positions))
  col_number_indices <- which(col_numbers %>%
    as.numeric() %>%
    suppressWarnings() > 0)

  # check for duplicates
  well_dups_check <- !duplicated(well_positions)

  out1 <- all(row_letter_check) && length(col_number_indices) == length(well_positions) && all(well_dups_check)

  if(length(col_number_indices)) {
    inval_cols <- well_positions[-col_number_indices]
  } else {
    inval_cols <- well_positions
  }

  out2 <- list(
    invalid_row = well_positions[!row_letter_check],
    invalid_col = inval_cols,
    duplicated = well_positions[!well_dups_check]
  )

  out <- list(out1 = out1, out2 = out2)
  return(out)
}

.CheckBarcodeArentRepeated <- function(database, formatted_upload_file){

  upload_barcodes <- formatted_upload_file %>% pull(label)
  dups <- upload_barcodes[duplicated(upload_barcodes)]
  out1 <- length(dups) == 0
  out <- list(out1 = out1, out2 = dups)
  return(out)
}

.CheckAllSamplesAreActive <- function(database, tokens, type) {

  switch(type,
  micronix={
    return(all(
        sampleDB::CheckTable(database = database, "matrix_tube") %>%
        inner_join(sampleDB::CheckTable(database = database, "storage_container"), by = c("id" = "id")) %>%
        filter(state_id == 1 & barcode %in% tokens) %>% nrow() == length(tokens)
      )
    )
  },
  {
    stopifnot("*** ERROR: .CheckAllSamplesAreActive() type unimplemented!!!" = FALSE)
  })
}

.CheckBarcodesInDatabase <- function(database = database, formatted_move_file_list = formatted_move_file_list){

  out_vector <- c()
  barcodes_missing_from_database_list <- list()
  for(item in names(formatted_move_file_list)){
    barcodes <- formatted_move_file_list[[item]] %>% pull(label)
    missing_barcodes <- barcodes[!barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode)]
    if(length(missing_barcodes) > 0){
      barcodes_missing_from_database_list[[item]] <- missing_barcodes
    }else{
      barcodes_missing_from_database_list[[item]] <- NULL
    }

    out_item <- all(barcodes %in% c(sampleDB::CheckTable(database = database, "matrix_tube")$barcode))
    out_vector <- c(out_vector, out_item)
  }

  out <- list(out1 = all(out_vector), out2 = barcodes_missing_from_database_list)

  return(out)
}

# Metadata Checks
.CheckMetadataColnamesOfUserProvidedMicronixFile <- function(users_upload_file, upload_file_type){

  #establish required metadata column names
  names.base <- c("StudySubject", "SpecimenType", "StudyCode")

  if(upload_file_type == "traxcer"){
    users_upload_file <- users_upload_file %>% setNames(.[2,]) %>% .[-c(1, 2),]
    out <- all(names.base %in% names(users_upload_file))
  }
  else{
    users_upload_file <- users_upload_file %>% setNames(.[1,]) %>% .[-c(1),]
    out <- all(names.base %in% names(users_upload_file))
  }
  return(out)
}

.CheckFormattedMetaDataColnames <- function(formatted_upload_file){
  valid_metadata_colnames <- c("study_subject_id", "specimen_type", "study_short_code", "collection_date", "plate_barcode")
  out <- all(valid_metadata_colnames %in% names(formatted_upload_file))
  return(out)
}


.CheckUploadSpecimenTypes <- function(database, formatted_upload_file){

  specimen_types <- formatted_upload_file %>% pull(specimen_type)
  out <- all(specimen_types %in% sampleDB::CheckTable(database = database, table = "specimen_type")$label)
  return(out)
}

.CheckUploadStudyShortCodes <- function( database, formatted_upload_file){

  study_short_codes <- formatted_upload_file %>% pull(study_short_code)
  out <- all(study_short_codes %in% sampleDB::CheckTable(database = database, table = "study")$short_code)
  return(out)
}

.CheckUploadDateFormat <- function(database, formatted_upload_file){

  if("collection_date" %in% names(formatted_upload_file)){
    collection_dates <- formatted_upload_file %>% pull(collection_date)
    collection_dates <- collection_dates[!is.na(collection_dates)]
    out <- all(!is.na(parse_date_time(collection_dates, orders = "ymd")) == TRUE)
  }else{
    out <- TRUE
  }
  return(out)
}

# Plate Checks
.CheckUploadContainerNameDuplication <- function(plate_name, database, only_active = FALSE){

  out <- FALSE
  if (isTRUE(only_active)) {
    joined_tube_container_tables <- inner_join(sampleDB::CheckTable(database = database, "matrix_tube"),
      sampleDB::CheckTable(database = database, "storage_container"), by = c("id" = "id"))

    out <- all(!(plate_name %in% c(
      filter(joined_tube_container_tables, state_id == 1) %>% #Active
        inner_join(sampleDB::CheckTable(database = database, "matrix_plate"), by = c("plate_id" = "id")) %>%
        filter(plate_name == plate_name))$plate_name))

    return(out)
  } else {
    out <- all(!(plate_name %in% c(sampleDB::CheckTable(database = database, "matrix_plate")$plate_name)))
  }
  return(out)
}


# Freezer Address Check
.CheckFreezerAddress <- function(freezer_address, database){
  # check freezer address exists
  tmp.location.tbl <- inner_join(tibble(location_name = freezer_address$location_name, level_I = freezer_address$level_I, level_II = freezer_address$level_II),
                                 sampleDB::CheckTable(database = database, "location"),
                                 by = c("location_name", "level_I", "level_II"))
  out <- nrow(tmp.location.tbl) != 0
  return(out)
}

#Freezer Checks
.CheckFreezerNameIsUnique <- function(input, database, freezer_address){

  freezer_address_dup_test <- filter(sampleDB::CheckTable("location"),
                                     location_name == freezer_address$freezer_name,
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

  freezer_address <- filter(sampleDB::CheckTable(database = database, "location"),
                            location_name == freezer_address$freezer_name,
                            level_I == freezer_address$freezer_levelI,
                            level_II == freezer_address$freezer_levelII)
  if(length(freezer_address$id) > 0){
    items_at_address <- filter(sampleDB::CheckTable(database = database, "matrix_plate"), location_id == freezer_address$id)
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
  specimen_type_dup_test <- filter(sampleDB::CheckTable(database = database, "specimen_type"), label == specimen_type) %>% nrow()
  if(specimen_type_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}


.CheckSpecimenTypeDeletion <- function(input, database, specimen_type){

  num_items_of_specimen_type <- 0
  specimen_type <- filter(sampleDB::CheckTable(database = database, "specimen_type"), label == specimen_type)
  if(length(specimen_type$id) > 0){
    num_items_of_specimen_type <- filter(sampleDB::CheckTable(database = database, "specimen"), specimen_type_id == specimen_type$id) %>% nrow()
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
  study_title_dup_test <- filter(sampleDB::CheckTable(database = database, "study"), title == study_title) %>% nrow()
  if(study_title_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

.CheckStudyShortCodeIsUnique <- function(study_short_code, test, input, database){
  study_short_code_dup_test <- filter(sampleDB::CheckTable(database = database, "study"), short_code == study_short_code) %>% nrow()
  if(study_short_code_dup_test > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

.CheckStudyDeletion <- function(study_ui, input, database){
  num_items_of_studies <- 0
  studies <- filter(sampleDB::CheckTable(database = database, "study"), short_code == study_ui)
  if(length(studies$id) > 0){
    num_items_of_studies <- filter(sampleDB::CheckTable(database = database, "study_subject"), study_id == studies$id) %>% nrow()
  }
  if(num_items_of_studies > 0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  return(out)
}

#upload a new micronix plate
.UploadMicronixPlate <- function(conn, container_name, container_barcode, freezer_address){
  eval.location_id <- filter(CheckTableTx(conn = conn, "location"), location_name == freezer_address$location, level_I == freezer_address$level_I, level_II == freezer_address$level_II)$id
  if(is.null(container_barcode) | is.na(container_barcode)) {
    container_barcode <- NA
  }
  else if(container_barcode == "" | container_barcode == "NA") {
    container_barcode <- NA
  }
  else{
    container_barcode <- container_barcode
  }

  AddToTable("matrix_plate",
                       list(created = lubridate::now() %>% as.character(),
                            last_updated = lubridate::now() %>% as.character(),
                            location_id = eval.location_id,
                            plate_name = container_name,
                            plate_barcode = container_barcode),
                       conn = conn) %>% suppressWarnings()

  eval.plate_id <- tail(sampleDB::CheckTableTx(conn = conn, "matrix_plate"), 1)$id

  return(eval.plate_id)

}


.ViewArchiveStatuses <- function(database) {
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)
  out_table <- RSQLite::dbGetQuery(conn, "SELECT * FROM view_archive_statuses") %>% tibble()
  RSQLite::dbDisconnect(conn)
  return(out_table)
}
