#' Upload Wetlab Samples
#'
#' `UploadSamples()` can be used to upload wetlab samples to the sampleDB database.
#'
#' @param storage_type_id A string specifying the type of samples that are being uploaded Options include: `micronix`, `cryovial` and `dbs`
#' @param upload_data A dataframe of SampleDB Upload data.\cr
#' Required `upload_data` columns are:\cr
#' `position`: the row and column of the sample in the storage housing
#' `label`: the sample's label or barcode
#' `study_subject_id`: the StudySubject id of for the subject in the cohort (ie study)
#' `study_short_code`: the code of the study
#' `specimen_type`: the sample type
#' `collection_date`: (optional) the date the sample was first collected from the cohort StudySubject
#'
#' #' **upload data example without collection_date**
#'
#' | position | label | study_subject_id | specimen_type | study_short_code |
#' | ------------- | ----- | ---------------- | ------------- | ---------------- |
#' | A0            | xxx1  | subject_1        | PLASMA        | KAM06            |
#' | A1            | xxx2  | subject_2        | PLASMA        | KAM06            |
#'
#' **upload data example with collection_date**
#'
#' | position | label | study_subject_id | specimen_type | study_short_code | collection_data |
#' | ------------- | ----- | ---------------- | ------------- | ---------------- | --------------- |
#' | A0            | xxx1  | subject_1        | PLASMA        | KAM06            | 2022-04-11      |
#' | A1            | xxx2  | subject_2        | PLASMA        | KAM06            | 2022-04-11      |
#'
#' @param container_name A string specifying the name of the container the samples are in. Names must be unique within each sample type.
#' @param container_barcode A string specifying the barcode for the container the samples are in. Container barcodes are optional. Barcodes must be unique within each sample type.
#' @param freezer_address A list specifying the freezer address used to store samples. \cr
#' Required items in the freezer_address list are `name`, `level_I` and `level_II`.
#' If the freezer_address type is `minus eighty` then `level_I` and `level_II` items specify the rack and position, respecively.
#' If the freezer_address type is `minus twenty` then `level_I` and `level_II` items specify the shelf and basket, respecively.
#' @examples
#' \dontrun{
#' UploadSamples(sample_type = "micronix",
#'               upload_data = tibble(position = c("A0"),
#'                                    label = c("XXX 1"),
#'                                    study_subject_id = c("1"),
#'                                    specimen_type = c("PLASMA"),
#'                                    study_short_code = c("KAM06"),
#'                                    collection_date = c("2021-04-10")),
#'                container_name = "test_container",
#'                freezer_address = list(name = "TBD",
#'                                       level_I = "TBD",
#'                                       level_II = "seve's working basket"))
#' }
#' @import dplyr
#' @import RSQLite
#' @import lubridate
#' @export

# Or 'UploadContainers'?
# control extraction can be (DNA (DBS) or DNA (WB))
UploadSpecimens <- function(user_data, storage_type_id=NULL, database = Sys.getenv("SDB_PATH")) {

  # locate the database and connect to it
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
  return_message <- NULL

  dbBegin(conn)

  .UploadSpecimens(upload_data = user_data, sample_type_id = storage_type_id, conn = conn)

  message(sprintf("Upload Successful! %d sample(s) were uploaded.", nrow(user_data)))

  dbCommit(conn)
  dbDisconnect(conn)
}

#' Upload Extracted DNA Data to SampleDB
#'
#' This function uploads extracted DNA data from a user-provided data frame 
#' to a SQLite database, specifically to SampleDB. The function also updates the 
#' active control count and commits the changes to the database.
#'
#' @param user_data A data frame containing the DNA extraction data.
#' @param control_extraction A character string specifying the type of control extraction.
#' @param database A character string specifying the path to the SampleDB SQLite database. 
#'   Default value is the system environment variable "SDB_PATH".
#'
#' @return A message indicating the success of the data upload operation.
#' @importFrom RSQLite dbConnect SQLite dbBegin dbCommit dbDisconnect
#' @importFrom dplyr filter inner_join select rename
#' @seealso \code{\link{.UpdateActiveControlCount}} and \code{\link{.UploadSpecimens}} for helper functions.
#' @export
#' @examples
#' \dontrun{
#' upload_extracted_dna(user_data = my_data_frame, control_extraction = "dbs_sheet")
#' }
upload_extracted_dna <- function(user_data, control_extraction, database = Sys.getenv("SDB_PATH")) {

# locate the database and connect to it
  con <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
  return_message <- NULL

  dbBegin(con)

  .UploadSpecimens(user_data, "micronix", conn = con) # always uploading micronix samples if it's a dna extraction

  .UpdateActiveControlCount(user_data, control_extraction, con = con)

  message(sprintf("Upload Successful! %d extractions (s) were uploaded.", nrow(user_data)))

  dbCommit(con)
  dbDisconnect(con)
}


#' Update the Number of Extracted DNA Controls After Extraction
#'
#' This internal function updates the number of active controls for a given type
#' of DNA extraction after the extraction has been performed.
#'
#' @param user_data A data frame containing the DNA extraction data.
#' @param control_extraction A character string specifying the type of control extraction.
#' @param con A database connection object.
#'
#' @importFrom dplyr filter inner_join select rename group_by reframe
#' @importFrom DBI dbExecute
#' @importFrom RSQLite dbReadTable
#' @keywords internal
.UpdateActiveControlCount <- function(user_data, control_extraction, con) {

  if ("dbs_sheet" == control_extraction) {

    # Fetch data necessary for determining exhaustion status
    df.payload <- user_data %>%
      dplyr::inner_join(dbReadTable(con, "study") %>% dplyr::rename(study_id = id), by = c("Batch" = "short_code")) %>%
      dplyr::inner_join(dbReadTable(con, "study_subject") %>% dplyr::rename(study_subject_id = id, ControlUID = name), by = c("study_id", "ControlUID")) %>%
      dplyr::inner_join(dbReadTable(con, "malaria_blood_control") %>% dplyr::rename(malaria_blood_control_id=id), by = c("study_subject_id")) %>%
      dplyr::inner_join(dbReadTable(con, "blood_spot_collection") %>% dplyr::rename(blood_spot_collection_id = id), by = c("malaria_blood_control_id")) %>%
      select(blood_spot_collection_id, exhausted, total) %>%
      group_by(blood_spot_collection_id) %>%
      mutate(exhausted = exhausted + n()) %>%
      distinct()

    # Check the total and exhausted count for each blood spot collection,
    # and the exhausted spots to archived_dbs_blood_spots
    if (nrow(df.payload) > 0) {
      for (ii in 1:nrow(df.payload)) {
        dbExecute(con, sprintf("UPDATE blood_spot_collection SET exhausted = %d WHERE id = %d;", df.payload[ii,]$exhausted, df.payload[ii,]$blood_spot_collection_id))
        message(sprintf("Updated exhausted count for collection ID %d.", df.payload[ii,]$blood_spot_collection_id))
      }
    }

  } else if ("whole_blood" == control_extraction) {

    df.payload = user_data %>%
      inner_join(dbReadTable(con, "study") %>% dplyr::rename(study_id=id), by=c("Batch"="short_code")) %>%
      inner_join(dbReadTable(con, "study_subject") %>% dplyr::rename(study_subject_id=id, ControlUID=name), by = c("study_id", "ControlUID")) %>%
      inner_join(dbReadTable(con, "malaria_blood_control") %>% dplyr::rename(malaria_blood_control_id=id), by = c("study_subject_id")) %>%
      inner_join(dbReadTable(con, "whole_blood_tube") %>% dplyr::rename(whole_blood_tube_id=id), by = c("malaria_blood_control_id"))

    for (ii in 1:nrow(df.payload)) {
      DBI::dbExecute(con, sprintf("DELETE FROM whole_blood_tube WHERE id = %d;", df.payload[ii,]$whole_blood_tube_id))
    }
  }

  message(sprintf("%d controls updated", nrow(df.payload)))
}


#' Upload Specimens to SampleDB
#'
#' This internal function uploads specimen data to the SampleDB database. 
#' It validates the data and checks for existing entries before updating the database.
#'
#' @param upload_data A data frame containing the specimen data.
#' @param sample_type_id A character or numeric identifier for the sample type.
#' @param conn A database connection object.
#' @param extraction This is an extraction from a control, not a regular sample. 
#' This will enforce some checks to make sure we are not writing to the database 
#' when we should only be reading from certain tables. An example is the study subject table,
#' which we should only read from during an extraction.
#'
#' @importFrom dplyr filter inner_join select rename
#' @importFrom DBI dbReadTable
#' @importFrom lubridate now as_date ymd
#' @importFrom RSQLite dbReadTable
#' @keywords internal
.UploadSpecimens <- function(upload_data, sample_type_id, conn, extraction = FALSE) {

  for(i in 1:nrow(upload_data)) {
    eval.specimen_type <- safe_extract(upload_data[i, ], "SpecimenType")
    eval.study_code <- safe_extract(upload_data[i, ], "StudyCode", "Batch")
    eval.subject <- safe_extract(upload_data[i, ], "StudySubject", "ControlUID")

    # For fields that might exist under different names
    eval.barcode <- safe_extract(upload_data[i, ], "Barcode", "Tube ID", "TubeCode") 
    eval.position <- safe_extract(upload_data[i, ], "Position", "ExtractedDNAPosition")
    eval.comment <- safe_extract(upload_data[i, ], "Comment")

    eval.plate_barcode <- safe_extract(upload_data[i,], "PlateBarcode", "BoxBarcode")
    eval.container_name <- safe_extract(upload_data[i,], "PlateName", "BoxName", "ContainerName")

    eval.freezer_address <- list(
      location = safe_extract(upload_data[i,], "FreezerName", "FreezerName"),
      level_I = safe_extract(upload_data[i,], "ShelfName", "RackName"),
      level_II = safe_extract(upload_data[i,], "BasketName", "RackPosition")
    )
    eval.label <- safe_extract(upload_data[i, ], "Label")

    eval.collection_date <- safe_extract(upload_data[i, ], "CollectionDate", "ExtractedOn")
    eval.container_type <- safe_extract(upload_data[i, ], "ContainerType")

    # if(is.na(upload_data[i, ]$"collection_date")){
    #   eval.collection_date <- as.character(lubridate::as_date(NA))
    # }else{
    #   eval.collection_date <- ymd(upload_data[i, ]$"collection_date")
    #   eval.collection_date <- as.character(lubridate::as_date(paste(year(eval.collection_date), month(eval.collection_date), day(eval.collection_date), sep = "-")))
    # }

    #get a database id for a upload item's specimen_type and study
    eval.specimen_type_id <- filter(CheckTableTx(conn = conn, "specimen_type"), name == eval.specimen_type)$id
    eval.study_id <- filter(CheckTableTx(conn = conn, "study"), short_code == eval.study_code)$id

    #2a. check if this upload item's StudySubject (subject+study combination) exists in the database
    tmp_table.study_subject <- inner_join(CheckTableTx(conn = conn, "study_subject")[, c("name", "study_id")],
                                          tibble(name = eval.subject, study_id = eval.study_id),
                                          by = c("name", "study_id"))

    if (extraction && nrow(tmp_table.study_subject) == 0) {
      errmsg <- sprintf("No control (study subject) was detected for this extraction!!! [StudySubject = %s; Batch = %s]", eval.subject, eval.study_code)
      stop(errmsg)
    }

    #if this upload item's StudySubject exists in the database, then get the necessary "study_subject" id
    if(nrow(tmp_table.study_subject) > 0){
      eval.study_subject_id <- filter(CheckTableTx(conn = conn, "study_subject"), name == eval.subject, study_id == eval.study_id)$id

      #3a. check if this sample exists (subject+study+specimen_type) in the database
      tmp_table.specimen <- inner_join(dbReadTable(conn = conn, "specimen") %>%
                                        select("study_subject_id", "specimen_type_id", "collection_date") %>%
                                        dplyr::mutate(collection_date = as.character(collection_date)),
                                       tibble(study_subject_id = eval.study_subject_id, specimen_type_id = eval.specimen_type_id, collection_date = eval.collection_date),
                                       by = c("study_subject_id", "specimen_type_id", "collection_date"))

      #if this upload item's sample exists in the database, then get the necessary "specimen" id
      if(nrow(tmp_table.specimen) > 0){

        #how specimen id is retrieved depends on whether the collection date is NA or !is NA
        if(is.na(eval.collection_date)){
          eval.specimen_id <- filter(CheckTableTx(conn = conn, "specimen"),
                                     study_subject_id == eval.study_subject_id,
                                     specimen_type_id == eval.specimen_type_id,
                                     is.na(eval.collection_date))$id

        }else{
          eval.specimen_id <- filter(CheckTableTx(conn = conn, "specimen"),
                                     study_subject_id == eval.study_subject_id,
                                     specimen_type_id == eval.specimen_type_id,
                                     collection_date == eval.collection_date)$id
        }
      }else{

        #3b. if this StudySubject (study_subject) exists in the database but sample (specimen) does not, then create a new "specimen"
        AddToTable("specimen",
                             list(created = lubridate::now() %>% as.character(),
                                  last_updated = lubridate::now() %>% as.character(),
                                  study_subject_id = eval.study_subject_id,
                                  specimen_type_id = eval.specimen_type_id,
                                  collection_date = eval.collection_date),
                             conn = conn) %>% suppressWarnings()

        #retrieve newly made specimen id
        eval.specimen_id <- tail(CheckTableTx(conn = conn, "specimen"), 1)$id
      }
    }else{
      #2b. if this upload item's StudySubject (combination of subject+study) does not exist in the database then create a new study_subject entry in the database
      AddToTable("study_subject",
                           list(created = lubridate::now() %>% as.character(),
                                last_updated = lubridate::now() %>% as.character(),
                                name = eval.subject,
                                study_id = eval.study_id),
                           conn = conn) %>% suppressWarnings()

      # get the newly created study_subject id
      eval.study_subject_id <- tail(CheckTableTx(conn = conn, "study_subject"), 1)$id

      #3b. create a new sample (study+subject+specimen_type) (ie specimen) in the database
      AddToTable("specimen",
                           list(created = lubridate::now() %>% as.character(),
                                last_updated = lubridate::now() %>% as.character(),
                                study_subject_id = eval.study_subject_id,
                                specimen_type_id = eval.specimen_type_id,
                                collection_date = eval.collection_date),
                           conn = conn) %>% suppressWarnings()

      # get the newly created specimen id
      eval.specimen_id <- tail(CheckTableTx(conn = conn, "specimen"), 1)$id
    }

    # sample automatically in use
    eval.status_id <- filter(CheckTableTx(conn = conn, "status"), name %in% "In Use")$id
    eval.state_id <- filter(CheckTableTx(conn = conn, "state"), name %in% "Active")$id

    #4. create the new item's storage container to the database using the item's sample id

    AddToTable("storage_container",
                         list(created = lubridate::now() %>% as.character(),
                              last_updated = lubridate::now() %>% as.character(),
                              specimen_id = eval.specimen_id,
                              status_id = eval.status_id, # In Use
                              state_id = eval.state_id,
                              comment = eval.comment),
                         conn = conn) %>% suppressWarnings()

    #5. get just item's storage container id and use it as the primary key in the matrix cryovial_tube table
    eval.id <- tail(CheckTableTx(conn = conn, "storage_container"), 1)$id

    # 6. Create new sample housing (if it does not alread exist) and upload samples into housing

    # micronix == 1
    if(sample_type_id == "micronix"){
      # create a new housing (if it does not already exist)
      if(!eval.container_name %in% CheckTableTx(conn = conn, "micronix_plate")$name){
        eval.plate_id <- .UploadPlate(conn = conn, container_name = eval.container_name, container_barcode = eval.plate_barcode, freezer_address = eval.freezer_address, table = "micronix_plate")
      }else{
        eval.plate_id <- filter(CheckTableTx(conn = conn, "micronix_plate"), name == eval.container_name)$id
      }

      df <- data.frame(id = eval.id, manifest_id = eval.plate_id, position = eval.position, barcode = eval.barcode)
      DBI::dbAppendTable(conn, "micronix_tube", df)

    }
    # cryovial == 2
    else if (sample_type_id == "cryovial") {
     # create a new housing (if it does not already exist)
      if(!eval.container_name %in% CheckTableTx(conn = conn, "cryovial_box")$name){
        eval.plate_id <- .UploadPlate(conn = conn, container_name = eval.container_name, container_barcode = eval.plate_barcode, freezer_address = eval.freezer_address, table = "cryovial_box")
      }else{
        eval.plate_id <- filter(CheckTableTx(conn = conn, "cryovial_box"), name == eval.container_name)$id
      }

      # 7. upload micronix sample
      df <- data.frame(id = eval.id, manifest_id = eval.plate_id, position = eval.position, barcode = eval.barcode)
      DBI::dbAppendTable(conn, "cryovial_tube", df)

    } else if (sample_type_id == "dbs_sample") {

      now <- as.character(lubridate::now())

      # 7. upload micronix sample
      eval.manifest_type <- tolower(eval.container_type)
      eval.location_id <- filter(CheckTableTx(conn = conn, "location"), location_root == eval.freezer_address$location, level_I == eval.freezer_address$level_I, level_II == eval.freezer_address$level_II)$id

      # create a new housing (if it does not already exist)
      if(!eval.container_name %in% CheckTableTx(conn = conn, eval.manifest_type)$name){
        # df <- data.frame(created = now, last_updated = now, location_id = eval.location_id, name = eval.container_name)
        result <- dbGetQuery(conn, paste("INSERT INTO", eval.manifest_type, "(created, last_updated, location_id, name) VALUES (:created, :last_updated, :location_id, :name) RETURNING id"),
          list(created = now, last_updated = now, location_id = eval.location_id, name = eval.container_name))
        eval.plate_id <- result$id[1]
      } else{
        eval.plate_id <- filter(CheckTableTx(conn = conn, eval.manifest_type), name == eval.container_name)$id
      }

      df <- data.frame(id = eval.id, manifest_id = eval.plate_id, manifest_type = eval.manifest_type, label = eval.label)
      DBI::dbAppendTable(conn, "paper", df)
    } else if(sample_type_id == "static_plate") {
      # create a new housing (if it does not already exist)
      if(!eval.container_name %in% CheckTableTx(conn = conn, "micronix_plate")$name){
        eval.plate_id <- .UploadPlate(conn = conn, container_name = eval.container_name, container_barcode = eval.plate_barcode, freezer_address = eval.freezer_address, table = "micronix_plate")
      }else{
        eval.plate_id <- filter(CheckTableTx(conn = conn, "micronix_plate"), name == eval.container_name)$id
      }

      df <- data.frame(id = eval.id, manifest_id = eval.plate_id, position = eval.position)
      DBI::dbAppendTable(conn, "static_well", df)

    } else {
      stop("No upload implementation for this sample type")
    }
  }
}

.UploadEmptyManifest <- function(manifest_table, database, container_name, container_barcode, freezer_address) {
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
  RSQLite::dbBegin(conn)
  eval.location_id <- filter(CheckTable(database = database, "location"), name == freezer_address$name, level_I == freezer_address$level_I, level_II == freezer_address$level_II)$id
  if(is.null(container_barcode)){
    container_barcode <- NA
  }
  else if(container_barcode == ""){
    container_barcode <- NA
  }
  else{
    container_barcode <- container_barcode
  }
  # print(container_barcode)
  AddToTable(manifest_table,
                       list(created = lubridate::now() %>% as.character(),
                            last_updated = lubridate::now() %>% as.character(),
                            location_id = eval.location_id,
                            name = container_name,
                            barcode = container_barcode),
                       conn = conn) %>% suppressWarnings()
  eval.plate_id <- tail(CheckTable(database = database, manifest_table), 1)$id

  RSQLite::dbCommit(conn)
  RSQLite::dbDisconnect(conn)

  return(eval.plate_id)

}

.SaveUploadCSV <- function(upload_data){

  manifests <- unique(upload_data$manifest_name)

  path <- normalizePath(
      file.path(dirname(Sys.getenv("SDB_PATH")), "upload_files"))

  manifests <- stringr::str_replace_all(manifests, "/", "_")
  manifests <- stringr::str_replace_all(manifests, " ", "_")
  print(manifests)

  if(dir.exists(path)) {
    lapply(manifests, function(manifest) {
      write.csv(upload_data %>% filter(manifest_name == manifest),
        suppressWarnings(
          normalizePath(
            file.path(path,
                   paste0(gsub("[T:]", "_",
                      lubridate::format_ISO8601(lubridate::now())),
                   "_", manifest, "_",
                   "UPLOAD.csv")))),
            row.names = FALSE)
    })
  }
}


.UploadPlate <- function(conn, container_name, container_barcode, freezer_address, table){
  eval.location_id <- filter(CheckTableTx(conn = conn, "location"), location_root == freezer_address$location, level_I == freezer_address$level_I, level_II == freezer_address$level_II)$id
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
