#' Upload Wetlab Samples 
#'
#' `UploadSamples()` can be used to upload wetlab samples to the sampleDB database.
#'
#' @param sample_type A string specifying the type of samples that are being uploaded Options include: `micronix`, `cryovial` and `dbs`
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

UploadSamples <- function(sample_type_id, upload_data) {

  # locate the database and connect to it
  database <- Sys.getenv("SDB_PATH")
  conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)

  # save a copy of the upload data as a csv
  # .SaveUploadCSV(upload_data)

  # upload data
  .UploadSamples(upload_data = upload_data, sample_type_id = sample_type_id, conn = conn)

  return_message <- paste("Upload Successful!", nrow(upload_data), "sample(s) were uploaded.")

  #close connection
  tryCatch(
    RSQLite::dbDisconnect(conn),
    warning=function(w){})

  message(return_message)
  return(return_message)
}

.UploadSamples <- function(upload_data, sample_type_id, conn){
  RSQLite::dbBegin(conn)
  for(i in 1:nrow(upload_data)){
    #1. get upload item's metadata
    eval.specimen_type <- upload_data[i, ]$"specimen_type" %>% as.character()
    eval.study_code <- upload_data[i, ]$"study_short_code" %>% as.character()
    eval.subject <- upload_data[i, ]$"study_subject" %>% as.character()
    eval.barcode <- upload_data[i,]$"barcode" %>% as.character()
    eval.position <- upload_data[i,]$"position"
    eval.comment <- upload_data[i,]$"comment" %>% as.character()
    eval.plate_barcode <- upload_data[i,]$"manifest_barcode" %>% as.character()
    eval.container_name <- upload_data[i,]$"manifest_name" %>% as.character()
    eval.freezer_address <- list(
      location = upload_data[i,]$"name",
      level_I = upload_data[i,]$"level_I",
      level_II = upload_data[i,]$"level_II"
    )

    if(is.na(upload_data[i, ]$"collection_date")){
      eval.collection_date <- as.character(lubridate::as_date(NA))
    }else{
      eval.collection_date <- ymd(upload_data[i, ]$"collection_date")
      eval.collection_date <- as.character(lubridate::as_date(paste(year(eval.collection_date), month(eval.collection_date), day(eval.collection_date), sep = "-")))
    }

    #get a database id for a upload item's specimen_type and study
    eval.specimen_type_id <- filter(CheckTableTx(conn = conn, "specimen_type"), name == eval.specimen_type)$id
    eval.study_id <- filter(CheckTableTx(conn = conn, "study"), short_code == eval.study_code)$id

    #2a. check if this upload item's StudySubject (subject+study combination) exists in the database
    tmp_table.study_subject <- inner_join(CheckTableTx(conn = conn, "study_subject")[, c("name", "study_id")],
                                          tibble(name = eval.subject, study_id = eval.study_id),
                                          by = c("name", "study_id"))

    #if this upload item's StudySubject exists in the database, then get the necessary "study_subject" id
    if(nrow(tmp_table.study_subject) > 0){
      eval.study_subject_id <- filter(CheckTableTx(conn = conn, "study_subject"), name == eval.subject, study_id == eval.study_id)$id

      #3a. check if this sample exists (subject+study+specimen_type) in the database
      tmp_table.specimen <- inner_join(CheckTableTx(conn = conn, "specimen")[,c("study_subject_id", "specimen_type_id", "collection_date")],
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
                              sample_type_id = sample_type_id,
                              specimen_id = eval.specimen_id,
                              status_id = eval.status_id, # In Use
                              state_id = eval.state_id,
                              comment = eval.comment),
                         conn = conn) %>% suppressWarnings()

    #5. get just item's storage container id and use it as the primary key in the matrix cryovial_tube table
    eval.id <- tail(CheckTableTx(conn = conn, "storage_container"), 1)$id

    # 6. Create new sample housing (if it does not alread exist) and upload samples into housing

    # micronix == 1
    if(sample_type_id == 1){
      # create a new housing (if it does not already exist)
      if(!eval.container_name %in% CheckTableTx(conn = conn, "micronix_plate")$name){
        eval.plate_id <- .UploadPlate(conn = conn, container_name = eval.container_name, container_barcode = eval.plate_barcode, freezer_address = eval.freezer_address, table = "micronix_plate")
      }else{
        eval.plate_id <- filter(CheckTableTx(conn = conn, "micronix_plate"), name == eval.container_name)$id
      }

      # 7. upload micronix sample
      AddToTable(table_name = "micronix_tube",
                           info_list = list(id = eval.id,
                                            manifest_id = eval.plate_id,
                                            position = eval.position,
                                            barcode = eval.barcode),
                           conn = conn) %>% suppressWarnings()
    }
    # cryovial == 2
    else if (sample_type_id == 2) {
     # create a new housing (if it does not already exist)
      if(!eval.container_name %in% CheckTableTx(conn = conn, "cryovial_box")$name){
        eval.plate_id <- .UploadPlate(conn = conn, container_name = eval.container_name, container_barcode = eval.plate_barcode, freezer_address = eval.freezer_address, table = "cryovial_box")
      }else{
        eval.plate_id <- filter(CheckTableTx(conn = conn, "cryovial_box"), name == eval.container_name)$id
      }

      # 7. upload micronix sample
      AddToTable(table_name = "cryovial_tube",
                           info_list = list(id = eval.id,
                                            manifest_id = eval.plate_id,
                                            position = eval.position,
                                            barcode = eval.barcode),
                           conn = conn) %>% suppressWarnings()

    } else if (sample_type_id == 3) {
     # create a new housing (if it does not already exist)
      if(!eval.container_name %in% CheckTableTx(conn = conn, "dbs_paper")$name){
        eval.plate_id <- .UploadPlate(conn = conn, container_name = eval.container_name, container_barcode = eval.plate_barcode, freezer_address = eval.freezer_address, table = "dbs_paper")
      }else{
        eval.plate_id <- filter(CheckTableTx(conn = conn, "dbs_paper"), name == eval.container_name)$id
      }

      df.payload <- data.frame(
        id = eval.id,
        manifest_id = eval.plate_id,
        position = upload_data[i,]$position %>% as.character(),
        strain = upload_data[i,]$strain %>% as.character(),
        `0.05` = upload_data[i,]$`0.05` %>% as.character(),
        `0.1` = upload_data[i,]$`0.1` %>% as.character(),
        `1` = upload_data[i,]$`1` %>% as.character(),
        `10` = upload_data[i,]$`10` %>% as.character(),
        `100` = upload_data[i,]$`100` %>% as.character(),
        `1k` = upload_data[i,]$`1k` %>% as.character(),
        `10k` = upload_data[i,]$`10k` %>% as.character(),
        check.names = FALSE
      )

      DBI::dbAppendTable(conn, "dbs_spot", df.payload)
    }
  }

  RSQLite::dbCommit(conn)

  #close connection
  tryCatch(
    RSQLite::dbDisconnect(conn),
    warning=function(w){
      message(w)
    })

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
