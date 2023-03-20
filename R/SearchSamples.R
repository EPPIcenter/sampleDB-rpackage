#' Search for Wetlab Samples in the EPPIcenter SampleDB database
#'
#' @description Search for wetlab samples.
#'
#' @param sample_type A string specifying the type of EPPIcenter sample. (`micronix`, `cryovile`, `rdt` and/or `paper`)
#' @param sample_label A list of vectors specifying the vector micronix, cryovile, rdt, and paper label names (`micronix.label_name`, `cryovile.label_name`, `rdt.label_name` and/or `paper.label_name`)
#' @param container_name A list of vectors specifying the vector micronix, cryovile, rdt, and paper container names (`micronix.container_name`, `cryovile.container_name`, `rdt.container_name` and/or `paper.container_name`)
#' @param study_subject A study subjects string or a vector of study name strings. If `study_subject.file` is TRUE the path to a .csv file containing one column named study_subject can be uploaded and used to search the database for study subjects.
#' @param specimen_type A specimen type string or a vector of specimen type strings.
#' @param study A study short code string or a vector of study short code strings.
#' @param collection_dates A list of date values strings (`date.to` and `date.from`) that can be used to filter EPPIcenter samples
#' @param archived A logical value. `TRUE` filters for archived samples and `FALSE` filters for active samples
#' @param freezer A list specifying the vector `name`, `level_I`, and/or`level_II`
#' @param return_sample_ids A logical value. Setting `return_sample_ids` to `TRUE` means `SearchSamples` returns sample ids as well as search results. Setting `return_sample_ids` to `FALSE` means `SearchSamples` returns only search results. Default value is `FALSE`.
#' @examples
#' \dontrun{
#' SearchSamples(study = "KAM06", study_subject = "subject_1")
#' }
#' @import dplyr
#' @import RSQLite
#' @import purrr
#' @export


SearchSamples <- function(sample_storage_type, filters = NULL, format = "na", database = Sys.getenv("SDB_PATH")) {

  browser()

  file_specs_json <- rjson::fromJSON(file = system.file("extdata", "file_specifications.json", package = .sampleDB$pkgname))

  ## Required Column Names

  file_index <- which(lapply(file_specs_json$file_types, function(x) x$id) == format)
  sample_storage_type_index <- which(lapply(file_specs_json$file_types[[file_index]]$sample_type, function(x) x$id) == sample_storage_type)

  if (length(sample_storage_type_index) == 0) {
    message("Unimplemented file specifications for this sample storage type.")
  } else {
    actions <- file_specs_json$file_types[[file_index]]$sample_type[[sample_storage_type_index]]$actions[['upload']]
    required_user_column_names <- actions[['required']]
    conditional_user_column_names <- actions[['conditional']]
    optional_user_column_names <- actions[['optional']]

    ## Shared fields
    sample_type_index <- which(lapply(file_specs_json$shared$sample_type, function(x) x$id) == sample_storage_type)

    required_user_column_names <- c(required_user_column_names, file_specs_json$shared$upload[['required']])

    conditional_user_column_names <- c(conditional_user_column_names, file_specs_json$shared$upload[['conditional']])
    optional_user_column_names <- c(optional_user_column_names, file_specs_json$shared$upload[['optional']])

    manifest_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$name
    manifest_barcode_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$barcode
    location_parameters <- file_specs_json$shared$sample_type[[sample_type_index]]$location
    location_parameters <- unlist(location_parameters[c("name", "level_I", "level_II")])

    example_data$user_input <- c(manifest_name, unname(location_parameters))
    optional_user_column_names <- c(optional_user_column_names, c(manifest_barcode_name))
  }

  container_tables <- list(
    "manifest" = switch(sample_storage_type,
      "1" = "micronix_plate",
      "2" = "cryovial_box",
      "3" = "dbs_paper"
    ),
    "container_class" = switch(sample_storage_type,
      "1" = "micronix_tube",
      "2" = "cryovial_tube",
      "3" = "dbs_spot"
    )
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), database)

  sql <- tbl(con, "study") %>% dplyr::rename(study_id = id)
  if (!is.null(filters$study_short_code)) {
    sql <- filter(sql, short_code == filters$short_code)
  }

  sql <- inner_join(sql, tbl(con, "study_subject") %>% dplyr::rename(study_subject_id = id, study_subject = name), by = c("study_id"))
  if (!is.null(filters$study_subject)) {
    sql <- filter(sql, study_subject == filters$study_subject)
  }

  sql <- inner_join(sql, tbl(con, "specimen") %>% dplyr::rename(specimen_id = id), by = c("study_subject_id"))

  if (!is.null(filters$collection_date) & sum(is.na(filters$collection_date)) == 0) {
    if (!is.null(filters$collection_date$date.from) & !is.null(filters$collection_date$date.to)) {
      intervals <- list()
      for (i in 1:length(filters$collection_date$date.from)) {
        intervals <- append(
          intervals,
          list(
            interval(
              lubridate::as_date(filters$collection_date$date.from[i]),
              lubridate::as_date(filters$collection_date$date.to[i])
            )
          )
        )
      }
    }

    sql <- filter(sql, collection_date %within% intervals)
  }

  sql <- inner_join(sql, tbl(con, "specimen_type") %>% dplyr::rename(specimen_type_id = id, specimen_type = name), by = c("specimen_type_id"))
  if (!is.null(filters$specimen_type)) {
    sql <- filter(sql, specimen_type == filters$specimen_type)
  }

  sql <- inner_join(sql, tbl(con, "storage_container") %>% dplyr::rename(storage_container_id = id), by = c("specimen_id"))
  sql <- inner_join(sql, tbl(con, "sample_type") %>% dplyr::rename(sample_type_id = id, sample_type_name = name), by = c("sample_type_id"))

  if (sample_storage_type != "all") {
    sql <- filter(sql, sample_type_id == sample_storage_type)
  }

  sql <- inner_join(sql, tbl(con, "status") %>% dplyr::rename(status_id = id, status = name), by = c("status_id"))
  sql <- inner_join(sql, tbl(con, "state") %>% dplyr::rename(state_id = id, state = name), by = c("state_id"))

  # note: should these always be set together?
  if (!is.null(filters$state) & is.null(filters$status)) {
    sql <- filter(sql, status == filters$status & state == filters$state)
  }


  if (sample_storage_type == "all") {
    sql <- inner_join(sql, tbl(con, "micronix_tube") %>% dplyr::rename(storage_container_id = id), by = c("storage_container_id"))
    sql <- inner_join(sql, tbl(con, "cryovial_tube") %>% dplyr::rename(storage_container_id = id), by = c("storage_container_id"))
    sql <- inner_join(sql, tbl(con, "dbs_spot") %>% dplyr::rename(storage_container_id = id), by = c("storage_container_id"))
  } else {
    sql <- inner_join(sql, tbl(con, container_tables[["container_class"]]) %>% dplyr::rename(storage_container_id = id), by = c("storage_container_id"))
  }

  # note: dbs does not have a barcode
  if (!is.null(filters$barcode) && sample_storage_type != 3) {
    sql <- filter(sql, barcode == filters$barcode)
  }

  if (sample_storage_type == "all") {
    sql <- inner_join(sql, tbl(con, "micronix_plate") %>% dplyr::rename(manifest_id = id, manifest = name), by = c("manifest_id"))
    sql <- inner_join(sql, tbl(con, "cryovial_box") %>% dplyr::rename(manifest_id = id, manifest = name), by = c("manifest_id"))
    sql <- inner_join(sql, tbl(con, "dbs_paper") %>% dplyr::rename(manifest_id = id, manifest = name), by = c("manifest_id"))
  } else {
    sql <- inner_join(sql, tbl(con, container_tables[["manifest"]]) %>% dplyr::rename(manifest_id = id, manifest = name), by = c("manifest_id"))
  }
  
  if (!is.null(filters$manifest)) {
    sql <- filter(sql, manifest == filters$manifest)
  }

  sql <- inner_join(sql, tbl(con, "location") %>% dplyr::rename(location_id = id, name = name), by = c("location_id"))

  if (!is.null(filters$location)) {
    sql <- filter(sql, location_name == filters$location[['name']] & level_I == filters$location[['level_I']] & level_II == filters$location[['level_II']])
  }

  # final mapping
  df <- data.frame()

  return (sql %>% select(names(filters)) %>% collect())
}

  