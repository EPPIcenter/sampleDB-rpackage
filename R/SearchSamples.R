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
#' @export SearchByType SearchSamples SearchControls


SearchByType <- function(sample_storage_type=NULL, control_type=NULL, filters = NULL, format = NULL, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG"), include_internal_sample_id = FALSE) {

  if (sum(!is.null(sample_storage_type, control_type)) != 1) {
    stop("Only one of sample_storage_type and control_type may be set at a time.")
  }

  browser()

  search.results <- NULL
  if (!is.null(sample_storage_type)) {
    search.results = SearchSamples(
      sample_storage_type = sample_storage_type,
      filters = filters,
      format = format,
      database = database,
      config_yml = config_yml,
      include_internal_sample_id = include_internal_sample_id
    )
  } else {
    search.results = SearchControls(
      control_type = control_type,
      filters = filters,
      format = format,
      database = database,
      config_yml = config_yml,
      include_internal_sample_id = include_internal_sample_id
    )      
  }

  return(search.results)
}


SearchControls <- function(control_type, filters = NULL, format = NULL, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG"), include_internal_sample_id = FALSE) {
  
  db.results <- NULL

  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    df = tbl(con, "control_strain") %>%
      left_join(tbl(con, "strain") %>% dplyr::rename(strain_id = id, strain = name), by = c("strain_id")) %>%
      left_join(tbl(con, "control") %>% dplyr::rename(control_id = id), by = c("control_id")) %>%
      left_join(tbl(con, "study_subject") %>% dplyr::rename(study_subject_id = id, control_uid = name), by = c("control_id"="study_subject_id")) %>%
      left_join(tbl(con, "study") %>% dplyr::rename(study_id = id, batch_creation_date=created) %>% filter(!is.na(control_collection_id)), by = c("study_id")) %>%
      # select(control_uid, control_id, short_code, density, strain_id, strain, percentage) %>%
      dplyr::rename(batch=short_code) %>%
      distinct()

    if (!is.null(rv$filters$strain) && rv$filters$strain != "") {
      df = df %>% filter(strain_id %in% local(rv$filters$strain))
    }

    if (!is.null(rv$filters$density) && rv$filters$density != "") {
      df = df %>% filter(density %in% local(rv$filters$density))
    }

    if (!is.null(rv$filters$percentage) && rv$filters$percentage != "") {
      df = df %>% filter(percentage %in% local(rv$filters$percentage))
    }

    if (!is.null(rv$filters$batch) && rv$filters$batch != "") {
      df = df %>% filter(batch %in% local(rv$filters$batch))
    }

    ## now grab the bag / location information

    df = df %>% inner_join(tbl(con, "dbs_control"), by = c("control_id"))
    df = df %>% inner_join(tbl(con, "dbs_control_sheet") %>% dplyr::rename(dbs_control_sheet_id=id), by = c("dbs_control_sheet_id"))
    df = df %>% inner_join(tbl(con, "dbs_bag") %>% dplyr::rename(bag_id=id, bag_name=name), by = c("bag_id"))
    df = df %>% inner_join(tbl(con, "location") %>% dplyr::rename(location_id=id, location_name=name), by = c("location_id"))

    rv$table = df %>% select(control_uid, batch, density, strain, percentage, bag_name, location_name, level_I, level_II) %>% collect()

    if (include_internal_sample_id) {

      ## Do date collection here because lubridate and purrr::map (used by dplyr sql backend) is not cooperating
      db.results <- sql %>% select("", names(dbmap)) %>% collect() %>% dplyr::mutate(batch_creation_date = as_date(batch_creation_date))

      if (!is.null(filters$collection_date) && sum(is.na(filters$collection_date)) == 0) {
        if (!is.null(filters$collection_date$date.from) && !is.null(filters$collection_date$date.to)) {
          intervals <- list()
          for (i in 1:length(filters$collection_date$date.from)) {
            intervals <- append(
              intervals,
              list(
                interval(
                  lubridate::as_date(local(filters$collection_date$date.from[i])),
                  lubridate::as_date(local(filters$collection_date$date.to[i]))
                )
              )
            )
          }
          db.results <- filter(db.results, batch_creation_date %within% intervals)
        }

      }

      if (!is.null(format)) {
        colnames(db.results) <- c("Sample ID", unname(dbmap))
      }

    } else {
      db.results <- sql %>% select("", names(dbmap)) %>% collect() %>% dplyr::mutate(batch_creation_date = as_date(batch_creation_date))

      if (!is.null(filters$collection_date) && sum(is.na(filters$collection_date)) == 0) {      
        if (!is.null(filters$collection_date$date.from) && !is.null(filters$collection_date$date.to)) {
          intervals <- list()
          for (i in 1:length(filters$collection_date$date.from)) {
            intervals <- append(
              intervals,
              list(
                interval(
                  lubridate::as_date(local(filters$collection_date$date.from[i])),
                  lubridate::as_date(local(filters$collection_date$date.to[i]))
                )
              )
            )
          }
          db.results <- filter(db.results, batch_creation_date %within% intervals) 
        }
      }

      if (!is.null(format)) {
        colnames(db.results) <- unname(dbmap)
      }
    }

    dbDisconnect(con)
  },
  error = function(e) {
    message(e$message)

  }, finally = {
    dbDisconnect(con)
  })

  return (db.results)
}


SearchSamples <- function(sample_storage_type, filters = NULL, format = NULL, database = Sys.getenv("SDB_PATH"), config_yml = Sys.getenv("SDB_CONFIG"), include_internal_sample_id = FALSE) {
  
  browser()
  db.results <- NULL

  tryCatch({
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
    dbBegin(con)

    sql <- tbl(con, "study") %>% dplyr::rename(study_id = id) %>% select(study_id, short_code)
    if (!is.null(filters$short_code)) {
      sql <- filter(sql, short_code == local(filters$short_code))
    }

    sql <- inner_join(sql, tbl(con, "study_subject") %>%
      dplyr::rename(study_subject_id = id, study_subject = name) %>%
      select(study_subject_id, study_subject, study_id), by = c("study_id"))

    if (!is.null(filters$study_subject)) {
      sql <- filter(sql, study_subject %in% local(filters$study_subject))
    }

    sql <- sql %>%
      inner_join(
        tbl(con, "specimen") %>%
          dplyr::rename(specimen_id = id) %>%
          select(specimen_id, study_subject_id, specimen_type_id, collection_date)
        , by = c("study_subject_id"))

    sql <- sql %>% inner_join(
      tbl(con, "specimen_type") %>%
        dplyr::rename(specimen_type_id = id, specimen_type = name) %>%
        select(specimen_type_id, specimen_type)
      , by = c("specimen_type_id"))

    if (!is.null(filters$specimen_type)) {
      sql <- filter(sql, specimen_type == local(filters$specimen_type))
    }

    sql <- inner_join(sql, tbl(con, "storage_container") %>% dplyr::rename(storage_container_id = id) %>% select(-c(created, last_updated)), by = c("specimen_id"))
    sql <- inner_join(sql, tbl(con, "sample_type") %>% dplyr::rename(sample_type_id = id, sample_type = name) %>% select(sample_type_id, sample_type), by = c("sample_type_id"))

    if (sample_storage_type != "all") {
      sql <- filter(sql, sample_type_id == sample_storage_type)
    }

    sql <- inner_join(sql, tbl(con, "status") %>% dplyr::rename(status_id = id, status = name), by = c("status_id"))
    sql <- inner_join(sql, tbl(con, "state") %>% dplyr::rename(state_id = id, state = name), by = c("state_id"))

    if (!is.null(filters$state)) {
      sql <- filter(sql, state == local(filters$state))
    }

    if (!is.null(filters$status)) {
      sql <- filter(sql, status == local(filters$status))
    }

    if (sample_storage_type == "all") {

      # Rf. https://stackoverflow.com/questions/53806023/row-bind-tables-in-sql-with-differing-columns
      list_of_tables <- c("micronix_tube", "cryovial_tube", "dbs_spot")
      eachnames <- sapply(list_of_tables, function(a) DBI::dbQuoteIdentifier(con, DBI::dbListFields(con, a)), simplify = FALSE)
      allnames <- unique(unlist(eachnames, use.names=FALSE))
      allnames <- allnames[1:4] # c("id", "manifest_id", "barcode", "position")

      list_of_fields <- lapply(eachnames, function(a) {
        paste(ifelse(allnames %in% a, allnames, paste("null as", allnames)), collapse = ", ")
      })

      qry <- paste0("CREATE TEMPORARY TABLE `storage_classes` AS\n", paste(
        mapply(function(nm, flds) {
          paste("select",
                paste(ifelse(allnames %in% flds, allnames, paste("null as", allnames)),
                      collapse = ", "),
                "from", nm)
          }, names(eachnames), eachnames),
          collapse = " union\n"))

      DBI::dbExecute(con, qry)
      sql <- inner_join(sql, tbl(con, "storage_classes") %>% dplyr::rename(storage_container_id = id), by = c("storage_container_id")) %>% collapse()

    } else {
      sql <- inner_join(sql, tbl(con, container_tables[["container_class"]]) %>% dplyr::rename(storage_container_id = id), by = c("storage_container_id")) %>% collapse()
    }

    # note: dbs does not have a barcode
    if (!is.null(filters$barcode) && sample_storage_type != 3) {
      sql <- filter(sql, barcode %in% local(filters$barcode)) 
    }


    if (sample_storage_type == "all") {

      list_of_tables <- c("micronix_plate", "cryovial_box", "dbs_paper")

      # This is needed because the container tables do not explitly keep track of the sample types they keep.
      # Thus, table order is important here!!!
      lapply(1:length(list_of_tables), function(i) { DBI::dbExecute(con, paste0("ALTER TABLE ", list_of_tables[i], " ADD COLUMN `sample_type_id` DEFAULT ", i)) })

      # Rf. https://stackoverflow.com/questions/53806023/row-bind-tables-in-sql-with-differing-columns
      eachnames <- sapply(list_of_tables, function(a) DBI::dbQuoteIdentifier(con, DBI::dbListFields(con, a)), simplify = FALSE)
      allnames <- unique(unlist(eachnames, use.names=FALSE))
      allnames <- c(allnames[3:6], allnames[length(allnames)]) # "`id`"          "`location_id`" "`name`"        "`barcode`"

      list_of_fields <- lapply(eachnames, function(a) {
        paste(ifelse(allnames %in% a, allnames, paste("null as", allnames)), collapse = ", ")
      })

      qry <- paste0("CREATE TEMPORARY TABLE `manifests` AS\n", paste(
        mapply(function(nm, flds) {
          paste("select",
                paste(ifelse(allnames %in% flds, allnames, paste("null as", allnames)),
                      collapse = ", "),
                "from", nm)
          }, names(eachnames), eachnames),
          collapse = " union\n"))

      DBI::dbExecute(con, qry)
      # NOT a bug - leave `sample_type_id` here and not in the manifest-specific pathway.
      sql <- inner_join(sql, tbl(con, "manifests") %>% dplyr::rename(manifest_id = id, manifest = name, manifest_barcode = barcode), by = c("manifest_id", "sample_type_id")) %>% collapse()
    } else {
      sql <- inner_join(sql, tbl(con, container_tables[["manifest"]]) %>% dplyr::rename(manifest_id = id, manifest = name, manifest_barcode = barcode), by = c("manifest_id")) %>% collapse()
    }

    if (!is.null(filters$manifest)) {
      sql <- filter(sql, manifest == local(filters$manifest))
    }

    sql <- inner_join(sql, tbl(con, "location") %>% dplyr::rename(location_id = id) %>% select(location_id, name, level_I, level_II), by = c("location_id"))

    if (!is.null(filters$location)) {
      if (!is.null(filters$location[['name']]) & !is.null(filters$location[['level_I']]) & !is.null(filters$location[['level_II']])) {
        sql <- filter(sql, name == local(filters$location[['name']]) & level_I == local(filters$location[['level_I']]) & level_II == local(filters$location[['level_II']]))
      } else if (!is.null(filters$location[['name']]) & !is.null(filters$location[['level_I']])) {
        sql <- filter(sql, name == local(filters$location[['name']]) & level_I == local(filters$location[['level_I']]))
      } else if (!is.null(filters$location[['name']])) {
        sql <- filter(sql, name == local(filters$location[['name']]))
      }
    }

    ## map results to the final columns
    ## note: order matters here

    dbmap <- list()

    ## Micronix
    if (!is.null(format) && sample_storage_type == 1 && format == "na") {
      dbmap$barcode <- "Barcode"
      dbmap$position <- "Position"
    } else if (!is.null(format) && sample_storage_type == 1 && format == "traxcer") {
      dbmap$barcode <- "Tube"

      dbmap$position <- ifelse(
        !is.na(config$traxcer_position$override),
        config$traxcer_position$override,
        config$traxcer_position$default
      )

    } else if (!is.null(format) && sample_storage_type == 1 && format == "visionmate") {
      dbmap$barcode <- "TubeCode"
      dbmap$position <- "Position"
    }

    ## Cryovial
    else if (sample_storage_type == 2) {
      dbmap$barcode <- "Barcode"
      dbmap$position <-  "Position"

    ## DBS
    } else if (sample_storage_type == 3) {
      dbmap$position <- "Position"
    } else {
      dbmap$barcode <- "Barcode"
      dbmap$position <- "Position"
    }

    if (sample_storage_type == 3) {
      dbmap$`0.05` <- "0.05"
      dbmap$`0.1` <- "0.1"
      dbmap$`1` <- "1"
      dbmap$`10` <- "10"
      dbmap$`100` <- "100"
      dbmap$`1k` <- "1k"
      dbmap$`10k` <- "10k"
      dbmap$strain <- "Strain"
    }

    dbmap$short_code <- "Study Code"
    dbmap$study_subject <- "Study Subject"
    dbmap$specimen_type <- "Specimen Type"
    dbmap$collection_date <- "Collection Date"

    dbmap$name <- "Location"
    if (sample_storage_type == 1) {
      dbmap$name <- "Freezer Name"
      dbmap$level_I <- "Shelf Name"
      dbmap$level_II <- "Basket Name"
      dbmap$manifest <- "Plate Name"
      dbmap$manifest_barcode <- "Plate Barcode"
    } else if (sample_storage_type == 2) {
      dbmap$name <- "Freezer Name"
      dbmap$level_I <- "Rack Number"
      dbmap$level_II <- "Rack Position"
      dbmap$manifest <- "Box Name"
      dbmap$manifest_barcode <- "Box Barcode"
    } else if (sample_storage_type == 3) {
      dbmap$name <- "Freezer Name"
      dbmap$level_I <- "Rack Number"
      dbmap$level_II <- "Rack Position"
      dbmap$manifest <- "Container Label"
      dbmap$manifest_barcode <- "Paper Barcode"
    } else {
      # Defaults
      dbmap$name <- "Location"
      dbmap$level_I <- "Level I"
      dbmap$level_II <- "Level II"
      dbmap$manifest <- "Manifest Name"
      dbmap$manifest_barcode <- "Manifest Barcode"
    }

    dbmap$comment <- "Comment"
    dbmap$state <- "State"
    dbmap$status <- "Status"

    if (include_internal_sample_id) {

      ## Do date collection here because lubridate and purrr::map (used by dplyr sql backend) is not cooperating
      db.results <- sql %>% select("storage_container_id", names(dbmap)) %>% collect() %>% dplyr::mutate(collection_date = as_date(collection_date))

      if (!is.null(filters$collection_date) && sum(is.na(filters$collection_date)) == 0) {      
        if (!is.null(filters$collection_date$date.from) && !is.null(filters$collection_date$date.to)) {
          intervals <- list()
          for (i in 1:length(filters$collection_date$date.from)) {
            intervals <- append(
              intervals,
              list(
                interval(
                  lubridate::as_date(local(filters$collection_date$date.from[i])),
                  lubridate::as_date(local(filters$collection_date$date.to[i]))
                )
              )
            )
          }
          db.results <- filter(db.results, collection_date %within% intervals)
        }

      }

      if (!is.null(format)) {
        colnames(db.results) <- c("Sample ID", unname(dbmap))
      }

    } else {
      db.results <- sql %>% select(names(dbmap)) %>% collect() %>% dplyr::mutate(collection_date = as_date(collection_date))

      if (!is.null(filters$collection_date) && sum(is.na(filters$collection_date)) == 0) {      
        if (!is.null(filters$collection_date$date.from) && !is.null(filters$collection_date$date.to)) {
          intervals <- list()
          for (i in 1:length(filters$collection_date$date.from)) {
            intervals <- append(
              intervals,
              list(
                interval(
                  lubridate::as_date(local(filters$collection_date$date.from[i])),
                  lubridate::as_date(local(filters$collection_date$date.to[i]))
                )
              )
            )
          }
          db.results <- filter(db.results, collection_date %within% intervals) 
        }
      }

      if (!is.null(format)) {
        colnames(db.results) <- unname(dbmap)
      }
    }
  },
  error = function(e) {
    message(e$message)

  }, finally = {
    dbDisconnect(con)
  })

  return (db.results)
}

