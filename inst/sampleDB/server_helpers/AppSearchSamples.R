library(shinyjs)
library(RSQLite)
library(DBI)
library(stringr)

SearchWetlabSamples <- function(session, input, database, output, DelArch = FALSE){
  
  # get search ui elements
  rv <- reactiveValues(user_file = NULL, error = NULL, search_table = NULL, filters = NULL)

  error <- reactiveValues(
    title = "",
    message = "",
    table = NULL
  )

  observeEvent(rv$error, ignoreInit = TRUE, {
    message("Running error workflow")

    df <- NULL
    if (!is.null(error$table)) {
      df <- error$table %>%
        dplyr::rename(
          Column = column, 
          Reason = reason,
          `Triggered By` = trigger
        ) %>%
        reactable(.)
    }

    showModal(
      modalDialog(
        title = error$title,
        error$message,
        tags$hr(),
        renderReactable({ df }),
        footer = modalButton("Exit")
      )
    )

    error$title = ""
    error$message = ""
    error$table = NULL
    rv$error <- NULL
  })

  observe({

    rv$filters <- list(
      barcode = input$SearchByBarcode,
      manifest = input$SearchByManifest,
      short_code = input$SearchByStudy,
      specimen_type = input$SearchBySpecimenType,
      study_subject = input$SearchBySubjectUID,
      collection_date = list(
        date.from = input$dateRange[1],
        date.to = input$dateRange[2]
      ), 
      input$dateRange,
      location = list(
        name = input$SearchByLocation,
        level_I = input$SearchByLevelI,
        level_II = input$SearchByLevelII
      ),
      state = input$SearchByState,
      status = input$SearchByStatus
    )
  })

  observe({
    message("Searching...")

    filters <- purrr::discard(rv$filters[names(rv$filters) != "location"], function(x) is.null(x) | "" %in% x | length(x) == 0)
    locations <- rv$filters$location[nchar(rv$filters$location) > 0]
    if(length(locations) > 0) {
      filters$location <- locations
    }
    rv$search_table <- SearchSamples(input$SearchBySampleType, filters)

    output$SearchResultsTable <- renderReactable({
      rt <- reactable(
        rv$search_table,
        defaultColDef = colDef(minWidth = 95, html = TRUE, sortable = TRUE, resizable = FALSE, na = "-", align = "center"),
        searchable = TRUE,
        striped = TRUE,
        showPageSizeOptions = TRUE,
        theme = reactableTheme(
          headerStyle = list(
            "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
            borderColor = "#555"
          )
        )
      )
      return(rt)
    })
  })

  observeEvent(input$SearchBySampleType, ignoreInit = FALSE, {

    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    manifest <- switch(
      input$SearchBySampleType,
      "1" = "micronix_plate",
      "2" = "cryovial_box",
      "3" = "dbs_paper",
      "all" = "All"
    )

    manifest_names <- c()

    if (manifest == "All") {
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "micronix_plate") %>% pull(name))
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "cryovial_box") %>% pull(name))
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "dbs_paper"))
    } else {
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, manifest) %>% pull(name))
    }

    updateSelectInput(
      session,
      "SearchByManifest",
      label = switch(
          input$SearchBySampleType,
          "1" = "Plate Name",
          "2" = "Box Name",
          "3" = "Paper Name",
          "all" = "All Containers"
      ),
      selected = "",
      choices = c("", manifest_names)
    )

    dbDisconnect(con)
  })
  
  
  # clear files
  .SearchReset(input)


  ### Search by file

  observeEvent(input$SearchByBarcode, ignoreInit = FALSE, {
    dataset <- input$SearchByBarcode

    message(paste("Loaded", dataset$name))

    tryCatch({
      ## format the file
      rv$user_file <- ProcessCSV(
        user_csv = dataset$datapath,
        user_action = "search",
        search_type = "barcode",
        validate = FALSE
      )

      head(rv$user_file)
      rv$filters$barcode <- rv$user_file[,2]
    },
    formatting_error = function(e) {
      message("Caught formatting error")
      error$title <- "Invalid File Detected"
      error$message <- e$message
      error$table <- e$df

      rv$error <- TRUE
    },
    error = function(e) {
      message(e)
      error$title <- "Error Detected"
      error$message <- e$message
      error$table <- NULL
      rv$error <- TRUE
    })
  })

  observeEvent(input$SearchBySubjectUIDFile, {
    dataset <- input$SearchBySubjectUIDFile

    message(paste("Loaded", dataset$name))

    tryCatch({
      ## format the file
      rv$user_file <- ProcessCSV(
        user_csv = dataset$datapath,
        user_action = "search",
        search_type = "study_subject",
        validate = FALSE
      )

      head(rv$filters$study_subject)
      rv$filters$study_subject <- rv$user_file[,2]
    },
    formatting_error = function(e) {
      message("Caught formatting error")
      error$title <- "Invalid File Detected"
      error$message <- e$message
      error$table <- e$df

      rv$error <- TRUE
    },
    error = function(e) {
      message(e)
      error$title <- "Error Detected"
      error$message <- e$message
      error$table <- NULL
      rv$error <- TRUE
    })
  })
  
  observeEvent(input$SearchReset, {

    message("Reset.")
    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    updateRadioButtons(session, selected = "individual", "SubjectUIDSearchType", label = NULL, choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"))

    manifest_label <- switch(
      input$SearchBySampleType,
      "1" = "Plate Name",
      "2" = "Box Name",
      "3" = "Paper Name",
      "all" = "All Containers"
    )
    updateSelectizeInput(session, "SearchByManifest", label = manifest_label, selected = "", choices = c("", dbUpdateEvent()$manifest))

    updateSelectizeInput(session, "SearchByStudy", "Study", selected = "", choices = c("", dbUpdateEvent()$study))
    updateSelectizeInput(session, "SearchBySubjectUID", "Study Subject", selected = "", choices = c("", dbUpdateEvent()$study_subject))
    updateSelectizeInput(session, "SearchBySpecimenType", "Specimen Type", selected = "", choices = c("", dbUpdateEvent()$specimen_type))
    updateSelectizeInput(session, "SearchByLocation", "Storage Location", selected = "", choices = c("", dbUpdateEvent()$location))
    updateSelectizeInput(session, "SearchByState", "State", selected = Global$DefaultStateSearchTerm, choices = dbReadTable(con, "state") %>% pull(name))
    updateDateRangeInput(session, "dateRange", start = NA, end = NA) %>% suppressWarnings()

    shinyjs::reset("SearchByBarcode")
    shinyjs::reset("SearchBySubjectUIDFile")

    # search file
    rv$user_file <- NULL

    # rv$filters <- NULL
    dbDisconnect(con)
  })

  ### Smart Dropdowns

  observeEvent(input$SearchByState, {
    choices <- NULL
    if (input$SearchByState %in% "Archived") {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), database)
      choices <- RSQLite::dbGetQuery(con, "SELECT * FROM view_archive_statuses") %>% pull(name)
      RSQLite::dbDisconnect(con)
    } else {
      choices <- "In Use"
    }
    selected <- choices[1]

    updateSelectizeInput(session, selected = selected, "SearchByStatus", "Status", choices = choices) 
  })

  observeEvent(input$SearchByStudy, ignoreInit = TRUE, { 

    choices <- NULL
    if (is.null(input$SearchByStudy) || input$SearchByStudy == "") {
      choices <- names(dbUpdateEvent()$study_subject)
    } else {
      study_id <- which(unname(dbUpdateEvent()$study) == match(input$SearchByStudy, names(dbUpdateEvent()$study)))
      req(study_id)
      subject_indexes <- which(unname(dbUpdateEvent()$study_subject) == study_id)
      choices <- names(dbUpdateEvent()$study_subject[subject_indexes])
    }

    updateSelectizeInput(
      session,
      "SearchBySubjectUID",
      "Study Subject",
      selected = "",
      choices = choices,
      server = TRUE)
  })

  observeEvent(input$SearchByLocation, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    updateSelectInput(
      session,
      "SearchByLevelI",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(name == local(input$SearchByLocation)) %>%
        collect() %>% 
        pull(level_I)
      )
    )
    DBI::dbDisconnect(con)

    shinyjs::reset("SearchByLevelI")
    shinyjs::reset("SearchByLevelII")
  })

  observeEvent(input$SearchByLevelI, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    updateSelectInput(
      session,
      "SearchByLevelII",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(name == local(input$SearchByLocation) && level_I == local(input$SearchByLevelI)) %>%
        collect() %>% 
        pull(level_II)
      )
    )
    DBI::dbDisconnect(con)
  })

  dbUpdateEvent <- reactivePoll(
    1000 * 5,
    session,
    function() file.mtime(Sys.getenv("SDB_PATH")),
    function() {
      con <- DBI::dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

      SampleType <- isolate({ input$SearchBySampleType })
      if (SampleType == "all") {
        dat <- list(
          study_subject = tbl(con, "study_subject") %>% pull(var = study_id, name = name), # uses study id currently as a short cut
          study = tbl(con, "study") %>% pull(var = id, name = short_code),
          specimen_type = unique(tbl(con, "specimen_type") %>% pull(name)),
          location = unique(tbl(con, "location") %>% pull(name))
        )

        # Rf. https://stackoverflow.com/questions/53806023/row-bind-tables-in-sql-with-differing-columns
        list_of_tables <- c("micronix_plate", "cryovial_box", "dbs_paper")
        eachnames <- sapply(list_of_tables, function(a) DBI::dbQuoteIdentifier(con, DBI::dbListFields(con, a)), simplify = FALSE)
        allnames <- unique(unlist(eachnames, use.names=FALSE))
        allnames <- allnames[5] # `name`

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
        dat$manifest <- unique(tbl(con, "manifests") %>% pull(name))

      } else {

        container_tables <- list(
          "manifest" = switch(
            SampleType,
            "1" = "micronix_plate",
            "2" = "cryovial_box",
            "3" = "dbs_paper"
          ),
          "container_class" = switch(
            SampleType,
            "1" = "micronix_tube",
            "2" = "cryovial_tube",
            "3" = "dbs_spot"
          )
        )
        sql <- tbl(con, "storage_container") %>%
          dplyr::filter(sample_type_id == SampleType) %>%
          inner_join(tbl(con, "specimen") %>% dplyr::rename(specimen_id = id), by = c("specimen_id")) %>%
          inner_join(tbl(con, "study_subject") %>% dplyr::rename(study_subject_id = id, study_subject = name), by = c("study_subject_id")) %>%
          inner_join(tbl(con, "specimen_type") %>% dplyr::rename(specimen_type_id = id, specimen_type = name), by = c("specimen_type_id")) %>%
          inner_join(tbl(con, "study") %>% dplyr::rename(study = short_code, study_id = id), by = c("study_id")) %>%
          select(id, study, specimen_type, study_subject) %>%
          distinct() %>%
          inner_join(tbl(con, container_tables[["container_class"]]), by = c("id")) %>%
          inner_join(tbl(con, container_tables[["manifest"]]) %>% dplyr::rename(manifest_id = id, manifest = name), by = c("manifest_id")) %>%
          inner_join(tbl(con, "location") %>% dplyr::rename(location_id = id, location = name), by = c("location_id")) %>%
          select(study_subject, study, specimen_type, manifest, location) %>%
          distinct() 

        db <- sql %>% collect()
        dat <- list(
          study_subject = db %>% pull(var = study_subject_id, name = study_subject),
          study = db %>% pull(var = study_id, name = study),
          specimen_type = unique(db$specimen_type),
          location = unique(db$location),
          manifest = unique(db$manifest)
        )
      }
      
      DBI::dbDisconnect(con)

      return(dat)
    }
  )
  

  observeEvent(dbUpdateEvent(), ignoreInit = TRUE, {

    updateSelectInput(
      session,
      "SearchByManifest",
      choices = c("", dbUpdateEvent()$manifest),
      selected = input$SearchByManifest
    )

    updateSelectInput(
      session,
      "SearchByStudy",
      choices = c("", dbUpdateEvent()$short_code),
      selected = input$SearchByStudy
    )

    updateSelectizeInput(
      session,
      "SearchBySpecimenType",
      choices = c("", dbUpdateEvent()$specimen_type),
      selected = input$SearchBySpecimenType
    )

    updateSelectizeInput(
      session,
      "SearchByLocation",
      choices = c("", dbUpdateEvent()$location),
      selected = input$SearchByLocation
    )

    updateSelectizeInput(
      session,
      "SearchBySubjectUID",
      choices = c("", dbUpdateEvent()$study_subject),
      selected = input$SearchBySubjectUID
    )
  })

  observe({
    output$DownloadSearchData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        substitute
        write.csv(rv$search_table, con, row.names = FALSE, quote = FALSE)
      }
    )
  })
}
