library(shinyjs)

DelArchSamples <- function(session, input, database, output, inputs, outputs){
  
  # get search ui elements
  ui_elements <- GetUIDelArchElements()
  
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
    rv$error <- NULL
  })

  observeEvent(input$Exit, ignoreInit = TRUE, {
    error$title = ""
    error$message = ""
    error$table = NULL
    rv$error <- NULL
    removeModal()
  })

  observe({
    rv$filters <- list(
      barcode = input$DelArchSearchByBarcode,
      manifest = input$DelArchSearchByManifest,
      short_code = input$DelArchSearchByStudy,
      specimen_type = input$DelArchSearchBySpecimenType,
      study_subject = input$DelArchSearchBySubjectUID,
      collection_date = input$DelArchdateRange,
      location = list(
        name = input$DelArchSearchByLocation,
        level_I = input$DelArchSearchByLevelI,
        level_II = input$DelArchSearchByLevelII
      ),
      state = input$DelArchSearchByState,
      status = input$DelArchSearchByStatus
    )
  })

  observe({
    message("Searching (DelArch)...")
    filters <- purrr::discard(rv$filters, function(x) is.null(x) | "" %in% x | length(x) == 0)
    values$data <- SearchSamples(input$DelArchSearchBySampleType, filters, include_internal_sample_id = TRUE)
    head(values$data)

    output$DelArchSearchResultsTable <- DT::renderDataTable({
      if (!is.null(values$data)) { 
        values$data %>% select(-`Sample ID`)
      } else {
        tibble(a = c(1)) %>% filter(a == 2)
      }
    }, options = DataTableRenderOptions(), rownames = FALSE)
  })

  observeEvent(input$DelArchSearchBySampleType, ignoreInit = FALSE, {

    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    manifest <- switch(
      input$DelArchSearchBySampleType,
      "1" = "micronix_plate",
      "2" = "cryovial_box",
      "3" = "dbs_paper",
      "all" = "All"
    )

    manifest_names <- c()

    if (manifest == "All") {
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "micronix_plate") %>% pull(name))
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "cryovial_box") %>% pull(name))
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "dbs_paper") %>% pull(name))
    } else {
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, manifest) %>% pull(name))
    }

    updateSelectInput(
      session,
      "DelArchSearchByManifest",
      label = switch(
          input$DelArchSearchBySampleType,
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

  observeEvent(input$DelArchSearchByBarcode, ignoreInit = FALSE, {
    dataset <- input$DelArchSearchByBarcode

    message(paste("Loaded", dataset$name))

    tryCatch({
      ## format the file
      rv$user_file <- ProcessCSV(
        user_csv = dataset$datapath,
        user_action = "search",
        validate = FALSE
      )
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

  # create a null value to store the search results
  list.search_results <- NULL
  
  #create empty value to store data for delarch
  values <- reactiveValues(data = NULL, selected = NULL, operation = NULL)

  # print search results
  output$DelArchSearchResultsTable <- DT::renderDataTable({
    if (!is.null(values$data)) { 
      values$data %>% select(-`Sample ID`)
    } else {
      tibble(a = c(1)) %>% filter(a == 2)
    }
  }, options = DataTableRenderOptions(), rownames = FALSE)

  values$selected <- reactive({ values$data[ input$DelArchSearchResultsTable_rows_selected, ] })

  # smart dropdown
  SmartFreezerDropdownFilter(database = database, session = session,
                             input = input,
                             location_ui = ui_elements$ui.input$SearchByLocation,
                             levelI_ui = ui_elements$ui.input$SearchByLevelI,
                             levelII_ui = ui_elements$ui.input$SearchByLevelII)
  
  # handle archive and deletions
  # - archive item
  
  observeEvent(input[[ui_elements$ui.input$ArchiveAction]], {
    output[[ui_elements$ui.output$DelArchMessage]] <- NULL
    values$operation <- "archive"
    req(input$DelArchStatus)

    showModal(dataModal(operation = values$operation, data = values$selected()))
  })
  
  observeEvent(input[[ui_elements$ui.input$DelArchVerification]], {
    shinyjs::disable("DelArchVerification")
    showNotification("Working...", id = "ArchDelNotification", type = "message", action = NULL, duration = 5, closeButton = FALSE)
    return_message <- ArchiveAndDeleteSamples(operation = values$operation,
                                                        data = values$selected(),
                                                        comment = input$DelArchComment,
                                                        status = input$DelArchStatus,
                                                        verification = F)

    database <- file.path(Sys.getenv("SDB_PATH"))
    if (values$operation %in% "archive") {
      updated_values <- values$data %>%
        select(`Sample ID`) %>%
        filter(`Sample ID` %in% values$selected()$`Sample ID`) %>%
        inner_join(CheckTable(database = database, table = "storage_container"), by = c("Sample ID" = "id")) %>%
        inner_join(CheckTable(database = database, table = "state"), by = c("state_id" = "id")) %>%
        inner_join(CheckTable(database = database, table = "status"), by = c("status_id" = "id")) %>%
        rename(
          State = name.x,
          Status = name.y,
          Comment = comment
        ) %>%
        select(-c(State))

      values$data <- values$data %>%
        mutate(
          # State = as.factor(replace(as.character(State), `Sample ID` %in% updated_values$`Sample ID`, updated_values$State)),
          Status = as.factor(replace(as.character(Status), `Sample ID` %in% updated_values$`Sample ID`, updated_values$Status)),
          Comment = as.factor(replace(as.character(Comment), `Sample ID` %in% updated_values$`Sample ID`, updated_values$Comment))
        )
    } else if (values$operation %in% "delete") {
      values$data <- values$data[!values$data$`Sample ID` %in% values$selected()$`Sample ID`, ]
      if (nrow(values$data) == 0) {
        values$data <- NULL
      }
    }
    removeNotification(id = "ArchDelNotification")
    removeModal()
    output[[ui_elements$ui.output$DelArchMessage]] <- renderPrint(return_message)
  })
  
  # - delete item
  observeEvent(input[[ui_elements$ui.input$DeleteAction]], {
    output[[ui_elements$ui.output$DelArchMessage]] <- NULL
    values$operation <- "delete"
    showModal(dataModal(operation = values$operation, data = values$selected()))
  })

  observe({
    updateSelectInput(session, selected = input$DelArchSearchByPlate, "DelArchSearchByPlate", label = "Plate Name", choices = c("", dbUpdateEvent()$micronix_plate_name))
    updateSelectInput(session, selected = input$DelArchSearchByBox, "DelArchSearchByBox", label = "Box Name", choices = c("", dbUpdateEvent()$cryovial_box_name))

    updateSelectizeInput(session, selected = input$DelArchSearchByStudy, "DelArchSearchByStudy", "Study", choices = c("", names(dbUpdateEvent()$study)))
    updateSelectizeInput(session, selected = input$DelArchSearchBySpecimenType, "DelArchSearchBySpecimenType", "Specimen Type", choices = c("", dbUpdateEvent()$specimen_type))
    updateSelectizeInput(session, selected = input$DelArchSearchByLocation, "DelArchSearchByLocation", "Storage Location", choices = c("", dbUpdateEvent()$location))

    updateSelectizeInput(session, selected = input$DelArchSearchByState, "DelArchSearchByState", "State", choices = c(dbUpdateEvent()$state))
  
    # name uid should be updated when db updates + when studies are selected
    .UpdateDelArchSubjectUID(session, input)
  })

  observeEvent(input$DelArchSearchReset, {

    manifest <- switch(
      input$DelArchSearchBySampleType,
      "1" = "micronix_plate",
      "2" = "cryovial_box",
      "3" = "dbs_paper",
      "all" = "All"
    )

    manifest_names <- c()

    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    if (manifest == "All") {
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "micronix_plate") %>% pull(name))
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "cryovial_box") %>% pull(name))
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "dbs_paper") %>% pull(name))
    } else {
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, manifest) %>% pull(name))
    }

    manifest_label <- switch(
      input$SearchBySampleType,
      "1" = "Plate Name",
      "2" = "Box Name",
      "3" = "Paper Name",
      "all" = "All Containers"
    )

    updateSelectInput(session, selected = NULL, "DelArchSearchByManifest", label = manifest_label, choices = c("", manifest_names))

    updateRadioButtons(session, selected = "individual", "DelArchSubjectUIDSearchType", label = NULL, choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"))
    
    updateSelectInput(session, selected = NULL, "DelArchSearchByPlate", label = "Plate Name", choices = c("", dbUpdateEvent()$micronix_plate_name))
    updateSelectInput(session, selected = NULL, "DelArchSearchByBox", label = "Box Name", choices = c("", dbUpdateEvent()$cryovial_box_name))

    updateSelectizeInput(session, selected = NULL, "DelArchSearchByStudy", "Study", choices = c("", names(dbUpdateEvent()$study)))
    updateSelectizeInput(session, selected = NULL, "DelArchSearchBySpecimenType", "Specimen Type", choices = c("", dbUpdateEvent()$specimen_type))
    updateSelectizeInput(session, selected = NULL, "DelArchSearchByLocation", "Storage Location", choices = c("", dbUpdateEvent()$location))

    updateSelectizeInput(session, selected = NULL, "DelArchSearchByState", "State", choices = c(dbUpdateEvent()$state))  
    updateDateRangeInput(session, "DelArchdateRange", start = NA, end = NA) %>% suppressWarnings()

    rv$user_file <- NULL

    dbDisconnect(con)
  })

  observeEvent(input$DelArchSearchByState, {
    choices <- NULL
    if (input$DelArchSearchByState %in% "Archived") {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), database)
      choices <- RSQLite::dbGetQuery(con, "SELECT * FROM view_archive_statuses") %>% pull(name)
      RSQLite::dbDisconnect(con)
    } else {
      choices <- "In Use"
    }
    selected <- choices[1]

    updateSelectizeInput(session, selected = selected, "DelArchSearchByStatus", "Status", choices = choices) 
  })

  observeEvent(input$DelArchSearchByStudy, { .UpdateDelArchSubjectUID(session, input) })

  dbUpdateEvent <- reactivePoll(
    1000 * 5,
    session,
    function() file.mtime(Sys.getenv("SDB_PATH")),
    function() {
      con <- DBI::dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

      SampleType <- isolate({ input$DelArchSearchBySampleType })
      if (SampleType == "all") {
        dat <- list(
          study_subject = unique(tbl(con, "study_subject") %>% pull(name)),
          study = unique(tbl(con, "study") %>% pull(short_code)),
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
          study_subject = unique(db$study_subject),
          study = unique(db$study),
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
      "DelArchSearchByManifest",
      choices = c("", dbUpdateEvent()$manifest),
      selected = input$DelArchSearchByManifest
    )

    updateSelectInput(
      session,
      "DelArchSearchByStudy",
      choices = c("", dbUpdateEvent()$study),
      selected = input$DelArchSearchByStudy
    )

    updateSelectizeInput(
      session,
      "DelArchSearchBySpecimenType",
      choices = c("", dbUpdateEvent()$specimen_type),
      selected = input$DelArchSearchBySpecimenType
    )

    updateSelectizeInput(
      session,
      "DelArchSearchByLocation",
      choices = c("", dbUpdateEvent()$location),
      selected = input$DelArchSearchByLocation
    )

    updateSelectizeInput(
      session,
      "DelArchSearchBySubjectUID",
      choices = c("", dbUpdateEvent()$study_subject),
      selected = input$DelArchSearchBySubjectUID
    )
  })
    
  # popup window
  dataModal <- function(failed = FALSE, operation, data) {
    modalDialog(
      div(tags$b(HTML(paste0("<h3>Are you sure you would like to <b>", toupper(operation), "</b> the following sample?</h3>")), style = "color: #ce2029;")),
      hr(),
      DT::renderDataTable(data, 
                          options = list(scrollX = T, ordering=F, paging = F, searching = F, info = FALSE), rownames = F),
      footer = tagList(
        actionButton("DelArchVerification", "Yes"),
        modalButton("Cancel")
      )
    )
  }
}

.UpdateDelArchSubjectUID <- function(session, input) {
  choices <- NULL
  if (nchar(input$DelArchSearchByStudy) == 0) {
    choices <- names(dbUpdateEvent()$subject)
  } else {
    study_id <- match(input$DelArchSearchByStudy, names(dbUpdateEvent()$study))
    req(study_id)
    subject_indexes <- which(unname(dbUpdateEvent()$subject) == study_id)
    choices <- names(dbUpdateEvent()$study_subject[subject_indexes])
  }

  updateSelectizeInput(session,
    "DelArchSearchBySubjectUID",
    "Study Subject",
    selected = "",
    choices = choices,
    server = TRUE)
}


.SearchReset <- function(input){
  observeEvent(input$ClearSearchBarcodes, ({shinyjs::reset("SearchByBarcode")}))
  observeEvent(input$ClearSearchUIDFile, ({shinyjs::reset("SearchBySubjectUIDFile")})) 
}
