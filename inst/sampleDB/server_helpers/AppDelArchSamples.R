library(shinyjs)

DelArchSamples <- function(session, input, database, output, inputs, outputs){
  
  # get search ui elements
  ui_elements <- GetUIDelArchElements()
  
  rv <- reactiveValues(user_file = NULL, error = NULL)

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

  observeEvent(input$DelArchSearchBySampleType, ignoreInit = FALSE, {

    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    manifest <- switch(
      input$DelArchSearchBySampleType,
      "1" = "micronix_plate",
      "2" = "cryovial_box",
      # "3" = "dbs_paper",
      "all" = "All"
    )

    manifest_names <- c()

    if (manifest == "All") {
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "micronix_plate") %>% pull(name))
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "cryovial_box") %>% pull(name))
      # manifest_names <- c(manifest_names, DBI::dbReadTable(con, "dbs_paper"))
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
  
  observe({
    #search
    list.search_results <- SearchFunction(input, output, ui_elements, rv$user_file)
    if (!is.null(list.search_results)) {
      search_results <- list.search_results$results
      storage_container_ids <- list.search_results$id.wetlab_samples 
      values$data <- search_results %>%
        mutate(`Sample ID` = storage_container_ids)    
    } else {
      values$data <- NULL
    }
  })

  # print search results
  output[[ui_elements$ui.output$SearchResultsTable]] <- DT::renderDataTable({
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
        )

      values$data <- values$data %>%
        mutate(
          State = as.factor(replace(as.character(State), `Sample ID` %in% updated_values$`Sample ID`, updated_values$State)),
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
      # "3" = "dbs_paper",
      "all" = "All"
    )

    manifest_names <- c()

    if (manifest == "All") {
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "micronix_plate") %>% pull(name))
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "cryovial_box") %>% pull(name))
      # manifest_names <- c(manifest_names, DBI::dbReadTable(con, "dbs_paper"))
    } else {
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, manifest) %>% pull(name))
    }

    manifest_label <- switch(
      input$SearchBySampleType,
      "1" = "Plate Name",
      "2" = "Box Name",
      "3" = "Paper Name",
      "all" = "All"
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
  })

  observeEvent(input$DelArchSearchByState, {
    choices <- NULL
    if (input$DelArchSearchByState %in% "Archived") {
      choices <- :.ViewArchiveStatuses(database = database)$name
    } else {
      choices <- "In Use"
    }
    selected <- choices[1]

    updateSelectizeInput(session, selected = selected, "DelArchSearchByStatus", "Status", choices = choices) 
  })

  observeEvent(input$DelArchSearchByStudy, { .UpdateDelArchSubjectUID(session, input) })
    
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
    choices <- names(dbUpdateEvent()$subject[subject_indexes])
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
