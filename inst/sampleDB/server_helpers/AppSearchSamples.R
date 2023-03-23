library(shinyjs)

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
      collection_date = input$dateRange,
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

    filters <- purrr::discard(rv$filters, function(x) is.null(x) | "" %in% x | length(x) == 0)
    rv$search_table <- SearchSamples(input$SearchBySampleType, filters)

    output$SearchResultsTable <- renderReactable({
      rt <- reactable(
        rv$search_table,
        defaultColDef = colDef(minWidth = 100, html = TRUE, sortable = TRUE, resizable = FALSE, na = "-", align = "center"),
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

  observeEvent(input$SearchByBarcode, ignoreInit = FALSE, {
    dataset <- input$SearchByBarcode

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

  
  observeEvent(input$SearchReset, {

    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    updateRadioButtons(session, selected = "individual", "SubjectUIDSearchType", label = NULL, choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"))
    
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
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "dbs_paper") %>% pull(name))
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
    updateSelectInput(session, selected = NULL, "SearchByManifest", label = manifest_label, choices = c("", manifest_names))

    updateSelectizeInput(session, selected = "", "SearchByStudy", "Study", choices = c("", dbReadTable(con, "study") %>% pull(short_code)))
    updateSelectizeInput(session, selected = "", "SearchBySpecimenType", "Specimen Type", choices = c("", dbReadTable(con, "specimen_type") %>% pull(name)))
    updateSelectizeInput(session, selected = "", "SearchByLocation", "Storage Location", choices = c("", dbReadTable(con, "location") %>% pull("name")))

    updateSelectizeInput(session, selected = Global$DefaultStateSearchTerm, "SearchByState", "State", choices = dbReadTable(con, "state") %>% pull("name"))
    updateDateRangeInput(session, "dateRange", start = NA, end = NA) %>% suppressWarnings()

    shinyjs::reset("SearchByBarcode")
    shinyjs::reset("SearchBySubjectUIDFile")

    # search file
    rv$user_file <- NULL
    # rv$filters <- NULL
    dbDisconnect(con)
  })



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

  observeEvent(input$SearchByStudy, { .SearchSubjectUID(session, input) })
}

.SearchSubjectUID <- function(session, input) {
  choices <- NULL
  if (nchar(input$SearchByStudy) == 0) {
    choices <- names(dbUpdateEvent()$subject)
  } else {
    study_id <- match(input$SearchByStudy, names(dbUpdateEvent()$study))
    req(study_id)
    subject_indexes <- which(unname(dbUpdateEvent()$subject) == study_id)
    choices <- names(dbUpdateEvent()$subject[subject_indexes])
  }

  updateSelectizeInput(session,
    "SearchBySubjectUID",
    "Study Subject",
    selected = "",
    choices = choices,
    server = TRUE)
}
