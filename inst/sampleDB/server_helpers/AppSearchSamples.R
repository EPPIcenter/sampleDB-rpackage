library(shinyjs)
library(RSQLite)
library(DBI)
library(stringr)

SearchWetlabSamples <- function(session, input, database, output, DelArch = FALSE){
  
  # get search ui elements
  rv <- reactiveValues(user_file = NULL, error = NULL, search_table = NULL, filters = NULL, dbmap = NULL)

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

  observeEvent(input$SearchBySampleType, {
    message("Reloading search table...")

    dbmap <- list()
    format <- "na" # this could be modified

    if (input$SearchBySampleType == "all") {
      dbmap$sample_type <- "Sample Storage Type"
    }

    ## Micronix
    if (input$SearchBySampleType == 1 && format == "na") {
      dbmap$barcode <- "Barcode"
      dbmap$position <- "Position"
    } 

    ## Cryovial
    else if (input$SearchBySampleType == 2) {
      dbmap$barcode <- "Barcode"
      dbmap$position <-  "Position"

    ## DBS
    } else if (input$SearchBySampleType == 3) {
      dbmap$position <- "Position"
    } else {
      dbmap$barcode <- "Barcode"
      dbmap$position <- "Position"
    }

    if (input$SearchBySampleType == 3) {
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
    if (input$SearchBySampleType == 1) {
      dbmap$name <- "Freezer Name"
      dbmap$level_I <- "Shelf Name"
      dbmap$level_II <- "Basket Name"
      dbmap$manifest <- "Plate Name"
      dbmap$manifest_barcode <- "Plate Barcode"
    } else if (input$SearchBySampleType == 2) {
      dbmap$name <- "Freezer Name"
      dbmap$level_I <- "Rack Number"
      dbmap$level_II <- "Rack Position"
      dbmap$manifest <- "Box Name"
      dbmap$manifest_barcode <- "Box Barcode"
    } else if (input$SearchBySampleType == 3) {
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
    dbmap$status <- "Status"

    rv$dbmap <- dbmap
    rv$search_table <- SearchSamples(input$SearchBySampleType)

    .ResetInputs(session, input, rv$search_table)
  })

  observe({
    output$SearchResultsTable <- renderReactable({
      rt = NULL
      if ("" != local(input$SearchByState) && "" != local(input$SearchByStatus)) {
        search_table = rv$search_table %>% select(names(rv$dbmap))
        colnames(search_table) <- unname(rv$dbmap)
        rt <- reactable(
          search_table,
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
      }
    })

    rv$filters <- list(
      manifest = input$SearchByManifest,
      short_code = input$SearchByStudy,
      study_subject = input$SearchBySubjectUID,
      specimen_type = input$SearchBySpecimenType,
      collection_date = list(
        date.from = input$dateRange[1],
        date.to = input$dateRange[2]
      ), 
      name = input$SearchByLocation,
      level_I = input$SearchByLevelI,
      level_II = input$SearchByLevelII,
      # state = input$SearchByState,
      status = input$SearchByStatus
    )
  })

  observeEvent(rv$filters, {

    filters <- purrr::discard(rv$filters[!names(rv$filters) %in% c("location", "collection_date")], function(x) is.null(x) | "" %in% x | length(x) == 0)

    if (!is.null(rv$search_table) && any(names(filters) %in% colnames(rv$search_table))) {

      filtered = inner_join(data.frame(filters), rv$search_table)
      intervals <- list()

      if (!is.null(rv$filters$collection_date) && sum(is.na(rv$filters$collection_date)) == 0) {      
        if (!is.null(rv$filters$collection_date$date.from) && !is.null(rv$filters$collection_date$date.to)) {
          for (i in 1:length(rv$filters$collection_date$date.from)) {
            intervals <- append(
              intervals,
              list(
                interval(
                  lubridate::as_date(local(rv$filters$collection_date$date.from[i])),
                  lubridate::as_date(local(rv$filters$collection_date$date.to[i]))
                )
              )
            )
          }
        }
      }

      filtered = if (length(intervals) > 0) filter(filtered, collection_date %within% intervals) else filtered
      filtered = filtered %>% select(names(rv$dbmap))
      colnames(filtered) <- unname(rv$dbmap)
      updateReactable("SearchResultsTable", data = filtered)
    } else {
      search_table <- rv$search_table %>% select(names(rv$dbmap))
      colnames(search_table) <- unname(rv$dbmap)
      updateReactable("SearchResultsTable", data = search_table)
    }
  })

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

  observeEvent(input$SearchBySubjectUIDFile, ignoreInit = TRUE, {
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
  
  observeEvent(input$SearchReset, ignoreInit = TRUE, {

    message("Reset")
    updateRadioButtons(session, selected = "individual", "SubjectUIDSearchType", label = NULL, choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"))
    
    updateDateRangeInput(session, "dateRange", start = NA, end = NA) %>% suppressWarnings()

    .ResetInputs(session, input, rv$search_table)

    ## these should be freed explicitly
    rv$filters <- list(
      manifest = NULL,
      short_code = NULL,
      study_subject = NULL,
      specimen_type = NULL,
      collection_date = list(
        date.from = NA,
        date.to = NA
      ), 
      name = NULL,
      level_I = NULL,
      level_II = NULL,
      state = Global$DefaultStateSearchTerm,
      status = Global$DefaultStatusSearchTerm
    )

    # search file
    rv$user_file <- NULL
  })

  ### Smart Dropdowns

  observeEvent(input$SearchByState, ignoreInit = TRUE, {
    choices <- NULL
    if (input$SearchByState %in% "Archived") {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), database)
      choices <- RSQLite::dbGetQuery(con, "SELECT * FROM view_archive_statuses") %>% pull(name)
      RSQLite::dbDisconnect(con)
    } else {
      choices <- "In Use"
    }
    selected <- choices[1]

    updateSelectizeInput(session, selected = selected, "SearchByStatus", "Status", choices = choices, server = TRUE) 
  })

  observeEvent(input$SearchByStudy, ignoreInit = TRUE, { 

    choices <- NULL
    if (!is.null(input$SearchByStudy) && input$SearchByStudy != "") {
      choices <- rv$search_table %>% filter(short_code == input$SearchByStudy) %>% pull(study_subject)
    } else {
      choices <- rv$search_table$study_subject
    }

    updateSelectizeInput(
      session,
      "SearchBySubjectUID",
      "Study Subject",
      selected = "",
      choices = choices,
      server = TRUE
    )
  })

  observeEvent(input$SearchByLocation, ignoreInit = TRUE, {
    updateSelectInput(
      session,
      "SearchByLevelI",
      selected = "",
      choices = c("", rv$search_table %>%
        filter(name == local(input$SearchByLocation)) %>%
        pull(level_I) %>%
        unique(.)
      )
    )

    shinyjs::reset("SearchByLevelII")
  })

  observeEvent(input$SearchByLevelI, ignoreInit = TRUE, {
    updateSelectInput(
      session,
      "SearchByLevelII",
      selected = "",
      choices = c("", rv$search_table %>%
        filter(name == local(input$SearchByLocation) & level_I == local(input$SearchByLevelI)) %>%
        collect() %>% 
        pull(level_II) %>%
        unique(.)
      )
    )
  })  

  observe({
    output$DownloadSearchData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(rv$search_table, con, row.names = FALSE, quote = FALSE)
      }
    )
  })
}


.ResetInputs <- function(session, input, search_table) {
  updateSelectizeInput(
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
    choices = c("", search_table$manifest),
    server = TRUE
  )

  updateSelectizeInput(session, "SearchByStudy", "Study", choices = c("", unique(search_table$short_code)), server = TRUE)
  updateSelectizeInput(session, "SearchBySubjectUID", "Study Subject", choices = c("", unique(search_table$study_subject)), server = TRUE)
  updateSelectizeInput(session, "SearchBySpecimenType", "Specimen Type", choices = c("", unique(search_table$specimen_type)), server = TRUE)
  updateSelectizeInput(session, "SearchByLocation", "Storage Location", choices = c("", unique(search_table$name)), server = TRUE)

  shinyjs::reset("SearchByBarcode")
  shinyjs::reset("SearchBySubjectUIDFile")
}