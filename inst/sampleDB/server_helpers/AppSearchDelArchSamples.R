library(shinyjs)
library(RSQLite)
library(DBI)
library(stringr)

AppSearchDelArchSamples <- function(session, input, database, output, dbUpdateEvent) {
  
  ## Set defaults
  # updateSelectInput(session, "DelArchSearchByState", selected = "Active")
  # updateSelectInput(session, "DelArchSearchByStatus", selected = "In Use")

# Reactive to store retrieved database data
  all_data <- reactiveVal(NULL)

  # get DelArchSearch ui elements
  rv <- reactiveValues(user_file = NULL, error = NULL, search_table = NULL, filters = NULL, dbmap = NULL, operation = NULL, filtered_sample_container_ids = NULL)

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

  # Initial data retrieval using default values
  # observe({
  #   initial_data <- SearchSamples(input$DelArchSearchBySampleType, filters = list(state = "Active", status = "In Use"))
    
  #   # Update the reactiveVal with the initial data
  #   all_data(initial_data)
  # })

  filtered_data <- reactive({
    
    # Build the filters
    filters <- list(
      manifest = input$DelArchSearchByManifest,
      short_code = input$DelArchSearchByStudy,
      study_subject = input$DelArchSearchBySubjectUID,
      specimen_type = input$DelArchSearchBySpecimenType,
      collection_date = list(
        date.from = input$DelArchdateRange[1],
        date.to = input$DelArchdateRange[2]
      ), 
      location = list(
        name = input$DelArchSearchByLocation,
        level_I = input$DelArchSearchByLevelI,
        level_II = input$DelArchSearchByLevelII
      ),
      state = input$DelArchSearchByState,
      status = input$DelArchSearchByStatus
    )

    browser()
    # Remove empty or NULL values
    filters <- purrr::map(filters, ~purrr::discard(.x, function(x) is.null(x) | "" %in% x | length(x) == 0))
    filters <- purrr::discard(filters, ~is.null(.x) | length(.x) == 0)

    # Obtain the search results
    results <- SearchSamples(input$DelArchSearchBySampleType, filters = filters, include_internal_sample_id = TRUE)

    # Prepare data for reactable
    if (!is.null(results)) {
      results 
    } else {
      tibble::tibble()
    }
  }) %>% debounce(500)  # 500ms delay


  observe({
    output$DelArchSearchResultsTable <- renderReactable({
      # Get filtered data from our reactive
      browser()
      search_table <- filtered_data() %>% select(-c(`Sample ID`))
      
      reactable(
        search_table,
        defaultColDef = colDef(minWidth = 95, html = TRUE, sortable = TRUE, resizable = FALSE, na = "-", align = "center"),
        searchable = TRUE,
        selection = "multiple", 
        onClick = "select",
        columns = list(
        .selection = colDef(
          headerStyle = list(pointerEvents = "none")
        )
        ),
        striped = TRUE,
        showPageSizeOptions = TRUE,
        theme = reactableTheme(
          headerStyle = list(
            "& input[type='checkbox']" = list(display = "none"),
            "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
            borderColor = "#555"
          ),
          rowSelectedStyle = list(backgroundColor = '#aafaff', boxShadow = 'inset 2px 0 0 0 #ffa62d')
        )
      )
    })
  })

   # Use the filtered data to update selections
  observeEvent(input$DelArchSearchBySampleType, {
    UpdateSelections(session, input, TRUE)
  })

  ### DelArchSearch by file

  observeEvent(input$DelArchSearchByBarcode, ignoreInit = FALSE, {
    dataset <- input$DelArchSearchByBarcode

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
      rv$filters$barcode <-rv$user_file %>% pull(Barcodes)
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

  observeEvent(input$DelArchSearchBySubjectUIDFile, ignoreInit = TRUE, {
    dataset <- input$DelArchSearchBySubjectUIDFile

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
      rv$filters$study_subject <- rv$user_file %>% pull(StudySubjects)
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
  
  observeEvent(input$DelArchSearchReset, ignoreInit = TRUE, {

    updateRadioButtons(session, selected = "individual", "SubjectUIDDelArchSearchType", label = NULL, choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"))
    
    updateDateRangeInput(session, "DelArchdateRange", start = NA, end = NA) %>% suppressWarnings()

    UpdateSelections(session, input, FALSE)
  })

  ### Smart Dropdowns

  observeEvent(input$DelArchSearchByState, ignoreInit = TRUE, {
    choices <- NULL
    if (input$DelArchSearchByState %in% "Archived") {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), database)
      choices <- RSQLite::dbGetQuery(con, "SELECT * FROM view_archive_statuses") %>% pull(name)
      RSQLite::dbDisconnect(con)
    } else {
      choices <- "In Use"
    }
    selected <- choices[1]

    updateSelectizeInput(session, selected = selected, "DelArchSearchByStatus", "Status", choices = choices, server = TRUE) 
  })

  observeEvent(input$DelArchSearchByStudy, ignoreInit = TRUE, { 

    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
    choices <- NULL
    if (!is.null(input$DelArchSearchByStudy) && input$DelArchSearchByStudy != "") {
      choices <- tbl(con, "study") %>%
        inner_join(tbl(con, "study_subject"), by = c("id"="study_id")) %>%
        filter(short_code == local(input$DelArchSearchByStudy)) %>%
        pull(name)
    } else {
      choices <- tbl(con, "study") %>%
        inner_join(tbl(con, "study_subject"), by = c("id"="study_id")) %>%
        pull(name)
    }

    updateSelectizeInput(
      session,
      "DelArchSearchBySubjectUID",
      "Study Subject",
      selected = "",
      choices = choices,
      server = TRUE
    )

    dbDisconnect(con)
  })

  observeEvent(input$DelArchSearchByLocation, ignoreInit = TRUE, {

    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    updateSelectInput(
      session,
      "DelArchSearchByLevelI",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(name == local(input$DelArchSearchByLocation)) %>%
        pull(level_I) %>%
        unique(.)
      )
    )

    dbDisconnect(con)

    shinyjs::reset("DelArchSearchByLevelII")
  })

  observeEvent(input$DelArchSearchByLevelI, ignoreInit = TRUE, {
    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    updateSelectInput(
      session,
      "DelArchSearchByLevelII",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(name == local(input$DelArchSearchByLocation) & level_I == local(input$DelArchSearchByLevelI)) %>%
        collect() %>% 
        pull(level_II) %>%
        unique(.)
      )
    )

    dbDisconnect(con)
  })  

  observe({
    output$DownloadDelArchSearchData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(filtered_data(), con, row.names = FALSE, quote = FALSE)
      }
    )
  })


  observeEvent(dbUpdateEvent(), ignoreInit = TRUE, {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    manifest_name <- switch(
      input$DelArchSearchBySampleType,
      "micronix" = "micronix_plate",
      "cryovial" = "cryovial_box",
      NULL
    )

    # Fetch manifests based on the manifest_name or get all if manifest_name is NULL
    manifests <- if (is.null(manifest_name)) {
      unique(c(
        tbl(con, "micronix_plate") %>% pull(name),
        tbl(con, "cryovial_box") %>% pull(name)
      ))
    } else {
      unique(tbl(con, manifest_name) %>% pull(name))
    }

    # Fetch other data from database in a single set of calls
    data_lists <- list(
      short_codes = tbl(con, "study") %>% pull(short_code),
      study_subjects = tbl(con, "study_subject") %>% pull(name),
      specimen_types = tbl(con, "specimen_type") %>% pull(name),
      locations = tbl(con, "location") %>% pull(location_root)
    )

    # Close the database connection
    dbDisconnect(con)

    # Update the select inputs
    updateSelectizeInput(
      session, "DelArchSearchByManifest",
      label = switch(
        input$DelArchSearchBySampleType,
        "micronix" = "Plate Name",
        "cryovial" = "Box Name",
        "all" = "All Containers"
      ),
      choices = manifests,
      selected = ifelse(input$DelArchSearchByManifest %in% manifests, input$DelArchSearchByManifest, ""),
      server = TRUE
    )

    # Use a mapping to streamline the updateSelectizeInput calls
    input_mapping <- list(
      DelArchSearchByStudy = list(name = "Study", choices = data_lists$short_codes),
      DelArchSearchBySubjectUID = list(name = "Study Subject", choices = data_lists$study_subjects),
      DelArchSearchBySpecimenType = list(name = "Specimen Type", choices = data_lists$specimen_types),
      DelArchSearchByLocation = list(name = "Storage Location", choices = data_lists$locations)
    )

    for (input_id in names(input_mapping)) {
      current_input_value <- input[[input_id]]
      valid_choices <- input_mapping[[input_id]]$choices
          
      selected_value <- ""
      if(!is.null(current_input_value) && current_input_value %in% valid_choices) {
        selected_value <- current_input_value
      }

      updateSelectizeInput(
        session, input_id,
        input_mapping[[input_id]]$name,
        choices = valid_choices,
        selected = selected_value,
        server = TRUE
      )
    }
  })



  ###### Delarch specific functionality

  selected <- reactive(getReactableState("DelArchSearchResultsTable", "selected"))

  observeEvent(input$ArchiveAction, ignoreInit = TRUE, {

    user.filtered.rows = filtered_data()
    user.selected.rows = user.filtered.rows[selected(), ]

    rt <- reactable(
      user.selected.rows,
      defaultColDef = colDef(
        minWidth = 95,
        html = TRUE,
        sortable = TRUE,
        resizable = FALSE,
        na = "-", 
        align = "center"
      )
    )

    rv$operation = "archive"

    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    showModal(
      modalDialog(
        title = "Archive Samples",
        size = "l",
        tags$em("Please review the following fields and your selected samples below.", style = "color: grey;font-size: 18px;"),
        hr(),
        fluidRow( 
          column(width = 6, selectizeInput("DelArchStatus", tags$strong("Status:"), choices = c("", RSQLite::dbGetQuery(con, "SELECT * FROM view_archive_statuses") %>% pull(name)), width = '75%')),
          column(width = 6, tags$p("Please enter a status for the samples you selected for", tags$strong("archival"), ". This is a", tags$strong("required"), "field, and is used to indicate why the sample is no longer", tags$em("In Use"), "."))
        ),
        hr(),
        fluidRow( 
          column(width = 6, textInput(label = tags$strong("Comment:"), inputId = "DelArchComment", width = '75%')),
          column(width = 6, tags$p("You may", tags$em("optionally"), "add a comment to further annotate why this sample is archived"))
        ),
        tags$hr(),
        tags$p("Please review your selected samples below before submitting. You may cancel by selecting", tags$em("Dismiss"), "below or by clicking outside of the dialog box."),
        renderReactable({ rt }),
        easyClose = TRUE,
        fade = TRUE,
        footer = tagList(actionButton("Archive", label = "Archive"), modalButton("Dismiss"))
      )
    )

    DBI::dbDisconnect(con)
  })

  observe({
    if (!is.null(input$DelArchStatus) && input$DelArchStatus != "") {
      shinyjs::enable("Archive")
    } else {
      shinyjs::disable("Archive")
    }

    if (length(selected()) > 0) {
      shinyjs::enable("ArchiveAction")
      shinyjs::enable("DeleteAction")
    } else {
      shinyjs::disable("ArchiveAction")
      shinyjs::disable("DeleteAction")
    }
  })

  observeEvent(input$Archive, ignoreInit = TRUE, { 
    message(sprintf("DelArch action: %s", "archive"))
    shinyjs::disable("Archive")
    showNotification("Working...", id = "ArchDelNotification", type = "message", action = NULL, duration = 5, closeButton = FALSE)

    user.filtered.rows = filtered_data()
    user.selected.rows = user.filtered.rows[selected(), ]
    user.selected.rows$storage_container_id <- user.selected.rows$`Sample ID`

    ArchiveAndDeleteSamples(
      operation = "archive",
      data = user.selected.rows,
      comment = input$DelArchComment,
      status = input$DelArchStatus,
      verification = FALSE
    )

    removeNotification(id = "ArchDelNotification")
    removeModal()

    # Get the filtered data
    updated_data <- user.filtered.rows

    # Remove the selected rows from the filtered data
    updated_data <- updated_data[!updated_data$`Sample ID` %in% user.selected.rows$`Sample ID`,]

    # Update the reactable table
    updateReactable(
      outputId = "DelArchSearchResultsTable",
      data = updated_data
    )
  })

  observeEvent(input$DeleteAction, ignoreInit = TRUE, {

    user.filtered.rows =  filtered_data()
    user.selected.rows = user.filtered.rows[selected(), ]

    rt <- reactable(
      user.selected.rows,
      defaultColDef = colDef(
        minWidth = 95,
        html = TRUE,
        sortable = TRUE,
        resizable = FALSE,
        na = "-", 
        align = "center"
      )
    )

    rv$operation = "deletion"

    showModal(
      modalDialog(
        title = "Delete Samples",
        size = "l",
        tags$p("Below are samples you have selected for deletion."),
        tags$strong("Warning: deleted samples are permanently removed. Do you wish to continue?", style = "color:red"),
        tags$p("You may cancel by selecting", tags$em("Dismiss"), "below or by clicking outside of the dialog box."),
        tags$hr(),
        renderReactable({ rt }),
        easyClose = TRUE,
        fade = TRUE,
        footer = tagList(actionButton("Delete", label = "Delete"), modalButton("Dismiss"))
      )
    )
  })

  observeEvent(input$Delete, ignoreInit = TRUE, { 
    message(sprintf("DelArch action: %s", "delete"))
    shinyjs::disable("Delete")
    showNotification("Working...", id = "ArchDelNotification", type = "message", action = NULL, duration = 5, closeButton = FALSE)

    user.filtered.rows =  filtered_data()
    user.selected.rows = user.filtered.rows[selected(), ]
    user.selected.rows$storage_container_id <- user.selected.rows$`Sample ID`

    ArchiveAndDeleteSamples(
      operation = "delete",
      data = user.selected.rows,
      comment = input$DelArchComment,
      status = input$DelArchStatus,
      verification = FALSE
    )

    removeNotification(id = "ArchDelNotification")
    removeModal()

    # Get the filtered data
    updated_data <- user.filtered.rows

    # Remove the selected rows from the filtered data
    updated_data <- updated_data[!updated_data$`Sample ID` %in% user.selected.rows$`Sample ID`,]

    # Update the reactable table
    updateReactable(
      outputId = "DelArchSearchResultsTable",
      data = updated_data
    )
  })

  observe({
    output$DelArchDownloadSearchData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        user.filtered.rows =  filtered_data()
        user.selected.rows <- if (length(selected() > 0)) user.filtered.rows[selected(), ] else user.filtered.rows
        write.csv(user.selected.rows, con, row.names = FALSE, quote = FALSE)
      }
    )
  })
}

UpdateSelections <- function(session, input, keepCurrentSelection = FALSE) {
  
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
  
  manifest_types <- list(
    "micronix" = list(name = "micronix_plate", label = "Plate Name"),
    "cryovial" = list(name = "cryovial_box", label = "Box Name")
  )
  
  manifests <- if (is.null(manifest_types[[input$DelArchSearchBySampleType]])) {
    c(unique(tbl(con, "micronix_plate") %>% pull(name)),
      unique(tbl(con, "cryovial_box") %>% pull(name)))
  } else {
    unique(tbl(con, manifest_types[[input$DelArchSearchBySampleType]]$name) %>% pull(name))
  }
  
  choices_list <- list(
    DelArchSearchByManifest = manifests,
    DelArchSearchByStudy = unique(tbl(con, "study") %>% pull(short_code)),
    DelArchSearchBySubjectUID = unique(tbl(con, "study_subject") %>% pull(name)),
    DelArchSearchBySpecimenType = unique(tbl(con, "specimen_type") %>% pull(name)),
    DelArchSearchByLocation = unique(tbl(con, "location") %>% pull(location_root))
  )
  
  labels_list <- list(
    DelArchSearchByManifest = manifest_types[[input$DelArchSearchBySampleType]]$label %||% "All Containers",
    DelArchSearchByStudy = "Study",
    DelArchSearchBySubjectUID = "Study Subject",
    DelArchSearchBySpecimenType = "Specimen Type",
    DelArchSearchByLocation = "Storage Location"
  )
  
  sapply(names(choices_list), function(input_name) {
    # Adjust the selected argument based on keepCurrentSelection parameter
    current_selected <- if (keepCurrentSelection) input[[input_name]] else FALSE
    
    updateSelectizeInput(
      session,
      input_name,
      label = labels_list[[input_name]],
      choices = choices_list[[input_name]],
      selected = current_selected, 
      server = TRUE
    )
  })
  
  DBI::dbDisconnect(con)
  
  shinyjs::reset("DelArchSearchByBarcode")
  shinyjs::reset("DelArchSearchBySubjectUIDFile")

  updateSelectInput(session, "DelArchSearchByState", selected = "Active")
  updateSelectInput(session, "DelArchSearchByStatus", selected = "In Use")
}
