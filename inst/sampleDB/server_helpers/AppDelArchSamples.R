library(shinyjs)
library(RSQLite)
library(DBI)
library(stringr)

DelArchSamples <- function(session, input, database, output, DelArch = FALSE){
  
  # get DelArchSearch ui elements
  rv <- reactiveValues(user_file = NULL, error = NULL, search_table = NULL, filters = NULL, dbmap = NULL, operation = NULL)

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

  observeEvent(input$DelArchSearchBySampleType, {
    message("Reloading DelArchSearch table...")

    dbmap <- list()
    format <- "na" # this could be modified

    if (input$DelArchSearchBySampleType == "all") {
      dbmap$sample_type <- "Sample Storage Type"
    }

    ## Micronix
    if (input$DelArchSearchBySampleType == 1 && format == "na") {
      dbmap$barcode <- "Barcode"
      dbmap$position <- "Position"
    } 

    ## Cryovial
    else if (input$DelArchSearchBySampleType == 2) {
      dbmap$barcode <- "Barcode"
      dbmap$position <-  "Position"

    ## DBS
    } else if (input$DelArchSearchBySampleType == 3) {
      dbmap$position <- "Position"
    } else {
      dbmap$barcode <- "Barcode"
      dbmap$position <- "Position"
    }

    if (input$DelArchSearchBySampleType == 3) {
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
    if (input$DelArchSearchBySampleType == 1) {
      dbmap$name <- "Freezer Name"
      dbmap$level_I <- "Shelf Name"
      dbmap$level_II <- "Basket Name"
      dbmap$manifest <- "Plate Name"
      dbmap$manifest_barcode <- "Plate Barcode"
    } else if (input$DelArchSearchBySampleType == 2) {
      dbmap$name <- "Freezer Name"
      dbmap$level_I <- "Rack Number"
      dbmap$level_II <- "Rack Position"
      dbmap$manifest <- "Box Name"
      dbmap$manifest_barcode <- "Box Barcode"
    } else if (input$DelArchSearchBySampleType == 3) {
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
    rv$search_table <- SearchSamples(input$DelArchSearchBySampleType, include_internal_sample_id = TRUE)

    .ResetDelArchInputs(session, input, rv$search_table)
  })

  observe({
    output$DelArchSearchResultsTable <- renderReactable({
      rt = NULL
      if ("" != local(input$DelArchSearchByState) && "" != local(input$DelArchSearchByStatus)) {
        search_table = rv$search_table %>% select(names(rv$dbmap))
        colnames(search_table) <- unname(rv$dbmap)
        rt <- reactable(
          search_table,
          defaultColDef = colDef(minWidth = 95, html = TRUE, sortable = TRUE, resizable = FALSE, na = "-", align = "center"),
          searchable = TRUE,
          selection = "multiple", onClick = "select",
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
            )
          )
        )

        return(rt)
      }
    })

    rv$filters <- list(
      manifest = input$DelArchSearchByManifest,
      short_code = input$DelArchSearchByStudy,
      study_subject = input$DelArchSearchBySubjectUID,
      specimen_type = input$DelArchSearchBySpecimenType,
      collection_date = list(
        date.from = input$dateRange[1],
        date.to = input$dateRange[2]
      ), 
      name = input$DelArchSearchByLocation,
      level_I = input$DelArchSearchByLevelI,
      level_II = input$DelArchSearchByLevelII,
      # state = input$DelArchSearchByState,
      status = input$DelArchSearchByStatus
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
      updateReactable("DelArchSearchResultsTable", data = filtered)
    } else {
      search_table <- rv$search_table %>% select(names(rv$dbmap))
      colnames(search_table) <- unname(rv$dbmap)
      updateReactable("DelArchSearchResultsTable", data = search_table)
    }
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

  observeEvent(input$DelArchSearchBySubjectUIDFile, ignoreInit = TRUE, {
    dataset <- input$DelArchSearchBySubjectUIDFile

    message(paste("Loaded", dataset$name))

    tryCatch({
      ## format the file
      rv$user_file <- ProcessCSV(
        user_csv = dataset$datapath,
        user_action = "search",
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
  
  observeEvent(input$DelArchSearchReset, ignoreInit = TRUE, {

    message("Reset")
    updateRadioButtons(session, selected = "individual", "SubjectUIDDelArchSearchType", label = NULL, choices = list("Single Study Subject" = "individual", "Multiple Study Subjects" = "multiple"))
    
    updateDateRangeInput(session, "dateRange", start = NA, end = NA) %>% suppressWarnings()

    .ResetDelArchInputs(session, input, rv$search_table)

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
      state = Global$DefaultStateDelArchSearchTerm,
      status = Global$DefaultStatusDelArchSearchTerm
    )

    # DelArchSearch file
    rv$user_file <- NULL
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

    choices <- NULL
    if (!is.null(input$DelArchSearchByStudy) && input$DelArchSearchByStudy != "") {
      choices <- rv$search_table %>% filter(short_code == input$DelArchSearchByStudy) %>% pull(study_subject)
    } else {
      choices <- rv$search_table$study_subject
    }

    updateSelectizeInput(
      session,
      "DelArchSearchBySubjectUID",
      "Study Subject",
      selected = "",
      choices = choices,
      server = TRUE
    )
  })

  observeEvent(input$DelArchSearchByLocation, ignoreInit = TRUE, {
    updateSelectInput(
      session,
      "DelArchSearchByLevelI",
      selected = "",
      choices = c("", rv$search_table %>%
        filter(name == local(input$DelArchSearchByLocation)) %>%
        pull(level_I) %>%
        unique(.)
      )
    )

    shinyjs::reset("DelArchSearchByLevelII")
  })

  observeEvent(input$DelArchSearchByLevelI, ignoreInit = TRUE, {
    updateSelectInput(
      session,
      "DelArchSearchByLevelII",
      selected = "",
      choices = c("", rv$search_table %>%
        filter(name == local(input$DelArchSearchByLocation) & level_I == local(input$DelArchSearchByLevelI)) %>%
        collect() %>% 
        pull(level_II) %>%
        unique(.)
      )
    )
  })  

  observe({
    output$DownloadDelArchSearchData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(rv$search_table, con, row.names = FALSE, quote = FALSE)
      }
    )
  })



  ## the code above is basically copy and pasted from the search panel

  ###### Delarch specific functionality

  selected <- reactive(getReactableState("DelArchSearchResultsTable", "selected"))

  observeEvent(input$ArchiveAction, ignoreInit = TRUE, {
    user.selected.rows = rv$search_table[selected(), ]
    rt.select = names(rv$dbmap[names(rv$dbmap) %in% colnames(user.selected.rows)])
    user.selected.rows.select = user.selected.rows %>% select(all_of(rt.select))
    colnames(user.selected.rows.select) <- unname(rv$dbmap)

    rt <- reactable(
      user.selected.rows.select,
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
        fluidRow( column(width = 6, selectizeInput("DelArchStatus", tags$strong("Status:"), choices = c("", RSQLite::dbGetQuery(con, "SELECT * FROM view_archive_statuses") %>% pull(name)), width = '75%')),
                  column(width = 6, tags$p("Please enter a status for the samples you selected for", tags$strong("archival"), ". This is a", tags$strong("required"), "field, and is used to indicate why the sample is no longer", tags$em("In Use"), "."))
        ),
        hr(),
        fluidRow( column(width = 6, textInput(label = tags$strong("Comment:"), inputId = "DelArchComment", width = '75%')),
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

    ArchiveAndDeleteSamples(
      operation = "archive",
      data = rv$search_table[selected(), ],
      comment = input$DelArchComment,
      status = input$DelArchStatus,
      verification = FALSE
    )

    database <- file.path(Sys.getenv("SDB_PATH"))
    updated_values <- rv$search_table %>%
      select(storage_container_id) %>%
      filter(storage_container_id %in% rv$search_table[selected(), ]$storage_container_id) %>%
      inner_join(CheckTable(database = database, table = "storage_container"), by = c("storage_container_id" = "id")) %>%
      inner_join(CheckTable(database = database, table = "state") %>% dplyr::rename(state = name), by = c("state_id" = "id")) %>%
      inner_join(CheckTable(database = database, table = "status") %>% dplyr::rename(status = name), by = c("status_id" = "id"))

    rv$search_table <- rv$search_table %>%
      mutate(
        # State = as.factor(replace(as.character(State), `Sample ID` %in% updated_values$`Sample ID`, updated_values$State)),
        position = as.factor(replace(as.character(position), storage_container_id %in% updated_values$storage_container_id, rep(NA, length(updated_values$storage_container_id)))),
        status = as.factor(replace(as.character(status), storage_container_id %in% updated_values$storage_container_id, updated_values$status)),
        comment = as.factor(replace(as.character(comment), storage_container_id %in% updated_values$storage_container_id, updated_values$comment))
      )

    removeNotification(id = "ArchDelNotification")
    removeModal()
  })


  observeEvent(input$DeleteAction, ignoreInit = TRUE, {
    user.selected.rows = rv$search_table[selected(), ]
    rt.select = names(rv$dbmap[names(rv$dbmap) %in% colnames(user.selected.rows)])
    user.selected.rows.select = user.selected.rows %>% select(all_of(rt.select))
    colnames(user.selected.rows.select) <- unname(rv$dbmap)

    rt <- reactable(
      user.selected.rows.select,
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

    ArchiveAndDeleteSamples(
      operation = "delete",
      data = rv$search_table[selected(), ],
      comment = input$DelArchComment,
      status = input$DelArchStatus,
      verification = FALSE
    )

    rv$search_table <- rv$search_table[!rv$search_table$storage_container_id %in% rv$search_table[selected(), ]$storage_container_id, ]
    if (nrow(rv$search_table) == 0) {
      rv$search_table <- NULL
    }

    removeNotification(id = "ArchDelNotification")
    removeModal()
  })
}


.ResetDelArchInputs <- function(session, input, search_table) {
    if (input$SearchBySampleType == "all") {
    df = search_table %>% select(sample_type, manifest) %>% distinct()
    manifests = split(df$manifest, df$sample_type)

    df = search_table %>% select(sample_type, short_code) %>% distinct()
    short_codes = split(df$short_code, df$sample_type)

    df = search_table %>% select(sample_type, study_subject) %>% distinct()
    study_subjects = split(df$study_subject, df$sample_type)

    df = search_table %>% select(sample_type, specimen_type) %>% distinct()
    specimen_types = split(df$specimen_type, df$sample_type)

    df = search_table %>% select(sample_type, name) %>% distinct()
    locations = split(df$name, df$sample_type)

  } else {
    manifests = unique(search_table$manifest)
    short_codes = unique(search_table$short_code)
    study_subjects = unique(search_table$study_subject)
    specimen_types = unique(search_table$specimen_type)
    locations = unique(search_table$name)
  }

  updateSelectizeInput(
    session,
    "DelArchSearchByManifest",
    label = switch(
        input$SearchBySampleType,
        "1" = "Plate Name",
        "2" = "Box Name",
        "3" = "Paper Name",
        "all" = "All Containers"
    ),
    selected = "",
    choices = c("", manifests),
    server = TRUE
  )

  updateSelectizeInput(session, "DelArchSearchByStudy", "Study", choices = c("", short_codes), server = TRUE)
  updateSelectizeInput(session, "DelArchSearchBySubjectUID", "Study Subject", choices = c("", study_subjects), server = TRUE)
  updateSelectizeInput(session, "DelArchSearchBySpecimenType", "Specimen Type", choices = c("", specimen_types), server = TRUE)
  updateSelectizeInput(session, "DelArchSearchByLocation", "Storage Location", choices = c("", locations), server = TRUE)
}

