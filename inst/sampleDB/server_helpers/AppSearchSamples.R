

SearchWetlabSamples <- function(session, input, database, output, DelArch = FALSE){
  
  # get search ui elements
  ui_elements <- GetUISearchElements()

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
  
  # create a null value to store the search results
  values <- reactiveValues(data = NULL)
  list.search_results <- NULL
  observe({
    
    #search
    list.search_results <- SearchFunction(input, output, ui_elements, rv$user_file)
    
    if(!is.null(list.search_results)){
      values$data <- list.search_results$results
    } else {
      values$data <- NULL
    }
    
    # print search results
    output[[ui_elements$ui.output$SearchResultsTable]] <- DT::renderDataTable({
      if(!is.null(values$data)){
        values$data
      }else{
        tibble(a = c(1)) %>% filter(a == 2)
      }
      }, options = DataTableRenderOptions(), rownames = FALSE)
    
    # download handler
    output[[ui_elements$ui.output$downloadData]] <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(values$data, con)
      }
    )
  })

  # smart dropdown
  SmartFreezerDropdownFilter(database = database, session = session,
                             input = input,
                             location_ui = ui_elements$ui.input$SearchByLocation,
                             levelI_ui = ui_elements$ui.input$SearchByLevelI,
                             levelII_ui = ui_elements$ui.input$SearchByLevelII)
  
  # load dropdown using the server -- saves time
  updateSelectizeInput(session, 'SearchBySubjectUID', 
                       choices = c("", sampleDB::CheckTable(database = database, "study_subject")$name %>% 
                                     unique()), 
                       server = TRUE)
  
  # clear files
  .SearchReset(input)

  observeEvent(input$SearchByBarcode, ignoreInit = FALSE, {
    dataset <- input$SearchByBarcode

    message(paste("Loaded", dataset$name))

    tryCatch({
      ## format the file
      rv$user_file <- sampleDB::ProcessCSV(
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

  observe({
    updateSelectInput(session, selected = input$SearchByPlate, "SearchByPlate", label = "Plate Name", choices = c("", dbUpdateEvent()$micronix_plate_name))
    updateSelectInput(session, selected = input$SearchByBox, "SearchByBox", label = "Box Name", choices = c("", dbUpdateEvent()$cryovial_box_name))

    updateSelectizeInput(session, selected = input$SearchByStudy, "SearchByStudy", "Study", choices = c("", names(dbUpdateEvent()$study)))
    updateSelectizeInput(session, selected = input$SearchBySpecimenType, "SearchBySpecimenType", "Specimen Type", choices = c("", dbUpdateEvent()$specimen_type))
    updateSelectizeInput(session, selected = input$SearchByLocation, "SearchByLocation", "Storage Location", choices = c("", dbUpdateEvent()$location))

    updateSelectizeInput(session, selected = input$SearchByState, "SearchByState", "State", choices = c(dbUpdateEvent()$state))
    
    # name uid should be updated when db updates + when studies are selected
    .SearchSubjectUID(session, input)
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
      manifest_names <- c(manifest_names, DBI::dbReadTable(con, "dbs_paper"))
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

    updateSelectizeInput(session, selected = NULL, "SearchByStudy", "Study", choices = c("", names(dbUpdateEvent()$study)))
    updateSelectizeInput(session, selected = NULL, "SearchBySpecimenType", "Specimen Type", choices = c("", dbUpdateEvent()$specimen_type))
    updateSelectizeInput(session, selected = NULL, "SearchByLocation", "Storage Location", choices = c("", dbUpdateEvent()$location))

    updateSelectizeInput(session, selected = Global$DefaultStateSearchTerm, "SearchByState", "State", choices = c(dbUpdateEvent()$state))
    updateDateRangeInput(session, "dateRange", start = NA, end = NA) %>% suppressWarnings()

    shinyjs::reset("SearchByBarcode")
    shinyjs::reset("SearchBySubjectUIDFile")

    # search file
    rv$user_file <- NULL

    dbDisconnect(con)
  })



  observeEvent(input$SearchByState, {
    choices <- NULL
    if (input$SearchByState %in% "Archived") {
      choices <- sampleDB:::.ViewArchiveStatuses(database = database)$name
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


.SearchReset <- function(input){
  observeEvent(input$ClearSearchBarcodes, ({shinyjs::reset("SearchByBarcode")}))
  observeEvent(input$ClearSearchUIDFile, ({shinyjs::reset("SearchBySubjectUIDFile")})) 
}

# SubsetPlateNames <- function(input, database){
#    study_ref_id <- filter(sampleDB::CheckTable(database = database, "study"), short_code %in% input$SearchByStudy)$id
#    study_subject_ref_id <- filter(sampleDB::CheckTable(database = database, "study_subject"), study_id %in% study_ref_id)$id
#    specimen_ref_id <- filter(sampleDB::CheckTable(database = database, "specimen"), study_subject_id %in% study_subject_ref_id)$id
#    storage_container_id <- filter(sampleDB::CheckTable(database = database, "storage_container"), specimen_id %in% specimen_ref_id)$id
#    matrix_tube_ids <- filter(sampleDB::CheckTable(database = database, "micronix_tube"), id %in% storage_container_id)$id
#    
#    plate_ids <- filter(sampleDB::CheckTable(database = database, "micronix_tube"), id %in% matrix_tube_ids)$plate_id %>% unique()
#    plate_names <- filter(sampleDB::CheckTable(database = database, "micronix_plate"), id %in% plate_ids)$uid
#    return(plate_names)
#  }


