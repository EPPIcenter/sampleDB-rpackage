library(shinyjs)
library(RSQLite)
library(DBI)
library(stringr)


AppSearchDelArchSamples <- function(session, input, database, output, dbUpdateEvent) {


  # Initialize the custom filter with default values
  filter_set <- createFilterSetReactive(
    list(
      state = "Active",
      status = "In Use"
    )
  )

  #' Initialize dropdowns that are not control or sample specific
  con <- init_db_conn(database)

  updateSelectInput(session, "DelArchSearchByState", selected = "Active", choices = tbl(con, "state") %>% pull(name))
  updateSelectInput(session, "DelArchSearchByStatus", selected = "In Use", choices = tbl(con, "status") %>% pull(name))
  
  DBI::dbDisconnect(con)

  # Declare filters for searching and establish any filter dependencies
  observe({
    # Build the new filters
    new_filters <- list(
      manifest = input$DelArchSearchByManifest,
      short_code = input$DelArchSearchByStudy,
      study_subject = input$DelArchSearchBySubjectUID,
      specimen_type = input$DelArchSearchBySpecimenType,
      collection_date = list(
        date.from = input$DelArchdateRange[1],
        date.to = input$DelArchdateRange[2]
      ), 
      location = list(
        location_root = input$DelArchSearchByLocation,
        level_I = input$DelArchSearchByLevelI,
        level_II = input$DelArchSearchByLevelII
      ),
      state = input$DelArchSearchByState,
      status = input$DelArchSearchByStatus
    )

    # Remove empty or NULL values from new_filters
    new_filters <- purrr::map(new_filters, ~purrr::discard(.x, function(x) is.null(x) || "" %in% x || length(x) == 0))
    new_filters <- purrr::discard(new_filters, ~is.null(.x) || length(.x) == 0)

    # Insert the new filters
    filter_set$insert(new_filters)

    # List of filters that should not be removed
    exception_list <- c("barcode")
    
    # Do not remove 'study_subject' if a file for it is loaded
    if (!is.null(input$DelArchSearchBySubjectUIDFile)) {
      exception_list <- c(exception_list, "study_subject")
    }

    # Identify filters to remove
    existing_filters <- filter_set$get()
    filters_to_remove <- setdiff(names(existing_filters), names(new_filters))

    # Remove filters not in the exception list
    filters_to_remove <- setdiff(filters_to_remove, exception_list)
    if (length(filters_to_remove) > 0) {
      filter_set$remove(filters_to_remove)
    }
  })

  # get DelArchSearch ui elements
  rv <- reactiveValues(user_file = NULL, error = NULL, operation = NULL, filtered_sample_container_ids = NULL)

  filtered_data <- reactive({
    # Obtain the search results
    
    # Ensure that state and status are set before performing any searches
    # Check for non-NULL and non-empty strings
    req(nzchar(input$DelArchSearchByState), nzchar(input$DelArchSearchByStatus))
    
    if (input$DelArchSearchType == "samples") {
      results <- SearchSamples(input$DelArchSearchBySampleType, filters = filter_set$get(), include_internal_sample_id = TRUE)
    } else {
      results <- SearchControls(input$DelArchSearchByControlType, filters = filter_set$get(), include_internal_control_id = TRUE) 
    }
    
    # Prepare data for reactable
    if (!is.null(results)) {
      results
    } else {
      tibble::tibble()
    }
  }) %>% debounce(500)  # 500ms delay


  # Store the most recent data in a reactive value
  most_recent_data <- reactiveVal(NULL)

  # Observe changes to filtered_data() and update most_recent_data
  observe({
    if (!is.null(filtered_data())) {
      most_recent_data(filtered_data())
    }
  })

  # Observe changes to study_subject_search_results() and update most_recent_data
  observe({
    if (!is.null(study_subject_search_results())) {
      most_recent_data(study_subject_search_results())
    }
  })

  # Your main observe block using most_recent_data()
  observe({
  output$DelArchSearchResultsTable <- renderReactable({
    # If most_recent_data is null (e.g. both filtered_data and study_subject_search_results are null initially)
    if (is.null(most_recent_data())) {
      return(NULL)
    }

    # Get filtered data from the most recent data source
    type <- isolate({ input$DelArchSearchType })
    if (type == "samples") {
      search_table <- most_recent_data() %>% select(-c(`Sample ID`))
    } else {
      search_table <- most_recent_data() %>% select(-c(ControlID))
    }
      
      reactable(
        search_table,
        defaultColDef = colDef(minWidth = ifelse(type == "samples", 105, 150), html = TRUE, sortable = TRUE, resizable = FALSE, na = "-", align = "center"),
        searchable = TRUE,
        selection = "multiple", 
        onClick = "select",
        striped = TRUE,
        showPageSizeOptions = TRUE,
        theme = reactableTheme(
          headerStyle = list(
            "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
            borderColor = "#555"
          ),
          rowSelectedStyle = list(backgroundColor = '#FFAC45', boxShadow = 'inset 2px 0 0 0 #FF4B4B')
        )
      )
    })
  })

   # Use the filtered data to update selections
   # NOTE: make one observeEvnet for the two
  observeEvent(input$DelArchSearchBySampleType, ignoreInit = TRUE, {
    UpdateSelections(session, input, TRUE)
  })

  observeEvent(input$DelArchSearchByControlType, ignoreInit = TRUE, {
    UpdateSelections(session, input, TRUE)
  })

  observeEvent(input$DelArchSearchType, {

    if (input$DelArchSearchType == "samples") {
      accordion_panel_update("DelArchSubjectsPanel", "Study & Subjects")
    } else {
      accordion_panel_update("DelArchSubjectsPanel", "Batch & Controls")
    }

  })

  ### DelArchSearch by file

  observeEvent(input$DelArchSearchByBarcode, ignoreInit = FALSE, {
    dataset <- input$DelArchSearchByBarcode
    message(paste("Loaded", dataset$name))

    tryCatch({
      ## format the file
      rv$user_file <- extract_search_criteria(
        user_csv = dataset$datapath,
        search_type = "barcode"
      )
      new_filter <- list(barcode = rv$user_file$Barcodes)
      filter_set$insert(new_filter)  # Insert new barcode filter
    },
    formatting_error = function(e) {
      show_formatting_error_modal(e)
    },
    error = function(e) {
      show_general_error_modal(e)
    })
  })

  study_subject_search_results <- eventReactive(input$DelArchSearchBySubjectUIDFile, ignoreInit = TRUE, {
    dataset <- input$DelArchSearchBySubjectUIDFile
    message(paste("Loaded", dataset$name))

    results <- tryCatch({
      ## format the file
      rv$user_file <- extract_search_criteria(
        user_csv = dataset$datapath,
        search_type = "study_subject"
      )

      # Begin by renaming just StudySubject which will always be present
      new_filter <- rv$user_file %>%
        select(StudySubject) %>%
        dplyr::rename(study_subject = StudySubject)

      # Add optional columns based on their presence in the dataset
      if ("StudyCode" %in% names(rv$user_file)) {
        new_filter$short_code <- rv$user_file$StudyCode
      }
      
      if ("CollectionDate" %in% names(rv$user_file)) {
        new_filter$collection_date <- rv$user_file$CollectionDate
      }

      if ("SpecimenType" %in% names(rv$user_file)) {
        new_filter$specimen_type <- rv$user_file$SpecimenType
      }

      search_by_study_subject_file_upload(input$DelArchSearchBySampleType, new_filter)

    },
    formatting_error = function(e) {
      show_formatting_error_modal(e)
    },
    error = function(e) {
      show_general_error_modal(e)
    })

    results
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
  
  observeEvent(input$DelArchSearchReset, ignoreInit = TRUE, {

    filter_set$reset()  # restore defaults

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



  observeEvent(input$DelArchSearchByLocation, ignoreInit = TRUE, {

    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    updateSelectInput(
      session,
      "DelArchSearchByLevelI",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(location_root == local(input$DelArchSearchByLocation)) %>%
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
        filter(location_root == local(input$DelArchSearchByLocation) & level_I == local(input$DelArchSearchByLevelI)) %>%
        collect() %>% 
        pull(level_II) %>%
        unique(.)
      )
    )

    dbDisconnect(con)
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

        user.filtered.rows =  most_recent_data()
        user.selected.rows <- if (length(selected() > 0)) user.filtered.rows[selected(), ] else user.filtered.rows

        if (input$DelArchSearchType == "controls") {
          concatenate_list <- function(lst) {
            map_chr(lst, ~paste(.x, collapse = ";"))
          }

          # Apply the function to list columns
          user.selected.rows <- user.selected.rows %>%
            mutate(across(where(is.list), concatenate_list))
        }

        # Quick Request Fix: Just remove the spaces from column names here
        names(user.selected.rows) <- gsub(" ", "", names(user.selected.rows), fixed = TRUE)

        write.csv(user.selected.rows, con, row.names = FALSE, quote = FALSE)
      }
    )
  })
}

UpdateSelections <- function(session, input, keepCurrentSelection = FALSE) {

  if (input$DelArchSearchType == "samples") {
    UpdateSampleSelections(session, input, keepCurrentSelection)
  } else {
    UpdateControlSelections(session, input, keepCurrentSelection)
  }

  updateSelectInput(session, "DelArchSearchByState", selected = ifelse(keepCurrentSelection, input$DelArchSearchByState, "Active"))
  updateSelectInput(session, "DelArchSearchByStatus", selected = ifelse(keepCurrentSelection, input$DelArchSearchByStatus, "In Use"))
  
  shinyjs::reset("DelArchSearchByBarcode")
  shinyjs::reset("DelArchSearchBySubjectUIDFile")
}

UpdateSampleSelections <- function(session, input, keepCurrentSelection = FALSE) {
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
    DelArchSearchByManifest = manifest_types[[input$DelArchSearchBySampleType]]$label,
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
}


UpdateControlSelections <- function(session, input, keepCurrentSelection = FALSE) {

  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

  ## Get the control container choices
  if (input$DelArchSearchByControlType == "dbs_sheet") {
    container_label <- "DBS Sheet"
    container_choices <- tbl(con, "dbs_control_sheet") %>% pull(label)
  } else {
    container_label <- "WB Cryovial Box"
    container_choices <- tbl(con, "cryovial_box") %>% pull(name)
  }

  ## Get the controls and batches
  controls <- tbl(con, "malaria_blood_control") %>%
    inner_join(tbl(con, "study_subject"), by = c("study_subject_id"="id"))

  batches <- controls %>%
    inner_join(tbl(con, "study"), by = c("study_id"="id"))

  found_controls <- controls %>% pull(name, name = id)
  found_batches <- batches %>% pull(short_code, name = id)
  composition_types <- get_composition_types(con)

  ## Get the identifiers and strains
  strains <- get_strains(con)
  
  choices_list <- list(
    DelArchSearchByManifest = unique(container_choices),
    DelArchSearchByStudy = unique(found_batches),
    DelArchSearchBySubjectUID = unique(found_controls),
    DelArchSearchBySpecimenType = c(), # leave empty for now
    DelArchSearchByLocation = unique(tbl(con, "location") %>% pull(location_root)),
    DelArchCompositionTypes = unique(composition_types),
    DelArchSearchByStrains = unique(strains)
  )
  
  labels_list <- list(
    DelArchSearchByManifest = container_label,
    DelArchSearchByStudy = "Batch",
    DelArchSearchBySubjectUID = "Control ID",
    DelArchSearchBySpecimenType = "Specimen Type",
    DelArchSearchByLocation = "Storage Location",
    DelArchCompositionTypes = "Composition Type",
    DelArchSearchByStrains = "Strain"
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
}
