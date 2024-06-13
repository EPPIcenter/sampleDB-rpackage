library(shinyjs)
library(RSQLite)
library(DBI)
library(stringr)
library(htmltools)
library(tidyr)
library(purrr)

AppSearchDelArchSamples <- function(session, input, database, output, dbUpdateEvent) {


  # Initialize the custom filter with default values
  filter_set <- createFilterSetReactive(
    list(
      state = "Active",
      status = "In Use"
    )
  )

  # Initialize Dropdowns
  observeEvent(TRUE, {
    UpdateSelections(session, input, FALSE)
  }, ignoreNULL = TRUE, once = TRUE)

  #' Initialize dropdowns that are not control or sample specific
  con <- init_db_conn(database)

  updateSelectInput(session, "DelArchSearchByState", selected = "Active", choices = tbl(con, "state") %>% pull(name))
  updateSelectInput(session, "DelArchSearchByStatus", selected = "In Use", choices = c("In Use"))
  
  DBI::dbDisconnect(con)

  # Declare filters for searching and establish any filter dependencies
  filter_observer <- observe({

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
      status = input$DelArchSearchByStatus,
      control_type = input$DelArchSearchByControlType,
      composition_type = input$DelArchCompositionTypes,
      strain = input$DelArchSearchByStrains,
      percentage = input$DelArchSearchByPercentages,
      density = input$DelArchSearchByDensity,
      container_type = input$SearchDBSSampleManifest
    )

    # Remove empty or NULL values from new_filters
    new_filters <- purrr::map(new_filters, ~purrr::discard(.x, function(x) is.null(x) || "" %in% x || length(x) == 0))
    new_filters <- purrr::discard(new_filters, ~is.null(.x) || length(.x) == 0)

    # If composition type is in the filters, update it to use a number.
    if ("composition_type" %in% names(new_filters)) {
      new_filters$composition_type <- as.integer(
        sub("-strain", "", new_filters$composition_type))
    }

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
  force_update <- reactiveVal(FALSE)

  filtered_data <- reactive({
    # Obtain the search results

    # Check if force update is set
    force_update()

    # Ensure that state and status are set before performing any searches
    # Check for non-NULL and non-empty strings
    req(nzchar(input$DelArchSearchByState))

    # Check for non-NULL and non-empty strings    
    if (input$DelArchSearchType == "samples") {
      results <- SearchSamples(input$DelArchSearchBySampleType, filters = filter_set$get(), include_internal_sample_id = TRUE)
    } else {
      results <- SearchControls(input$DelArchSearchByControlType, filters = filter_set$get(), include_internal_control_id = TRUE) 
    }

    # Toggle force update back to FALSE
    force_update(FALSE)
    
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
      columns_to_remove <- c("Sample ID", "ControlID", "CollectionID", "TubeID", "ArchivedSpotsID")
      search_table <- most_recent_data() %>% select(-any_of(columns_to_remove))
        
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
    print(sprintf("Updating selections for sample type: %s", input$DelArchSearchBySampleType))
    UpdateSelections(session, input, TRUE)
  })

  observeEvent(input$DelArchSearchByControlType, ignoreInit = TRUE, {
    print(sprintf("Updating selections for control type: %s", input$DelArchSearchByControlType))
    UpdateSelections(session, input, TRUE)
  })

  observeEvent(input$DelArchSearchType, {

    newTitle <- if (input$DelArchSearchType == "samples") {
      "Study & Subjects"
    } else {
      "Batch & Controls"
    }
    
    accordion_panel_update(
      id = "DelArchSubjectsPanel",   # ID of the accordion
      target = "DelArchSubjectsPanel",  # The value that identifies the panel
      title = newTitle,              # New title based on user input
      session = session              # Current Shiny session
    )

    # Update all of the filters 
    UpdateSelections(session, input, FALSE)

    # Hide the search by study subject option if we are searching by controls
    if (input$DelArchSearchType == "samples") {
      shinyjs::show("DelArchSubjectUIDSearchType")
    } else {
      shinyjs::hide("DelArchSubjectUIDSearchType")      
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
      show_general_error_modal(e, input, output)
    })
  })

  study_subject_search_results <- eventReactive(input$DelArchSearchBySubjectUIDFile, ignoreInit = TRUE, {
    dataset <- input$DelArchSearchBySubjectUIDFile
    message(paste("Loaded", dataset$name))

    results <- tryCatch({
      ## format the file
      rv$user_file <- extract_search_criteria(
        user_csv = dataset$datapath,
        search_type = if (input$DelArchSearchType == "samples") {
          "study_subject" 
          } else {
          "control"
          }
      )

      new_filter <- NULL
      # Add optional columns based on their presence in the dataset
      if (input$DelArchSearchType == "samples") {
        # Begin by renaming just StudySubject which will always be present
        new_filter <- rv$user_file %>%
          select(StudySubject) %>%
          dplyr::rename(study_subject = StudySubject)

        if ("StudyCode" %in% names(rv$user_file)) {
          new_filter$short_code <- rv$user_file$StudyCode
        }
        
        if ("CollectionDate" %in% names(rv$user_file)) {
          new_filter$collection_date <- rv$user_file$CollectionDate
        }

        if ("SpecimenType" %in% names(rv$user_file)) {
          new_filter$specimen_type <- rv$user_file$SpecimenType
        }
      } 

      search_by_study_subject_file_upload(input$DelArchSearchBySampleType, new_filter)
    },
    formatting_error = function(e) {
      show_formatting_error_modal(e)
    },
    error = function(e) {
      show_general_error_modal(e, input, output)
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
  
  observeEvent(c(input$DelArchSearchReset, dbUpdateEvent()), ignoreInit = TRUE, {

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
  archiveWarning <- reactiveVal(NULL)
  editWarning <- reactiveVal(NULL)
  archiveWarningExhaustedStatus <- reactiveVal(NULL)

  observe({
    output$archiveWarningUI <- renderUI({
      if (length(archiveWarning()) > 0) {
        invalid_rows_str <- paste(archiveWarning(), collapse = ", ")
        tags$div(class = "alert alert-warning", 
                 HTML(sprintf("The sum of archived spots exceeds the total available spots in rows: %s. Please adjust the values in these rows.", invalid_rows_str)))
      }
    })

    output$archiveWarningExhaustedStatusUI <- renderUI({
      if (length(archiveWarningExhaustedStatus()) > 0) {
        invalid_rows_str <- paste(archiveWarningExhaustedStatus(), collapse = ", ")
        tags$div(class = "alert alert-warning", 
                 HTML(sprintf("The number of exhausted spots ('Exhausted') does not equal the total ('Total') in rows: %s. Please archive remaining spots if necessary with a status other than 'Exhausted'.", invalid_rows_str)))
      }
    })

    output$editWarningUI <- renderUI({
      if (length(editWarning()) > 0) {
        invalid_rows_str <- paste(editWarning(), collapse = ", ")
        tags$div(class = "alert alert-warning", 
                 HTML(sprintf("Invalid rows detected: %s. Exhausted counts can only be reduced and must not include any spots that were exhausted by archival. Please double check your input.", invalid_rows_str)))
      }
    })
  })

  observeEvent(input$ArchiveAction, ignoreInit = TRUE, {

    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    user.filtered.rows = filtered_data()
    user.selected.rows = user.filtered.rows[selected(), ]

    if (input$DelArchSearchType == "controls" && input$DelArchSearchByControlType == "dbs_sheet") {

      if (input$DelArchSearchByState == "Active") {
        # Make sure to check for spots if we are archiving DBS spots
        user.selected.rows.selected <- user.selected.rows %>% 
          dplyr::mutate(MaxSpots = Total - Exhausted) %>% 
          dplyr::select(-dplyr::all_of(c("CollectionID", "ControlID")))

        scrollable_table <- CreateNumericInputScrollableTable(user.selected.rows.selected, "MaxSpots")

        already_archived_spots_df <- dbReadTable(con, "archived_dbs_blood_spots") %>%
          dplyr::select(CollectionID = blood_spot_collection_id, ArchivedSpotsCount = archived_spots_count)

        user.selected.rows.selected.archived <- user.selected.rows %>%
          dplyr::left_join(already_archived_spots_df, by = join_by("CollectionID")) %>%
          dplyr::mutate(ArchivedSpotsCount = replace_na(ArchivedSpotsCount, 0)) %>%
          dplyr::select(-dplyr::all_of(c("CollectionID", "ControlID")))

        # Create the scrollable table for editing counts
        scrollable_table_edit <- CreateNumericInputScrollableTable(
          user.selected.rows.selected.archived,
          max_value_column = "Exhausted",
          default_value_column = "Exhausted",
          min_value_column = "ArchivedSpotsCount",
          enabled = TRUE
        )

      } else {
        user.selected.rows.selected <- user.selected.rows %>% 
          select(-all_of(c("ControlID", "ArchivedSpotsID")))

        scrollable_table <- CreateNumericInputScrollableTable(user.selected.rows.selected, enabled = FALSE)

        # Only edit active dbs blood spot controls for now
        scrollable_table_edit <- NULL
      } 

      # If we are updating archived sub-collections, then make sure we track whether the user is updating more 
      # than the total number of spots in the collection using the sum of the archived sub-collections. If the 
      # sum is greater than the total, display a warning and disable the archive button.

      # Fetch the statuses from the database
      status_df <- RSQLite::dbGetQuery(con, "SELECT id, name FROM view_archive_statuses")

      # Filter out the 'Exhausted' status (we do not want to manually set controls to 'Exhausted')
      filtered_status_df <- filter(status_df, name != "Exhausted")

      # Convert to a named vector
      dbs_spot_archive_choices <- filtered_status_df %>% pull(id, name=name)

      showModal(
        modalDialog(
          title = "Archive & Edit DBS Controls",
          size = "l",
          tags$em("Please review the following fields and your selected controls below.", style = "color: grey;font-size: 18px;"),
          hr(),
          radioButtons("DelArchDBSAction", label = tags$strong("Action:"), choices = c("Archive" = "archive", "Edit" = "edit"), selected = "archive", inline = TRUE),
          conditionalPanel(
            condition = "input.DelArchDBSAction == 'archive'",
            fluidRow( 
              column(width = 6, selectizeInput("DelArchStatus", tags$strong("Status:"), choices = c("", dbs_spot_archive_choices), width = '75%')),
              column(width = 6, tags$p("Please enter a status for the samples you selected for", tags$strong("archival"), ". This is a", tags$strong("required"), "field, and is used to indicate why the control is no longer", tags$em("In Use"), "."))
            ),
            hr(),
            fluidRow( 
              column(width = 6, textInput(label = tags$strong("Comment:"), inputId = "DelArchComment", width = '75%')),
              column(width = 6, tags$p("You may", tags$em("optionally"), "add a comment to further annotate why this control is archived"))
            )
          ),
          tags$hr(),
          tags$p("Please review your selected blood spots below before submitting. You may cancel by selecting", tags$em("Dismiss"), "below or by clicking outside of the dialog box."),
          conditionalPanel(
            condition = "input.DelArchDBSAction == 'edit' && input.DelArchSearchByState == 'Active'",
            tags$strong("Information:"),
            tags$p("The number of exhausted dry blood spot (DBS) controls may be decremented, if needed. A case when this may be helpful is if you upload an extraction, delete the uploaded extractions because of an error you later notice, and then reupload. This will cause the application to increase the exhausted counts again, which may interfere with your upload if the collection becomes exhausted. The correct action is to manually reduce the exhausted counts by the number of extractions you performed from each DBS collection, BEFORE you reupload."),
            tags$p("You will not be able to lower the exhausted count below the number of existing archived spots ('ArchivedSpotsCount'). For example, if 30 spots are 'Shipped', you will not be able to edit the exhausted counts below 30. If the exhausted count equals the number of existing archived spots, please dismiss this window and exclude this collection while editing other collections."),
            tags$p("You must reduce spots in all collections to continue. If you cannot in one, then you must start over and exclude the collection from the table."),
            scrollable_table_edit
          ),
          conditionalPanel(
            condition = "input.DelArchDBSAction == 'archive'",
            scrollable_table
          ),
          uiOutput("archiveWarningUI"),
          uiOutput("editWarningUI"),
          uiOutput("archiveWarningExhaustedStatusUI"),
          easyClose = TRUE,
          fade = TRUE,
          footer = tagList(actionButton("Archive", label = "Submit"), modalButton("Dismiss"))
        )
      )

    } else if (input$DelArchSearchType == "controls" && input$DelArchSearchByControlType == "whole_blood") {
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

      showModal(
        modalDialog(
          title = "Archive Whole Blood",
          size = "l",
          tags$em("Please review the following fields and your selected controls below.", style = "color: grey;font-size: 18px;"),
          hr(),
          fluidRow( 
            column(width = 6, selectizeInput("DelArchStatus", tags$strong("Status:"), choices = c("", RSQLite::dbGetQuery(con, "SELECT * FROM view_archive_statuses") %>% pull(id, name=name)), width = '75%')),
            column(width = 6, tags$p("Please enter a status for the samples you selected for", tags$strong("archival"), ". This is a", tags$strong("required"), "field, and is used to indicate why the sample is no longer", tags$em("In Use"), "."))
          ),
          hr(),
          fluidRow( 
            column(width = 6, textInput(label = tags$strong("Comment:"), inputId = "DelArchComment", width = '75%')),
            column(width = 6, tags$p("You may", tags$em("optionally"), "add a comment to further annotate why this sample is archived"))
          ),
          tags$hr(),
          tags$p("Please review your selected whole blood below before submitting. You may cancel by selecting", tags$em("Dismiss"), "below or by clicking outside of the dialog box."),
          rt, # Insert the reactable here,
          easyClose = TRUE,
          fade = TRUE,
          footer = tagList(actionButton("Archive", label = "Archive"), modalButton("Dismiss"))
        )
      )

    } else if (input$DelArchSearchType == "samples") {

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

      showModal(
        modalDialog(
          title = "Archive Samples",
          size = "l",
          tags$em("Please review the following fields and your selected samples below.", style = "color: grey;font-size: 18px;"),
          hr(),
          fluidRow( 
            column(width = 6, selectizeInput("DelArchStatus", tags$strong("Status:"), choices = c("", RSQLite::dbGetQuery(con, "SELECT * FROM view_archive_statuses") %>% pull(id, name=name)), width = '75%')),
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
    } else {
      stop("No archive implementation available!")
    }

    DBI::dbDisconnect(con)
  })

  observe({
    # Only allow editing of active dbs spots
    if (input$DelArchSearchByState == "Active") {
      shinyjs::enable("DelArchDBSAction")
    } else {
      shinyjs::disable("DelArchDBSAction")
    }

    if ((!is.null(input$DelArchStatus) && input$DelArchStatus != "" && input$DelArchDBSAction == "archive") || (!is.null(input$DelArchDBSAction) && input$DelArchDBSAction == "edit")) {
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
    showNotification("Working...", id = "ArchDelNotification", type = "message", action = NULL, duration = 5, closeButton = FALSE)

    if (input$DelArchSearchType == "controls" && input$DelArchSearchByControlType == "dbs_sheet") {
      con <- dbConnect(SQLite(), database)
      on.exit(dbDisconnect(con), add = TRUE)

      user.filtered.rows <- filtered_data()
      user.selected.rows <- user.filtered.rows[selected(), ]

      if (input$DelArchSearchByState == "Active") {
        if (input$DelArchDBSAction == "archive") {
          spots_to_archive <- sapply(seq_len(nrow(user.selected.rows)), function(i) {
              as.numeric(input[[paste0("modifyControl_", i)]])
          })
          user.selected.rows$SpotsToArchive <- spots_to_archive

          # Calculate whether the number of archived samples is greater than the total number of spots in the collection
          # If so, prevent the archival and let the user know which rows need to be adjusted
          user.selected.rows.new.archived <- user.selected.rows %>%
            dplyr::group_by(CollectionID) %>%
            dplyr::summarise(
              Total = first(Total),
              Exhausted = first(Exhausted),
              SpotsToArchive = sum(SpotsToArchive)
            ) %>%
            dplyr::mutate(NewArchiveCount = Total - Exhausted - SpotsToArchive)

          # If the status is exhausted, we need to double check that the number of 
          # exhausted spots equal the total, and flag if that is not the case. Spots
          # can be archived as exhausted if the exhausted count equals the total.
          # REMINDER: The exhausted count can be increased by archiving spots.
          if (input$DelArchStatus == 2) {
            collection.ids <- user.selected.rows.new.archived %>%
              filter(Exhausted != Total) %>%
              pull(CollectionID)

            if (length(collection.ids) > 0) {
              rows.exceeding.total <- which(user.selected.rows$CollectionID %in% collection.ids)
              archiveWarningExhaustedStatus(rows.exceeding.total)
              return(NULL)
            }
          }

          collection.ids <- user.selected.rows.new.archived %>%
            filter(user.selected.rows.new.archived$NewArchiveCount < 0) %>%
            pull(CollectionID)

          if (length(collection.ids) > 0) {
            rows.exceeding.total <- which(user.selected.rows$CollectionID %in% collection.ids)
            archiveWarning(rows.exceeding.total)
            return(NULL)
          }

          # Disable the archive button while the transaction is in progress
          shinyjs::disable("Archive")

          # Start the transaction
          dbWithTransaction(con, {
            for(i in seq_len(nrow(user.selected.rows))) {
              row <- user.selected.rows[i, ]

              # Insert new archived spots if they are any status other than 'Exhausted', OR if we
              # are exhausting spots (Status set to 'Exhausted', zero spots to archive, and 'Total' equals 'Exhausted')
              if (row$SpotsToArchive > 0 || (input$DelArchStatus == 2 && row$SpotsToArchive == 0 && row$Total == row$Exhausted)) {
                # Insert into archived_dbs_blood_spots
                dbExecute(con, "INSERT INTO archived_dbs_blood_spots (blood_spot_collection_id, archived_spots_count, reason, status_id) VALUES (:id, :spots, :reason, :status_id)",
                          params = list(id = row$CollectionID, spots = row$SpotsToArchive, reason = input$DelArchComment, status_id = input$DelArchStatus))
              } 

              if(row$SpotsToArchive > 0) {
                # Update the exhausted count in blood_spot_collection
                dbExecute(con, "UPDATE blood_spot_collection SET exhausted = exhausted + :spots WHERE id = :id",
                          params = list(spots = row$SpotsToArchive, id = row$CollectionID))
                
              }
            }
          })
        } else if (input$DelArchDBSAction == "edit") {
          spots_to_exhaust <- sapply(seq_len(nrow(user.selected.rows)), function(i) {
              as.numeric(input[[paste0("modifyControl_", i)]])
          })

          user.selected.rows$SpotsToExhaust <- spots_to_exhaust

          # This will be used to specify how many spots we want to archive if
          # we exhausted == total. The number of exhausted spots will be the 
          # total minus the number of already archived spots. This set of spots
          # are the 'Exhausted' spots, meaning they have been "used up".
          already_archived_spots_df <- dbReadTable(con, "archived_dbs_blood_spots") %>%
            dplyr::select(CollectionID = id, ArchivedSpotsCount = archived_spots_count)

          user.selected.rows.selected.archived <- user.selected.rows %>%
            dplyr::left_join(already_archived_spots_df, by = join_by("CollectionID")) %>%
            dplyr::mutate(ArchivedSpotsCount = replace_na(ArchivedSpotsCount, 0))

          # Validation to ensure that we have valid exhasuted counts. Exhausted
          # counts should never be above total counts and should be greater than or equal
          # to the already archived counts. We also only allow exhausted counts to be decremented.
          collection.ids <- user.selected.rows.selected.archived %>%
            filter(SpotsToExhaust < 0 | SpotsToExhaust < ArchivedSpotsCount | SpotsToExhaust > Total | SpotsToExhaust > Exhausted) %>%
            pull(CollectionID)

          if (length(collection.ids) > 0) {
            invalid.rows <- which(user.selected.rows.selected.archived$CollectionID %in% collection.ids)
            editWarning(invalid.rows)
            return(NULL)
          }

          # Disable the archive button while the transaction is in progress
          shinyjs::disable("Archive")

          # Start the transaction
          dbWithTransaction(con, {
            for(i in seq_len(nrow(user.selected.rows.selected.archived))) {
              row <- user.selected.rows.selected.archived[i, ]
              if(row$SpotsToExhaust > 0 && row$SpotsToExhaust > row$ArchivedSpotsCount) {
                # Update the exhausted count in blood_spot_collection
                dbExecute(con, "UPDATE blood_spot_collection SET exhausted = :exhausted WHERE id = :id",
                          params = list(exhausted = row$SpotsToExhaust, id = row$CollectionID))
              }
            }
          })
        } else {
          stop("Invalid DBS action!!!")
        }
      } else {

        # No spots are being archived as they were already archived, and 
        # this is represented as reassigning the previously archived spots back
        # to `SpotsToArchive`, which effectively does not change the value in the table.
        user.selected.rows$SpotsToArchive <- user.selected.rows$ArchivedSpots

        shinyjs::disable("Archive")

        # Start the transaction
        dbWithTransaction(con, {
          for(i in seq_len(nrow(user.selected.rows))) {
            row <- user.selected.rows[i, ]
            if(row$SpotsToArchive > 0) {
              # Update existing row in archived_dbs_blood_spots
              dbExecute(con, "UPDATE archived_dbs_blood_spots SET archived_spots_count = :spots, reason = :reason, status_id = :status_id WHERE id = :id",
                        params = list(id = row$ArchivedSpotsID, spots = row$SpotsToArchive, reason = input$DelArchComment, status_id = input$DelArchStatus))
            }
          }
        })
      }

    } else if (input$DelArchSearchType == "controls" && input$DelArchSearchByControlType == "whole_blood") {
      con <- dbConnect(SQLite(), database)
      on.exit(dbDisconnect(con), add = TRUE)

      user.filtered.rows <- filtered_data()
      user.selected.rows <- user.filtered.rows[selected(), ]
      archived.state.id <- 2  # "Archived" state

      # Start the transaction for whole blood archival
      dbWithTransaction(con, {
        for(i in seq_len(nrow(user.selected.rows))) {
          row <- user.selected.rows[i, ]

          # Archive the whole blood tube
          dbExecute(con, "UPDATE whole_blood_tube SET state_id = :state_id, status_id = :status_id, reason = :reason WHERE id = :tube_id",
                    params = list(state_id = archived.state.id, status_id = input$DelArchStatus, reason = input$DelArchComment, tube_id = row$TubeID))
        }
      })

    } else if (input$DelArchSearchType == "samples") {
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

    } else {
      stop("No archive implementation available!")
    }

    force_update(TRUE)
    removeNotification(id = "ArchDelNotification")
    removeModal()
  })

  observeEvent(input$DeleteAction, ignoreInit = TRUE, {

    user.filtered.rows =  filtered_data()
    user.selected.rows = user.filtered.rows[selected(), ]

    rv$operation = "deletion"

    # If this is a DBS sheet, we need to ask how many spots should be deleted.
    if (input$DelArchSearchType == "controls" && input$DelArchSearchByControlType == "dbs_sheet") {

      if (input$DelArchSearchByState == "Active") {
        # Make sure to check for spots if we are archiving DBS spots
        user.selected.rows.selected <- user.selected.rows %>% 
          mutate(MaxSpots = Total - Exhausted) %>% 
          select(-all_of(c("CollectionID", "ControlID")))

        scrollable_table <- CreateNumericInputScrollableTable(user.selected.rows.selected, "MaxSpots")

      } else {
        user.selected.rows.selected <- user.selected.rows %>% 
          select(-all_of(c("ControlID", "ArchivedSpotsID")))

        scrollable_table <- CreateNumericInputScrollableTable(user.selected.rows.selected, enabled = FALSE)
      } 
      
      showModal(
        modalDialog(
          title = "Delete DBS Controls",
          size = "l",
          tags$p("Below are BDS controls you have selected for deletion."),
          tags$strong("Warning: deleted controls are permanently removed. Do you wish to continue?", style = "color:red"),
          tags$p("You may cancel by selecting", tags$em("Dismiss"), "below or by clicking outside of the dialog box."),
          tags$hr(),
          scrollable_table, # Insert the scrollable div here
          easyClose = TRUE,
          fade = TRUE,
          footer = tagList(
            actionButton("Delete", label = "Delete"), 
            modalButton("Dismiss")
          )
        )
      )

    } else if (input$DelArchSearchType == "controls" && input$DelArchSearchByControlType == "whole_blood") {
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

      showModal(
        modalDialog(
          title = "Delete Whole Blood Controls",
          size = "l",
          tags$p("Below are whole blood controls you have selected for deletion."),
          tags$strong("Warning: deleted controls are permanently removed. Do you wish to continue?", style = "color:red"),
          tags$p("You may cancel by selecting", tags$em("Dismiss"), "below or by clicking outside of the dialog box."),
          tags$hr(),
          renderReactable({ rt }),
          easyClose = TRUE,
          fade = TRUE,
          footer = tagList(actionButton("Delete", label = "Delete"), modalButton("Dismiss"))
        )
      )
    
    # Sample specific deletion goes here. This is the same dialog for all sample types,
    # as there is no need for a sample type specific deletion workflow at this time. 
    } else if (input$DelArchSearchType == "samples") {

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
    } else {
      stop("No delete implementation for this type.")
    }
  })

  observeEvent(input$Delete, ignoreInit = TRUE, { 

    message(sprintf("DelArch action: %s", "delete"))
    shinyjs::disable("Delete")
    showNotification("Working...", id = "ArchDelNotification", type = "message", action = NULL, duration = 5, closeButton = FALSE)

    user.filtered.rows =  filtered_data()
    user.selected.rows = user.filtered.rows[selected(), ]

    if (input$DelArchSearchType == "controls" && input$DelArchSearchByControlType == "dbs_sheet" && input$DelArchSearchByState == "Archived") {

      con <- dbConnect(SQLite(), database)
      on.exit(dbDisconnect(con), add = TRUE)

      # Start the transaction
      dbWithTransaction(con, {
        for(i in seq_len(nrow(user.selected.rows))) {
          row <- user.selected.rows[i, ]

          # Delete rows from archived_dbs_blood_spots based on the condition
          dbExecute(con, "DELETE FROM archived_dbs_blood_spots WHERE id = :id",
                    params = list(id = row$ArchivedSpotsID))
        }
      })

    } else if (input$DelArchSearchType == "controls" && input$DelArchSearchByControlType == "dbs_sheet") {
      con <- dbConnect(SQLite(), database)
      spots_to_delete <- sapply(seq_len(nrow(user.selected.rows)), function(i) {
        as.numeric(input[[paste0("modifyControl_", i)]])
      })

      user.selected.rows$SpotsToDelete <- spots_to_delete
      
      # Start the transaction
      tryCatch({
        dbWithTransaction(con, {
          for(i in seq_len(nrow(user.selected.rows))) {
            row <- user.selected.rows[i, ]
            if(row$SpotsToDelete >= row$Total) {
              # Delete the record if the spots to delete is greater than or equal to total
              dbExecute(con, "DELETE FROM blood_spot_collection WHERE id = :id", params = list(id = row$CollectionID))
            } else {
              # Update the total count if spots to delete is less than total
              dbExecute(con, "UPDATE blood_spot_collection SET total = total - :spots WHERE id = :id",
                        params = list(spots = row$SpotsToDelete, id = row$CollectionID))
            }
          }
        })
      },
      error = function(e) {
        show_general_error_modal(e, input, output)
      })

    } else if (input$DelArchSearchType == "controls" && input$DelArchSearchByControlType == "whole_blood") {

      DeleteWholeBloodSamples(user.selected.rows$TubeID)

    } else if (input$DelArchSearchType == "samples") {
      
      user.selected.rows$storage_container_id <- user.selected.rows$`Sample ID`

      ArchiveAndDeleteSamples(
        operation = "delete",
        data = user.selected.rows,
        comment = input$DelArchComment,
        status = input$DelArchStatus,
        verification = FALSE
      )

    } else {
      stop("No delete implementation for this type.")
    }

    force_update(TRUE)
    removeNotification(id = "ArchDelNotification")
    removeModal()
  })

  observe({
    output$DelArchDownloadSearchData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {

        user.filtered.rows =  most_recent_data()
        print(user.filtered.rows)
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

  observeEvent(input$DelArchAdvancedSearchLink, {
    showModal(modalDialog(
      title = "Advanced Micronix Tube Search",
      tags$p("Upload a scanned micronix plate file to search for samples."),
      tags$ul(
        tags$li("Only one file may be uploaded at a time"),
        tags$li("The name of the file should be name of the plate (e.g. 'Plate1.csv')")
      ),
      hr(),
      radioButtons("AdvancedSearchUploadFileType", "Choose a file type", choices = get_file_types_for_sample("micronix"), inline = TRUE),
      fileInput("AdvancedSearchFileUpload", "Choose CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), multiple = FALSE),
      conditionalPanel(
        condition = "input.AdvancedSearchUploadFileType == 'traxcer'",
        checkboxInput("AdvanceSearchTraxcerStripFromFilename", "Remove datetime strip from filename",value = TRUE, width = NULL)
      ),
      actionButton("AdvancedSearchAction", "Run Search"),
      tags$div(id = "resultsSummary", uiOutput("resultsList")),
      downloadButton("downloadErrors", "Download Error Details"),
      size = "l",
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$SearchDBSSampleManifest, ignoreInit = TRUE, {
    dbs_sample_data <- list(name = input$SearchDBSSampleManifest, label = switch(
      input$SearchDBSSampleManifest, "all" = "All", "box" = "Box Name", "bag" = "Bag Name"))
    con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    choices <- list()
    if (input$SearchDBSSampleManifest == "all") {

      box_tbl <- tbl(con, "box") %>%
        dplyr::mutate(manifest_type = "box") %>%
        dplyr::rename(manifest_id = id, manifest = name)

      bag_tbl <- tbl(con, "bag") %>%
        dplyr::mutate(manifest_type = "bag") %>%
        dplyr::rename(manifest_id = id, manifest = name)

      box_bag_union_df <- union_all(box_tbl, bag_tbl) %>%
        dplyr::select(manifest, manifest_type) %>%
        dplyr::mutate(manifest_type = ifelse(manifest_type == "bag", "Bag", "Box")) %>%
        dplyr::collect()

      updateSelectizeInput(
        session,
        "DelArchSearchByManifest",
        label = "All DBS",
        choices = box_bag_union_df$manifest,
        selected = "", 
        server = TRUE
      )

    } else {
      choices <- tbl(con, dbs_sample_data$name) %>% pull(name) %>% unique(.)
      updateSelectizeInput(
        session,
        "DelArchSearchByManifest",
        label = dbs_sample_data$label,
        choices = choices,
        selected = "", 
        server = TRUE
      )
    }

    

    dbDisconnect(con)
  })

  observeEvent(input$AdvancedSearchAction, {
    req(input$AdvancedSearchFileUpload) # Ensure a file is uploaded

    results <- NULL
    plate_name <- NULL

    bOk <- tryCatch({
      file_path <- input$AdvancedSearchFileUpload$datapath[1]
      file_column_attr <- get_sample_file_columns("micronix", "move", input$AdvancedSearchUploadFileType)

      actual_plate_name <- sub('\\.csv$', '', input$AdvancedSearchFileUpload$name)

      if(input$AdvancedSearchUploadFileType == "traxcer" && input$AdvanceSearchTraxcerStripFromFilename) {
        plate_name <- substr(actual_plate_name, 1, nchar(actual_plate_name)-16)
        if (nchar(plate_name) == 0) {
          stop_formatting_error(
            "Traxcer name was completely removed - did you mean to remove the datetime string from the filename?",
            format_error(
              actual_plate_name,
              "If you check the box, the filename must contain the datetime added by Traxcer",
              "File name was too short"
            )
          )
        }
      } else {
        plate_name <- actual_plate_name
      }

      micronix_search_df <- read_and_preprocess_csv(file_path)
      micronix_search_df <- set_user_file_header(micronix_search_df, file_column_attr)

      # Find missing columns.
      missing_columns <- detect_missing_specimen_columns(micronix_search_df, file_column_attr)

      # If there are any missing columns still, throw!
      if (length(missing_columns) > 0) {
        stop_formatting_error("Missing required columns", format_error(missing_columns))
      }

      # Add the plate name column. This function will throw if it already exists in the csv.
      micronix_search_df <- bind_new_data(micronix_search_df, setNames(plate_name, "PlateName"))

      position_col <- get_position_column_by_sample("micronix", input$AdvancedSearchUploadFileType)

      # Create the position column
      micronix_search_df$Position <- if (input$AdvancedSearchUploadFileType == "na") {
        sprintf(
          "%s%02d",
          micronix_search_df[[position_col[1]]], # Row
          as.integer(micronix_search_df[[position_col[2]]]) # Column
        )
      } else {
        dplyr::pull(micronix_search_df, !!rlang::sym(position_col))
      }

      micronix_search_df$Barcode <- dplyr::pull(micronix_search_df, any_of(c("Barcode", "Tube ID")))
      micronix_search_df <- dplyr::select(micronix_search_df, Barcode, Position, PlateName)

      # Run the search function for the file
      results <- search_micronix_tube(database, micronix_search_df)
      TRUE

    }, formatting_error = function(e) {
      show_formatting_error_modal(e)
      FALSE
    }, error = function(e) {
      show_general_error_modal(e, input, output)
      FALSE
    })

    # Exit early if there is an issue
    if (!bOk) return (NULL)

    # Extract test names (descriptions) from 'errorCollection'
    testDescriptionsToUIIDs <- list(
      "Scanned samples are missing from the database" = "NotFound",
      "Scanned samples found on a different plate than expected" = "IncorrectLocation",
      "Samples on the scanned plate have been archived" = "Archived"
    )

    fileTestDescriptionsToUIIDs <- list(
      "Scanned sample does not exist in SampleDB" = "NotFound",
      "Scanned sample is on a different plate than expected" = "IncorrectLocation",
      "Scanned sample is archived" = "Archived",
      "No Error" = "Empty",
      "No Error" = "Correct"
    )

    get_test_description <- function(test, map) {
      testDescIdx <- which(map == test)
      if (length(testDescIdx) == 0) {
        stop("Test description not found in testDescriptionsToUIIDs")
      }

      names(map)[testDescIdx]
    }

    # A helper function to summarize error types from the results DataFrame
    summarise_error_types <- function(df) {
      error_split <- df %>%
        mutate(Status = strsplit(as.character(Status), ";")) %>%
        unnest(Status) %>%
        filter(Status != "Correct" & Status != "NoError")

      # Count occurrences of each error type
      error_summary <- error_split %>%
        group_by(Status) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(errorType = Status) %>%
        select(errorType, count)
      
      return(as.data.frame(error_summary))
    }


    output$resultsList <- renderUI({
      # Initialize an empty list to store UI elements
      ui_elements <- list()
      
      # Define a mapping for statuses to UI icons and colors for error instances
      status_ui_mapping <- list(
        iconError = shiny::icon("exclamation-triangle", class = "text-danger"),
        textStyleError = "color: #dc3545;",
        iconNoError = shiny::icon("check-circle", class = "text-secondary"),
        textStyleNoError = "color: #6c757d;"
      )

      # Create a summary of errors based on the 'Status' column in 'results'
      if (!is.null(results) && nrow(results) > 0) {
        error_summary <- summarise_error_types(results)
        
        # Iterate over each status in the mapping
        for (status in c("NotFound", "IncorrectLocation", "Archived")) {
          count <- ifelse(status %in% error_summary$errorType, error_summary$count[error_summary$errorType == status], 0)
          if (count > 0) {
            ui_element <- tags$div(
              style = status_ui_mapping$textStyleError,
              status_ui_mapping$iconError,
              " ",
              sprintf("%s: %d instances", status, count)
            )
          } else {  # Grey out the error icon and text if there are no instances of the error
            ui_element <- tags$div(
              style = status_ui_mapping$textStyleNoError,
              status_ui_mapping$iconNoError,
              " ",
              sprintf("%s: %d instances", status, count)
            )
          }

          ui_elements <- c(ui_elements, list(ui_element))
        }
      } else {
        ui_elements <- list(tags$p("No data available."))
      }
      
      do.call(tagList, ui_elements)
    })

    # Adjust the download button functionality for the consolidated DataFrame
    output$downloadErrors <- downloadHandler(
      filename = function() {
        paste0(sprintf("micronix-plate-search_%s_", plate_name), Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(results, file, row.names = FALSE, quote = TRUE)
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
    "cryovial" = list(name = "cryovial_box", label = "Box Name"),
    "dbs_sample" = list(name = input$SearchDBSSampleManifest, label = switch(
      input$SearchDBSSampleManifest, "all" = "All", "box" = "Box Name", "bag" = "Bag Name"))
  )
  
  if (is.null(manifest_types)) {
    stop("Invalid sample type!!!")
  }

  manifest_choices <- NULL
  if (input$SearchDBSSampleManifest == "all") {
    manifest_choices <- c(
      unique(tbl(con, "box") %>% pull(name)), 
      unique(tbl(con, "bag") %>% pull(name))
    )
  } else {
    manifest_choices <- unique(tbl(con, manifest_types[[input$DelArchSearchBySampleType]]$name) %>% pull(name))
  }
  
  choices_list <- list(
    DelArchSearchByManifest = manifest_choices,
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
    current_selected <- if (keepCurrentSelection) input[[input_name]] else ""
    
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
  densities <- get_densities(con)
  percentages <- get_percentages(con)

  ## Get the identifiers and strains
  strains <- get_strains(con)
  
  choices_list <- list(
    DelArchSearchByManifest = unique(container_choices),
    DelArchSearchByStudy = unique(found_batches),
    DelArchSearchBySubjectUID = unique(found_controls),
    DelArchSearchBySpecimenType = c(), # leave empty for now
    DelArchSearchByLocation = unique(tbl(con, "location") %>% pull(location_root)),
    DelArchCompositionTypes = unique(composition_types),
    DelArchSearchByStrains = unique(strains),
    DelArchSearchByPercentages = unique(percentages),
    DelArchSearchByDensity = unique(densities)
  )
  
  labels_list <- list(
    DelArchSearchByManifest = container_label,
    DelArchSearchByStudy = "Batch",
    DelArchSearchBySubjectUID = "Control ID",
    DelArchSearchBySpecimenType = "Specimen Type",
    DelArchSearchByLocation = "Storage Location",
    DelArchCompositionTypes = "Composition Types",
    DelArchSearchByStrains = "Strains"
  )
  
  sapply(names(choices_list), function(input_name) {
    # Adjust the selected argument based on keepCurrentSelection parameter
    current_selected <- if (keepCurrentSelection) input[[input_name]] else ""
    
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

#' Create a Scrollable Table with Numeric Inputs for Each Row
#'
#' This function generates a scrollable HTML table where each row contains a numeric input element. 
#' The numeric inputs can have their maximum values and default values set dynamically based on the data frame columns specified. 
#' Additionally, these inputs can be enabled or disabled globally.
#'
#' @param data A data frame containing the data to be displayed in the table. 
#' The data frame can optionally include columns specified by `max_value_column` and `default_value_column` to dynamically set properties of the numeric inputs.
#' @param max_value_column The name of the column in `data` that contains the maximum value for the numeric inputs.
#' If this column is not found, a default max value of 100 is used.
#' @param default_value_column Optional; the name of the column in `data` that contains the default value for the numeric inputs. 
#' If not provided or the column is not found, a default value of 0 is used for all inputs.
#' @param enabled A logical value indicating whether the numeric inputs should be enabled (`TRUE`) or disabled (`FALSE`).
#' By default, inputs are enabled.
#'
#' @return An HTML div element containing a scrollable table. Each row of the table includes a numeric input 
#' with properties determined by the function parameters and the corresponding row in the input data frame.
#'
CreateNumericInputScrollableTable <- function(data, max_value_column = NULL, default_value_column = NULL, enabled = TRUE, min_value_column = NULL) {
  # Format the 'Percentage' and 'Strain' columns, if they exist
  if ("Percentage" %in% names(data)) {
    data$Percentage <- sapply(data$Percentage, function(x) paste(x, "%", collapse = ","))
  }
  if ("Strain" %in% names(data)) {
    data$Strain <- sapply(data$Strain, function(x) paste(x, collapse = ","))
  }
  
  # Build the HTML table rows
  table_rows <- lapply(seq_len(nrow(data)), function(i) {
    row <- data[i, ]
    
    max_spots <- if (!is.null(max_value_column) && max_value_column %in% names(row)) row[[max_value_column]] else 100
    min_spots <- if (!is.null(min_value_column) && min_value_column %in% names(row)) row[[min_value_column]] else 0
    default_value <- if (!is.null(default_value_column) && default_value_column %in% names(row)) row[[default_value_column]] else 0
    
    # Manually creating the numeric input HTML
    numeric_input_id <- paste0("modifyControl_", i)
    disabled_attr <- ifelse(enabled, "", "disabled")
    numeric_input_html <- tags$input(type = "number", id = numeric_input_id, class = "form-control", 
                                     style = "width: 80px;", min = min_spots, max = max_spots, 
                                     value = default_value, disabled_attr)
    
    tags$tr(
      tags$td(style = "position: sticky; left: 0; background: white;", numeric_input_html),
      lapply(row, function(value) tags$td(as.character(value)))
    )
  })

  # Create table header, including a header for the numeric inputs
  table_header <- tags$tr(
    tags$th(style = "position: sticky; left: 0; background: white;", "# Spots"),
    lapply(names(data), function(name) tags$th(name))
  )
  
  # Create a scrollable div to contain the table
  scrollable_table <- div(
    style = "overflow-x: auto; overflow-y: auto; height: 400px; width: 100%; position: relative;",
    tags$table(class = "table table-striped", table_header, do.call(tagList, table_rows))
  )
  
  return(scrollable_table)
}

