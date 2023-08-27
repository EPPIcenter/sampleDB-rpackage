ControlReference <- function(session, input, output, database, dbUpdateEvent) {
	rv <- reactiveValues(
		error = NULL,
		user_file = NULL
  )

  #' Open modal dialog for Strain Information
  #' Opens a modal dialog where users can manually enter a new strain
  #' or upload a CSV file to add new strains.
  observeEvent(input$StrainModalID, {
    showModal(modalDialog(
      title = "Strain Information",
      h3("Manual Entry"),
      textInput("InputControlNewStrain", "Enter New Strain Name:"),
      actionButton("InputCreateStrainSubmit", "Submit"),
      hr(),
      h3("Upload by File"),
      fileInput("InputUploadStrains", "Choose CSV File"),
      actionButton("InputUploadStrainFromCSVSubmit", "Upload File"),
      footer = modalButton("Close")
    ))
  })

  #' Add new Strain Manually
  #' Takes a manually entered strain name and saves it to the database.
  observeEvent(input$InputCreateStrainSubmit, ignoreInit = TRUE, {
    tryCatch({
      if (!is.null(input$InputControlNewStrain) && input$InputControlNewStrain != "") {
        con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
        df.payload <- data.frame(name = input$InputControlNewStrain)
        res <- dbAppendTable(con, "strain", df.payload)
        dbDisconnect(con)

        show_success_notification(session, "strain", res)
      }
    }, error = function(e) {
      show_general_error_modal(e)
    })
  })

  #' Add new Strains from CSV File
  #' Takes a CSV file of new strains and saves them to the database.
  observeEvent(input$InputUploadStrainFromCSVSubmit, ignoreInit = TRUE, {
    
    # Check dataset
    dataset <- input$InputUploadStrains
    if (is.null(dataset) || is.null(dataset$datapath)) {
      message("Aborting upload - no file uploaded")
      return()
    }
    
    # Format file if user_file is null
    if (is.null(rv$user_file)) {
      success <- tryCatch({
        rv$user_file <- process_reference_csv(
          user_csv = dataset$datapath,
          user_action = "upload",
          reference = "strains"
        )
        TRUE
      },
      formatting_error = function(e) {
        show_formatting_error_modal(e)
        FALSE
      },
      validation_error = function(e) {
        show_validation_error_modal(e)
        FALSE
      },
      error = function(e) {
        show_general_error_modal(e)
        FALSE
      })

      if (!success) return()
    }
    
    tryCatch({
      shinyjs::reset("InputUploadStrains")
      res <- append_strains_to_db(rv$user_file)
      show_success_notification(session, "strain", res)
    },
    error = function(e) {
      show_general_error_modal(e)
    },
    finally = {
      rv$user_file <- NULL
    })
  })

  #' Open modal dialog for Batch ID Information
  #' Opens a modal dialog where users can manually enter 
  #' Batch ID, Batch Date, Description, and Lead Person.
  observeEvent(input$BatchModalID, {
    showModal(modalDialog(
      title = "Batch ID Information",
      dateInput("InputCreateBatchDate", "Batch Date:", value = Sys.Date()),
      textInput("InputCreateBatchDescription", "Description:"),
      textInput("InputCreateBatchLeadPerson", "Lead Person:"),
      actionButton("InputBatchIDUploadSubmit", "Submit"),
      footer = modalButton("Close")
    ))
  })

  #' Add new Batch ID Information
  #' Takes the user input for Batch ID, Batch Date, Description, and Lead Person
  #' and saves it into the database.
  observeEvent(input$InputBatchIDUploadSubmit, {
    # Retrieve the inputs
    batch <- as.character(input$InputCreateBatchDate)
    batch_desc <- input$InputCreateBatchDescription
    lead_person <- input$InputCreateBatchLeadPerson
    now <- as.character(lubridate::now())
    
    # Prepare the inputs in a data frame
    user_data <- data.frame(
      RowNumber = 1, # RowNumber is required for error handling purposes
      title = batch,
      Batch = batch, # 'Batch' instead of 'short_code' for error handling purposes
      description = batch_desc,
      lead_person = lead_person,
      created = now,
      last_updated = now,
      is_longitudinal = 0,
      stringsAsFactors = FALSE
    )

    tryCatch({
      # Validate the references if applicable
      validate_references(database, user_data, "batch", "create")
    
      # Initialize database connection
      con <- sampleDB::init_db_conn(database)
      on.exit(dbDisconnect(con), add = TRUE)
    
      # Append the data (and rename the column back)
      user_data <- user_data %>% dplyr::rename(short_code = Batch) %>% select(-c(RowNumber))
      dbAppendTable(con, "study", user_data)
    
      # Notify success
      show_success_notification(session, "batch", 1) # 1 batch created
    },
    validation_error = function(e) {
      show_validation_error_modal(e)
    },
    error = function(e) {
      show_general_error_modal(e)
    })
  })

  #' Open modal dialog for Uploading Compositions
  #' Opens a modal dialog where users can upload compositions.
  observeEvent(input$CompositionModalID, ignoreInit = TRUE, {
    
    # Trigger a modal for uploading compositions
    showModal(modalDialog(
      title = "Upload Compositions",
      fileInput("InputUploadCompositionIDs", "Choose CSV File"),
      actionButton("CompositionUploadSubmit", "Submit"),
      footer = modalButton("Close")
    ))
    
  })

  #' Upload Compositions
  #' Takes the user input for compositions (either as a dataset or a path to a CSV),
  #' validates it, and then saves it into the database.
  observeEvent(input$CompositionUploadSubmit, ignoreInit = TRUE, {
    
    # Check dataset
    dataset <- input$InputUploadCompositionIDs
    if (is.null(dataset) || is.null(dataset$datapath)) {
      message("Aborting upload - no file uploaded")
      return()
    }

    # Format file if user_file is null
    if (is.null(rv$user_file)) {
      success <- tryCatch({
        rv$user_file <- process_reference_csv(
          user_csv = dataset$datapath,
          user_action = "upload",
          reference = "compositions"
        )
        TRUE
      },
      formatting_error = function(e) {
        show_formatting_error_modal(e)
        FALSE
      },
      validation_error = function(e) {
        show_validation_error_modal(e)
        FALSE
      },
      error = function(e) {
        show_general_error_modal(e)
        FALSE
      })

      if (!success) return()
    }

    tryCatch({
      shinyjs::reset("InputCompositionUploadAction")
      all_labels <- upload_compositions(rv$user_file)
      show_success_notification(session, "composition", length(all_labels))
    },
    error = function(e) {
      show_general_error_modal(e)
    },
    finally = {
      rv$user_file <- NULL
    })

  })

  observeEvent(dbUpdateEvent(), {

    con <- init_db_conn(database)
    on.exit(dbDisconnect(con), add = TRUE)
    updateSelectizeInput(session, "InputControlSearchBatch", choices = get_batches(con), selected = input$InputControlSearchBatch)
    updateSelectizeInput(session, "InputControlSearchStrain", choices = get_strains(con), selected = input$InputControlSearchStrain)
    updateSelectizeInput(session, "InputControlSearchPercentage", choices = get_percentages(con), selected = input$InputControlSearchPercentage)
    updateSelectizeInput(session, "InputControlSearchCompositionTypes", choices = get_composition_types(con), selected = input$InputControlSearchCompositionTypes)
  })


  # Create a compositions table

  # Initialize the custom filter with default values
  composition_filter_set <- createFilterSetReactive()

  # Declare filters for searching and establish any filter dependencies
  observe({
    input_filters <- list(
      strain = input$InputControlSearchStrain,
      percentage = input$InputControlSearchPercentage,
      composition_types = input$InputControlSearchCompositionTypes
    )
    filter_keys <- c("strain", "percentage", "composition_types")
    
    process_filters(input_filters, filter_keys, composition_filter_set)
  })

  filtered_composition_data <- reactive({
    
    # Get the data from the database
    compositions <- search_compositions(composition_filter_set$get()) %>%
      arrange(strain_count, label, percentage) %>%
      select(strain_count, strain, percentage, label, legacy) %>%
      dplyr::rename(
        `Composition Type` = strain_count,
        Strain = strain,
        Percentage = percentage,
        CompositionID = label,
        Legacy = legacy
      ) 
     
    if (!is.null(compositions)) {
      compositions
    } else {
      tibble::tibble()
    }
  }) %>% debounce(500)  # 500ms delay


  observe({
    output$OutputControlSearchResults <- renderReactable({
      # Get filtered data from our reactive
      search_table <- filtered_composition_data()

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

  selected <- reactive(getReactableState("OutputControlSearchResults", "selected"))

  observe({
    # Download compositions table
    output$DownloadControlSearchResults <- downloadHandler(
      filename = function() {
        paste("compositions", "csv", sep = ".")
      },
      content = function(file) {
        write.csv(filtered_composition_data() %>% dplyr::slice(selected()), file, row.names = FALSE, quote = FALSE)
      }
    )
  })
}
