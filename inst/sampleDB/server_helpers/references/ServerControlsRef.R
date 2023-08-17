ControlReference <- function(session, input, output, database) {
	rv <- reactiveValues(
		error = NULL,
		user_file = NULL
  )

  observeEvent(input$InputCreateStrain, ignoreInit = TRUE, {

    tryCatch({

      if (!is.null(input$InputControlNewStrain) && input$InputControlNewStrain != "") {

        con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))  # Adjust database connection path as required

        df.payload <- data.frame(name = input$InputControlNewStrain)
        res <- dbAppendTable(con, "strain", df.payload)

        dbDisconnect(con)  # Closing the connection

        msg <- sprintf("%d strain added...", res)
        showNotification(msg, id = "ControlCreateStrain", type = "message", action = NULL, duration = 3, closeButton = FALSE)
      }

    }, error = function(e) {
      show_general_error_modal(e)
    })
  })


  observeEvent(input$InputBatchIDUploadAction, {

    # Retrieve the inputs
    title <- input$InputCreateBatchID
    batch_date <- input$InputCreateBatchID
    batch_desc <- input$InputCreateBatchDescription
    lead_person <- input$InputCreateBatchLeadPerson
    
    # Prepare the inputs in a data frame
    user_data <- data.frame(
      RowNumber = 1,
      Title = title,
      Batch = batch_date,
      Description = batch_desc,
      LeadPerson = lead_person,
      stringsAsFactors = FALSE
    )
    
    # We assume you've set up a database connection or reference named "database"
    tryCatch({
      validate_references(database, user_data, "batch", "create")

      user_data$RowNumber <- NULL
      now <- lubridate::now()

      user_data$created <- now
      user_data$last_updated <- now
      user_data$is_longitudinal <- 0

      colnames(user_data) <- c("title", "short_code", "description", "lead_person", "created", "last_updated", "is_longitudinal")

      user_data$short_code <- as.character(user_data$short_code)
      user_data$title <- as.character(user_data$title)

      con <- sampleDB::init_db_conn(database)
      on.exit(dbDisconnect(con), add = TRUE)
      dbAppendTable(con, "study", user_data)

      showNotification("Batch data validated and uploaded successfully!", type = "success", duration = 3)

    },
    validation_error = function(e) {
      show_validation_error_modal(e)
    },
    error = function(e) {
      show_general_error_modal(e)
    })
  })


  ###### Delarch specific functionality

  observeEvent(input$InputControlArchiveAction, ignoreInit = TRUE, {

    con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))

    user.filtered.rows = rv$table
    user.selected.rows = user.filtered.rows[selected(), ]

    rt.select = rv$update.table[rv$update.table %in% colnames(user.selected.rows)]
    user.selected.rows.select = user.selected.rows %>% select(all_of(rt.select))
    colnames(user.selected.rows.select) <- unname(rv$dbmap[names(rv$dbmap) %in% rv$update.table])

    rv$user_selected_rows = user.selected.rows.select

    rt <- reactable(
      user.selected.rows.select,
      defaultColDef = colDef(
        minWidth = 95,
        html = TRUE,
        sortable = TRUE,
        resizable = FALSE,
        na = "-", 
        align = "center"
      ),
      selection = "single",
      onClick = "select",
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

    showModal(
      modalDialog(
        title = "Update Controls",
        size = "l",
        tags$em("Please review the following fields and your selected samples below.", style = "color: grey;font-size: 18px;"),
        hr(),
        tags$p("Please review your selected samples below before submitting. You may cancel by selecting", tags$em("Dismiss"), "below or by clicking outside of the dialog box."),
        renderReactable({ rt }),
        hr(),
        fluidRow( column(width = 6, numericInput(label = tags$strong("Number of DBS Controls Punched"), inputId = "ControlInputNumControls", value = 0, width = '75%')),
                  column(width = 6, tags$p("Please indicate the number of controls that were punched."))
        ),
        easyClose = TRUE,
        fade = TRUE,
        footer = tagList(actionButton("ArchiveControlAction", label = "Update"), modalButton("Dismiss"))
      )
    )

    DBI::dbDisconnect(con)
  })


  observeEvent(input$InputUploadStrainAction, ignoreInit = TRUE, {
    
    # Check dataset
    dataset <- input$InputUploadStrains
    if (is.null(dataset) || is.null(dataset$datapath)) {
      message("Aborting upload - no file uploaded")
      return()
    }

    # Function to show spinner if needed
    show_spinner_if_needed <- function(data) {
      if (nrow(data) > 5) {
        show_modal_spinner(
          spin = "double-bounce",
          color = "#00bfff",
          text = paste("Uploading", nrow(data), "strains, please be patient...")
        )
        return(TRUE)
      }
      return(FALSE)
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

    message("Starting Upload...")
    
    b_use_wait_dialog <- show_spinner_if_needed(rv$user_file)

    tryCatch({
      shinyjs::reset("InputUploadStrains")
      res <- append_strains_to_db(rv$user_file)
    },
    error = function(e) {
      show_general_error_modal(e)
    },
    finally = {
      if (b_use_wait_dialog) {
        remove_modal_spinner()
      }
      rv$user_file <- NULL
    })
  })

  observeEvent(input$InputCompositionIDUploadAction, ignoreInit = TRUE, {
    
    # Check dataset
    dataset <- input$InputUploadCompositionIDs
    if (is.null(dataset) || is.null(dataset$datapath)) {
      message("Aborting upload - no file uploaded")
      return()
    }

    # Function to show spinner if needed
    show_spinner_if_needed <- function(data) {
      if (nrow(data) > 5) {
        show_modal_spinner(
          spin = "double-bounce",
          color = "#00bfff",
          text = paste("Uploading", nrow(data), "compositions, please be patient...")
        )
        return(TRUE)
      }
      return(FALSE)
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

    message("Starting Upload...")

    b_use_wait_dialog <- show_spinner_if_needed(rv$user_file)
    
    tryCatch({
      shinyjs::reset("InputCompositionUploadAction")
      upload_compositions(rv$user_file)
    },
    error = function(e) {
      show_general_error_modal(e)
    },
    finally = {
      if (b_use_wait_dialog) {
        remove_modal_spinner()
      }
      rv$user_file <- NULL
    })

  })


  observeEvent(input$InputControlStudyAction, ignoreInit=TRUE, {

    database_path <- Sys.getenv("SDB_PATH")

    # Create the study in the database
    res <- append_study_to_db(
      title = input$InputControlNewStudy,
      short_code = input$InputControlNewStudy,
      description = input$InputControlStudyDesc,
      lead_person = input$InputControlBatchPerson,
      is_longitudinal = 0, 
      database = database_path
    )

  })

  observeEvent(input$InputControlNewStrain, {

    # Check if input value is provided
    if (is.null(input$InputControlNewStrain) || input$InputControlNewStrain == "") {
      shinyjs::disable("InputCreateStrain")
      return()
    }

    con <- dbConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))
    
    # Check for existing strain
    existing_strain <- tbl(con, "strain") %>% 
      filter(name %in% local(input$InputControlNewStrain)) %>%
      collect()

    # Enable or Disable based on existence
    if (nrow(existing_strain) == 0) {
      shinyjs::enable("InputCreateStrain")
    } else {
      shinyjs::disable("InputCreateStrain")
    }

    dbDisconnect(con)
  })

}

