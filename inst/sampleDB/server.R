library(dplyr)
library(sampleDB)
library(shiny)
library(lubridate)
library(DT)
# library(purrr)

# Load helper files
for(server_helper in list.files(path = "server_helpers", full.names = TRUE, recursive = TRUE)){
  source(server_helper, local = TRUE)
}

# Function to show error modal and terminate the app on acknowledgment
show_error_modal <- function(error, input, output, session) {
  # Convert the call to a single character string, checking for NULL
  errcall <- if (is.null(error$call)) {
    "No function call information available"
  } else {
    paste(as.character(error$call), collapse="\n")
  }

  errmsg <- ifelse(is.null(error$message), "No message available", error$message)
  
  # Initialize error details string
  errdetails <- ""
  
  # Iterate over each element in the error object
  error_names <- names(error)
  for (name in error_names) {
    value <- error[[name]]
    # Convert each element to a character string
    value_str <- toString(if(is.list(value)) {
      sapply(value, function(x) toString(x))
    } else {
      value
    })
    errdetails <- paste(errdetails, sprintf("%s: %s\n", name, value_str), sep="\n")
  }

  # Download handler for the error report
  output$downloadError <- downloadHandler(
    filename = function() {
      paste("error-details-", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      writeLines(errdetails, file)
    }
  )
  
  showModal(
    modalDialog(
      size = "l",
      title = "An Error During Database Setup",
      tags$p(HTML("Something went wrong. Please download the error details below and contact Brian Palmer at <a href='mailto:brian.palmer@ucsf.edu'>brian.palmer@ucsf.edu</a>.")),
      tags$hr(),
      tags$div(
        id = "errdetails",
        style = "max-height: 400px; overflow-y: auto;",
        tags$pre(style = "white-space: pre-wrap;", errdetails)
      ),
      downloadButton('downloadError', 'Download Error Details'),
      footer = tagList(
        tags$strong("The application will terminate when you close this dialog."),
        actionButton("error_modal_ok", "Close the application")
      )
    )
  )
  
  observeEvent(input$error_modal_ok, {
    shiny::stopApp()
  })
}

function(input, output, session) {

  # Function to terminate the user session
  terminate_user_session <- function(error) {
    message("Terminating user session due to database update failure.")
    show_error_modal(error, input, output, session)
  }

  # Unified function for database operations
  handle_database_operation <- function(operation) {
    # Variables (Replace with actual sources/values as needed)
    pkgname <- "sampleDB"
    database <- Sys.getenv("SDB_PATH")

    tryCatch({
      expected_versions <- get_expected_versions("sampleDB")

      setup_database(expected_versions$database, pkgname, database)

      # After successful setup or upgrade
      update_required(FALSE)  # Set update_required to FALSE

      # Close the modal
      removeModal()

      showNotification(
        "Database upgrade/setup completed successfully!",
        type = "message",  
        duration = 5,      
        closeButton = TRUE 
      )

    }, error = function(e) {
      # Terminate the user session on error
      terminate_user_session(e)
    })
  }

  dbUpdateEvent <- reactivePoll(1000 * 5, session,
    function() file.mtime(Sys.getenv("SDB_PATH")),
    function() {
      current_filestamp <- file.mtime(Sys.getenv("SDB_PATH"))
      message(paste("Database last updated at", current_filestamp))
      return (current_filestamp)
    }
  )

  # Reactive value to track if the database update is required
  update_required <- reactiveVal(FALSE)

  # Check the database version initially and set update_required if needed
  observe({
    # Set path to .sqlite database
    database <- Sys.getenv("SDB_PATH")
    expected_versions <- get_expected_versions("sampleDB")
    current_version <- get_db_version(database)

    cat("Current database version: ", current_version, "\n")
    cat("Database path: ", database, "\n")

    if (is.na(current_version) || !file.exists(database)) {
      update_required(TRUE)

      showModal(modalDialog(
        title = "Clean Install Detected",
        paste0("It appears this is a clean install. Setting up the database with version ", expected_versions$database, "."),
        footer = actionButton("btn_setup", "Setup New Database"),
        easyClose = FALSE # Prevents dismissing the modal without action
      ))
    } else if (current_version != expected_versions$database) {
      update_required(TRUE)

      # Display modal dialog if update is required
      showModal(modalDialog(
        title = "Database Version Mismatch",
        paste0("The current database version is ", current_version, 
               ". The expected version is ", expected_versions$database, 
               ". Please upgrade to ensure the application functions correctly."),
        footer = actionButton("btn_upgrade", "Upgrade"),
        easyClose = FALSE # This ensures the modal cannot be dismissed without pressing "Upgrade"
      ))
    } else {
      update_required(FALSE)
    }
  })


  # Handle the database setup for a clean install
  observeEvent(input$btn_setup, {
    handle_database_operation("setup")
  })

  # # Handle the database update when the action button is clicked
  observeEvent(input$btn_upgrade, {
    handle_database_operation("upgrade")
  })

  # Conditionally load main app logic if update is not required
  observe({
    # Set path to .sqlite database
    database <- Sys.getenv("SDB_PATH")
    if (!update_required()) {

      # Back up database when app is fired up... supplementary files such as the backup generator are stored in /extdata
      # for (i in system("bash /sampleDB_backup_generator.sh", intern = TRUE)) message(i)
      Backup_SampleDB(checksum = TRUE) 

      data("micronix_na")
      data("cryovial_na")

      backups <- list.files(file.path(dirname(database), "backups"))
      if (length(backups) > 10) {
        oldest_backup <- file.path(dirname(database), "backups", head(backups, 1))
        message(paste0("Removing oldest backup: ", oldest_backup))
        file.remove(file.path(dirname(database), "backups", head(backups, 1)))
      }

      # --------- Upload Samples -------------

      # Upload Micronix Samples
      AppUploadSamples(session, input, output, database, dbUpdateEvent)

      # -------- Search, Archive and Delete Samples -------------
      
      AppSearchDelArchSamples(session, input, database, output, dbUpdateEvent) 
      
      # # # -------- Move Samples -------------

      AppMoveSamples(session, input, output, database)

      # # # -------- Edit Containers --------
      
      EditWetlabContainers(session, input, database, output, dbUpdateEvent)  
      
      # # -------- Update References ---------------
      
      # Update Freezers
      UpdateLabFreezers(session, input, output, database)
      
      # # Update Specimen Types
      UpdateSpecimenTypes(session, input, output, database)
      
      # # Update EPPIcenter Lab Studies
      UpdateLabStudies(session, input, output, database)

      ControlReference(session, input, output, database, dbUpdateEvent)

      # # Configuration panel
      AppPreferencesPanel(session, input, output, database)

      # --------------- About ------------
    }
  })
}
