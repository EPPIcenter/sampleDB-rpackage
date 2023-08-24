library(dplyr)
library(sampleDB)
library(shiny)
library(lubridate)
library(DT)
library(purrr)

#load helper files
for(server_helper in list.files(path = "server_helpers", full.names = T, recursive = T)){
  source(server_helper, local = TRUE)
}

function(input, output, session) {


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

      # Prompt for session restart
      showModal(modalDialog(
        title = "Restart Needed",
        "Operation completed successfully. Please restart the session for changes to take effect.",
        footer = actionButton("restartBtn", "Restart Session")
      ))

    }, error = function(e) {
      # Display any errors in a modal for feedback
      showModal(modalDialog(
        title = "Error",
        paste("Failed to", operation, "the database:", e$message)
      ))
    })
  }
 
  dbUpdateEvent <- reactivePoll(1000 * 5, NULL,
    function() file.mtime(Sys.getenv("SDB_PATH")),
    function() TRUE
  )

  # Reactive value to track if the database update is required
  update_required <- reactiveVal(FALSE)

  # Check the database version initially and set update_required if needed
  observe({
    # Set path to .sqlite database
    database <- Sys.getenv("SDB_PATH")
    expected_versions <- get_expected_versions("sampleDB")
    current_version <- get_db_version(database)

    if (is.na(current_version) || !file.exists(database)) {
      showModal(modalDialog(
        title = "Clean Install Detected",
        "It appears this is a clean install. Setting up the database...",
        footer = actionButton("btn_setup", "Setup New Database"),
        easyClose = FALSE # Prevents dismissing the modal without action
      ))
    } else if (current_version != expected_versions$database) {
      update_required(TRUE)

      # Display modal dialog if update is required
      showModal(modalDialog(
        title = "Database Version Mismatch",
        "The database version is incorrect. Please upgrade to ensure the application functions correctly.",
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

  # Restart the session when the restart button is clicked
  observeEvent(input$restartBtn, {
    removeModal()
    session$reload()
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
      
      # AppSearchDelArchSamples(session, input, database, output, dbUpdateEvent) 
      
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

      # ControlReference(session, input, output, database)

      # # Configuration panel
      AppPreferencesPanel(session, input, output, database)

      # --------------- About ------------
    }
  })
}
