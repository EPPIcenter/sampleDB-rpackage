library(shiny)
library(shinybusy)
library(shinyjs)
library(purrr)
library(RSQLite)
library(dbplyr)
library(reactable)

# App Function for Uploading Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

AppMoveSamples <- function(session, output, input, database) {

  rv <- shiny::reactiveValues(
    user_file = NULL, # this holds a file that is ready for upload
    console_verbatim = FALSE, # whether to print mulitple lines to the console
    error = FALSE, # whether to start an error workflow
    user_action_required = FALSE, # whether the user needs to add additional inputs
    required_elements = NULL # elements on form that need user attention
  )
  error <- shiny::reactiveValues(
    title = "",
    message = "",
    caption = "",
    table = NULL
  )

  observeEvent(rv$error, ignoreInit = TRUE, {

    message("Running error workflow")

    df <- error$table %>%
      dplyr::rename(
        Column = column, 
        Reason = reason,
        Trigger = trigger
      ) %>%
      reactable(.)

    showModal(
      modalDialog(
        title = error$title,
        error$message,
        error$caption,
        renderReactable({ df }),
        footer = modalButton("Exit")
      )
    )
    rv$error = NULL
  })

  observeEvent(input$Exit, ignoreInit = TRUE, {
    error$title = ""
    error$message = ""
    error$caption = ""
    error$table = NULL
    rv$error = NULL
    removeModal()
  })

  observeEvent(input$MoveSampleDataSet, ignoreInit = TRUE, {
    dataset <- input$MoveSampleDataSet

    message(paste("Loaded", dataset$name))

    tryCatch({
      withCallingHandlers({

        ## format the file
        rv$user_file <- sampleDB::ProcessCSV(
          user_csv = dataset$datapath,
          user_action = "move",
          file_type = input$MoveFileType,
          sample_storage_type = input$MoveSampleType,
          container_name = dataset$name
        )
      },
      message = function(m) {
        shinyjs::html(id = "MoveOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
        rv$console_verbatim <- TRUE
      })
    },
    formatting_error = function(e) {
      message("Caught formatting error")
      print(e$df)

      rv$error <- TRUE
      error$title = "Invalid File Detected"
      error$message = e$message
      error$caption = "Please see the table below"
      error$table = e$df
    },
    validation_error = function(e) {
      message("Caught validation error")
      
      html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
      shinyjs::html(id = "UploadOutputConsole", html = html, add = rv$console_verbatim)

      # rv$error <- TRUE
      # error$title <- "Validation error"
      # error$message <- e$message
      # error$caption <- "Please see the table below."

      print(e$df)
    },
    error = function(e) {
      print(e)
      html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
      shinyjs::html(id = "UploadOutputConsole", html = html, add = rv$console_verbatim)
    })

    rv$console_verbatim <- FALSE
  })

  observeEvent(input$MoveAction, ignoreInit = TRUE, {

    if (isTRUE(rv$user_action_required)) {
      message("Upload action halted - user action required")
      return()
    }

    early_stop <- FALSE
    if (is.null(rv$user_file)) {
      dataset <- input$MoveSampleDataSet
      message(paste("Loaded", dataset$name))

      tryCatch({
        withCallingHandlers({

          ## format the file
          rv$user_file <- sampleDB::ProcessCSV(
            user_csv = dataset$datapath,
            user_action = "move",
            file_type = input$MoveFileType,
            sample_storage_type = input$MoveSampleType,
            container_name = dataset$name
          )
        },
        message = function(m) {
          shinyjs::html(id = "MoveOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
          rv$console_verbatim <- TRUE
        })
      },
      validation_error = function(e) {
        message("Caught validation error")
        early_stop <<- TRUE
        html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
        shinyjs::html(id = "MoveOutputConsole", html = html, add = rv$console_verbatim)
        rv$console_verbatim <- FALSE

        # rv$error <- TRUE
        # error$title <- "Validation error"
        # error$message <- e$message
        # error$caption <- "Please see the table below."

        print(e$df)

      },
      error = function(e) {
        early_stop <<- TRUE
        html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
        shinyjs::html(id = "MoveOutputConsole", html = html, add = rv$console_verbatim)
        rv$console_verbatim <- FALSE
      })

    }

    if (early_stop) { return() }

    message("Starting Move...")

    b_use_wait_dialog <- FALSE

    tryCatch({
      withCallingHandlers({

        # simple way to add a dialog or not
        b_use_wait_dialog <- nrow(rv$user_file) > 5

        if (b_use_wait_dialog) {
          show_modal_spinner(
            spin = "double-bounce",
            color = "#00bfff",
            text = paste("Moving", nrow(rv$user_file), "samples, please be patient...")
          )
        }

        shinyjs::reset("MoveAction")
        sampleDB::UploadSamples(sample_type_id = as.integer(input$MoveSampleType), upload_data = rv$user_file)
      },
      message = function(m) {
        shinyjs::html(id = "MoveOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
      })
    },
    error = function(e) {
      message(e)
      html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
      shinyjs::html(id = "MoveOutputConsole", html = html, add = rv$console_verbatim)
    },
    finally = {
      if (b_use_wait_dialog)
        remove_modal_spinner()

      rv$user_file <- NULL
      rv$console_verbatim <- FALSE
    })

  })

  observeEvent(input$MoveSampleType, {

    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    sample_type_id <- as(local(input$MoveSampleType), "integer")

    sample_type_name <- DBI::dbReadTable(con, "sample_type") %>%
      filter(id == sample_type_id) %>%
      pull(name)

    ## Read File Specification File
    file_specs_json <- rjson::fromJSON(file = system.file(
      "extdata", "file_specifications.json", package = .sampleDB$pkgname))

    sample_type_index <- which(lapply(file_specs_json$sample_type, function(x) x$id) == input$MoveSampleType)
    sample_file_types <- file_specs_json$sample_type[[sample_type_index]]$file_types
    file_type_indexes <- which(lapply(file_specs_json$file_types, function(x) x$id) %in% sample_file_types)
    file_type_names <- lapply(file_type_indexes, function(x) file_specs_json$file_types[[x]]$name)
    names(sample_file_types) <- file_type_names

    updateRadioButtons(
      session,
      "MoveFileType",
      choices = sample_file_types,
      inline = TRUE
    )

    DBI::dbDisconnect(con)
  })

  observeEvent(input$ClearMoveForm, ignoreInit = TRUE, {
    shinyjs::enable("MoveSampleType")
    shinyjs::enable("MoveFileType")
    shinyjs::reset("MoveDataSet")

    rv$user_file <- NULL
  })
}
