library(shiny)
library(shinybusy)
library(shinyjs)
library(purrr)
library(RSQLite)
library(dbplyr)
library(shinyBS)
library(reactable)

# App Function for Uploading Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

AppUploadSamples <- function(session, input, output, database) {

  rv <- reactiveValues(
    user_file = NULL, # this holds a file that is ready for upload
    console_verbatim = FALSE, # whether to print mulitple lines to the console
    error = FALSE, # whether to start an error workflow
    user_action_required = FALSE, # whether the user needs to add additional inputs
    required_elements = NULL # elements on form that need user attention
  )
  error <- reactiveValues(
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
    rv$error <- NULL
  })

  observeEvent(input$Exit, ignoreInit = TRUE, {
    error$title = ""
    error$message = ""
    error$caption = ""
    error$table = NULL
    rv$error <- NULL
    removeModal()
  })

  observeEvent(input$UploadSampleDataSet, ignoreInit = TRUE, {
    dataset <- input$UploadSampleDataSet

    message(paste("Loaded", dataset$name))

    tryCatch({
      withCallingHandlers({

        ## format the file
        rv$user_file <- sampleDB::ProcessCSV(
          user_csv = dataset$datapath,
          user_action = "upload",
          file_type = input$UploadFileType,
          sample_storage_type = input$UploadSampleType
        )
      },
      message = function(m) {
        shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
        rv$console_verbatim <- TRUE
      })
    },
    formatting_error = function(e) {
      message("Caught formatting error")
      print(e$df)

      ## Read File Specification File
      file_specs_json <- rjson::fromJSON(file = system.file(
        "extdata", "file_specifications.json", package = .sampleDB$pkgname))

      sample_type_index <- which(lapply(file_specs_json$shared$sample_type, function(x) x$id) == input$UploadSampleType)

      ## UI components that are put in place to fill missing data go here

      manifest_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$name
      location_parameters <- file_specs_json$shared$sample_type[[sample_type_index]]$location
      location_parameters <- unlist(location_parameters[c("name", "level_I", "level_II")])
      required_elements <- c()

      columns <- e$df$column

      missing <- columns[!columns %in% c(location_parameters, manifest_name)]
      if (length(missing) > 0) {
        rv$error <- TRUE
        error$title = "Invalid File Detected"
        error$message = e$message
        error$caption = "Please see the table below"
        error$table = e$df
      } else {
        if (manifest_name %in% columns) {
          shinyjs::show("UploadManifestName")
          required_elements <- c(required_elements, "UploadManifestName")
        }

        # should be all or none
        if (all(location_parameters %in% columns)) {
          shinyjs::show("UploadLocationRoot")
          shinyjs::show("UploadLocationLevelI")
          shinyjs::show("UploadLocationLevelII")
          required_elements <- c(required_elements, c("UploadManifestName", "UploadLocationLevelI", "UploadLocationLevelII"))

        }

        shinyjs::disable("UploadSampleType")
        shinyjs::disable("UploadFileType")

        rv$required_elements <- required_elements
        rv$user_action_required <- FALSE
        rv$user_file <- NULL # sanity check
      }
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

  observeEvent(input$UploadAction, ignoreInit = TRUE, {

    if (isTRUE(rv$user_action_required)) {
      message("Upload action halted - user action required")
      return()
    }

    dataset <- input$UploadSampleDataSet
    early_stop <- FALSE
    if (is.null(rv$user_file)) {
      # dataset <- input$UploadSampleDataSet
      message(paste("Loaded", dataset$name))

      tryCatch({
        withCallingHandlers({

          container_name <- NULL
          if (typeof(input$UploadManifestName) == "character" && input$UploadManifestName != "") {
            container_name <- input$UploadManifestName 
          }

          location_parameters <- NULL
          if (typeof(input$UploadLocationRoot) == "character" && input$UploadLocationRoot != "") {
            location_parameters <- c(location_parameters, list(name = input$UploadLocationRoot))
          }
          if (typeof(input$UploadLocationLevelI) == "character" && input$UploadLocationLevelI != "") {
            location_parameters <- c(location_parameters, list(level_I = input$UploadLocationLevelI))
          }
          if (typeof(input$UploadLocationLevelII) == "character" && input$UploadLocationLevelII != "") {
            location_parameters <- c(location_parameters, list(level_II = input$UploadLocationLevelII))
          }

          if (!is.null(location_parameters) && !all(c("name", "level_I", "level_II") %in% names(location_parameters))) {
            stop("Missing location parameter")
          }

          ## format the file
          rv$user_file <- sampleDB::ProcessCSV(
            user_csv = dataset$datapath,
            user_action = "upload",
            file_type = input$UploadFileType,
            sample_storage_type = input$UploadSampleType,
            container_name = container_name,
            freezer_address = location_parameters
          )
        },
        message = function(m) {
          shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
          rv$console_verbatim <- TRUE
        })
      },
      validation_error = function(e) {
        message("Caught validation error")
        early_stop <<- TRUE
        html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
        shinyjs::html(id = "UploadOutputConsole", html = html, add = rv$console_verbatim)
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
        shinyjs::html(id = "UploadOutputConsole", html = html, add = rv$console_verbatim)
        rv$console_verbatim <- FALSE
      })

    }

    if (early_stop) { return() }

    message("Starting Upload...")

    b_use_wait_dialog <- FALSE

    tryCatch({
      withCallingHandlers({

        # simple way to add a dialog or not
        b_use_wait_dialog <- nrow(rv$user_file) > 5

        if (b_use_wait_dialog) {
          show_modal_spinner(
            spin = "double-bounce",
            color = "#00bfff",
            text = paste("Uploading", nrow(rv$user_file), "samples, please be patient...")
          )
        }

        shinyjs::reset("UploadAction")
        sampleDB::UploadSamples(sample_type_id = as.integer(input$UploadSampleType), upload_data = rv$user_file)
      },
      message = function(m) {
        shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
      })
    },
    error = function(e) {
      message(e)
      html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
      shinyjs::html(id = "UploadOutputConsole", html = html, add = rv$console_verbatim)
    },
    finally = {
      if (b_use_wait_dialog)
        remove_modal_spinner()

      rv$user_file <- NULL
      rv$console_verbatim <- FALSE
    })

  })

  observeEvent(input$UploadSampleType, {

    shinyjs::reset("UploadLocationRoot")
    shinyjs::reset("UploadLocationLevelI")
    shinyjs::reset("UploadLocationLevelII")

    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    sample_type_id <- as(local(input$UploadSampleType), "integer")

    sample_type_name <- DBI::dbReadTable(con, "sample_type") %>%
      filter(id == sample_type_id) %>%
      pull(name)

    ## Read File Specification File
    file_specs_json <- rjson::fromJSON(file = system.file(
      "extdata", "file_specifications.json", package = .sampleDB$pkgname))

    sample_type_index <- which(lapply(file_specs_json$sample_type, function(x) x$id) == input$UploadSampleType)
    sample_file_types <- file_specs_json$sample_type[[sample_type_index]]$file_types
    file_type_indexes <- which(lapply(file_specs_json$file_types, function(x) x$id) %in% sample_file_types)
    file_type_names <- lapply(file_type_indexes, function(x) file_specs_json$file_types[[x]]$name)
    names(sample_file_types) <- file_type_names

    updateRadioButtons(
      session,
      "UploadFileType",
      choices = sample_file_types,
      inline = TRUE
    )

    updateSelectInput(
      session, 
      "UploadLocationRoot",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        collect() %>% 
        pull(name) %>%
        unique(.)
      )
    )

    manifest <- switch(
      sample_type_name,
      "Micronix" = "micronix_plate",
      "Cryovial" = "cryovial_box",
      "DBS" = "dbs_paper"
    )


    updateSelectizeInput(
      session,
      "UploadManifestName",
      label = switch(
        sample_type_name,
        "Micronix" = "Plate Name",
        "Cryovial" = "Box Name",
        "DBS" = "Paper Name"
      ),
      selected = "",
      choices = c("", DBI::dbReadTable(con, manifest) %>% pull(name)),
      options = list(create = TRUE)
    )

    updateSelectInput(
      session,
      "UploadLocationLevelI",
      label = switch(
        sample_type_name,
        "Micronix" = "Shelf Name", 
        "Cryovial" = "Rack Number",
        "DBS" = "To Be Implemented"
      )
    )

    updateSelectInput(
      session,
      "UploadLocationLevelII",
      label = switch(
        sample_type_name,
        "Micronix" = "Basket Name",
        "Cryovial" = "Rack Position",
        "DBS" = "To Be Implemented"
      )
    )

    DBI::dbDisconnect(con)
  })

  example_data <- reactiveValues(
    required = NULL,
    user_input = NULL,
    conditional = NULL,
    optional = NULL
  )

  observe({
    ## Read File Specification File
    file_specs_json <- rjson::fromJSON(file = system.file(
      "extdata", "file_specifications.json", package = .sampleDB$pkgname))

    ## Required Column Names

    file_index <- which(lapply(file_specs_json$file_types, function(x) x$id) == input$UploadFileType)
    sample_storage_type_index <- which(lapply(file_specs_json$file_types[[file_index]]$sample_type, function(x) x$id) == input$UploadSampleType)

    if (length(sample_storage_type_index) == 0) {
      stop("Unimplemented file specifications for this sample storage type.")
    }

    actions <- file_specs_json$file_types[[file_index]]$sample_type[[sample_storage_type_index]]$actions[['upload']]
    required_user_column_names <- actions[['required']]
    conditional_user_column_names <- actions[['conditional']]
    optional_user_column_names <- actions[['optional']]

    ## Shared fields
    sample_type_index <- which(lapply(file_specs_json$shared$sample_type, function(x) x$id) == input$UploadSampleType)

    example_data$required  <- c(required_user_column_names, file_specs_json$shared$upload[['required']])
    example_data$conditional <- conditional_user_column_names <- c(conditional_user_column_names, file_specs_json$shared$upload[['conditional']])
    optional_user_column_names <- c(optional_user_column_names, file_specs_json$shared$upload[['optional']])

    manifest_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$name
    manifest_barcode_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$barcode
    location_parameters <- file_specs_json$shared$sample_type[[sample_type_index]]$location
    location_parameters <- unlist(location_parameters[c("name", "level_I", "level_II")])

    example_data$user_input <- c(manifest_name, unname(location_parameters))
    example_data$optional <- c(optional_user_column_names, c(manifest_barcode_name))
  })

  observe({
    output$UploadFileExampleRequired <- renderReactable({
      mat <- matrix(nrow = 0, ncol = length(example_data$required))
      colnames(mat) <- example_data$required
      return(reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE)))
    })

    output$UploadFileExampleUserInput <- renderReactable({
      mat <- matrix(nrow = 0, ncol = length(example_data$user_input))
      colnames(mat) <- example_data$user_input
      return(reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE)))
    })

    output$UploadFileExampleConditional <- renderReactable({
      mat <- matrix(nrow = 0, ncol = length(example_data$conditional))
      colnames(mat) <- example_data$conditional
      return(reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE)))
    })

    output$UploadFileExampleOptional <- renderReactable({
      mat <- matrix(nrow = 0, ncol = length(example_data$optional))
      colnames(mat) <- example_data$optional
      return(reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE)))
    })
  })

  observeEvent(input$UploadLocationRoot, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    updateSelectInput(
      session,
      "UploadLocationLevelI",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(name == local(input$UploadLocationRoot)) %>%
        collect() %>% 
        pull(level_I)
      )
    )
    DBI::dbDisconnect(con)

    shinyjs::reset("UploadLocationLevelI")
    shinyjs::reset("UploadLocationLevelII")
  })

  observeEvent(input$UploadLocationLevelI, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    updateSelectInput(
      session,
      "UploadLocationLevelII",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(name == local(input$UploadLocationRoot) && level_I == local(input$UploadLocationLevelI)) %>%
        collect() %>% 
        pull(level_II)
      )
    )
    DBI::dbDisconnect(con)
  })

  observeEvent(input$ClearUploadForm, {
    shinyjs::enable("UploadSampleType")
    shinyjs::enable("UploadFileType")
    shinyjs::reset("UploadSampleDataSet")
    
    shinyjs::hide("UploadManifestName")
    shinyjs::hide("UploadLocationRoot")
    shinyjs::hide("UploadLocationLevelI")
    shinyjs::hide("UploadLocationLevelII")

    shinyjs::reset("UploadManifestName")
    shinyjs::reset("UploadLocationRoot")
    shinyjs::reset("UploadLocationLevelI")
    shinyjs::reset("UploadLocationLevelII")

    rv$user_file <- NULL
  })
}
