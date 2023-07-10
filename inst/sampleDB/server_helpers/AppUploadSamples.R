library(shiny)
library(shinyjs)
library(purrr)
library(RSQLite)
library(reactable)
library(shinybusy)

# App Function for Uploading Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

AppUploadSamples <- function(session, input, output, database, dbUpdateEvent) {

  rv <- reactiveValues(
    user_file = NULL, # this holds a file that is ready for upload
    user_file_error_annotated = NULL,
    console_verbatim = FALSE, # whether to print mulitple lines to the console
    error = FALSE, # whether to start an error workflow
    user_action_required = FALSE, # whether the user needs to add additional inputs
    required_elements = NULL, # elements on form that need user attention
    upload_template = NULL # template file to download
  )

  error <- reactiveValues(
    title = "",
    type = "",
    message = "",
    table = NULL
  )

  observe({
    output$ErrorFileDownload <- downloadHandler(
      filename = function() {
        paste(paste(c(input$UploadSampleDataSet$name, "annotated"), collapse="_"), '.csv', sep='')
      },
      content = function(con) {
        write.csv(rv$user_file_error_annotated, con, row.names = FALSE, quote=FALSE)
      }
    )
  })

  observeEvent(rv$error, ignoreInit = TRUE, {
    message("Running error workflow")
    df <- error$list
    modal_size <- "m"
    if (!is.null(error$type) && error$type == "formatting") {
      df <- error$list %>%
        dplyr::rename(
          Column = column, 
          Reason = reason,
          `Triggered By` = trigger
        ) %>%
        reactable(.)

      showModal(
        modalDialog(
          size = "m",
          title = error$title,
          error$message,
          tags$hr(),
          renderReactable({ df }),
          footer = modalButton("Exit")
        )
      )
    } else if (!is.null(error$type) && error$type == "validation") {
      errors <- unique(names(error$list))
      errors <- data.frame(errors)
      colnames(errors) <- "Error"
      df <- reactable(errors, details = function(index) {
        data <- error$list[[index]]$Columns
        htmltools::div(style = "padding: 1rem",
          reactable(
            data, 
            outlined = TRUE, 
            striped = TRUE,
            # rownames = TRUE,
            theme = reactableTheme(
            headerStyle = list(
              "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
              "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
              borderColor = "#555"
            )),
            defaultColDef = colDef(na = "-", align = "center")
          )
        )
      })

      showModal(
        modalDialog(
          size = "l",
          title = error$title,
          tags$p("One or more rows had invalid or missing data. See the errors below and expand them to see which rows caused this error."),
          tags$p("Press the button below to download your file with annotations"),
          downloadButton("ErrorFileDownload"),
          tags$hr(),
          renderReactable({ df }),
          footer = modalButton("Exit")
        )
      )
    } else {
      errmsg = ifelse(is.null(error$message), "No message available", error$message)
      showModal(
        modalDialog(
          size = "l",
          title = error$title,
          tags$p("Something went wrong - contact the app author, and report the error message below."),
          tags$hr(),
          tags$p(errmsg),
          footer = modalButton("Exit")
        )
      )
    }

    rv$error <- NULL
    error$title <- ""
    error$message <- ""
    error$type <- ""
    error$list <- NULL
  })


  # Download a complete upload template
  observe({
    output$UploadFileTemplate <- downloadHandler(
        filename = function() {
          storage_type <- switch(
            input$UploadSampleType,
            "1" = "micronix",
            "2" = "cryovial",
            "3" = "dbs"
          )
          paste(paste(c(storage_type, input$UploadFileType, "upload", "template"), collapse="_"), '.csv', sep='')
        },
        content = function(con) {
          write.csv(rv$upload_template, con, row.names = FALSE, quote=FALSE)
        }
    )
  })

  observeEvent(input$UploadSampleDataSet, ignoreInit = TRUE, {
    dataset <- input$UploadSampleDataSet

    message(paste("Loaded", dataset$name))

    tryCatch({
      withCallingHandlers({

        ## format the file
        rv$user_file <- ProcessCSV(
          user_csv = dataset$datapath,
          user_action = "upload",
          file_type = input$UploadFileType,
          sample_storage_type = storage.type.id.map[[input$UploadSampleType]]
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

      error$type <- "formatting"

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
        error$list = e$df
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

      rv$error <- TRUE
      error$type <- "validation"
      error$title <- e$message
      error$list <- e$data

      # TODO: breakup process csv into three stages(but keep calls in global process csv).
      # Just download the error data frame for now.
      errors <- names(e$data)
      df <- lapply(1:length(errors), function(idx) {
        e$data[[idx]]$CSV %>%
          mutate(Error = errors[idx]) %>%
          mutate(ErrCol = paste(e$data[[idx]]$Columns, collapse = ",")) %>%
          select(Error, colnames(e$data[[idx]]$CSV)) 
      })

      rv$user_file_error_annotated <- do.call("rbind", df) 
    },
    error = function(e) {
      print(e)
      html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
      shinyjs::html(id = "UploadOutputConsole", html = html, add = rv$console_verbatim)

      error$title = "Unknown Error"
      error$type = "unknown"
      error$message = e$message
      error$list = NULL
      rv$error = TRUE
    })

    rv$console_verbatim <- FALSE
  })

  observeEvent(input$UploadAction, ignoreInit = TRUE, {

    if (isTRUE(rv$user_action_required)) {
      message("Upload action halted - user action required")
      return()
    }

    dataset <- input$UploadSampleDataSet
    if (is.null(dataset) || is.null(dataset$datapath)) {
      message("Aborting upload - no file uploaded")
      return()
    }

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
          rv$user_file <- ProcessCSV(
            user_csv = dataset$datapath,
            user_action = "upload",
            file_type = input$UploadFileType,
            sample_storage_type = storage.type.id.map[[input$UploadSampleType]],
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

        rv$error <- TRUE
        error$type <- "validation"
        error$title <- e$message
        error$list <- e$data

        # TODO: just download the error data frame for now
        errors <- names(e$data)
        df <- lapply(1:length(errors), function(idx) {
          e$data[[idx]]$CSV %>%
            mutate(Error = errors[idx]) %>%
            mutate(ErrCol = paste(e$data[[idx]]$Columns, collapse = ",")) %>%
            select(Error, colnames(e$data[[idx]]$CSV)) 
        })

        rv$user_file_error_annotated <- do.call("rbind", df) %>%
          select(-c(RowNumber))

      },
      formatting_error = function(e) {
        message("Caught formatting error")
        print(e$df)

        error$type <- "formatting"
        early_stop <<- TRUE

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
          error$list = e$df
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
      error = function(e) {
        early_stop <<- TRUE
        html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
        shinyjs::html(id = "UploadOutputConsole", html = html, add = rv$console_verbatim)
        rv$console_verbatim <- FALSE

        rv$error <- TRUE
        error$title = "Unknown Error"
        error$type = "unknown"
        error$message = e$message
        error$list = NULL
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
        UploadSamples(sample_type_id = as.integer(input$UploadSampleType), upload_data = rv$user_file)
      },
      message = function(m) {
        shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
      })
    },
    error = function(e) {
      message(e)
      html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
      shinyjs::html(id = "UploadOutputConsole", html = html, add = rv$console_verbatim)
      error$title = "Unknown Error"
      error$type = "unknown"
      error$message = e$message
      error$list = NULL
      rv$error <- TRUE
    },
    finally = {
      if (b_use_wait_dialog)
        remove_modal_spinner()

      rv$user_file <- NULL
      rv$console_verbatim <- FALSE
    })

  })

  observeEvent(dbUpdateEvent(), {

    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
      sample_type_id <- as(local(input$UploadSampleType), "integer")

    sample_type_name <- DBI::dbReadTable(con, "sample_type") %>%
      filter(id == sample_type_id) %>%
      pull(name)

    updateSelectInput(
      session, 
      "UploadLocationRoot",
      selected = input$UploadLocationRoot,
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
      selected = input$UploadManifestName,
      choices = c("", DBI::dbReadTable(con, manifest) %>% pull(name)),
      options = list(create = TRUE)
    )

    updateSelectInput(
      session,
      "UploadLocationLevelI",
      selected = input$UploadLocationLevelI,
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
      selected = input$UploadLocationLevelII,
      label = switch(
        sample_type_name,
        "Micronix" = "Basket Name",
        "Cryovial" = "Rack Position",
        "DBS" = "To Be Implemented"
      )
    )

    dbDisconnect(con)

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

  ## create the example data to display and to download
  observe({
    ## Read File Specification File
    file_specs_json <- rjson::fromJSON(file = system.file(
      "extdata", "file_specifications.json", package = .sampleDB$pkgname))

    ## Required Column Names

    file_index <- which(lapply(file_specs_json$file_types, function(x) x$id) == input$UploadFileType)
    sample_storage_type_index <- which(lapply(file_specs_json$file_types[[file_index]]$sample_type, function(x) x$id) == storage.type.id.map[[input$UploadSampleType]])

    if (length(sample_storage_type_index) == 0) {
      message("Unimplemented file specifications for this sample storage type.")
    } else {
      actions <- file_specs_json$file_types[[file_index]]$sample_type[[sample_storage_type_index]]$actions[['upload']]
      required_user_column_names <- actions[['required']]
      conditional_user_column_names <- actions[['conditional']]
      optional_user_column_names <- actions[['optional']]

      ## Shared fields
      sample_type_index <- which(lapply(file_specs_json$shared$sample_type, function(x) x$id) == input$UploadSampleType)

      required_user_column_names <- c(required_user_column_names, file_specs_json$shared$upload[['required']])
      if (input$UploadFileType == "traxcer") {
        ## Read Configuration File and replace with user override from user preferences
        config <- yaml::read_yaml(Sys.getenv("SDB_CONFIG"))
        if (!is.na(config$traxcer_position$override)) {
          required_user_column_names <- stringr::str_replace(required_user_column_names, config$traxcer_position$default, config$traxcer_position$override)
        }
      }
      example_data$required <- required_user_column_names
      example_data$conditional <- conditional_user_column_names <- c(conditional_user_column_names, file_specs_json$shared$upload[['conditional']])
      optional_user_column_names <- c(optional_user_column_names, file_specs_json$shared$upload[['optional']])

      manifest_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$name
      manifest_barcode_name <- file_specs_json$shared$sample_type[[sample_type_index]]$manifest$barcode
      location_parameters <- file_specs_json$shared$sample_type[[sample_type_index]]$location
      location_parameters <- unlist(location_parameters[c("name", "level_I", "level_II")])

      example_data$user_input <- c(manifest_name, unname(location_parameters))
      example_data$optional <- c(optional_user_column_names, c(manifest_barcode_name))
    }
  })

  observe({
    output$UploadFileExampleRequired <- renderReactable({
      rt <- NULL
      if (input$UploadFileType == "na") {

        sample_type_name <- switch(
          input$UploadSampleType,
          "1" = "micronix",
          "2" = "cryovial",
          "3" = "dbs"
        )
        example <- paste(c(sample_type_name, input$UploadFileType), collapse="_")
        rt <- reactable(eval(as.symbol(example))[, example_data$required], defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
      } else {
        mat <- matrix(nrow = 0, ncol = length(example_data$required))
        colnames(mat) <- example_data$required
        rt <- reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
      }

      return(rt)
    })

    output$UploadFileExampleUserInput <- renderReactable({
      rt <- NULL
      if (input$UploadFileType == "na") {

        sample_type_name <- switch(
          input$UploadSampleType,
          "1" = "micronix",
          "2" = "cryovial",
          "3" = "dbs"
        )

        example <- paste(c(sample_type_name, input$UploadFileType), collapse="_")
        rt <- reactable(eval(as.symbol(example)) %>% select(example_data$user_input), defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
      } else {
        mat <- matrix(nrow = 0, ncol = length(example_data$user_input))
        colnames(mat) <- example_data$user_input
        rt <- reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
      }
      return(rt)
    })

    output$UploadFileExampleConditional <- renderReactable({
      rt <- NULL
      if (input$UploadFileType == "na") {

        sample_type_name <- switch(
          input$UploadSampleType,
          "1" = "micronix",
          "2" = "cryovial",
          "3" = "dbs"
        )
        example <- paste(c(sample_type_name, input$UploadFileType), collapse="_")
        rt <- reactable(eval(as.symbol(example)) %>% select(example_data$conditional), defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
      } else {
        mat <- matrix(nrow = 0, ncol = length(example_data$conditional))
        colnames(mat) <- example_data$conditional
        rt <- reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
      }
      return(rt)
    })

    output$UploadFileExampleOptional <- renderReactable({
      rt <- NULL
      if (input$UploadFileType == "na") {

        sample_type_name <- switch(
          input$UploadSampleType,
          "1" = "micronix",
          "2" = "cryovial",
          "3" = "dbs"
        )
        example <- paste(c(sample_type_name, input$UploadFileType), collapse="_")
        rt <- reactable(eval(as.symbol(example)) %>% select(example_data$optional), defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
      } else {
        mat <- matrix(nrow = 0, ncol = length(example_data$optional))
        colnames(mat) <- example_data$optional
        rt <- reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
      }
      return(rt)
    })

    cols <- c(
      example_data$required, 
      example_data$user_input,
      example_data$conditional,
      example_data$optional
    )
    template <- matrix(ncol = length(cols), nrow = 0)
    colnames(template) <- cols
    rv$upload_template <- template
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
