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

AppMoveSamples <- function(session, input, output, database) {

  rv <- reactiveValues(
    user_file = NULL, # this holds a file that is ready for upload
    console_verbatim = FALSE, # whether to print mulitple lines to the console
    error = FALSE, # whether to start an error workflow
    new_manifest_trigger = FALSE, # user wants to add a new manifest
    user_action_required = FALSE, # whether the user needs to add additional inputs
    required_elements = NULL, # elements on form that need user attention
    user_file_error_annotated = NULL
  )
  error <- reactiveValues(
    title = "",
    message = "",
    table = NULL
  )

  example_data <- reactiveValues(
    required = NULL,
    user_input = NULL,
    conditional = NULL,
    optional = NULL
  )

  observeEvent(input$CreateNewManifest, ignoreInit = TRUE, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

    updateSelectInput(
      session,
      "ManifestLocationRoot",
      selected = "",
      label = "Upload Location",
      choices = c("", tbl(con, "location") %>%
        collect() %>%
        pull(name) %>%
        unique(.)
      )
    )

    updateSelectInput(
      session,
      "ManifestLocationRootLevelI",
      label = switch(
        input$MoveSampleType,
        "1" = "Shelf Name",
        "2" = "Rack Number",
        "3" = "To Be Implemented"
      )
    )

    updateSelectInput(
      session,
      "ManifestLocationRootLevelII",
      label = switch(
        input$MoveSampleType,
        "1" = "Basket Name",
        "2" = "Rack Position",
        "3" = "To Be Implemented"
      )
    )

    sample_type_name <- DBI::dbReadTable(con, "sample_type") %>%
      filter(id == input$MoveSampleType) %>%
      pull(name)

    showModal(
      modalDialog(
        title = tags$h3("Create a new place to store", tags$strong(sample_type_name), "samples."),

        tags$h5("1. Document the location where the new container will be stored."),
        selectInput("ManifestLocationRoot", label = NULL, width = '47%', choices = NULL),
        selectInput("ManifestLocationRootLevelI", label = NULL, width = '47%', choices = NULL),
        selectInput("ManifestLocationRootLevelII", label = NULL, width = '47%', choices = NULL),

        tags$h5("2. Create a new name for the container."),
        textInput("ManifestID", label = "Human Readable Name", placeholder = "PRISM-2022-001"),
        uiOutput("ManifestIDCheck"),
        textInput("ManifestBarcode", label = "Barcode"),
        uiOutput("ManifestBarcodeCheck"),

        footer = tagList(
          modalButton("Cancel"),
          actionButton("ManifestCreateAction", "OK")
        )
      )
    )
    shinyjs::disable("ManifestCreateAction")
    dbDisconnect(con)
  })

  observe({
    req(input$ManifestID, input$ManifestLocationRoot, input$ManifestLocationRootLevelI, input$ManifestLocationRootLevelII)
    if (input$ManifestID != "" && input$ManifestLocationRoot != "" && input$ManifestLocationRootLevelI != "" && input$ManifestLocationRootLevelII != "") {
      shinyjs::enable("ManifestCreateAction")
    } else {
      shinyjs::disable("ManifestCreateAction")
    }
  })

  observeEvent(input$ManifestCreateAction, {
    tryCatch({

      con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

      dbBegin(con)

      manifest <- switch(
        input$MoveSampleType,
        "1" = "micronix_plate",
        "2" = "cryovial_box",
        "3" = "dbs_paper"
      )

      result <- tbl(con, manifest) %>%
        filter(name %in% local(input$ManifestID)) %>%
        count() %>%
        pull(n)

      if (result > 0) {

        # note: this should be stale
        error$title = "Error"
        error$message = paste0("Cannot create the container ", input$ManifestID, " because it already exists.")
        error$table = NULL

        rv$error <- TRUE

        return()
      }

      result <- tbl(con, manifest) %>%
        filter(barcode %in% local(input$ManifestBarcode)) %>%
        count() %>%
        pull(n)

      if (result > 0) {
        # note: this should be stale
        error$title = "Error"
        error$message = paste0("The barcode ", input$ManifestBarcode, " already exists for this type of container.")
        error$table = NULL

        rv$error <- TRUE

        return()
      }

      location_id <- tbl(con, "location") %>%
        filter(name %in% local(input$ManifestLocationRoot) & level_I %in% local(input$ManifestLocationRootLevelI) & level_II %in% local(input$ManifestLocationRootLevelII)) %>%
        pull(id)

      df.payload <- data.frame(
        location_id = location_id,
        name = input$ManifestID,
        barcode = ifelse(input$ManifestBarcode == "", NA, input$ManifestBarcode)
      )

      result <- DBI::dbAppendTable(con, manifest, df.payload)
      if (result == 1) {
        dbCommit(con)
        shinyjs::html(id = "MoveOutputConsole", html = paste0("Empty Container Created: ", df.payload$name), add = FALSE)
        removeModal()
      } else {
        stop("Unknown error creating new container in database")
      }
    }, error = function(e) {
      error$title = "Internal Error"
      error$message = e$message
      error$table = NULL

      rv$error <- TRUE
    }, finally = {
      dbDisconnect(con)
    })
  })

  observeEvent(input$ManifestID, ignoreInit = TRUE, {
    output$ManifestIDCheck <- renderUI({

      html <- paste0("<span></span>")

      if (dbCanConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))) {
        con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

        if (input$ManifestID != "") {
          manifest <- switch(
            input$MoveSampleType,
            "1" = "micronix_plate",
            "2" = "cryovial_box",
            "3" = "dbs_paper"
          )

          result <- tbl(con, manifest) %>%
            filter(name %in% local(input$ManifestID)) %>%
            count() %>%
            pull(n)

          if (result > 0) {
            html <- paste0("<span style=color:#0000ff>", "Name is in use!", "</span>")
          }
        }

        dbDisconnect(con)
      }

      HTML(html)
    })
  })

  observeEvent(input$ManifestBarcode, ignoreInit = TRUE, {
    output$ManifestBarcodeCheck <- renderUI({

      html <- paste0("<span></span>")

      if (dbCanConnect(RSQLite::SQLite(), Sys.getenv("SDB_PATH"))) {
        con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

        if (input$ManifestBarcode != "") {
          manifest <- switch(
            input$MoveSampleType,
            "1" = "micronix_plate",
            "2" = "cryovial_box",
            "3" = "dbs_paper"
          )

          result <- tbl(con, manifest) %>%
            filter(barcode %in% local(input$ManifestBarcode)) %>%
            count() %>%
            pull(n)

          if (result > 0) {
            html <- paste0("<span style=color:#0000ff>", "Barcode is in use!", "</span>")
          }
        }

        dbDisconnect(con)
      }

      HTML(html)
    })
  })

  observeEvent(input$ManifestLocationRoot, ignoreInit = TRUE, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    updateSelectInput(
      session,
      "ManifestLocationRootLevelI",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(name == local(input$ManifestLocationRoot)) %>%
        collect() %>%
        pull(level_I)
      )
    )
    DBI::dbDisconnect(con)

    shinyjs::reset("ManifestLocationRootLevelI")
    shinyjs::reset("ManifestLocationRootLevelII")
  })

  observeEvent(input$ManifestLocationRootLevelI, ignoreInit = TRUE, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    updateSelectInput(
      session,
      "ManifestLocationRootLevelII",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(name == local(input$ManifestLocationRoot) && level_I == local(input$ManifestLocationRootLevelI)) %>%
        collect() %>%
        pull(level_II)
      )
    )
    DBI::dbDisconnect(con)
  })

  observe({
    output$ErrorMoveFileDownload <- downloadHandler(
      filename = function() {
        paste(paste(c("move", "annotated"), collapse="_"), '.csv', sep='')
      },
      content = function(con) {
        write.csv(rv$user_file_error_annotated, con, row.names = FALSE, quote=FALSE)
      }
    )
  })

  observeEvent(rv$error, ignoreInit = TRUE, {
    message("Running error workflow")
    df <- error$table
    modal_size <- "m"
    message(error$type)
    if (!is.null(error$type) && error$type == "formatting") {
      df <- error$table %>%
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
      errors <- unique(names(error$table))
      errors <- data.frame(errors)
      colnames(errors) <- "Error"
      df <- reactable(errors, details = function(index) {
        data <- error$table[[index]]$Columns
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
          downloadButton("ErrorMoveFileDownload"),
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
    error$table <- NULL
  })

  observeEvent(input$MoveAction, ignoreInit = TRUE, {

    if (isTRUE(rv$user_action_required)) {
      message("Upload action halted - user action required")
      return()
    }

    early_stop <- FALSE
    dataset <- input$MoveDataSet

    if (is.null(dataset) || nrow(dataset) == 0) {
      message("Aborting move - no file uploaded")
      return()
    }

    # message(paste("Loaded", dataset$name))

    tryCatch({
      withCallingHandlers({
        move_data_list <- list()
        for (i in 1:length(dataset[,1])) {
          manifest_name <- sub('\\.csv$', '', dataset[i,]$name)

          if(input$MoveFileType == "traxcer" && input$MoveTraxcerStripFromFilename) {
            manifest_name <- substr(manifest_name, 1, nchar(manifest_name)-16)
            if (nchar(manifest_name) == 0) {
              stop(sprintf("Traxcer name was completely removed - did you mean to remove the datetime string from the filename?"))
            }
          }

          ## format the file
          result <- ProcessCSV(
            user_csv = dataset[i,]$datapath,
            user_action = "move",
            file_type = input$MoveFileType,
            sample_storage_type = input$MoveSampleType,
            container_name = manifest_name
          )

          # Special Case. The file can contain the plate name
          # and will override what is extracted from the file path.
          # Reassign manifest name the value that is in the resulting
          # dataframe because it is guaranteed to have either the value
          # in the file of the value extracted from the filename.
          manifest_name <- unique(result$manifest_name)

          move_data_list <- c(move_data_list, list(result))
          names(move_data_list)[i] <- manifest_name
        }

        rv$user_file <- move_data_list
      },
      message = function(m) {
        # shinyjs::html(id = "MoveOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
        # rv$console_verbatim <- TRUE
      })
    },
    validation_error = function(e) {
        message("Caught validation error")
        early_stop <<- TRUE
        html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
        shinyjs::html(id = "MoveOutputConsole", html = html, add = rv$console_verbatim)
        rv$console_verbatim <- FALSE

        error$type <- "validation"
        error$title <- e$message
        error$table <- e$data

        # TODO: breakup process csv into three stages(but keep calls in global process csv).
        # Just download the error data frame for now.
        errors <- names(e$data)
        df <- lapply(1:length(errors), function(idx) {
          e$data[[idx]]$CSV %>%
            mutate(Error = errors[idx]) %>%
            mutate(ErrCol = paste(e$data[[idx]]$Columns, collapse = ",")) %>%
            select(Error, colnames(e$data[[idx]]$CSV)) 
        })

        rv$user_file_error_annotated <- do.call("rbind", df) %>%
          select(-c(RowNumber))

        print(e$data)

      },
      formatting_error = function(e) {
        message("Caught formatting error")
        early_stop <<- TRUE
        error$title = "Invalid File Detected"
        error$type = "formatting"
        error$message = e$message
        error$table = e$df
      },
      error = function(e) {
        early_stop <<- TRUE
        html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
        shinyjs::html(id = "MoveOutputConsole", html = html, add = rv$console_verbatim)
        rv$console_verbatim <- FALSE
        error$title = "Unknown Error"
        error$type = "unknown"
        error$message = e$message
        error$table = NULL
      }
    )

    if (isTRUE(early_stop)) { 
      rv$error <- TRUE
      return()
    }

    message("Starting Move...")

    b_use_wait_dialog <- FALSE

    tryCatch({
      withCallingHandlers({
        # simple way to add a dialog or not
        b_use_wait_dialog <- length(rv$user_file) > 5

        if (b_use_wait_dialog) {
          show_modal_spinner(
            spin = "double-bounce",
            color = "#00bfff",
            text = paste("Working on", length(rv$user_file), "files, please be patient...")
          )
        }

        shinyjs::reset("MoveAction")

        # note: this is to make things work retroactively
        MoveSamples(sample_type = as.integer(input$MoveSampleType), move_data = rv$user_file)
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
    shinyjs::reset("MoveOutputConsole")

    rv$user_file <- NULL
  })


  ## create the example data to display and to download
  observe({
    ## Read File Specification File
    file_specs_json <- rjson::fromJSON(file = system.file(
      "extdata", "file_specifications.json", package = .sampleDB$pkgname))

    ## Required Column Names

    file_index <- which(lapply(file_specs_json$file_types, function(x) x$id) == input$MoveFileType)
    sample_storage_type_index <- which(lapply(file_specs_json$file_types[[file_index]]$sample_type, function(x) x$id) == input$MoveSampleType)

    if (length(sample_storage_type_index) == 0) {
      message("Unimplemented file specifications for this sample storage type.")
    } else {
      actions <- file_specs_json$file_types[[file_index]]$sample_type[[sample_storage_type_index]]$actions[['move']]
      required_user_column_names <- actions[['required']]
      if (input$MoveFileType == "traxcer") {
        ## Read Configuration File and replace with user override from user preferences
        config <- yaml::read_yaml(Sys.getenv("SDB_CONFIG"))
        if (!is.na(config$traxcer_position$override)) {
          required_user_column_names <- stringr::str_replace(
            required_user_column_names,
            config$traxcer_position$default,
            config$traxcer_position$override
          )
        }
      }
      example_data$required <- required_user_column_names
    }
  })

  observe({
    output$MoveFileExampleRequired <- renderReactable({
      rt <- NULL

      if (input$MoveFileType == "na") {

        sample_type_name <- switch(
          input$MoveSampleType,
          "1" = "micronix",
          "2" = "cryovial",
          "3" = "dbs"
        )
        example <- paste(c(sample_type_name, input$MoveFileType), collapse="_")
        rt <- reactable(eval(as.symbol(example)) %>% select(example_data$required), defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
      } else {
        mat <- matrix(nrow = 0, ncol = length(example_data$required))
        colnames(mat) <- example_data$required
        rt <- reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
      }

      return (rt)
    })

    template <- matrix(ncol = length(example_data$required), nrow = 0)
    colnames(template) <- example_data$required
    rv$template <- template
  })

  # Download a complete move template
  observe({
    output$MoveFileTemplate <- downloadHandler(
      filename = function() {
        storage_type <- switch(
          input$MoveSampleType,
          "1" = "micronix",
          "2" = "cryovial"
        )
        paste(paste(c(storage_type, input$MoveFileType, "move", "template"), collapse="_"), '.csv', sep='')
      },
      content = function(con) {
        write.csv(rv$template, con, row.names = FALSE, quote=FALSE)
      }
    )
  })
}
