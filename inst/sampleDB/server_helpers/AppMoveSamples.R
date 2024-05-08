library(shiny)
library(shinyjs)
# library(purrr)
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
    message = ""
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
        pull(location_root) %>%
        unique(.)
      )
    )

    updateSelectInput(
      session,
      "ManifestLocationRootLevelI",
      label = switch(
        input$MoveSampleType,
        "micronix" = "Shelf Name",
        "cryovial" = "Rack Number"
      )
    )

    updateSelectInput(
      session,
      "ManifestLocationRootLevelII",
      label = switch(
        input$MoveSampleType,
        "micronix" = "Basket Name",
        "cryovial" = "Rack Position"
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
        "micronix" = "micronix_plate",
        "cryovial" = "cryovial_box"
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
        filter(location_root %in% local(input$ManifestLocationRoot) & level_I %in% local(input$ManifestLocationRootLevelI) & level_II %in% local(input$ManifestLocationRootLevelII)) %>%
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
            "micronix" = "micronix_plate",
            "cryovial" = "cryovial_box"
          )

          result <- tbl(con, manifest) %>%
            filter(name %in% local(input$ManifestID)) %>%
            count() %>%
            pull(n)

          if (result > 0) {
            html <- paste0("<span style=color:#0000ff>", "Name is in use!", "</span>")
            shinyjs::disable("ManifestCreateAction")
          } else {
            shinyjs::enable("ManifestCreateAction")
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
            "micronix" = "micronix_plate",
            "cryovial" = "cryovial_box"
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
        filter(location_root == local(input$ManifestLocationRoot)) %>%
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
        filter(location_root == local(input$ManifestLocationRoot) && level_I == local(input$ManifestLocationRootLevelI)) %>%
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
    message("Running move specific error workflow")
    modal_size <- "m"

    showModal(
      modalDialog(
        size = "m",
        title = error$title,
        error$message,
        tags$hr(),
        footer = modalButton("Exit")
      )
    )

    rv$error <- NULL
    error$title <- ""
    error$message <- ""
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

    early_stop <- tryCatch({
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

          container <- get_container_by_sample(sample_type = input$MoveSampleType)

          ## format the file
          result <- process_specimen_csv(
            user_csv = dataset[i,]$datapath,
            user_action = "move",
            file_type = input$MoveFileType,
            sample_type = input$MoveSampleType,
            bind_data = setNames(manifest_name, container$container_name_key)
          )

          move_data_list <- c(move_data_list, list(result))
          names(move_data_list)[i] <- manifest_name
        }

        rv$user_file <- move_data_list
        FALSE
      },
      message = function(m) {
        # shinyjs::html(id = "MoveOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
        # rv$console_verbatim <- TRUE
      })
    },
    validation_error = function(e) {
        message("Caught validation error")
        show_validation_error_modal(output, e, dataset[i,]$name)
        TRUE
      },
      formatting_error = function(e) {
        message("Caught formatting error")
        show_formatting_error_modal(e, dataset[i,]$name)
        TRUE
      },
      error = function(e) {
        show_general_error_modal(e, input, output)
        TRUE
      }
    )

    if (isTRUE(early_stop)) { 
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
        MoveSamples(sample_type = input$MoveSampleType, move_data = rv$user_file)
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
    updateRadioButtons(
      session,
      "MoveFileType",
      choices = get_file_types_for_sample(input$MoveSampleType),
      inline = TRUE
    )
  })

  observeEvent(input$ClearMoveForm, ignoreInit = TRUE, {
    shinyjs::enable("MoveSampleType")
    shinyjs::enable("MoveFileType")
    shinyjs::reset("MoveDataSet")
    shinyjs::reset("MoveOutputConsole")

    rv$user_file <- NULL
  })
}
