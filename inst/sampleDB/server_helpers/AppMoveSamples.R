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

  observeEvent(input$CreateNewManifest, ignoreInit = TRUE, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

    # Update location root options
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

    # Update the labels based on the sample type being moved
    updateSelectInput(
      session,
      "ManifestLocationRootLevelI",
      label = switch(
        input$MoveSampleType,
        "micronix" = "Shelf Name",
        "cryovial" = "Rack Number",
        "whole_blood" = "Rack Number",
        "dbs_sheet" = "Bag Rack Number"
      )
    )

    updateSelectInput(
      session,
      "ManifestLocationRootLevelII",
      label = switch(
        input$MoveSampleType,
        "micronix" = "Basket Name",
        "cryovial" = "Rack Position",
        "whole_blood" = "Rack Position",
        "dbs_sheet" = "Bag Position"
      )
    )

    # Fetch the sample type name from the database
    sample_type_name <- DBI::dbReadTable(con, "sample_type") %>%
      filter(id == input$MoveSampleType) %>%
      pull(name)

    # Show the modal dialog for creating a new container or bag
    showModal(
      modalDialog(
        title = tags$h3("Create a new place to store", tags$strong(sample_type_name), "samples."),

        tags$h5("1. Document the location where the new container or bag will be stored."),
        selectInput("ManifestLocationRoot", label = NULL, width = '47%', choices = NULL),
        selectInput("ManifestLocationRootLevelI", label = NULL, width = '47%', choices = NULL),
        selectInput("ManifestLocationRootLevelII", label = NULL, width = '47%', choices = NULL),

        tags$h5("2. Create a new name for the container or bag."),
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

  # Enable/Disable Create Action button based on inputs
  observe({
    req(input$ManifestID, input$ManifestLocationRoot, input$ManifestLocationRootLevelI, input$ManifestLocationRootLevelII)
    if (input$ManifestID != "" && input$ManifestLocationRoot != "" && input$ManifestLocationRootLevelI != "" && input$ManifestLocationRootLevelII != "") {
      shinyjs::enable("ManifestCreateAction")
    } else {
      shinyjs::disable("ManifestCreateAction")
    }
  })

  # Handle creation of a new cryovial box or DBS bag
  observeEvent(input$ManifestCreateAction, {
    tryCatch({

      con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

      dbBegin(con)

      # Determine the correct table based on the sample type
      manifest <- switch(
        input$MoveType,
        "samples" = switch(
          input$MoveSampleType,
          "micronix" = "micronix_plate",
          "cryovial" = "cryovial_box"
        ),
        "controls" = switch(
          input$MoveControlType,
          "whole_blood" = "cryovial_box",  # whole_blood uses cryovial_box
          "dbs_sheet" = "dbs_bag"          # dbs_sheet uses dbs_bag
        )
      )

      # Check if the container or bag already exists by name
      result <- tbl(con, manifest) %>%
        filter(name %in% local(input$ManifestID)) %>%
        count() %>%
        pull(n)

      if (result > 0) {
        error$title = "Error"
        error$message = paste0("Cannot create the container ", input$ManifestID, " because it already exists.")
        error$table = NULL
        rv$error <- TRUE
        return()
      }

      # Check if the barcode already exists
      result <- tbl(con, manifest) %>%
        filter(barcode %in% local(input$ManifestBarcode)) %>%
        count() %>%
        pull(n)

      if (result > 0) {
        error$title = "Error"
        error$message = paste0("The barcode ", input$ManifestBarcode, " already exists for this type of container.")
        error$table = NULL
        rv$error <- TRUE
        return()
      }

      # Get the location ID based on location information
      location_id <- tbl(con, "location") %>%
        filter(location_root %in% local(input$ManifestLocationRoot) & level_I %in% local(input$ManifestLocationRootLevelI) & level_II %in% local(input$ManifestLocationRootLevelII)) %>%
        pull(id)

      # Prepare the data for insertion into the correct manifest table
      df.payload <- data.frame(
        location_id = location_id,
        name = input$ManifestID,
        barcode = ifelse(input$ManifestBarcode == "", NA, input$ManifestBarcode)
      )

      # Insert the new container or bag
      result <- DBI::dbAppendTable(con, manifest, df.payload)
      if (result == 1) {
        dbCommit(con)
        shinyjs::html(id = "MoveOutputConsole", html = paste0("Empty Container or Bag Created: ", df.payload$name), add = FALSE)
        removeModal()
      } else {
        stop("Unknown error creating new container or bag in the database")
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
          # Determine the correct table based on the sample type
          manifest <- switch(
            input$MoveType,
            "samples" = switch(
              input$MoveSampleType,
              "micronix" = "micronix_plate",
              "cryovial" = "cryovial_box"
            ),
            "controls" = switch(
              input$MoveControlType,
              "whole_blood" = "cryovial_box",  # whole_blood uses cryovial_box
              "dbs_sheet" = "dbs_bag"          # dbs_sheet uses dbs_bag
            )
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

  observeEvent(input$MoveDataSet, ignoreInit = TRUE, {
    dataset <- input$MoveDataSet
    rv$user_file <- NULL

    message(paste("Loaded", dataset$name))

    tryCatch({
      withCallingHandlers({

        ## format the file based on the move type
        if (input$MoveType == "samples") {
          rv$user_file <- process_specimen_csv(
            user_csv = dataset$datapath,
            user_action = "move",
            file_type = input$MoveFileType,
            sample_type = input$MoveSampleType
          )
        } else if (input$MoveType == "controls") {
          rv$user_file <- process_control_csv(
            user_csv = dataset$datapath,
            user_action = "move",  # For controls, we are only moving
            file_type = input$MoveFileType,
            control_type = input$MoveControlType
          )
        }
      },
      message = function(m) {
        shinyjs::html(id = "MoveOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
        rv$console_verbatim <- TRUE
      })
    },
    formatting_error = function(e) {
      if (input$MoveType == "samples" && input$MoveSampleType %in% c("micronix", "cryovial")) {
        check_if_special_columns_missing(e, rv, input)
      } else {
        show_formatting_error_modal(e)
      }
    },
    validation_error = function(e) {
      show_validation_error_modal(output, e)
    },
    error = function(e) {
      show_general_error_modal(e, input, output)
    })

    rv$console_verbatim <- FALSE
  })

  observeEvent(input$MoveAction, ignoreInit = TRUE, {
    # Determine the dataset depending on the type of move
    if (input$MoveType == "controls") {
      dataset <- input$MoveControlDataSet
    } else {
      dataset <- input$MoveDataSet
    }

    if (is.null(dataset) || is.null(dataset$datapath)) {
      message("Aborting move - no file uploaded")
      return()
    }

    early_stop <- FALSE
    if (is.null(rv$user_file)) {
      message(paste("Loaded", dataset$name))

      tryCatch({
        withCallingHandlers({

          ## format the file based on the move type
          if (input$MoveType == "samples") {
            rv$user_file <- process_specimen_csv(
              user_csv = dataset$datapath,
              user_action = "move",
              file_type = input$MoveFileType,
              sample_type = input$MoveSampleType,
              bind_data = user_input_data
            )
          } else {
            rv$user_file <- process_control_csv(
              user_csv = dataset$datapath,
              user_action = "move",
              file_type = input$MoveFileType,
              control_type = input$MoveControlType,
              bind_data = user_input_data
            )
          }

        },
        message = function(m) {
          shinyjs::html(id = "MoveOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
          rv$console_verbatim <- TRUE
        })
      },
      validation_error = function(e) {
        show_validation_error_modal(output, e)
        early_stop <<- TRUE
      },
      formatting_error = function(e) {
        if (input$MoveType == "samples" && input$MoveSampleType %in% c("micronix", "cryovial")) {
          check_if_special_columns_missing(e, rv, input)
        } else {
          show_formatting_error_modal(e)
        }
        early_stop <<- TRUE
      },
      error = function(e) {
        show_general_error_modal(e, input, output)
        early_stop <<- TRUE
      })
    }

    if (early_stop) return()

    message("Starting Move...")

    tryCatch({
      withCallingHandlers({
        # Move the data depending on the type
        sample_type <- if (input$MoveType == "controls") input$MoveControlType else input$MoveSampleType
        MoveSpecimens(sample_type = sample_type, move_data = rv$user_file)
      },
      message = function(m) {
        shinyjs::html(id = "MoveOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
      })
    },
    error = function(e) {
      show_general_error_modal(e, input, output)
    },
    finally = {
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

  move_example_data <- reactiveValues(
    required = NULL,
    optional = NULL
  )

  # Download a complete upload template
  observe({
    output$MoveFileTemplatePlaceholder <- renderUI({

      get_specific_move_type <- function() {
        sample_types <- get_sample_types()
        sample_file_types <- get_file_types_for_sample(input$MoveSampleType)

        match_index <- match(input$MoveSampleType, sample_types)
        sample_display_name <- names(sample_types[match_index])

        match_index <- match(input$MoveSampleType, sample_file_types)
        file_type_display_name <- names(sample_file_types[match_index])

        sprintf("%s (FileType: '%s')", sample_display_name, file_type_display_name)
      }

      downloadButton("MoveFileTemplate", label = paste("Download", get_specific_move_type(), "Move Template"))
    })

    # NOTE: Should add a case for controls when they are added.
    output$MoveFileTemplate <- downloadHandler(
      filename = function() {
        filename_base <- paste(c(input$MoveSampleType, input$MoveFileType, "move", "template"), collapse = "_")
        paste(filename_base, ".csv", sep = "")
      },
      content = function(con) {
        # Retrieve column data for samples based on selected sample type
        column_data <- get_sample_file_columns(input$MoveSampleType, "move", input$MoveFileType)
        
        # Generate an empty data frame with the correct column names for downloading
        if (!is.null(column_data)) {
          all_columns <- c(column_data$required)
          move_template <- data.frame(matrix(ncol = length(all_columns), nrow = 0))
          colnames(move_template) <- all_columns
        } else {
          stop("Column data is null!!!")
        }
        write.csv(move_template, con, row.names = FALSE, quote = FALSE)
      }
    )
  })

  observe({
    ## Read File Specification File
    file_specs_json <- get_sample_file_columns(input$MoveSampleType, "move", input$MoveFileType)

    ## Required Column Names
    move_example_data$required <- file_specs_json$required
  })

  observe({
    output$MoveFileExampleRequired <- renderReactable({
      rt <- NULL
      if (input$MoveFileType == "na") {
        example <- paste(c(input$MoveSampleType, input$MoveFileType), collapse="_")
        rt <- reactable(eval(as.symbol(example))[, move_example_data$required], defaultColDef = colDef(minWidth = 130, html = TRUE, sortable = FALSE, resizable = FALSE))
      } else {
        mat <- matrix(nrow = 0, ncol = length(move_example_data$required))
        colnames(mat) <- move_example_data$required
        rt <- reactable(mat, defaultColDef = colDef(minWidth = 130, html = TRUE, sortable = FALSE, resizable = FALSE))
      }

      return(rt)
    })

    cols <- c(
      move_example_data$required
    )

    template <- matrix(ncol = length(cols), nrow = 0)
    colnames(template) <- cols
    rv$move_template <- template
  })
}
