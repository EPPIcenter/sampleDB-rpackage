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

AppMoveSamples <- function(session, input, output, database) {

  rv <- reactiveValues(
    user_file = NULL, # this holds a file that is ready for upload
    console_verbatim = FALSE, # whether to print mulitple lines to the console
    error = FALSE, # whether to start an error workflow
    new_manifest_trigger = FALSE, # user wants to add a new manifest
    user_action_required = FALSE, # whether the user needs to add additional inputs
    required_elements = NULL # elements on form that need user attention
  )
  error <- reactiveValues(
    title = "",
    message = "",
    caption = "",
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
        error$caption = ""
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
        shinyjs::html(id = "MoveOutputConsole", html = paste0("Empty Container Created: ", df.payload$name), add = FALSE)
        removeModal()
      } else {
        stop("Unknown error creating new container in database")
      }
    }, error = function(e) {
      error$title = "Internal Error"
      error$message = paste0("Cannot create the container ", input$ManifestID, " due to the following error:")
      error$caption = e$message
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

  observeEvent(rv$error, ignoreInit = TRUE, {

    message("Running error workflow")

    df <- NULL
    if (!is.null(error$table)) {
      df <- error$table %>%
        dplyr::rename(
          Column = column, 
          Reason = reason,
          Trigger = trigger
        ) %>%
        reactable(.)
    }

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

  observeEvent(input$MoveAction, ignoreInit = TRUE, {

    if (isTRUE(rv$user_action_required)) {
      message("Upload action halted - user action required")
      return()
    }

    early_stop <- FALSE
    dataset <- input$MoveDataSet
    message(paste("Loaded", dataset$name))

    tryCatch({
      withCallingHandlers({

        ## format the file
        rv$user_file <- sampleDB::ProcessCSV(
          user_csv = dataset$datapath,
          user_action = "move",
          file_type = input$MoveFileType,
          sample_storage_type = input$MoveSampleType,
          container_name = sub('\\.csv$', '', dataset$name)
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
      html<-paste0("<font color='red'>", paste(c(dataset$name, e$message, e$values), collapse=": "), "</font>")
      shinyjs::html(id = "MoveOutputConsole", html = html, add = rv$console_verbatim)
      rv$console_verbatim <- FALSE

      print(e$values)

    },
    error = function(e) {
      early_stop <<- TRUE
      html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
      shinyjs::html(id = "MoveOutputConsole", html = html, add = rv$console_verbatim)
      rv$console_verbatim <- FALSE
    })

    if (early_stop) { return() }

    message("Starting Move...")

    b_use_wait_dialog <- FALSE

    tryCatch({
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

        # note: this is to make things work retroactively 
        move_file_list <- list(rv$user_file)
        names(move_file_list) <- unique(rv$user_file$manifest_name)

        sampleDB::MoveSamples(sample_type = as.integer(input$MoveSampleType), move_data = move_file_list)
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
      example_data$required <- actions[['required']]
    }
  })

  observe({
    output$MoveFileExampleRequired <- renderReactable({
      mat <- matrix(nrow = 0, ncol = length(example_data$required))
      colnames(mat) <- example_data$required
      return(reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE)))
    })

    template <- matrix(ncol = length(example_data$required), nrow = 0)
    colnames(template) <- example_data$required
    rv$template <- template
  })

  # Download a complete move template
  observe({
    storage_type <- switch(
      input$MoveSampleType,
      "1" = "micronix",
      "2" = "cryovial"
    )

    output$MoveFileTemplate <- downloadHandler(
      filename = function() {
        paste(paste(c(storage_type, input$MoveFileType, "move", "template"), collapse="_"), '.csv', sep='')
      },
      content = function(con) {
        write.csv(rv$template, con, row.names = FALSE, quote=FALSE)
      }
    )
  })
}
