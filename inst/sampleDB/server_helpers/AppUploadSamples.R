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

AppUploadSamples <- function(session, input, output, database, dbUpdateEvent) {

  rv <- reactiveValues(
    user_file = NULL, # this holds a file that is ready for upload
    console_verbatim = FALSE, # whether to print mulitple lines to the console
    error = FALSE, # whether to start an error workflow
    required_elements = NULL, # elements on form that need user attention
    upload_template = NULL # template file to download
  )

  # Download a complete upload template
  observe({
    output$UploadFileTemplate <- downloadHandler(
      filename = function() {
        if (input$UploadType == "samples") {
          paste(paste(c(input$UploadSampleType, input$UploadFileType, "upload", "template"), collapse = "_"), ".csv", sep = "")
        } else {
          paste(paste(c(input$UploadControlType, input$UploadControlAction, "template"), collapse = "_"), ".csv", sep = "")
        }
      },
      content = function(con) {
         # Check if the user is uploading samples or controls
        if (input$UploadType == "samples") {
          # Retrieve column data for samples based on selected sample type
          column_data <- get_sample_file_columns(input$UploadSampleType, "upload", input$UploadFileType)
        } else if (input$UploadType == "controls") {
          # Retrieve column data for controls based on selected control type
          column_data <- get_control_file_columns(input$UploadControlType, "create")  # Using action from the input
        }
        
        # Generate an empty data frame with the correct column names for downloading
        if (!is.null(column_data)) {
          all_columns <- c(column_data$required, column_data$conditional, column_data$optional)
          upload_template <- data.frame(matrix(ncol = length(all_columns), nrow = 0))
          colnames(upload_template) <- all_columns
        }
        write.csv(upload_template, con, row.names = FALSE, quote = FALSE)
      }
    )
  })

  observeEvent(input$InputUploadControls, ignoreInit = TRUE, {
    dataset <- input$InputUploadControls

    message(paste("Loaded", dataset$name))

    tryCatch({
      ## format the file
      rv$user_file <- process_control_csv(
        user_csv = dataset$datapath,
        user_action = input$UploadControlAction,
        file_type = input$UploadFileType,
        control_type = input$UploadControlType
      )
    },
    formatting_error = function(e) {
      show_formatting_error_modal(e)
    },
    validation_error = function(e) {
      show_validation_error_modal(output, e)
    },
    error = function(e) {
      show_general_error_modal(e, input, output)
    })
  })

  observeEvent(input$UploadSampleDataSet, ignoreInit = TRUE, {
    dataset <- input$UploadSampleDataSet

    message(paste("Loaded", dataset$name))

    tryCatch({
      withCallingHandlers({

        ## format the file
        rv$user_file <- process_specimen_csv(
          user_csv = dataset$datapath,
          user_action = "upload",
          file_type = input$UploadFileType,
          sample_type = input$UploadSampleType
        )
      },
      message = function(m) {
        shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
        rv$console_verbatim <- TRUE
      })
    },
    formatting_error = function(e) {
      check_if_special_columns_missing(e, rv, input)
    },
    validation_error = function(e) {
      show_validation_error_modal(output, e)
    },
    error = function(e) {
      show_general_error_modal(e, input, output)
    })

    rv$console_verbatim <- FALSE
  })

  observeEvent(input$UploadAction, ignoreInit = TRUE, {

    if(input$UploadType == "controls") {
      dataset = input$InputUploadControls
    } else {
      dataset = input$UploadSampleDataSet
    }
 
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

          user_input_data <- collate_user_input_sample_data(input$UploadSampleType, input)

          if (input$UploadType == "samples") {
            ## format the file
            rv$user_file <- process_specimen_csv(
              user_csv = dataset$datapath,
              user_action = "upload",
              file_type = input$UploadFileType,
              sample_type = input$UploadSampleType,
              bind_data = user_input_data
            )
          } else {
            ## format the file
            rv$user_file <- process_control_csv(
              user_csv = dataset$datapath,
              user_action = input$UploadControlAction,
              file_type = input$UploadFileType,
              control_type = input$UploadControlType,
              bind_data = user_input_data
            )            
          }

        },
        message = function(m) {
          shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
          rv$console_verbatim <- TRUE
        })
      },
      validation_error = function(e) {
        show_validation_error_modal(output, e)
        early_stop <<- TRUE
      },
      formatting_error = function(e) {
        if (input$UploadType == "samples") {
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
        if (input$UploadType == "samples") {
          UploadSpecimens(user_data = rv$user_file, storage_type_id=input$UploadSampleType)
        } else if (input$UploadType == "controls" && input$UploadControlAction == "create") { 
          upload_controls(user_data = rv$user_file, control_type = input$UploadControlType)
        } else if (input$UploadType == "controls" && input$UploadControlAction == "extraction") {
          upload_extracted_dna(user_data = rv$user_file, control_extraction = input$UploadControlType)
        } else {
          message("Unknown upload type!!!")
        }
      },
      message = function(m) {
        shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
      })
    },
    error = function(e) {
      show_general_error_modal(e, input, output)
    },
    finally = {
      if (b_use_wait_dialog) {
        remove_modal_spinner()
      }

      rv$user_file <- NULL
      rv$console_verbatim <- FALSE
    })

  })

  observeEvent(dbUpdateEvent(), {

    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

    # UNUSED FOR NOW
    updateSelectizeInput(
      session,
      "UploadControlStudy",
      selected = input$UploadControlStudy,
      choices = dbReadTable(con, "study") %>% pull(id, name = "short_code")
    )


    updateSelectInput(
      session, 
      "UploadLocationRoot",
      selected = input$UploadLocationRoot,
      choices = c("", tbl(con, "location") %>%
        collect() %>% 
        pull(location_root) %>%
        unique(.)
      )
    )

    manifest <- switch(
      input$UploadSampleType,
      "micronix" = "micronix_plate",
      "cryovial" = "cryovial_box"
    )

    updateSelectizeInput(
      session,
      "UploadManifestName",
      label = switch(
        input$UploadSampleType,
        "micronix" = "Plate Name",
        "cryovial" = "Box Name"
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
        input$UploadSampleType,
        "micronix" = "Shelf Name", 
        "cryovial" = "Rack Number"
      )
    )

    updateSelectInput(
      session,
      "UploadLocationLevelII",
      selected = input$UploadLocationLevelII,
      label = switch(
        input$UploadSampleType,
        "micronix" = "Basket Name",
        "cryovial" = "Rack Position"
      )
    )

    dbDisconnect(con)

  })

  observeEvent(input$UploadSampleType, ignoreInit = TRUE, {


    shinyjs::reset("UploadLocationRoot")
    shinyjs::reset("UploadLocationLevelI")
    shinyjs::reset("UploadLocationLevelII")

    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

    updateRadioButtons(
      session,
      "UploadFileType",
      choices = get_file_types_for_sample(input$UploadSampleType),
      inline = TRUE,
      selected = "na",
    )

    updateSelectInput(
      session, 
      "UploadLocationRoot",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        collect() %>% 
        pull(location_root) %>%
        unique(.)
      )
    )

    manifest <- switch(
      input$UploadSampleType,
      "micronix" = "micronix_plate",
      "cryovial" = "cryovial_box",
      "dbs_sample" = switch(
        input$UploadDBSSampleManifest,
        "dbs_sheet" = "dbs_bag",
        "box" = "box"
      )
    )

    updateSelectizeInput(
      session,
      "UploadManifestName",
      label = switch(
        input$UploadSampleType,
        "micronix" = "Plate Name",
        "cryovial" = "Box Name",
        "dbs_sample" = switch(
          input$UploadDBSSampleManifest,
          "dbs_sheet" = "Bag",
          "box" = "Box"
        )
      ),
      selected = FALSE,
      choices = DBI::dbReadTable(con, manifest) %>% pull(name),
      options = list(create = TRUE)
    )

    updateSelectInput(
      session,
      "UploadLocationLevelI",
      label = switch(
        input$UploadSampleType,
        "micronix" = "Shelf Name", 
        "cryovial" = "Rack Number"
      )
    )

    updateSelectInput(
      session,
      "UploadLocationLevelII",
      label = switch(
        input$UploadSampleType,
        "micronix" = "Basket Name",
        "cryovial" = "Rack Position"
      )
    )

    DBI::dbDisconnect(con)
  })

  observeEvent(input$UploadLocationRoot, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    updateSelectInput(
      session,
      "UploadLocationLevelI",
      selected = "",
      choices = c("", tbl(con, "location") %>%
        filter(location_root == local(input$UploadLocationRoot)) %>%
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
        filter(location_root == local(input$UploadLocationRoot) && level_I == local(input$UploadLocationLevelI)) %>%
        collect() %>% 
        pull(level_II)
      )
    )
    DBI::dbDisconnect(con)
  })

  observeEvent(input$ClearUploadForm, ignoreInit = TRUE, {
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



build_annotated_csv <- function(e) {
  # TODO: breakup process csv into three stages(but keep calls in global process csv).
  # Just download the error data frame for now.
  errors <- names(e$data)
  df <- lapply(1:length(errors), function(idx) {
    if (all(c("Columns", "CSV") %in% names(e$data[[idx]]))) {
      e$data[[idx]]$CSV %>%
        mutate(Error = errors[idx]) %>%
        mutate(ErrCol = paste(e$data[[idx]]$Columns, collapse = ",")) %>%
        select(Error, colnames(e$data[[idx]]$CSV)) 
    } else { 
      return (NULL)
    }
  })

  user_file_error_annotated <- do.call("rbind", df) 

  return (user_file_error_annotated)
}

# This is a function to handle any formatting errors that happen during processing.
# Error handling should be improved upon in the future to be done at the server level.
check_if_special_columns_missing <- function(e, rv, input) {

  message("Checking whether to ask user for input.")
  # Use the get_sample_file_columns function to retrieve file column attributes

  file_column_attr <- get_sample_file_columns(
    sample_type = input$UploadSampleType,
    action = "upload",
    file_type = input$UploadFileType
  )

  locations <- get_location_by_sample(input$UploadSampleType)
  container <- get_container_by_sample(input$UploadSampleType)
  columns <- e[['data']]$column

  missing <- columns[!columns %in% c(locations, container)]

  # If there are any missing columns outside of the special colums
  # or if not all location columns were found that were missing,
  # then show the error dialog. All locations are required
  # because only showing one add complexity in the UI and it is 
  # likely not an intenional decision by the user to do this and 
  # will likely cause errors.
  if (length(missing) > 0 || !all(locations %in% columns)) {
    show_formatting_error_modal(e)
  } else {
    if (container[['container_name_key']] %in% columns) {
      shinyjs::show("UploadManifestName")
    }

    # should be all or none
    if (locations$location_root %in% columns) {
      shinyjs::show("UploadLocationRoot")
    }

    if (locations$level_i %in% columns) {
      shinyjs::show("UploadLocationLevelI")
    }

    if (locations$level_ii %in% columns) {
      shinyjs::show("UploadLocationLevelII")
    }

    shinyjs::disable("UploadSampleType")
    shinyjs::disable("UploadFileType")

    rv$user_file <- NULL # sanity check
  }
}