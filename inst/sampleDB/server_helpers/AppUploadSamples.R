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
    console_verbatim = FALSE, # whether to print mulitple lines to the console
    error = FALSE, # whether to start an error workflow
    user_action_required = FALSE, # whether the user needs to add additional inputs
    required_elements = NULL, # elements on form that need user attention
    upload_template = NULL # template file to download
  )

  # Download a complete upload template
  observe({
    output$UploadFileTemplate <- downloadHandler(
        filename = function() {
          paste(paste(c(input$UploadSampleType, input$UploadFileType, "upload", "template"), collapse="_"), '.csv', sep='')
        },
        content = function(con) {
          write.csv(rv$upload_template, con, row.names = FALSE, quote=FALSE)
        }
    )
  })

  observeEvent(input$InputUploadControls, ignoreInit = TRUE, {
    dataset <- input$InputUploadControls

    message(paste("Loaded", dataset$name))

    tryCatch({

      ## format the file
      rv$user_file <- ProcessCSV(
        user_csv = dataset$datapath,
        user_action = input$UploadControlAction,
        file_type = input$UploadFileType,
        control_type = input$UploadControlType
      )
    },
    formatting_error = function(e) {
      check_if_special_columns_missing(e, rv, input)
    },
    validation_error = function(e) {
      show_validation_error_modal(e)
    },
    error = function(e) {
      show_general_error_modal(e)
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
          sample_storage_type = input$UploadSampleType
        )
      },
      message = function(m) {
        shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
        rv$console_verbatim <- TRUE
      })
    },
    formatting_error = function(e) {
      check_if_special_columns_missing(e)
    },
    validation_error = function(e) {
      show_validation_error_modal(e)
    },
    error = function(e) {
      show_general_error_modal(e)
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
            location_parameters <- c(location_parameters, list(location_root = input$UploadLocationRoot))
          }
          if (typeof(input$UploadLocationLevelI) == "character" && input$UploadLocationLevelI != "") {
            location_parameters <- c(location_parameters, list(level_I = input$UploadLocationLevelI))
          }
          if (typeof(input$UploadLocationLevelII) == "character" && input$UploadLocationLevelII != "") {
            location_parameters <- c(location_parameters, list(level_II = input$UploadLocationLevelII))
          }

          if (!is.null(location_parameters) && !all(c("location_root", "level_I", "level_II") %in% names(location_parameters))) {
            stop("Missing location parameter")
          }

          if (input$UploadType == "samples") {
            ## format the file
            rv$user_file <- process_specimen_csv(
              user_csv = dataset$datapath,
              user_action = "upload",
              file_type = input$UploadFileType,
              sample_storage_type = input$UploadSampleType,
              container_name = container_name,
              freezer_address = location_parameters
            )
          } else {
            ## format the file
            rv$user_file <- process_control_csv(
              user_csv = dataset$datapath,
              user_action = input$UploadControlAction,
              file_type = input$UploadFileType,
              control_type = input$UploadControlType,
              container_name = container_name,
              freezer_address = location_parameters
            )            
          }

        },
        message = function(m) {
          shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
          rv$console_verbatim <- TRUE
        })
      },
      validation_error = function(e) {
        show_validation_error_modal(e)
      },
      formatting_error = function(e) {
        check_if_special_columns_missing(e)
      },
      error = function(e) {
        show_general_error_modal(e)
      })
    }

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
        } else if (input$UploadType == "Controls" && input$UploadControlAction == "extraction") { 
          UploadExtractedDNA(user_data = rv$user_file, control_extraction=input$UploadControlType)
        } else {
          UploadControls(user_data=rv$user_file, control_type=input$UploadControlType)
        }
      },
      message = function(m) {
        shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset$name, ": ", m$message), add = rv$console_verbatim)
      })
    },
    error = function(e) {
      show_general_error_modal(e)
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

  observeEvent(input$UploadSampleType, {


    browser()
    shinyjs::reset("UploadLocationRoot")
    shinyjs::reset("UploadLocationLevelI")
    shinyjs::reset("UploadLocationLevelII")

    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

    updateRadioButtons(
      session,
      "UploadFileType",
      choices = global_sample_file_types[[input$UploadSampleType]],
      inline = TRUE,
      selected = dplyr::first(global_sample_file_types[[input$UploadSampleType]])
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

  example_data <- reactiveValues(
    required = NULL,
    user_input = NULL,
    conditional = NULL,
    optional = NULL
  )

  ## create the example data to display and to download

  observe({

    # Initialize attributes based on UploadType
    file_column_attributes <- switch(input$UploadType,
      controls = {
        get_control_file_columns(input$UploadControlType, "create")
      },
      samples = {
        # Check if the UploadSampleType is valid and the UploadFileType exists for the given UploadSampleType
        if (!is.null(global_sample_file_types[[input$UploadSampleType]]) && 
            input$UploadFileType %in% global_sample_file_types[[input$UploadSampleType]]) {
          get_sample_file_columns(input$UploadSampleType, "upload", input$UploadFileType)
        } else {
          stop(paste("Invalid UploadFileType:", input$UploadFileType, "for UploadSampleType:", input$UploadSampleType))
        }
      },
      NULL  # Default case
    )

    
    # Ensure we've got attributes to work with
    if (!is.null(file_column_attributes)) {
      # Populate example_data using the FileColumnAttributes methods
      example_data$required <- file_column_attributes$get_required_colnames()
      example_data$conditional <- file_column_attributes$get_conditional_colnames()
      example_data$optional <- file_column_attributes$get_optional_colnames()
      example_data$location <- file_column_attributes$get_location_colnames()
      example_data$container <- file_column_attributes$get_container_colnames()
    }

    create_template <- function(file_column_attributes) {
      # Additional code to create a template for user download
      cols <- file_column_attributes$all_fields()
      template <- matrix(ncol = length(cols), nrow = 0)
      colnames(template) <- cols
      return(template)
    }

    # create template for the user to download
    rv$template <- create_template(file_column_attributes)

  })

  observe({
    # Helper functions
    get_example_name <- function(type, sample_type, control_type, file_type) {
      if (type == "samples") {
        return(paste0(sample_type, "_", file_type))
      } 
      return(paste0(control_type, "_", file_type))
    }

    create_empty_reactable <- function(columns) {
      mat <- matrix(nrow = 0, ncol = length(columns))
      colnames(mat) <- columns
      reactable(mat, defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE))
    }

    create_reactable_from_example <- function(example, columns) {
      # Check if the data exists and has columns
      data_to_use <- if (exists(example, mode = "data.frame") && ncol(eval(as.symbol(example))) > 0) {
        eval(as.symbol(example))[, columns]
      } else {
        return(create_empty_reactable(columns))  # return an empty reactable if data isn't valid
      }

      reactable(
        data_to_use,
        defaultColDef = colDef(minWidth = 120, html = TRUE, sortable = FALSE, resizable = FALSE)
      )
    }

    # Reactables
    generate_reactable_output <- function(data_columns) {
      if (input$UploadFileType != "na") {
        return(create_empty_reactable(data_columns))
      }
      
      example <- get_example_name(input$UploadType, input$UploadSampleType, input$UploadControlType, input$UploadFileType)
      create_reactable_from_example(example, data_columns)
    }

    output$UploadFileExampleRequired <- renderReactable({
      generate_reactable_output(example_data$required)
    })

    # output$UploadFileExampleUserInput <- renderReactable({
    #   # Combine example_data$location and example_data$container
    #   if (!is.null(example_data$location) && !is.null(example_data$container)) {
    #     combined_data <- c(example_data$location, example_data$container)
    #     create_reactable_from_example(combined_data, names(combined_data))
    #   } else {
    #     create_empty_reactable(NULL)  # Modify create_empty_reactable to handle NULL if needed
    #   }
    # })

    output$UploadFileExampleConditional <- renderReactable({
      if (!is.null(example_data$conditional)) {
        generate_reactable_output(example_data$conditional)
      } else {
        create_empty_reactable(NULL)  # Modify create_empty_reactable to handle NULL if needed
      }
    })

    output$UploadFileExampleOptional <- renderReactable({
      if (!is.null(example_data$optional)) {
        generate_reactable_output(example_data$optional)
      } else {
        create_empty_reactable(NULL)  # Modify create_empty_reactable to handle NULL if needed
      }
    })
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

# This is function to handle any formatting errors that happen during processing.
# Error handling should be improved upon in the future to be done at the server level.
check_if_special_columns_missing <- function(e, rv, input) {

  # Use the get_sample_file_columns function to retrieve file column attributes
  file_column_attr <- get_sample_file_columns(input$UploadSampleType, input$UploadAction, input$UploadFileType)

  locations <- file_column_attr$locations[['location_root']] # Assuming manifest name is the first element in location colnames
  location_parameters <- file_column_attr$get_location_colnames()

  required_elements <- c()

  columns <- e$df$column

  missing <- columns[!columns %in% c(location_parameters, manifest_name)]
  if (length(missing) > 0) {
    show_formatting_error_modal(e)
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
}