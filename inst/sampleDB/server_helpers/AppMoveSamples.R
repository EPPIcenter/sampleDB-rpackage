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
    user_file_error_annotated = NULL,
    selected_containers = NULL,
    empty_containers = NULL,
    manifest_table = NULL
  )
  error <- reactiveValues(
    title = "",
    message = ""
  )

  observeEvent(input$DeleteEmptyManifest, ignoreInit = TRUE, {
    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))

    # Determine the correct manifest table based on the sample/control type
    manifest <- switch(
      input$MoveType,
      "samples" = switch(
        input$MoveSampleType,
        "micronix" = "micronix_plate",
        "cryovial" = "cryovial_box",
        "dbs_sample" = "bag|box"
      ),
      "controls" = switch(
        input$MoveControlType,
        "whole_blood" = "cryovial_box",
        "dbs_sheet" = "dbs_bag"
      )
    )

    # Store manifest table name in rv
    rv$manifest_table <- manifest

    sample_type <- switch(
      input$MoveType,
      "samples" = switch(
        input$MoveSampleType,
        "micronix" = "micronix_tube",
        "cryovial" = "cryovial_tube|whole_blood_tube",
        "dbs_sample" = "paper"
      ),
      "controls" = switch(
        input$MoveControlType,
        "whole_blood" = "cryovial_tube|whole_blood_tube",
        "dbs_sheet" = "dbs_control_sheet"
      )
    )

    container_indentifier <- switch(
      input$MoveType,
      "samples" = switch(
        input$MoveSampleType,
        "micronix" = "manifest_id",
        "cryovial" = "manifest_id",
        "dbs_sample" = "manifest_id"
      ),
      "controls" = switch(
        input$MoveControlType,
        "whole_blood" = "cryovial_box_id",
        "dbs_sheet" = "dbs_bag_id"
      )
    )

    joins <- setNames(container_indentifier, "id")

    # Find empty containers
    if (manifest == "bag|box") {
      sample_type_df <- collect(tbl(con, sample_type)) %>%
        dplyr::rename(sample_type_id = id)

      empty_containers_box_df <- dbReadTable(con, "box") %>%
        dplyr::left_join(sample_type_df %>% filter(manifest_type == "box"), by = joins) %>%
        filter(is.na(sample_type_id)) %>%
        dplyr::mutate(manifest_type = "box") %>%
        dplyr::select(manifest_id = id, manifest_name = name, manifest_type) %>%
        collect()

      empty_containers_bag_df <- dbReadTable(con, "bag") %>%
        dplyr::left_join(sample_type_df %>% filter(manifest_type == "bag"), by = joins) %>%
        filter(is.na(sample_type_id)) %>%
        dplyr::mutate(manifest_type = "bag") %>%
        dplyr::select(manifest_id = id, manifest_name = name, manifest_type) %>%
        collect()

      empty_containers_df <- bind_rows(empty_containers_box_df, empty_containers_bag_df)

    } else {
      if (sample_type == "cryovial_tube|whole_blood_tube") {

        empty_containers_df <- tbl(con, manifest) %>%
          dplyr::anti_join(tbl(con, "cryovial_tube") %>% dplyr::rename(cry_id = id), by = c("id" = "manifest_id")) %>%
          dplyr::anti_join(tbl(con, "whole_blood_tube") %>% dplyr::rename(wb_id = id), by = c("id" = "cryovial_box_id")) %>%
          dplyr::select(manifest_id = id, manifest_name = name) %>%
          collect()

      } else {

        sample_type_tbl <- tbl(con, sample_type) %>%
          dplyr::rename(sample_type_id = id)

        empty_containers_df <- tbl(con, manifest) %>%
          dplyr::left_join(sample_type_tbl, by = joins) %>%
          filter(is.na(sample_type_id)) %>%
          dplyr::select(manifest_id = id, manifest_name = name) %>%
          collect()
        }
    }

    dbDisconnect(con)

    if (nrow(empty_containers_df) == 0) {
      showNotification("No empty containers found.", type = "warning")
      return()
    }

    # Store empty containers in reactive values
    rv$empty_containers <- empty_containers_df

    table_columns <- if (manifest == "bag|box") {
      list(
        manifest_name = colDef(name = "Container Name"),
        manifest_type = colDef(name = "Box / Bag")
      )
    } else {
      list(
        manifest_name = colDef(name = "Container Name")
      )
    }

    # Render the reactable table
    output$emptyContainersTable <- renderReactable({
      reactable(
        empty_containers_df %>% select(-manifest_id),
        selection = "multiple",
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        defaultSelected = NULL,
        onClick = "select",
        columns = table_columns
      )
    })

    # Show modal for selecting containers to delete
    showModal(
      modalDialog(
        title = tags$h3("Delete Empty Containers"),
        tags$p("Select empty containers to delete."),
        reactableOutput("emptyContainersTable"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("DeleteSelectedContainers", "Delete Selected")
        )
      )
    )
  })

  # Update selected containers
  observe({
    selected_rows <- getReactableState("emptyContainersTable", "selected")

    if (!is.null(selected_rows) && length(selected_rows) > 0) {
      rv$selected_containers <- rv$empty_containers[selected_rows, ]$manifest_id
    } else {
      rv$selected_containers <- NULL
    }
  })

  # Handle delete action
  observeEvent(input$DeleteSelectedContainers, {

    if (is.null(rv$selected_containers) || length(rv$selected_containers) == 0) {
      showNotification("No containers selected.", type = "error")
      return()
    }

    con <- dbConnect(SQLite(), Sys.getenv("SDB_PATH"))
    dbBegin(con)

    if (is.null(rv$manifest_table)) {
      showNotification("Manifest table not found.", type = "error")
      dbDisconnect(con)
      return()
    }

    if (rv$manifest_table == "bag|box") {
      container_table <- rv$empty_containers[rv$empty_containers$manifest_id %in% rv$selected_containers,]
      for (ii in 1:nrow(container_table)) {
        dbExecute(con, sprintf("DELETE FROM %s WHERE id IN (%s)", container_table$manifest_type[ii], container_table$manifest_id[ii]))
      }
    } else {
      dbExecute(con, sprintf("DELETE FROM %s WHERE id IN (%s)", rv$manifest_table, paste(rv$selected_containers, collapse = ",")))
    }

    dbCommit(con)
    dbDisconnect(con)

    showNotification("Selected containers deleted successfully.", type = "message")

    # Remove the deleted containers from rv$empty_containers
    rv$empty_containers <- NULL
    rv$manifest_table <- NULL
    rv$selected_containers <- NULL

    removeModal()
  })

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

    # Determine the correct table based on the sample type
    sample_type_name <- switch(
      input$MoveType,
      "samples" = switch(
        input$MoveSampleType,
        "micronix" = "Micronix Samples",
        "cryovial" = "Cryovial Samples",
        "dbs_sample" = "DBS Samples"
      ),
      "controls" = switch(
        input$MoveControlType,
        "whole_blood" = "Whole Blood Controls",  # whole_blood uses cryovial_box
        "dbs_sheet" = "DBS Controls"          # dbs_sheet uses dbs_bag
      )
    )

    # Show the modal dialog for creating a new container or bag
    showModal(
      modalDialog(
        title = tags$h3("Create a new place to store", tags$strong(sample_type_name)),

        tags$h5("1. Document the location where the new container or bag will be stored."),
        selectInput("ManifestLocationRoot", label = NULL, width = '47%', choices = NULL),
        selectInput("ManifestLocationRootLevelI", label = NULL, width = '47%', choices = NULL),
        selectInput("ManifestLocationRootLevelII", label = NULL, width = '47%', choices = NULL),
        conditionalPanel(
          condition = "input.MoveSampleType == 'dbs_sample'",
          radioButtons("MoveDBSSampleType", label = "Choice:", choices = c("Box" = "box", "Bag" = "bag"))
        ),
        tags$h5("2. Create a new name for the container or bag."),
        textInput("ManifestID", label = "Human Readable Name", placeholder = "PRISM-2022-001"),
        uiOutput("ManifestIDCheck"),

        conditionalPanel(
          condition = "(input.MoveType == 'controls' && input.MoveControlType == 'whole_blood') && (input.MoveType == 'samples' && (input.MoveSampleType == 'micronix' || input.MoveSampleType == 'cryovial'))",
          textInput("ManifestBarcode", label = "Barcode"),
          uiOutput("ManifestBarcodeCheck")
        ),

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
          "cryovial" = "cryovial_box",
          "dbs_sample" = input$MoveDBSSampleType,
          "static_plate" = "micronix_plate"
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

      # Get the location ID based on location information
      location_id <- tbl(con, "location") %>%
        filter(location_root %in% local(input$ManifestLocationRoot) & level_I %in% local(input$ManifestLocationRootLevelI) & level_II %in% local(input$ManifestLocationRootLevelII)) %>%
        pull(id)


      now <- as.character(lubridate::now())
      # Check if the barcode already exists
      if (manifest %in% c("cryovial_box", "micronix_plate")) {
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

        # Prepare the data for insertion into the correct manifest table
        df.payload <- data.frame(
          location_id = location_id,
          name = input$ManifestID,
          barcode = ifelse(input$ManifestBarcode == "", NA, input$ManifestBarcode),
          last_updated = now,
          created = now
        )
      } else {
        # Prepare the data for insertion into the correct manifest table
        df.payload <- data.frame(
          location_id = location_id,
          name = input$ManifestID,
          last_updated = now,
          created = now
        )
      }

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
              "cryovial" = "cryovial_box",
              "dbs_sample" = input$MoveDBSSampleType,
              "static_plate" = "micronix_plate"
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
          # Determine the correct table based on the sample type
          manifest <- switch(
            input$MoveType,
            "samples" = switch(
              input$MoveSampleType,
              "micronix" = "micronix_plate",
              "cryovial" = "cryovial_box",
              "dbs_sample" = NULL # No barcode
            ),
            "controls" = switch(
              input$MoveControlType,
              "whole_blood" = "cryovial_box", 
              "dbs_sheet" = NULL # No barcode
            )
          )

          if (!is.null(manifest)) {

            result <- tbl(con, manifest) %>%
              filter(barcode %in% local(input$ManifestBarcode)) %>%
              count() %>%
              pull(n)

            if (result > 0) {
              html <- paste0("<span style=color:#0000ff>", "Barcode is in use!", "</span>")
            }
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

  observeEvent(input$ContinueMove, {
    # User confirmed warnings, allow upload
    removeModal()  # Close the modal
    message("User confirmed warnings and wants to proceed with the move.")
    showNotification("User confirmed warnings and wants to proceed with the move.")
  })

  observeEvent(input$CancelMove, {
    rv$user_file <- NULL
    removeModal()  # Close the modal
    message("User reviewed warnings and decided to cancel the move.")
    showNotification("User reviewed warnings and decided to cancel the move.")
  })

  observeEvent(input$MoveAction, ignoreInit = TRUE, {

    if (isTRUE(rv$user_action_required)) {
      message("Upload action halted - user action required")
      return()
    }

    early_stop <- FALSE
    warnings_list <- ValidationErrorCollection$new()

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

    # message(paste("Loaded", dataset$name))
    if (is.null(rv$user_file)) {
      early_stop <- tryCatch({
        withCallingHandlers({

          move_data_list <- list()

          for (i in 1:length(dataset[,1])) {
            extract_name_from_filename <- (input$MoveType == "samples" && input$MoveSampleType %in% c("micronix", "cryovial")) ||
              (input$MoveType == "controls" && input$MoveControlType == "whole_blood")

            if (extract_name_from_filename) {
              manifest_name <- sub('\\.csv$', '', dataset[i,]$name)

              if(input$MoveFileType == "traxcer" && input$MoveTraxcerStripFromFilename) {
                manifest_name <- substr(manifest_name, 1, nchar(manifest_name)-16)
                if (nchar(manifest_name) == 0) {
                  stop(sprintf("Traxcer name was completely removed - did you mean to remove the datetime string from the filename?"))
                }
              }

              ## format the file
              if (input$MoveType == "samples") {
                container <- get_container_by_sample(sample_type = input$MoveSampleType)

                result <- process_specimen_csv(
                  user_csv = dataset[i,]$datapath,
                  user_action = "move",
                  file_type = input$MoveFileType,
                  sample_type = input$MoveSampleType,
                  bind_data = setNames(manifest_name, container$container_name_key)
                )

                if (!is.data.frame(result)) {
                  move_data_list <- c(move_data_list, list(result$data))
                  names(move_data_list)[i] <- manifest_name

                  if (warnings_list$length() == 0) warnings_list <- result$warnings$clone(deep = TRUE)
                  else warnings_list$concatenate(result$warnings)

                } else {
                  move_data_list <- c(move_data_list, list(result))
                  names(move_data_list)[i] <- manifest_name
                }
              } else { # Controls
                container <- get_container_by_control(control_type = input$MoveControlType)

                result <- process_control_csv(
                  user_csv = dataset$datapath,
                  user_action = "move",
                  file_type = input$MoveFileType,
                  control_type = input$MoveControlType,
                  bind_data = setNames(manifest_name, container$container_name_key)
                )

                if (!is.data.frame(result)) {
                  move_data_list <- c(move_data_list, list(result$data))
                  names(move_data_list)[i] <- manifest_name

                  if (warnings_list$length() == 0) warnings_list <- result$warnings$clone(deep = TRUE)
                  else warnings_list$concatenate(result$warnings)

                } else {
                  move_data_list <- c(move_data_list, list(result))
                  names(move_data_list)[i] <- manifest_name
                }
              }
            } else { ## NOTE: we possibly can remove the extract_name_from_filename if-else I think...
              ## format the file
              if (input$MoveType == "samples") {

                result <- process_specimen_csv(
                  user_csv = dataset[i,]$datapath,
                  user_action = "move",
                  file_type = input$MoveFileType,
                  sample_type = input$MoveSampleType
                )

                if (!is.data.frame(result)) {
                  move_data_list <- c(move_data_list, list(result$data))
                  names(move_data_list)[i] <- length(move_data_list) # this could just be the file name...

                  if (warnings_list$length() == 0) warnings_list <- result$warnings$clone(deep = TRUE)
                  else warnings_list$concatenate(result$warnings)

                } else {
                  move_data_list <- c(move_data_list, list(result))
                  names(move_data_list)[i] <- length(move_data_list)
                }

              } else { # Controls

                result <- process_control_csv(
                  user_csv = dataset$datapath,
                  user_action = "move",
                  file_type = input$MoveFileType,
                  control_type = input$MoveControlType
                )

                if (!is.data.frame(result)) {
                  move_data_list <- c(move_data_list, list(result$data))
                  names(move_data_list)[i] <- length(move_data_list)

                  if (warnings_list$length() == 0) warnings_list <- result$warnings$clone(deep = TRUE)
                  else warnings_list$concatenate(result$warnings)

                } else {
                  move_data_list <- c(move_data_list, list(result))
                  names(move_data_list)[i] <- length(move_data_list)
                }
              }            
            }

            rv$user_file <- move_data_list
            FALSE
          }
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
    }

    if (isTRUE(early_stop)) { 
      rv$user_file <- NULL      
      return()
    }

    if (warnings_list$length() > 0) {
      show_validation_warning_modal(input, output, warnings_list, action = "move")
      return(NULL) # Force the reactive to stop here
    }

    message("Starting Move...")

    b_use_wait_dialog <- FALSE

    tryCatch({
      withCallingHandlers({
        # simple way to add a dialog or not
        b_use_wait_dialog <- if (is.list(rv$user_file) && length(rv$user_file) == 1) { nrow(rv$user_file[[1]]) > 5 } else { length(rv$user_file) > 1 } 

        if (b_use_wait_dialog) {
          total <- if (is.list(rv$user_file) && length(rv$user_file) == 1) { nrow(rv$user_file[[1]]) } else { length(rv$user_file)}
          unit <- if (is.list(rv$user_file) && length(rv$user_file) == 1) { "rows" } else { "files" }
          show_modal_spinner(
            spin = "double-bounce",
            color = "#00bfff",
            text = sprintf("Working on %d %s, please be patient...", total, unit)
          )
        }

        shinyjs::reset("MoveAction")

        # note: this is to make things work retroactively
        # Move the data depending on the type
        sample_type <- if (input$MoveType == "controls") input$MoveControlType else input$MoveSampleType
        MoveSpecimens(sample_type = sample_type, move_data = rv$user_file)
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

  move_example_data <- reactiveValues(
    required = NULL,
    optional = NULL
  )

  # Download a complete upload template
  observe({

    output$MoveFileTemplatePlaceholder <- renderUI({

      get_specific_move_type <- switch(input$MoveType, 
        "samples" = function() { 
          sample_types <- get_sample_types()
          sample_file_types <- get_file_types_for_sample(input$MoveSampleType)

          match_index <- match(input$MoveSampleType, sample_types)
          sample_display_name <- names(sample_types[match_index])

          match_index <- match(input$MoveFileType, sample_file_types)
          file_type_display_name <- names(sample_file_types[match_index])

          sprintf("%s (FileType: '%s')", sample_display_name, file_type_display_name)
        },
        "controls" = function() { 
          control_types <- get_control_types()
          control_action_types <- get_control_action_types(input$MoveControlType)

          match_index <- match(input$MoveControlType, control_types)
          control_display_name <- names(control_types[match_index])

          sprintf("%s", control_display_name)
        }
      )
      downloadButton("MoveFileTemplate", label = paste("Download", get_specific_move_type(), "Move Template"))
    })

    # NOTE: Should add a case for controls when they are added.
    output$MoveFileTemplate <- downloadHandler(
      filename = function() {
        if (input$MoveType == "samples") {
          filename_base <- paste(c(input$MoveSampleType, input$MoveFileType, "upload", "template"), collapse = "_")
          paste(filename_base, ".csv", sep = "")
        } else {
          paste(paste(c(input$MoveControlType, "move", "template"), collapse = "_"), ".csv", sep = "")
        }
      },
      content = function(con) {
         # Check if the user is uploading samples or controls
        if (input$MoveType == "samples") {
          # Retrieve column data for samples based on selected sample type
          column_data <- get_sample_file_columns(input$MoveSampleType, "upload", input$MoveFileType)
        } else if (input$MoveType == "controls") {
          # Retrieve column data for controls based on selected control type
          column_data <- get_control_file_columns(input$MoveControlType, "move")  # Using action from the input
        } else {
          stop("Invalid upload type!!!")
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
