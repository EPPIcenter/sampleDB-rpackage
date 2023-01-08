library(shiny)
library(shinybusy)
library(shinyjs)
library(purrr)

# App Function for Uploading Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

AppUploadSamples <- function(session, output, input, database) {

  rv <- reactiveValues(user_files = NULL, console_verbatim = FALSE)

  observeEvent(input$UploadSampleDataSet, ignoreInit = TRUE, {
    dataset <- input$UploadSampleDataSet
    rv$user_files <- NULL

    for (i in 1:nrow(dataset)) {
      tryCatch({
        withCallingHandlers({
          processed_file <- sampleDB::ProcessCSV(
            user_csv = dataset[i, ]$datapath,
            user_action = "upload",
            file_type = input$UploadFileType,
            sample_storage_type = switch(
              input$UploadSampleType,
              "1" = "micronix",
              "2" = "cryovial"
            )
          )

          rv$user_files <- append(rv$user_files, processed_file)
          names(rv$user_files[length(rv$user_files)]) <- dataset[i, ]$name

          # output$UploadFilesTabset <- renderUI({
          #   map(col_names(), ~ textInput(.x, NULL))
          # })
        },
        message = function(m) {
          message(m$message)
          shinyjs::html(id = "UploadOutputConsole", html = paste0(dataset[i, ]$name, ": ", m$message, "\n"), add = rv$console_verbatim)
          rv$console_verbatim <- TRUE
        })
      },
      error = function(e) {
        print(e)
        html<-paste0("<font color='red'>", paste0(dataset[i, ]$name, ": ", e$message, "\n"), "</font>")
        shinyjs::html(id = "UploadOutputConsole", html = html, add = rv$console_verbatim)
      })
    }

    rv$console_verbatim <- FALSE
  })


  observeEvent(input$UploadAction, ignoreInit = TRUE, {
    if (!is.null(rv$user_files)) {
      return()
    }

    file_type <- input$UploadFileType
    container_name <- input$UploadManifestName

    # todo: this should be mapped somewhere else
    sample_storage_type <- switch(input$UploadSampleType,
      "1" = "micronix",
      "2" = "cryovial"
    )

    formatted_file <- NULL
    b_use_wait_dialog <- FALSE

    output$UploadOutputConsole <- renderText({
      tryCatch({
        #check colnames of user provided file

        # simple way to add a dialog or not
        b_use_wait_dialog <- nrow(rv$user_files) > 5

        if (b_use_wait_dialog) {
          show_modal_spinner(
            spin = "double-bounce",
            color = "#00bfff",
            text = paste("Uploading", nrow(formatted_file), "samples, please be patient...")
          )
        }

        shinyjs::reset("UploadAction")
        sampleDB::UploadSamples(sample_type_id = as.integer(input$UploadSampleType), upload_data = rv$user_files)                                
      },
      error = function(e) {
        rv$user_files <- NULL
        message(e)
        e$message
      },
      finally = {
        rv$user_files <- NULL
        if (b_use_wait_dialog) {
          remove_modal_spinner()
        }
      })
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

    manifest <- switch(sample_type_name,
      "Micronix" = "micronix_plate",
      "Cryovial" = "cryovial_box")


    updateSelectizeInput(
      session,
      "UploadManifestName",
      label = switch(sample_type_name,
        "Micronix" = "Plate Name",
        "Cryovial" = "Box Name"
      ),
      selected = "",
      choices = c("", DBI::dbReadTable(con, manifest) %>% pull(name)),
      options = list(create = TRUE)
    )

    updateSelectInput(
      session,
      "UploadLocationLevelI",
      label = switch(sample_type_name,
        "Micronix" = "Shelf Name", 
        "Cryovial" = "Rack Number"
      )
    )

    updateSelectInput(
      session,
      "UploadLocationLevelII",
      label = switch(sample_type_name,
        "Micronix" = "Basket Name",
        "Cryovial" = "Rack Position" 
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
    shinyjs::reset("UploadSampleDataSet")
    shinyjs::reset("UploadFilesTabset")
    shinyjs::reset("UploadManifestName")
    shinyjs::reset("UploadLocationRoot")
    shinyjs::reset("UploadLocationLevelI")
    shinyjs::reset("UploadLocationLevelII")
    rv$user_files <- NULL
  })
}
