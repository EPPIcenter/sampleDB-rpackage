library(shiny)
library(shinybusy)
library(shinyjs)
library(purrr)
library(RSQLite)
library(dbplyr)

# App Function for Uploading Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

AppUploadSamples <- function(session, output, input, database) {

  rv <- reactiveValues(user_file = NULL, console_verbatim = FALSE, b_use_wait_dialog = FALSE, cleanup = FALSE)

  observeEvent(input$UploadSampleDataSet, ignoreInit = TRUE, {
    dataset <- input$UploadSampleDataSet

    tryCatch({
      withCallingHandlers({
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
    error = function(e) {
      print(e)
      html<-paste0("<font color='red'>", paste0(dataset$name, ": ", e$message), "</font>")
      shinyjs::html(id = "UploadOutputConsole", html = html, add = rv$console_verbatim)
    })

    rv$console_verbatim <- FALSE
  })


  observeEvent(input$UploadAction, ignoreInit = TRUE, {
    if (is.null(rv$user_file)) {
      message("Upload action halted - no file uploaded")
      return()
    }

    file_type <- input$UploadFileType
    container_name <- input$UploadManifestName

    formatted_file <- NULL
    rv$b_use_wait_dialog <- FALSE
    output$UploadOutputConsole <- renderText({
      tryCatch({

        # simple way to add a dialog or not
        rv$b_use_wait_dialog <- nrow(rv$user_file) > 5

        if (rv$b_use_wait_dialog) {
          show_modal_spinner(
            spin = "double-bounce",
            color = "#00bfff",
            text = paste("Uploading", nrow(formatted_file), "samples, please be patient...")
          )
        }

        shinyjs::reset("UploadAction")
        sampleDB::UploadSamples(sample_type_id = as.integer(input$UploadSampleType), upload_data = rv$user_file)       
      },
      error = function(e) {
        message(e)
        e$message
      },
      finally = {
        rv$cleanup <- TRUE
      })
    })
  })

  observeEvent(rv$cleanup, ignoreInit = TRUE, {
    remove_modal_spinner()
    rv$user_file <- NULL
    rv$b_use_wait_dialog <- FALSE
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
    rv$user_file <- NULL
  })
}
