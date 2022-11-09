library(shiny)
library(shinybusy)
library(shinyjs)

# App Function for Uploading Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

AppUploadSamples <- function(session, output, input, database){

  rv <- reactiveValues(user_file = NULL)

  observe({
    req(input$UploadSampleDataSet)
    rv$user_file <- input$UploadSampleDataSet$datapath
  })

  # 1. get path to user provided file, if path exists perform checks and reformat file

  observeEvent(input$UploadAction, {

    print("rv_file")
    print(rv$user_file)
    print("UploadStorageContainerDestID")
    print(input$UploadStorageContainerDestID)
    print("UploadStorageContainerDestLocation")
    print(input$UploadStorageContainerDestLocation)
    print("UploadStorageContainerDestLocationLevelI")
    print(input$UploadStorageContainerDestLocationLevelI)
    print("UploadStorageContainerDestLocationLevelII")
    print(input$UploadStorageContainerDestLocationLevelII)
    req(
      rv$user_file,
      input$UploadStorageContainerDestID,
      input$UploadStorageContainerDestLocation,
      input$UploadStorageContainerDestLocationLevelI,
      input$UploadStorageContainerDestLocationLevelII
    )

    file_type <- input$UploadFileType
    sample_storage_type <- input$UploadSampleType
    container_name <- input$UploadStorageContainerDestID

    formatted_file <- NULL
    b_use_wait_dialog <- FALSE
    output$UploadMicronixReturnMessage2 <- renderText({
      tryCatch({
          user_file <- isolate({ rv$user_file })

          #check colnames of user provided file
          user_file <- sampleDB::ProcessCSV(
            user_csv = user_file,
            user_action = "upload",
            file_type = file_type,
            sample_storage_type = sample_storage_type,
            container_name = container_name)

          # simple way to add a dialog or not
          b_use_wait_dialog <- nrow(user_file) > 5

          if (b_use_wait_dialog) {
            show_modal_spinner(
              spin = "double-bounce",
              color = "#00bfff",
              text = paste("Uploading", nrow(formatted_file), "samples, please be patient...")
            )
          }

          rv$user_file <- NULL
          shinyjs::reset("UploadAction")

          sampleDB::UploadSamples(sample_type = sample_storage_type, 
                                                upload_data = user_file, 
                                                container_name = isolate({ input$UploadStorageContainerDestID }),
                                                freezer_address = list(location_name = input$UploadStorageContainerDestLocation, 
                                                                       level_I = input$UploadStorageContainerDestLocationLevelI, 
                                                                       level_II = input$UploadStorageContainerDestLocationLevelII))
        },
        warning = function(w) {
          message(w)
          w$message
        },
        error = function(e) {
          message(e)
          e$message
        },
        finally = {
          if (b_use_wait_dialog) {
            remove_modal_spinner()
          }
        }
      )
    })

  })

  observeEvent(input$UploadSampleType, {
    container_choices <- switch(input$UploadSampleType,
      "micronix" = dbUpdateEvent()$micronix_plate_name,
      "cryovial" = dbUpdateEvent()$cryovial_box_name
    )

    shinyjs::reset(input$UploadStorageContainerDestID)
    updateSelectizeInput(session, selected = "", "UploadStorageContainerDestID", choices = container_choices %>% sort(), option = list(create = TRUE), server = TRUE)
    print(input$UploadStorageContainerDestID)
  })

  observeEvent(dbUpdateEvent(), {
    updateSelectInput(session, selected = input$UploadStorageContainerDestLocation, "UploadStorageContainerDestLocation", choices = dbUpdateEvent()$location %>% sort())
    
    container_choices <- switch(input$UploadSampleType,
      "micronix" = dbUpdateEvent()$micronix_plate_name,
      "cryovial" = dbUpdateEvent()$cryovial_box_name
    )

    updateSelectizeInput(session, selected = "", "UploadStorageContainerDestID", choices = container_choices %>% sort(), option = list(create = TRUE), server = TRUE)
  })


  #3. check plate info
  observe({
    if(input$UploadStorageContainerDestID != "" && !is.null(input$UploadStorageContainerDestID)) {
      CheckPlates(input = input, output = output, database = database, sample_type = input$UploadSampleType) 
    }
  })

  # allow user to reset ui
  # UploadReset(input = input, output = output, sample_type = input$UploadSampleType)
  
  # auto-filter freezer addresses in dropdown
  SmartFreezerDropdownFilter(database = database, session = session,
                             input = input,
                             location_ui = "UploadStorageContainerDestLocation", 
                             levelI_ui = "UploadStorageContainerDestLocationLevelI", 
                             levelII_ui = "UploadStorageContainerDestLocationLevelII")
  
  # present micronix upload examples
  MicronixUploadExamples(input = input, database = database, output = output)
}

MicronixUploadExamples <- function(input, database, output){
  
  observeEvent(input$UploadSampleType, {
    if(input$UploadSampleType == "micronix"){
        ui.output <- list(LogisticsItems = "LogisticsItems",
                          MetadataItems = "MetadataItems",
                          CombinedItems = "CombinedItems")
      }
  }) 
}