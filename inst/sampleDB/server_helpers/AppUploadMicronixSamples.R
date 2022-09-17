library(shiny)
library(shinybusy)

# App Function for Uploading Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

MicronixUpload <- function(session, output, input, database){

  # 1. get path to user provided file, if path exists perform checks and reformat file
  file_to_upload <- reactiveVal(NULL)

  observeEvent(input$UploadMicronixActionButton, {

    req(
      input$UploadMicronixDataSet$datapath,
      input$UploadMicronixPlateID,
      input$UploadMicronixLocation,
      input$UploadLocationMicronixLevelI,
      input$UploadLocationMicronixLevelII
    )

    unformatted_file <- input$UploadMicronixDataSet$datapath
    formatted_file <- NULL
    b_use_wait_dialog <- FALSE
    output$UploadMicronixReturnMessage2 <- renderText({
      tryCatch({

          users_upload_file <- read.csv(unformatted_file, header = F) %>% suppressWarnings() # will throw a pointless corrupt last line warning if file comes from excel

          # simple way to add a dialog or not
          b_use_wait_dialog <- nrow(users_upload_file) > 10

          #check colnames of user provided file
          UploadFileLogisticalColnameCheck <- CheckLogisticalColnamesOfUserProvidedMicronixFile(input = input, output = output, users_upload_file = users_upload_file, ui_elements = GetUIUploadElements("micronix"))
          UploadFileMetadataColnameCheck <- CheckMetadataColnamesOfUserProvidedMicronixFile(input = input, output = output, users_upload_file = users_upload_file, ui_elements = GetUIUploadElements("micronix"))
           
          validate(need(isTRUE(UploadFileLogisticalColnameCheck), "Logistical column name check failed."))
          validate(need(isTRUE(UploadFileMetadataColnameCheck), "Metadata column name check failed."))

          #There have been bugs caused by empty colums
          #Find and remove columns on upload
          #Alternatively, could reject files that have empty columns but this 
          #probably is okay on upload

          empty_columns <- colSums(is.na(users_upload_file) | users_upload_file == "") == nrow(users_upload_file)
          users_upload_file <- users_upload_file[, !empty_columns]
          users_upload_file <- users_upload_file[!apply(users_upload_file, 1, function(row) all(row == "")),] 

          #reformat upload file
          formatted_file <- FormatMicronixUploadData(database, input = input, users_upload_file = users_upload_file, ui_elements = GetUIUploadElements("micronix"))
          #after formatting takes place, check upload data content
          validate(need(!is.null(formatted_file), "Formatting micronix upload data."))

          CheckFormattedUploadFile(output = output, database = database, formatted_upload_file = formatted_file, ui_elements = GetUIUploadElements("micronix")) 
          
          if (b_use_wait_dialog) {
            show_modal_spinner(
              spin = "double-bounce",
              color = "#00bfff",
              text = paste("Uploading", nrow(formatted_file), "samples, please be patient...")
            )
          }

          sampleDB::UploadSamples(sample_type = input$UploadSampleType, 
                                                upload_data = formatted_file, 
                                                container_name = isolate({ input$UploadMicronixPlateID }), 
                                                container_barcode = input$UploadMicronixPlateBarcode, 
                                                freezer_address = list(location_name = input$UploadMicronixLocation, 
                                                                       level_I = input$UploadLocationMicronixLevelI, 
                                                                       level_II = input$UploadLocationMicronixLevelII))
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

  observeEvent(dbUpdateEvent(), {
    updateSelectInput(session, selected = input$UploadMicronixLocation, "UploadMicronixLocation", choices = dbUpdateEvent()$location %>% sort())
    updateSelectizeInput(session, selected = input$UploadMicronixPlateID, "UploadMicronixPlateID", choices = dbUpdateEvent()$plate_name %>% sort(), option = list(create = TRUE))
  })


  #3. check plate info
  observe({
    if(input[["UploadMicronixPlateID"]] != ""){
      CheckPlates(input = input, output = output, database = database, sample_type = "micronix") 
    }
  })

  # allow user to reset ui
  UploadReset(input = input, output = output, sample_type = "micronix")
  
  # auto-filter freezer addresses in dropdown
  SmartFreezerDropdownFilter(database = database, session = session,
                             input = input,
                             location_ui = "UploadMicronixLocation", 
                             levelI_ui = "UploadLocationMicronixLevelI", 
                             levelII_ui = "UploadLocationMicronixLevelII")
  
  # present micronix upload examples
  MicronixUploadExamples(input = input, database = database, output = output, sample_type = "micronix")
}

MicronixUploadExamples <- function(input, database, output, sample_type){
  
  if(sample_type == "micronix"){
    ui.output <- list(LogisticsItems = "LogisticsItems",
                      MetadataItems = "MetadataItems",
                      CombinedItems = "CombinedItems")
  }
  
  output[[ui.output$LogisticsItems]] <- renderTable({.ExampleLogisticsItems()}, striped = T, bordered = T)
  output[[ui.output$MetadataItems]] <- renderTable({.ExampleMetadataItems()}, striped = T, bordered = T)
  output[[ui.output$CombinedItems]] <- renderTable({.ExampleCombinedItems()}, striped = T, bordered = T)
}
