
# App Function for Uploading Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

MicronixUpload <- function(session, output, input, database){
  
  # 1. create variable for storing formatted_upload_file once it is created
  reactive_vals <- reactiveValues(formatted_upload_file = NULL)

  # 2. get path to user provided file, if path exists perform checks and reformat file
  observe({
    
    users_upload_file_path <- input[["UploadMicronixDataSet"]]$datapath
    if(!is.null(users_upload_file_path)){
      
      users_upload_file <- read.csv(users_upload_file_path, header = F) %>% suppressWarnings() # will throw a pointless corrupt last line warning if file comes from excel
      
      print(paste0("MicronixUpload: ", users_upload_file))
      #There have been bugs caused by empty colums
      #Find and remove columns on upload
      #Alternatively, could reject files that have empty columns but this 
      #probably is okay on upload

      empty_columns <- colSums(is.na(users_upload_file) | users_upload_file == "") == nrow(users_upload_file)
      users_upload_file <- users_upload_file[, !empty_columns]

      #check colnames of user provided file
      UploadFileLogisticalColnameCheck <- CheckLogisticalColnamesOfUserProvidedMicronixFile(input = input, output = output, users_upload_file = users_upload_file, ui_elements = GetUIUploadElements("micronix"))
      UploadFileMetadataColnameCheck <- CheckMetadataColnamesOfUserProvidedMicronixFile(input = input, output = output, users_upload_file = users_upload_file, ui_elements = GetUIUploadElements("micronix"))
      
      if(isTRUE(UploadFileLogisticalColnameCheck) && isTRUE(UploadFileMetadataColnameCheck)){
        #reformat upload file
        reactive_vals$formatted_upload_file <- FormatMicronixUploadData(database, input = input, users_upload_file = users_upload_file, ui_elements = GetUIUploadElements("micronix"))

        #after formatting takes place, check upload data content
        if(!is.null(reactive_vals$formatted_upload_file)){
          CheckFormattedUploadFile(output = output, database = database, formatted_upload_file = reactive_vals$formatted_upload_file, ui_elements = GetUIUploadElements("micronix")) 
        } 
      }
    }
  })
  
  #3. check plate info
  observe({
    if(input[["UploadMicronixPlateID"]] != ""){
      CheckPlates(input = input, output = output, database = database, sample_type = "micronix") 
    }
  })
  
  observeEvent(
    input$UploadMicronixActionButton,
    ({
      # set upload reqs (reqs are enforced ui checks)
      SetUploadRequirements(input = input, database = database, sample_type = "micronix")
      # fire upload trigger
      updateTextInput(session = session, "ActionUploadMatrix", value = "Go"); Sys.sleep(.75)
      # print "Working..." user message
      output$UploadMicronixReturnMessage1 <- renderText({"Working..."})
    }))
  
  # upload samples - the pkg upload fun contains enforced checks
  observe({
    if(input$ActionUploadMatrix == "Go"){
      return_message <- sampleDB::UploadSamples(sample_type = "micronix", 
                                                upload_data = reactive_vals$formatted_upload_file, 
                                                container_name = input$"UploadMicronixPlateID", 
                                                container_barcode = input$"UploadMicronixPlateBarcode", 
                                                freezer_address = list(location_name = input$"UploadMicronixLocation", 
                                                                       level_I = input$"UploadLocationMicronixLevelI", 
                                                                       level_II = input$"UploadLocationMicronixLevelII"))
      # print user message
      output$UploadMicronixReturnMessage2 <- renderText({return_message})
      # reset trigger
      updateTextInput(session = session, "ActionUploadMatrix", value = "")
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
