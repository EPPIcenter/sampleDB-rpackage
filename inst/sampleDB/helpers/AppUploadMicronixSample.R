

MicronixUpload <- function(session, output, input, database){
  
  # Overview
  # perform various checks of user provided micronix upload file, reformat user provided micronix upload file, and print user messages if file does not pass a check
  # checks in ui are not enforced, they are used to create warning messages for user
  
  # 1. create variable for storing formatted_upload_file once it is created
  reactive_vals <- reactiveValues(formatted_upload_file = NULL)

  # 2. get path to user provided micronix upload file, if path exists perform checks and reformat file
  observe({
    
    users_upload_file_path <- input[["UploadMicronixDataSet"]]$datapath
    if(!is.null(users_upload_file_path)){
      
      users_upload_file <- read.csv(users_upload_file_path, check.names = F) %>% suppressWarnings() # will throw a pointless corrupt last line warning if file comes from excel
      
      #check colnames of user provided micronix upload file
      UploadFileColnameCheck <- CheckColnamesOfUserProvidedMicronixFileFormat(input = input, output = output, sample_type = "micronix", users_upload_file = users_upload_file)
      
      if(isTRUE(UploadFileColnameCheck)){
        #reformat upload data file
        reactive_vals$formatted_upload_file <- FormatMicronixUploadData(input = input, sample_type = "micronix", users_upload_file = users_upload_file)

        #after formatting takes place, check upload data content
        if(!is.null(reactive_vals$formatted_upload_file)){
          ConductBasicFormattedUploadFileChecks(output = output, database = database, sample_type = "micronix", formatted_upload_file = reactive_vals$formatted_upload_file) 
        } 
      }
    }
  })
  
  #3. check plate info
  observe({
    if(input[["UploadMicronixPlateID"]] != ""){
      ConductUploadPlateChecks(input = input, output = output, database = database, sample_type = "micronix") 
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
  
  # upload samples - the upload fun contains enforced checks
  observe({
    if(input$ActionUploadMatrix == "Go"){
      sampleDB::UploadSamples(sample_type = "micronix", 
                              upload_data = reactive_vals$formatted_upload_file, 
                              container_name = input$"UploadMicronixPlateID", 
                              container_barcode = input$"UploadMicronixPlateBarcode", 
                              freezer_address = list(location_name = input$"UploadMicronixLocation", 
                                                     level_I = input$"UploadLocationMicronixLevelI", 
                                                     level_II = input$"UploadLocationMicronixLevelII"))
      # print "Upload Complete" user message
      output$UploadMicronixReturnMessage2 <- renderText({"Upload Complete"})
      # reset trigger
      updateTextInput(session = session, "ActionUploadMatrix", value = "")
    }
  })
  
  # allow user to reset ui
  # NOTE: cannot push reset if inputboxes are empty!
  UploadReset(input = input, output = output, sample_type = "micronix")
  
  # auto-filter freezer addresses in dropdown
  SmartFreezerDropdownFilter(input = input, database = database, session = session)
  
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