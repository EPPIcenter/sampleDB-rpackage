
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
        users_upload_file <- users_upload_file[!apply(users_upload_file, 1, function(row) all(row == "")),] 

        #check colnames of user provided file
        UploadFileLogisticalColnameCheck <- CheckLogisticalColnamesOfUserProvidedMicronixFile(input = input, output = output, users_upload_file = users_upload_file, ui_elements = GetUIUploadElements("micronix"))
        UploadFileMetadataColnameCheck <- CheckMetadataColnamesOfUserProvidedMicronixFile(input = input, output = output, users_upload_file = users_upload_file, ui_elements = GetUIUploadElements("micronix"))
        
        validate(need(isTRUE(UploadFileLogisticalColnameCheck), "*** ERROR: Logistical column name check failed."))
        validate(need(isTRUE(UploadFileMetadataColnameCheck), "*** ERROR: Metadata column name check failed."))

        #reformat upload file
        formatted_file <- FormatMicronixUploadData(database, input = input, users_upload_file = users_upload_file, ui_elements = GetUIUploadElements("micronix"))
        #after formatting takes place, check upload data content
        validate(need(!is.null(formatted_file), "*** ERROR: Formatting micronix upload data."))

        CheckFormattedUploadFile(output = output, database = database, formatted_upload_file = formatted_file, ui_elements = GetUIUploadElements("micronix")) 
        
        # finally, remove any missing values (sometimes not collectiondate) 
        study <- filter(sampleDB::CheckTable(database = database, "study"), short_code %in% formatted_file$study_short_code)
        if (0 < nrow(study) && 0 == study$is_longitudinal) {
          formatted_file <- tidyr::drop_na(formatted_file, -collection_date) 
        } else {
          formatted_file <- tidyr::drop_na(formatted_file) 
        }

        reactive_vals$formatted_upload_file <- formatted_file
      }
  })

  observe({
    updateSelectInput(session, selected = input$UploadMicronixLocation, "UploadMicronixLocation", choices = dbUpdateEvent()$location %>% sort())
    updateSelectizeInput(session, selected = input$UploadMicronixPlateID, "UploadMicronixPlateID", choices = dbUpdateEvent()$plate_name %>% sort(), option = list(create = TRUE))
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
      output$UploadMicronixReturnMessage2 <- renderText({
        # set upload reqs (reqs are enforced ui checks)
        SetUploadRequirements(input = input, database = database, sample_type = "micronix")
        # upload samples - the pkg upload fun contains enforced checks
        sampleDB::UploadSamples(sample_type = "micronix", 
                                                upload_data = reactive_vals$formatted_upload_file, 
                                                container_name = input$"UploadMicronixPlateID", 
                                                container_barcode = input$"UploadMicronixPlateBarcode", 
                                                freezer_address = list(location_name = input$"UploadMicronixLocation", 
                                                                       level_I = input$"UploadLocationMicronixLevelI", 
                                                                       level_II = input$"UploadLocationMicronixLevelII"))
        })
    })
  )
  
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
