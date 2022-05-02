
# App Function for Moving Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

MoveWetlabSamples <- function(session, input, database, output){
  
  # 1. create variable for storing formatted_move_file_list once it is created
  reactive_vals <- reactiveValues(formatted_move_file_list = NULL) #list with one or multiple move files
  
  # 2. get path to user provided file(s), if path exists perform checks and reformat items in list
  observe({
    users_move_file <- input[["MoveDataSet"]]
    if(!is.null(users_move_file)){
      
      move_data_list <- list()
      for(i in 1:length(users_move_file[,1])){
        plate.name <- users_move_file[[i, 'name']] %>% gsub("\\.csv","",.)
        move_data <- read.csv(users_move_file[[i, 'datapath']]) %>% suppressWarnings() # will throw a pointless corrupt last line warning if file comes from excel
        move_data_list[[plate.name]] <- move_data
      }

      #check colnames of user provided file
      MoveLogisticalResults <- c() #store the T or F results of the Logistical File Checks. All must be true in order to reformat
      for(item in names(move_data_list)){
        MoveFileLogisticalColnameCheck <- CheckLogisticalColnamesOfUserProvidedMicronixFile(input = input, output = output, users_upload_file = move_data_list[[item]], ui_elements = GetUIMoveElements("micronix")) 
        MoveLogisticalResults <- c(MoveLogisticalResults, MoveFileLogisticalColnameCheck)
      }
      
      if(all(MoveLogisticalResults)){
        #reformat move file(s)
        reactive_vals$formatted_move_file_list <- FormatMicronixMoveData(ui_elements = GetUIMoveElements("micronix"), micronix_move_data = move_data_list, input = input)
        
        #after formatting takes place, check move content
        if(!is.null(reactive_vals$formatted_move_file_list)){
          CheckFormattedMoveFile(output = output, database = database, sample_type = "micronix", formatted_move_file_list = reactive_vals$formatted_move_file_list) 
        } 
      }
    }
  })
  
  observeEvent(
    input$MoveAction,
    ({
      
      # set move reqs (reqs are enforced ui checks)
      SetMoveRequirements(input, sample_type = "micronix")
      # fire move trigger
      updateTextInput(session = session, "ActionMoveMatrix", value = "Go"); Sys.sleep(.75)
      # print "Working..." user message
      output$MoveReturnMessage1 <- renderText({"Working..."})
    }))
  
  # move samples - the pkg move fun contains enforced checks
  observe({
    if(input$ActionMoveMatrix == "Go"){
      return_message <- sampleDB::MoveSamples(sample_type = input$MoveSampleType,
                            move_data = reactive_vals$formatted_move_file_list)
      
      # print user message
      output$MoveReturnMessage2 <- renderText({return_message})
      # reset trigger
      updateTextInput(session = session, "ActionMoveMatrix", value = "")
    }
  })
  
  # allow user to reset ui
  MoveReset(input, output)
  
  # present move examples
  MoveUploadExamples(database = database, output = output, sample_type = "micronix")
}

MoveUploadExamples <- function(database, output, sample_type){
  
  if(sample_type == "micronix"){
    ui.output <- list(
      InDatabasePlateOne = "InDatabasePlateOne",
      InDatabasePlateTwo = "InDatabasePlateTwo",
      PlateOneMove = "PlateOneMove",
      PlateTwoMove = "PlateTwoMove")
  }
  
  output[[ui.output$PlateOneMove]] <- renderTable({.ExamplePlateOneMove()}, striped = T, bordered = T)
  output[[ui.output$PlateTwoMove]] <- renderTable({.ExamplePlateTwoMove()}, striped = T, bordered = T)
  output[[ui.output$InDatabasePlateOne]] <- renderTable({.ExampleInDatabasePlateOne()}, striped = T, bordered = T)
  output[[ui.output$InDatabasePlateTwo]] <- renderTable({.ExampleInDatabasePlateTwo()}, striped = T, bordered = T)
}
