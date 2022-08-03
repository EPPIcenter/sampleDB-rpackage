
# App Function for Moving Samples

# Overview
# perform various checks of "user provided" file, reformat "user provided" file, and print user messages if file does not pass checks
# checks in ui can unfortunately be ignored by the user

MoveWetlabSamples <- function(session, input, database, output){
  
  # 1. create variable for storing formatted_move_file_list once it is created
  
  # 2. get path to user provided file(s), if path exists perform checks and reformat items in list
  
  observeEvent(input$MoveAction, {
    req(input[["MoveDataSet"]])

    users_move_file <- input[["MoveDataSet"]]
    if(!is.null(users_move_file)) {

      showNotification("Working...", id = "MoveNotification", type = "message", action = NULL, duration = NULL, closeButton = FALSE)

      move_data_list <- list()
      for(i in 1:length(users_move_file[,1])) {
        plate.name <- users_move_file[[i, 'name']] %>% gsub("\\.csv","",.)
        #if strip traxcer header toggle is on then strip the last 16 characters
        if(input$"MoveTraxcerStripFromFilename" == "strip"){
          plate.name <- substr(plate.name, 1, nchar(plate.name)-16)
        }
        move_data <- read.csv(users_move_file[[i, 'datapath']], header = F) %>% suppressWarnings() # will throw a pointless corrupt last line warning if file comes from excel
        #There have been bugs caused by empty colums
        #Find and remove columns on upload
        #Alternatively, could reject files that have empty columns but this
        #probably is okay on upload

        empty_columns <- colSums(is.na(move_data) | move_data == "") == nrow(move_data)
        move_data_list[[plate.name]] <- move_data[, !empty_columns]
      }

      #check colnames of user provided file
      MoveLogisticalResults <- c() #store the T or F results of the Logistical File Checks. All must be true in order to reformat
      for(item in names(move_data_list)){
        MoveFileLogisticalColnameCheck <- CheckLogisticalColnamesOfUserProvidedMicronixFile(input = input, output = output, users_upload_file = move_data_list[[item]], ui_elements = GetUIMoveElements("micronix")) 
        MoveLogisticalResults <- c(MoveLogisticalResults, MoveFileLogisticalColnameCheck)
      }
      
      if(all(MoveLogisticalResults)){
        #reformat move file(s)
        formatted_move_file_list <- FormatMicronixMoveData(ui_elements = GetUIMoveElements("micronix"), micronix_move_data = move_data_list, input = input)
        
        #after formatting takes place, check move content
        if(!is.null(formatted_move_file_list)){
          CheckFormattedMoveFile(output = output, database = database, sample_type = "micronix", formatted_move_file_list = formatted_move_file_list) 
        } 
      }

      output$MoveReturnMessage2 <- renderText({
        sampleDB::MoveSamples(sample_type = input$MoveSampleType,
                            move_data = formatted_move_file_list)
      })
    
      removeNotification(id = "MoveNotification")
    }
  })
  
  # allow user to reset ui
  MoveReset(input, output)
  
  # present move examples
  MoveUploadExamples(database = database, output = output, sample_type = "micronix")
  
  # add blank plate to database
  CreateEmptyMicronixPlate(input = input, output = output, database = database)

  # auto-filter freezer addresses in dropdown
  SmartFreezerDropdownFilter(database = database, session = session,
                             input = input,
                             location_ui = "CreateEmptyMicronixPlateLocation", 
                             levelI_ui = "CreateEmptyMicronixPlateLevelI", 
                             levelII_ui = "CreateEmptyMicronixPlateLevelII")
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

CreateEmptyMicronixPlate <- function(input, output, database){

  vals <- reactiveValues(data = NULL)
  
  # Show modal when button is clicked.
  observeEvent(input$CreateEmptyMicronixPlate, {
    showModal(dataModal(database = database))
  })
  
  observeEvent(input$CreatEmptyMicronixPlateoOk, {
    # create empty micronix plate using user input
    # use a "req" to require "CreateEmptyMicronixPlateID", "CreateEmptyMicronixPlateLocation", etc
    # throw error if user uses name that is already in the database
    conn <-  RSQLite::dbConnect(RSQLite::SQLite(), database)
    RSQLite::dbBegin(conn)
    sampleDB:::.UploadMicronixPlate(conn = conn,
                                    container_name = input[["CreateEmptyMicronixPlateID"]],
                                    container_barcode = input[["CreateEmptyMicronixPlateBarcode"]],
                                    freezer_address = list(location_name = input[["CreateEmptyMicronixPlateLocation"]],
                                                           level_I = input[["CreateEmptyMicronixPlateLevelI"]],
                                                           level_II = input[["CreateEmptyMicronixPlateLevelII"]]))
    
    RSQLite::dbCommit(conn)
    RSQLite::dbDisconnect(conn)
    vals$data <- ""
    removeModal()
  })
  
  output$CreateEmptyMicronixPlateMessage <- renderPrint({
    if(!is.null(vals$data)){
      "Created Empty Matrix Plate"
    }
  })
}

dataModal <- function(failed = FALSE, database) {
  modalDialog(
    HTML("<h2>Create a Blank Micronix Plate</h2>"),
    HTML("<h4>Fill out the section below</h4>"),
    br(),
    fluidRow(column(width = 6, HTML("<p>Human Readable Name</p>"), textInput("CreateEmptyMicronixPlateID", label = NULL, placeholder = "PRISM-2022-001")),
             column(width = 6,  HTML("<p>Barcode (Optional)</p>"), textInput("CreateEmptyMicronixPlateBarcode", label = NULL))),
    HTML("<p>Freezer Name</p>"), selectInput("CreateEmptyMicronixPlateLocation", label = NULL, width = '47%', choices = c("", sampleDB::CheckTable(database = database, "location")$location_name) %>% sort()),
    HTML("<p>Shelf Name</p>"), selectInput("CreateEmptyMicronixPlateLevelI", label = NULL, width = '47%', choices = NULL),
    HTML("<p>Basket Name</p>"), selectInput("CreateEmptyMicronixPlateLevelII", label = NULL, width = '47%', choices = NULL),
    if(failed){
      div(tags$b("ERROR", style = "color: red;")) 
    },
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("CreatEmptyMicronixPlateoOk", "OK")
    )
    
  )
}